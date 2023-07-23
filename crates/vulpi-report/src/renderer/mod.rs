use std::path::PathBuf;

use vulpi_location::Byte;
use vulpi_storage::file_system::FileSystem;
use yansi::Paint;

use crate::{Color, Diagnostic, Style, Text, Word};

pub trait Renderer<T> {
    fn render(&self, ctx: &T, writer: &mut impl std::io::Write) -> std::io::Result<()>;
}

#[derive(Debug)]
pub struct LineGuide {
    line_bytes: Vec<(usize, usize)>,
}

impl LineGuide {
    pub fn new(content: &str) -> Self {
        let mut line_bytes = Vec::new();

        let mut line_start = 0;
        let mut line_end = 0;

        for (i, c) in content.char_indices() {
            if c == '\n' {
                line_bytes.push((line_start, line_end));
                line_start = i + 1;
            }

            line_end = i + 1;
        }

        line_bytes.push((line_start, line_end));

        Self { line_bytes }
    }

    pub fn to_line_and_column(&self, place: Byte) -> Option<(usize, usize)> {
        let place = place.0;

        for (i, (start, end)) in self.line_bytes.iter().enumerate() {
            if place >= *start && place <= *end {
                return Some((i, place - start));
            }
        }

        None
    }
}

pub struct Classic<'a> {
    fs: &'a dyn FileSystem<PathBuf, String>,
    cwd: PathBuf,
}

impl<'a> Classic<'a> {
    pub fn new(fs: &'a (dyn FileSystem<PathBuf, String> + 'static), cwd: PathBuf) -> Self {
        Self { fs, cwd }
    }
}

fn get_paint(color: &Color) -> fn(String) -> yansi::Paint<String> {
    match color {
        Color::Fst => Paint::red,
        Color::Snd => Paint::yellow,
        Color::Trd => Paint::blue,
        Color::Fth => Paint::green,
    }
}

impl<'a> Renderer<Classic<'a>> for (&Color, &str) {
    fn render(&self, _: &Classic<'a>, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        let paint = get_paint(self.0);
        write!(writer, "{}", paint(self.1.to_string()))
    }
}

impl<'a> Renderer<Classic<'a>> for (&Style, &str) {
    fn render(&self, _: &Classic<'a>, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        match self.0 {
            Style::Bold => write!(writer, "{}", Paint::new(self.1).bold()),
            Style::Dimmed => write!(writer, "{}", Paint::new(self.1).dimmed()),
            Style::Normal => write!(writer, "{}", self.1),
        }
    }
}

impl<'a> Renderer<Classic<'a>> for Word {
    fn render(&self, _: &Classic<'a>, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        let Word(style, color, text) = self;

        let paint = get_paint(color)(text.to_string());

        let paint = match style {
            Style::Bold => paint.bold(),
            Style::Dimmed => paint.dimmed(),
            Style::Normal => paint,
        };

        write!(writer, "{}", paint)
    }
}

impl<'a> Renderer<Classic<'a>> for Text {
    fn render(&self, ctx: &Classic<'a>, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Text::Phrase(words) => {
                for (i, word) in words.iter().enumerate() {
                    word.render(ctx, writer)?;

                    if i != words.len() - 1 {
                        write!(writer, " ")?;
                    }
                }

                Ok(())
            }
            Text::Styled(style, t) => (style, t.as_str()).render(ctx, writer),
            Text::Colored(color, t) => (color, t.as_str()).render(ctx, writer),
            Text::Text(text) => write!(writer, "{}", text),
            Text::Break => writeln!(writer),
        }
    }
}

#[derive(Default)]
pub struct Reader(String);

impl ToString for Reader {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

impl std::io::Write for Reader {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.push_str(std::str::from_utf8(buf).unwrap());
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<'a> Renderer<Classic<'a>> for Diagnostic {
    fn render(&self, ctx: &Classic<'a>, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        // At this point we are probably sure that the file exists, so we can unwrap.
        let path = ctx.fs.path(self.location().file).unwrap();
        let relative = path.strip_prefix(&ctx.cwd).unwrap();

        let content = ctx.fs.read(self.location().file).unwrap();

        let range = self.location().range;

        let line_guide = LineGuide::new(content);

        let start = line_guide.to_line_and_column(range.start).unwrap();
        let end = line_guide.to_line_and_column(range.end).unwrap();

        write!(
            writer,
            "{}:{}:{}: ",
            relative.display(),
            start.0 + 1,
            start.1 + 1
        )?;

        self.message().render(ctx, writer)?;

        writeln!(writer)?;
        writeln!(writer)?;

        let is_inline = start.0 == end.0;

        let lines = content.lines().collect::<Vec<_>>();

        let minimum = start.0.saturating_sub(2);
        let maximum = (end.0 + 2).min(lines.len());

        for (i, line) in lines[minimum..maximum].iter().enumerate() {
            let line_number = minimum + i + 1;

            write!(writer, "  {:>3} | ", line_number)?;

            if is_inline && line_number == start.0 + 1 {
                let line = line.to_string();

                writeln!(writer, "{}", line)?;

                writeln!(
                    writer,
                    "      | {}{}",
                    " ".repeat(start.1),
                    "^".repeat(end.1 - start.1)
                )?;
            } else if is_inline && line_number == end.0 + 1 {
                let mut line = line.to_string();

                line.insert(end.1 + 1, '^');

                writeln!(writer, "{}", line)?;
            } else {
                writeln!(writer, "{}", line)?;
            }
        }

        writeln!(writer)
    }
}
