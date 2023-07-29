//! Simple renderer for diagnostics.

pub mod classic;

use vulpi_location::Byte;

/// Trait for rendering diagnostics.
pub trait Renderer<T> {
    fn render(&self, ctx: &T, writer: &mut impl std::io::Write) -> std::io::Result<()>;
}

/// A guide for lines and columns.
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

/// A reader is just a wrapper around a string for [std::io::Write].
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
