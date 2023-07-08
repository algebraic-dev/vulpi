//! Module for handling errors that can occur during the compilation process. It's used to report
//! errors to the user.

use std::{cell::RefCell, rc::Rc};

use vulpi_location::Location;
use vulpi_storage::id::{File, Id};

pub mod renderer;

/// A type for representing the severity of a [Diagnostic].
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// A type for representing the color of a [Word]. It's all numerated because it's easier to change
/// the color of a word according to what the user wants.
pub enum Color {
    Fst,
    Snd,
    Trd,
    Fth,
}

/// A type for representing the style of a [Word].
pub enum Style {
    Bold,
    Dimmed,
    Normal,
}

/// A type for representing a word in a [Text].
pub struct Word(Style, Color, String);

/// A type for representing a text. It's used to generate error messages.
pub enum Text {
    Phrase(Vec<Word>),
    Styled(Style, String),
    Colored(Color, String),
    Text(String),
    Break,
}

impl From<&str> for Text {
    fn from(s: &str) -> Self {
        Text::Text(s.to_owned())
    }
}

impl From<String> for Text {
    fn from(s: String) -> Self {
        Text::Text(s)
    }
}

pub struct Marker {
    pub position: Location,
    pub subtitle: Option<Text>,
}

/// Errors that can occur during the compilation process.
pub trait IntoDiagnostic {
    fn code(&self) -> Option<usize> {
        None
    }

    fn hint(&self) -> Option<Text> {
        None
    }

    fn message(&self) -> Text;

    fn markers(&self) -> Vec<Marker>;

    fn severity(&self) -> Severity;

    fn location(&self) -> Location;
}

#[derive(Clone)]
pub struct Diagnostic(Rc<dyn IntoDiagnostic>);

impl Diagnostic {
    pub fn new(diagnostic: impl IntoDiagnostic + 'static) -> Self {
        Self(Rc::new(diagnostic))
    }

    pub fn code(&self) -> Option<usize> {
        self.0.code()
    }

    pub fn hint(&self) -> Option<Text> {
        self.0.hint()
    }

    pub fn message(&self) -> Text {
        self.0.message()
    }

    pub fn markers(&self) -> Vec<Marker> {
        self.0.markers()
    }

    pub fn severity(&self) -> Severity {
        self.0.severity()
    }

    pub fn location(&self) -> Location {
        self.0.location()
    }
}

/// A reporter is a structure that gets and record errors. It's used to store and report errors to
/// the user.
pub trait Reporter {
    /// Reports a new error to the reporter.
    fn report(&mut self, diagnostic: Diagnostic);

    /// Gets all the diagnostics of a file.
    fn diagnostics(&self, file: Id<File>) -> &[Diagnostic];

    /// Get all diagnostics
    fn all_diagnostics(&self) -> Vec<Diagnostic>;

    /// Clears all the diagnostics of a file. It's used for LSP.
    fn clear(&mut self, file: Id<File>);

    /// Check if has errors
    fn has_errors(&self) -> bool;
}

/// A structure that stores and reports errors to the user. It's inside a Rc or Arc because it
/// needs to be shared between all steps of the compiler
#[derive(Clone)]
pub struct Report(Rc<RefCell<dyn Reporter>>);

impl Report {
    pub fn new(reporter: impl Reporter + 'static) -> Self {
        Self(Rc::new(RefCell::new(reporter)))
    }

    pub fn report(&self, diagnostic: Diagnostic) {
        self.0.borrow_mut().report(diagnostic);
    }

    pub fn diagnostics(&self, file: Id<File>) -> Vec<Diagnostic> {
        self.0.borrow().diagnostics(file).to_vec()
    }

    pub fn all_diagnostics(&self) -> Vec<Diagnostic> {
        self.0.borrow().all_diagnostics()
    }

    pub fn clear(&self, file: Id<File>) {
        self.0.borrow_mut().clear(file);
    }

    pub fn has_errors(&self) -> bool {
        self.0.borrow().has_errors()
    }
}
