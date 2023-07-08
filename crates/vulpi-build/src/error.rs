use std::collections::HashMap;

use vulpi_report::{Diagnostic, Reporter};
use vulpi_storage::id::{File, Id};

#[derive(Default)]
pub struct HashReporter {
    map: HashMap<Id<File>, Vec<Diagnostic>>,
    errored: bool,
}

impl HashReporter {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Reporter for HashReporter {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.errored = true;
        self.map
            .entry(diagnostic.location().file)
            .or_default()
            .push(diagnostic);
    }

    fn diagnostics(&self, file: Id<File>) -> &[Diagnostic] {
        self.map.get(&file).map_or(&[], |v| v)
    }

    fn clear(&mut self, file: Id<File>) {
        self.map.remove(&file);
    }

    fn all_diagnostics(&self) -> Vec<Diagnostic> {
        self.map.values().flatten().cloned().collect()
    }

    fn has_errors(&self) -> bool {
        self.errored
    }
}
