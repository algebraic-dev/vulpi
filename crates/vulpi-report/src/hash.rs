//! Simple reporter for diagnostics using a hashmap to store things.

use crate::{Diagnostic, Reporter};
use std::collections::HashMap;
use vulpi_location::FileId;

#[derive(Default)]
pub struct HashReporter {
    map: HashMap<FileId, Vec<Diagnostic>>,
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

    fn diagnostics(&self, file: FileId) -> &[Diagnostic] {
        self.map.get(&file).map_or(&[], |v| v)
    }

    fn clear(&mut self, file: FileId) {
        self.map.remove(&file);
    }

    fn all_diagnostics(&self) -> Vec<Diagnostic> {
        self.map.values().flatten().cloned().collect()
    }

    fn has_errors(&self) -> bool {
        self.errored
    }
}
