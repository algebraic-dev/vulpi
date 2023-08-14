//! Module for resolution of modules that are out of the current crate. This module is responsible
//! for loading the modules from the disk or other places to resolve them.

use vulpi_syntax::concrete::tree::Program;

use crate::{error::ResolverErrorKind, paths};

/// Trait for IO operations.
pub trait IO {
    fn read_module(&mut self, id: paths::Path) -> Result<Program, ResolverErrorKind>;
}
