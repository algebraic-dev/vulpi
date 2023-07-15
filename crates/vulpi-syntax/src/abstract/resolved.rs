use std::ops::Range;

use vulpi_location::Byte;
use vulpi_macros::{node_of, Tree};
use vulpi_storage::interner::Symbol;

use super::*;

#[derive(Debug, Clone, Tree)]
#[node_of(TypeImpl)]
pub struct ResolvedQualified {
    #[not]
    pub path: Symbol,
    #[not]
    pub last: Symbol,
    #[not]
    pub range: Range<Byte>,
}
