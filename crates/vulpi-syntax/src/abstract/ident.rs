use vulpi_macros::{node_of, Tree};
use vulpi_show::{Show, TreeDisplay};
use vulpi_storage::interner::Symbol;

use super::*;

#[derive(Debug, Clone)]
#[node_of]
pub struct Ident(#[not] pub Symbol, #[not] pub Range<Byte>);

impl Ident {
    pub fn generate(symbol: Symbol) -> Self {
        Self(symbol, Byte(0)..Byte(0))
    }
}

impl Show for Ident {
    fn show(&self) -> vulpi_show::TreeDisplay {
        TreeDisplay::label(&self.0.get())
    }
}

#[derive(Debug, Clone, Tree)]
#[node_of]
pub struct Qualified {
    #[not]
    pub segments: Vec<Ident>,
    #[not]
    pub last: Ident,
    #[not]
    pub range: Range<Byte>,
}
