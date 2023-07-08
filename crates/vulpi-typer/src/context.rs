use vulpi_report::Report;
use vulpi_storage::interner::Symbol;

use crate::types::{Level, Scheme};

#[derive(Clone)]
pub struct Env {
    pub variables: im_rc::HashMap<Symbol, Scheme>,
    pub type_variables: im_rc::HashSet<Symbol, ()>,
    pub reporter: Report,
    pub level: Level,
}
