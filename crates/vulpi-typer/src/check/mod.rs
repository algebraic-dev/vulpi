//! This module defines the [Check] trait that is responsible for checking an expression against
//! a type and returning an elaborated expression.

use crate::types::Type;

pub trait Check {
    fn infer(&self, typ: Type);
}
