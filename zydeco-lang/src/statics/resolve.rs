use super::err::TypeCheckError;
use crate::syntax::binder::{CtorV, DtorV, TermV, TypeV};
use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateDeclaration { name: String },
    EmptyDeclaration { name: String },
    UnknownIdentifier { name: String },
    UnboundTypeVariable { tvar: TypeV },
    UnboundTermVariable { var: TermV },
    UnknownConstructor { ctor: CtorV },
    UnknownDestructor { dtor: DtorV },
}
use NameResolveError::*;

impl fmt::Display for NameResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DuplicateDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            EmptyDeclaration { name } => {
                write!(
                    f,
                    "{} declared with neither type signature nor binding",
                    name
                )
            }
            UnknownIdentifier { name } => {
                write!(f, "Unknown identifier {}", name)
            }
            UnboundTypeVariable { tvar: name } => {
                write!(f, "Unbound type variable {}", name)
            }
            UnboundTermVariable { var: name } => {
                write!(f, "Unbound term variable {}", name)
            }
            UnknownConstructor { ctor } => {
                write!(f, "Unknown constructor {}", ctor)
            }
            UnknownDestructor { dtor: name } => {
                write!(f, "Unknown destructor {}", name)
            }
        }
    }
}

impl From<NameResolveError> for TypeCheckError {
    fn from(err: NameResolveError) -> Self {
        TypeCheckError::NameResolve(err)
    }
}
