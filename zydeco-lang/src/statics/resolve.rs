use super::err::TypeCheckError;
use crate::syntax::binder::{CtorV, DtorV, TermV, TypeV};
use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateTypeDeclaration { name: TypeV },
    DuplicateCtorDeclaration { name: CtorV },
    DuplicateDtorDeclaration { name: DtorV },
    EmptyDeclaration { name: String },
    ExternalDeclaration { name: String },
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
            DuplicateTypeDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            DuplicateCtorDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            DuplicateDtorDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            EmptyDeclaration { name } => {
                write!(
                    f,
                    "{} declared with neither type signature nor binding",
                    name
                )
            }
            ExternalDeclaration { name } => {
                write!(
                    f,
                    "{} declared as external but has implementation",
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
