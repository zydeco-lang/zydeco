use super::err::TypeCheckError;
use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateDeclaration { name: String },
    EmptyDeclaration { name: String },
    UnknownIdentifier { name: String },
    UnboundTypeVariable { name: String },
    UnboundTermVariable { name: String },
    UnknownConstructor { name: String },
    UnknownDestructor { name: String },
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
            UnboundTypeVariable { name } => {
                write!(f, "Unbound type variable {}", name)
            }
            UnboundTermVariable { name } => {
                write!(f, "Unbound term variable {}", name)
            }
            UnknownConstructor { name } => {
                write!(f, "Unknown constructor {}", name)
            }
            UnknownDestructor { name } => {
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
