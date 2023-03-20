use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateDeclaration { name: String },
    EmptyDeclaration { name: String },
    UnknownIdentifier { name: String },
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
        }
    }
}
