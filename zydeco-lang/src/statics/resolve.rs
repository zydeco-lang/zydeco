use crate::utils::ann::Ann;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateDeclaration { name: String, ann: Ann },
    EmptyDeclaration { name: String, ann: Ann },
    UnknownIdentifier { name: String, ann: Ann },
}
use NameResolveError::*;

impl fmt::Display for NameResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DuplicateDeclaration { name, ann } => {
                write!(f, "{} declared multiple times ({:?})", name, ann)
            }
            EmptyDeclaration { name, ann } => {
                write!(f, "{} declared with neither type signature nor binding ({:?})", name, ann)
            }
            UnknownIdentifier { name, ann } => {
                write!(f, "Unknown identifier {} ({:?})", name, ann)
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NamespaceSort {
    Type,
    Value,
    Ctor,
    Dtor,
}

pub struct NamespaceResolver {
    pub namespaces: HashMap<(NamespaceSort, String), usize>,
}
