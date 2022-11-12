use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum NameResolveError<Ann> {
    DuplicateDeclaration { name: String, ann: Ann },
    UnknownIdentifier { name: String, ann: Ann },
}
use std::fmt;
use NameResolveError::*;
impl<Ann> fmt::Display for NameResolveError<Ann>
where
    Ann: std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DuplicateDeclaration { name, ann } => {
                write!(f, "{} declared multiple times ({:?})", name, ann)
            }
            UnknownIdentifier { name, ann } => {
                write!(f, "Unknown identifier {} at {:?}", name, ann)
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
