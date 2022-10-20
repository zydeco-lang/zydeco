use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum NameResolveError<Ann> {
    DuplicateDeclaration { name: String, ann: Ann },
    UnknownIdentifier { name: String, ann: Ann },
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
