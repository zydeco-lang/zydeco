use derive_more::From;
use zydeco_syntax::*;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<VarName>, pub T);

/* ----------------------------------- Use ---------------------------------- */

/// `..`
#[derive(Clone, Debug)]
pub struct UseAll;
/// `binder = origin`
#[derive(Clone, Debug)]
pub struct UseAlias(pub VarName, pub VarName);
#[derive(Clone, From, Debug)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(Uses),
}
#[derive(Clone, Debug)]
pub struct UsePath(pub NameRef<UseEnum>);
#[derive(Clone, Debug)]
pub struct Uses(pub Vec<UsePath>);

mod impls {
    use super::*;
    use std::fmt;

    impl fmt::Display for NameRef<VarName> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let NameRef(path, VarName(name)) = self;
            for VarName(name) in path {
                write!(f, "{}/", name)?;
            }
            write!(f, "{}", name)
        }
    }
}
