use derive_more::From;
use zydeco_syntax::*;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub bool, pub Vec<VarName>, pub T);

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

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct Modifiers<T> {
    pub public: bool,
    pub inner: T,
}
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, inner } = self;
        Ok(Modifiers { public: *public, inner: f(inner)? })
    }
}

mod impls {
    use super::*;
    use std::fmt;

    impl NameRef<VarName> {
        pub fn syntactic_local(&self) -> Option<VarName> {
            if self.0 && self.1.is_empty() {
                Some(self.2.clone())
            } else {
                None
            }
        }
    }

    impl From<Vec<VarName>> for NameRef<()> {
        fn from(path: Vec<VarName>) -> Self {
            NameRef(false, path, ())
        }
    }

    impl Extend<VarName> for NameRef<()> {
        fn extend<T: IntoIterator<Item = VarName>>(&mut self, iter: T) {
            self.1.extend(iter);
        }
    }

    impl IntoIterator for NameRef<VarName> {
        type Item = VarName;

        type IntoIter = std::vec::IntoIter<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            let NameRef(_, mut path, name) = self;
            path.push(name);
            path.into_iter()
        }
    }

    impl fmt::Display for NameRef<VarName> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let NameRef(root, path, VarName(name)) = self;
            if *root {
                write!(f, "root/")?;
            }
            for VarName(name) in path {
                write!(f, "{}/", name)?;
            }
            write!(f, "{}", name)
        }
    }
}
