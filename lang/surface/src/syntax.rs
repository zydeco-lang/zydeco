use derive_more::From;
use zydeco_syntax::*;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub bool, pub Vec<VarName>, pub T);

/* -------------------------------- Primitive ------------------------------- */

#[derive(Clone, Debug)]
pub enum Internal {
    VType,
    CType,
    Thunk,
    Ret,
    Unit,
    Int,
    Char,
    String,
    Top,
    OS,
    Monad,
    Algebra,
}

/* ------------------------------- Structural ------------------------------- */

/// `e1 e2` shaped application
#[derive(Clone, Debug)]
pub struct Appli<T>(pub Vec<T>);

/// `(...)` as paren-shaped container
#[derive(Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);

/* ----------------------------------- Use ---------------------------------- */

/// `..`
#[derive(Clone, Debug)]
pub struct UseAll;
/// `binder = origin`
#[derive(Clone, Debug)]
pub struct UseAlias(pub VarName, pub VarName);
/// the use tree
#[derive(Clone, From, Debug)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(Uses),
}
/// `a/b/c`
#[derive(Clone, Debug)]
pub struct UsePath(pub NameRef<UseEnum>);
/// `(aa, bb, cc)`
#[derive(Clone, Debug)]
pub struct Uses(pub Vec<UsePath>);

/* -------------------------------- TopLevel -------------------------------- */

// Note: use macro to declare externs?
#[derive(Clone, Debug)]
pub struct Modifiers<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, external, inner } = self;
        Ok(Modifiers { public: *public, external: *external, inner: f(inner)? })
    }
}

mod impls {
    use super::*;
    use std::fmt;

    impl<T> NameRef<T> {
        /// the last element of the name, should only be used if no path prefix exist
        pub fn leaf(&self) -> &T {
            assert!(self.1.is_empty());
            &self.2
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
