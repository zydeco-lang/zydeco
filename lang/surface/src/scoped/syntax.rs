pub use super::arena::*;
pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use std::collections::HashSet;
use zydeco_utils::cells::SingCell;

/* --------------------------------- Context -------------------------------- */

/// Context is what variables we *can use* at a given term site.
#[derive(Clone, Debug)]
pub struct Context<T> {
    pub defs: im::HashMap<DefId, T>,
}

mod impls_context {
    use super::*;
    use std::ops::{Add, AddAssign, Index};

    impl<T> From<im::HashMap<DefId, T>> for Context<T> {
        fn from(defs: im::HashMap<DefId, T>) -> Self {
            Self { defs }
        }
    }

    impl<T> Into<im::HashMap<DefId, T>> for Context<T> {
        fn into(self) -> im::HashMap<DefId, T> {
            self.defs
        }
    }

    impl<T> AsRef<im::HashMap<DefId, T>> for Context<T> {
        fn as_ref(&self) -> &im::HashMap<DefId, T> {
            &self.defs
        }
    }

    impl<T> Add for Context<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let Context { mut defs } = self;
            defs.extend(other.defs);
            Self { defs }
        }
    }
    impl<T> Add<(DefId, T)> for Context<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, (def, t): (DefId, T)) -> Self {
            let Context { mut defs } = self;
            defs.insert(def, t);
            Self { defs }
        }
    }
    impl<T> AddAssign<(DefId, T)> for Context<T>
    where
        T: Clone,
    {
        fn add_assign(&mut self, (def, t): (DefId, T)) {
            *self = self.clone() + (def, t);
        }
    }
    impl<T> Index<&DefId> for Context<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.defs[def]
        }
    }
}

/* -------------------------------- CoContext ------------------------------- */

/// CoContext is what variables we *have used* at a given term site.
#[derive(Clone, Debug)]
pub struct CoContext<T> {
    pub defs: im::HashMap<DefId, T>,
}

mod impls_co_context {
    use super::*;
    use std::ops::{Add, AddAssign, Index, Sub, SubAssign};
    use zydeco_utils::{imc::ImmutableMonoidMap, monoid::Monoid};

    // implementing the `ImmutableMonoidMap` trait for `CoContext`

    impl<T> From<im::HashMap<DefId, T>> for CoContext<T> {
        fn from(defs: im::HashMap<DefId, T>) -> Self {
            Self { defs }
        }
    }
    impl<T> Into<im::HashMap<DefId, T>> for CoContext<T> {
        fn into(self) -> im::HashMap<DefId, T> {
            self.defs
        }
    }
    impl<T> AsRef<im::HashMap<DefId, T>> for CoContext<T> {
        fn as_ref(&self) -> &im::HashMap<DefId, T> {
            &self.defs
        }
    }

    impl<T: Clone> Monoid for CoContext<T> {}

    impl<T: Clone> Default for CoContext<T> {
        fn default() -> Self {
            Self::new()
        }
    }
    impl<T> Add for CoContext<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let CoContext { mut defs } = self;
            defs.extend(other.defs);
            Self { defs }
        }
    }
    impl<T> Add<(DefId, T)> for CoContext<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, (def, t): (DefId, T)) -> Self {
            let CoContext { mut defs } = self;
            defs.insert(def, t);
            Self { defs }
        }
    }
    impl<S, T> Sub<Context<S>> for CoContext<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn sub(mut self, Context { defs: cx_defs }: Context<S>) -> Self {
            for def in cx_defs.keys() {
                self = self - def;
            }
            self
        }
    }
    impl<T> Sub<&DefId> for CoContext<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn sub(self, def: &DefId) -> Self {
            let CoContext { mut defs } = self;
            defs.remove(def);
            Self { defs }
        }
    }
    impl<T> AddAssign<(DefId, T)> for CoContext<T>
    where
        T: Clone,
    {
        fn add_assign(&mut self, (def, t): (DefId, T)) {
            *self = self.clone() + (def, t);
        }
    }
    impl<T> SubAssign<&DefId> for CoContext<T>
    where
        T: Clone,
    {
        fn sub_assign(&mut self, def: &DefId) {
            self.defs.remove(def);
        }
    }
    impl<T> Index<&DefId> for CoContext<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.defs[def]
        }
    }

    impl<T> IntoIterator for CoContext<T>
    where
        T: Clone,
    {
        type Item = (DefId, T);
        type IntoIter = im::hashmap::ConsumingIter<(DefId, T)>;
        fn into_iter(self) -> Self::IntoIter {
            self.defs.into_iter()
        }
    }
}

/* -------------------------------- TopLevel -------------------------------- */

pub struct SccDeclarations<'decl>(pub &'decl HashSet<DeclId>);

/* -------------------------------- Primitive ------------------------------- */

/// Primitive definitions
///
/// Collects the primitive definitions from the surface syntax.
/// To add a new primitive form:
/// 1. Add a new field to this struct.
/// 2. Check if the form can be introduced during desugaring, e.g. annotations.
///    If so, add it to [`crate::bitter::syntax::PrimTerms`] too.
/// 3. Implement the `check` method to ensure all fields are filled.
#[derive(Default)]
pub struct PrimDefs {
    pub vtype: SingCell<DefId>,
    pub ctype: SingCell<DefId>,
    pub thk: SingCell<DefId>,
    pub ret: SingCell<DefId>,
    pub unit: SingCell<DefId>,
    pub int: SingCell<DefId>,
    pub char: SingCell<DefId>,
    pub string: SingCell<DefId>,
    pub os: SingCell<DefId>,
    pub monad: SingCell<DefId>,
    pub algebra: SingCell<DefId>,
}

mod impls {
    use super::*;
    use crate::scoped::err::*;
    impl PrimDefs {
        pub fn check(&self) -> Result<()> {
            self.vtype.get_or_else(|| ResolveError::MissingPrim("VType"))?;
            self.ctype.get_or_else(|| ResolveError::MissingPrim("CType"))?;
            self.thk.get_or_else(|| ResolveError::MissingPrim("Thk"))?;
            self.ret.get_or_else(|| ResolveError::MissingPrim("Ret"))?;
            self.unit.get_or_else(|| ResolveError::MissingPrim("Unit"))?;
            self.int.get_or_else(|| ResolveError::MissingPrim("Int"))?;
            self.char.get_or_else(|| ResolveError::MissingPrim("Char"))?;
            self.string.get_or_else(|| ResolveError::MissingPrim("String"))?;
            self.os.get_or_else(|| ResolveError::MissingPrim("OS"))?;
            self.monad.get_or_else(|| ResolveError::MissingPrim("Monad"))?;
            self.algebra.get_or_else(|| ResolveError::MissingPrim("Algebra"))?;
            Ok(())
        }
    }
}
