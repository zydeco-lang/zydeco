//! Environments used by the Zydeco type checker.

use super::*;
use derive_more::{Deref, DerefMut, From, Index, IndexMut, Into};
use std::ops::{Add, AddAssign};
use zydeco_utils::prelude::With;

/// Environment mapping definitions to annotations or substitutions.
#[derive(Clone, Debug, From, Into, Deref, DerefMut, Index, IndexMut)]
pub struct Env<T>(rpds::HashTrieMapSync<DefId, T>);

mod impls_env {
    use super::*;

    impl<Iter, T> Add<Iter> for Env<T>
    where
        T: Clone,
        Iter: IntoIterator<Item = (DefId, T)>,
    {
        type Output = Self;
        fn add(self, iter: Iter) -> Self {
            let Env(defs) = self;
            Self(iter.into_iter().fold(defs, |defs, (def, ann)| defs.insert(def, ann)))
        }
    }
    impl<Iter, T> AddAssign<Iter> for Env<T>
    where
        T: Clone,
        Iter: IntoIterator<Item = (DefId, T)>,
    {
        fn add_assign(&mut self, iter: Iter) {
            let Env(defs) = self;
            *defs = iter.into_iter().fold(defs.clone(), |defs, (def, ann)| defs.insert(def, ann));
        }
    }
    impl<T> Env<T> {
        pub fn new() -> Self {
            Self(rpds::HashTrieMapSync::new_sync())
        }

        pub fn len(&self) -> usize {
            self.0.size()
        }

        pub fn is_empty(&self) -> bool {
            self.0.is_empty()
        }
    }
    impl<T> Default for Env<T> {
        fn default() -> Self {
            Self::new()
        }
    }
    impl<T> FromIterator<(DefId, T)> for Env<T>
    where
        T: Clone,
    {
        fn from_iter<I: IntoIterator<Item = (DefId, T)>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }

    impl<T: Clone> IntoIterator for Env<T> {
        type Item = (DefId, T);
        type IntoIter = std::vec::IntoIter<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            self.0.iter().map(|(def, ann)| (*def, ann.clone())).collect::<Vec<_>>().into_iter()
        }
    }

    impl<'a, T> IntoIterator for &'a Env<T> {
        type Item = (&'a DefId, &'a T);
        type IntoIter = <&'a rpds::HashTrieMapSync<DefId, T> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            (&self.0).into_iter()
        }
    }
}

/// Existential witnesses visible at a typing site.
///
/// The set is persistent: extending or narrowing a scope produces a new value,
/// so a typing environment also records the lexical scope in which it arose.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SkolemScope(rpds::HashTrieSetSync<AbstId>);

mod impls_skolem_scope {
    use super::*;

    impl SkolemScope {
        pub fn contains(&self, skolem: &AbstId) -> bool {
            self.0.contains(skolem)
        }

        pub fn with(&self, skolem: AbstId) -> Self {
            Self(self.0.insert(skolem))
        }

        pub fn without<'a>(&self, skolems: impl IntoIterator<Item = &'a AbstId>) -> Self {
            skolems.into_iter().fold(self.clone(), |scope, skolem| Self(scope.0.remove(skolem)))
        }

        pub fn intersection(&self, other: &Self) -> Self {
            Self(self.0.iter().filter(|skolem| other.contains(skolem)).copied().collect())
        }

        pub fn union(&self, other: &Self) -> Self {
            Self(self.0.iter().chain(other.0.iter()).copied().collect())
        }
    }

    impl FromIterator<AbstId> for SkolemScope {
        fn from_iter<I: IntoIterator<Item = AbstId>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }
}

/// Typing environment mapping definitions to annotations together with the
/// existential witnesses visible at that point.
#[derive(Clone, Debug)]
pub struct TyEnv {
    defs: Env<AnnId>,
    skolems: SkolemScope,
}

mod impls_ty_env {
    use super::*;

    impl TyEnv {
        pub fn new() -> Self {
            Self { defs: Env::new(), skolems: SkolemScope::default() }
        }

        pub fn skolem_scope(&self) -> &SkolemScope {
            &self.skolems
        }

        pub fn with_skolem(mut self, skolem: AbstId) -> Self {
            self.skolems = self.skolems.with(skolem);
            self
        }

        pub fn with_skolem_scope(mut self, scope: SkolemScope) -> Self {
            self.skolems = scope;
            self
        }

        /// Whether this environment preserves every binding and skolem visible in `base`.
        pub fn is_extension_of(&self, base: &Self) -> bool {
            let preserves_definitions = self.defs.0.ptr_eq(&base.defs.0)
                || (self.defs.len() >= base.defs.len()
                    && base.defs.iter().all(|(def, ann)| self.defs.get(def) == Some(ann)));
            let preserves_skolems = self.skolems.0.ptr_eq(&base.skolems.0)
                || (self.skolems.0.size() >= base.skolems.0.size()
                    && base.skolems.0.iter().all(|skolem| self.skolems.contains(skolem)));
            preserves_definitions && preserves_skolems
        }

        pub fn monadic_new(tycker: &mut Tycker<'_>, ori: &TyEnv) -> Self {
            use zydeco_surface::arena::ArenaAccess;

            let defs = ori
                .clone()
                .into_iter()
                .filter(|(def, _)| {
                    !matches!(tycker.statics.annotations_var[def], AnnId::Type(_))
                        || tycker.statics.global_defs.get(def).is_some()
                })
                .collect();
            Self { defs, skolems: ori.skolems.clone() }
        }
        pub fn recursively_get_type(&self, tycker: &Tycker<'_>, def: &DefId) -> Option<&AnnId> {
            let ann = self.defs.get(def)?;
            match ann {
                | AnnId::Set | AnnId::Kind(_) => unreachable!(),
                | AnnId::Type(ty) => {
                    let ty = tycker.statics.types_pre[ty].to_owned();
                    match ty {
                        | Fillable::Done(Type::Var(ref def)) => {
                            self.recursively_get_type(tycker, def)
                        }
                        | _ => Some(ann),
                    }
                }
            }
        }
    }

    impl Default for TyEnv {
        fn default() -> Self {
            Self::new()
        }
    }

    impl std::ops::Deref for TyEnv {
        type Target = Env<AnnId>;

        fn deref(&self) -> &Self::Target {
            &self.defs
        }
    }

    impl std::ops::DerefMut for TyEnv {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.defs
        }
    }

    impl<Iter> Add<Iter> for TyEnv
    where
        Iter: IntoIterator<Item = (DefId, AnnId)>,
    {
        type Output = Self;

        fn add(mut self, iter: Iter) -> Self::Output {
            self += iter;
            self
        }
    }

    impl<Iter> AddAssign<Iter> for TyEnv
    where
        Iter: IntoIterator<Item = (DefId, AnnId)>,
    {
        fn add_assign(&mut self, iter: Iter) {
            self.defs += iter;
        }
    }

    impl FromIterator<(DefId, AnnId)> for TyEnv {
        fn from_iter<I: IntoIterator<Item = (DefId, AnnId)>>(iter: I) -> Self {
            Self { defs: iter.into_iter().collect(), skolems: SkolemScope::default() }
        }
    }

    impl IntoIterator for TyEnv {
        type Item = (DefId, AnnId);
        type IntoIter = <Env<AnnId> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.defs.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a TyEnv {
        type Item = (&'a DefId, &'a AnnId);
        type IntoIter = <&'a Env<AnnId> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            (&self.defs).into_iter()
        }
    }
}

/// substituting types for type variables;
/// S for substitution / statics
/// PLEASE NOTE: when performing substitution, the environment should be applied one by one
///
/// `.info`: the environment of type variables; should be applied from the first to the last
// Note: should be ordered?
/// A typed payload paired with a typing environment.
pub type TyEnvT<T> = With<TyEnv, T>;

/// Substitution environment mapping defs to defs.
pub type SubstEnv = Env<DefId>;
/// Substitution environment for abstract types.
pub type SubstAbstEnv = rpds::HashTrieMapSync<AbstId, AbstId>;
pub type SubstEnvT<T> = With<SubstEnv, T>;

/// Structure environment used during algebra translation.
#[derive(Clone, Default)]
pub struct StrEnv {
    // Todo: remove this useless non-sense
    pub def_map: rpds::HashTrieMapSync<DefId, AbstId>,
    pub absts: rpds::HashTrieMapSync<AbstId, ValueId>,
}

mod impls_str_env {
    use super::*;

    impl StrEnv {
        pub fn new() -> Self {
            Self::default()
        }
        // pub fn extended(
        //     &self, abst: AbstId, def: Option<DefId>, str: impl MonConstruct<ValueId>,
        //     tycker: &mut Tycker, env: MonEnv,
        // ) -> Self {
        //     let mut new = self.clone();
        //     if let Some(def) = def {
        //         new.def_map.insert(def, abst);
        //     }
        //     let Ok((_env, str)) = str.mbuild(tycker, env) else { unreachable!() };
        //     new.absts.insert(abst, str);
        //     new
        // }
    }
}

/// A payload paired with a structure environment.
pub type StrEnvT<T> = With<StrEnv, T>;

/// The ordinary type constructors selected for one monadic translation.
#[derive(Clone, Copy)]
pub struct MonadicTypeBasis {
    pub monad: TypeId,
    pub algebra: TypeId,
}

/// Monadic translation environment (types, substitutions, and structure state).
#[derive(Clone)]
pub struct MonEnv {
    pub ty: TyEnv,
    pub subst: SubstEnv,
    pub subst_abst: SubstAbstEnv,
    pub structure: StrEnv,
    pub basis: MonadicTypeBasis,
    pub monad_ty: TypeId,
    pub monad_impl: ValueId,
}

/* --------------------------- Environment Interning ------------------------- */

/// Typing environments compare by structural equivalence: clones of one
/// environment share persistent allocations, so pointer equality answers the
/// common case in O(1), while independently built environments fall back to a
/// content comparison.
impl PartialEq for TyEnv {
    fn eq(&self, other: &Self) -> bool {
        if self.defs.0.ptr_eq(&other.defs.0) && self.skolems.0.ptr_eq(&other.skolems.0) {
            return true;
        }
        self.defs.len() == other.defs.len()
            && self.defs.iter().all(|(def, ann)| other.defs.get(def) == Some(ann))
            && self.skolems.0.size() == other.skolems.0.size()
            && self.skolems.0.iter().all(|skolem| other.skolems.contains(skolem))
    }
}

impl Eq for TyEnv {}

impl std::hash::Hash for TyEnv {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Both components are unordered collections, so fold the item hashes
        // with xor: equal environments hash equally regardless of traversal
        // order, which the content-based equality requires.
        let mut acc = 0u64;
        for (def, ann) in self.defs.iter() {
            let def = def.key_space().as_u64() ^ u64::from(def.raw().into_u32());
            let ann = match ann {
                | AnnId::Set => 1,
                | AnnId::Kind(kind) => kind.key_space().as_u64() ^ 2,
                | AnnId::Type(ty) => ty.key_space().as_u64() ^ 3,
            };
            acc ^= def ^ ann;
        }
        for skolem in self.skolems.0.iter() {
            acc ^= skolem.key_space().as_u64() ^ u64::from(skolem.raw().into_u32());
        }
        acc.hash(state);
    }
}

/// Cache of typing environments shared between the millions of per-node
/// arena entries that repeat one of comparatively few environments. Interning
/// them leaves the arena holding only shared pointers.
#[derive(Clone, Debug, Default)]
pub struct TyEnvInterner {
    table: std::collections::HashMap<TyEnv, std::sync::Arc<TyEnv>>,
    /// The most recently interned environment. Adjacent checking sites almost
    /// always share one environment, so a pointer comparison against this
    /// entry answers most requests without touching the hash table.
    last: Option<(TyEnv, std::sync::Arc<TyEnv>)>,
}

impl TyEnvInterner {
    pub fn intern(&mut self, env: &TyEnv) -> std::sync::Arc<TyEnv> {
        // Adjacent sites usually share one environment; a pointer comparison
        // against the previous entry answers those requests in O(1). Content
        // equality is resolved by the hash table for everything else.
        if let Some((last, shared)) = &self.last
            && last.defs.0.ptr_eq(&env.defs.0)
            && last.skolems.0.ptr_eq(&env.skolems.0)
        {
            return shared.clone();
        }
        let shared =
            self.table.entry(env.clone()).or_insert_with(|| std::sync::Arc::new(env.clone()));
        self.last = Some((env.clone(), shared.clone()));
        shared.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_utils::arena::{KeySpaceId, derived_id};

    fn def(slot: u32) -> DefId {
        derived_id(KeySpaceId::derive(0xEE11_EE11, 0, 0, 0), slot)
    }

    fn abst(slot: u32) -> AbstId {
        derived_id(KeySpaceId::derive(0xEE11_EE12, 0, 0, 0), slot)
    }

    fn sample() -> TyEnv {
        TyEnv::from_iter([(
            def(0),
            AnnId::Type(derived_id(KeySpaceId::derive(0xEE11_EE13, 0, 0, 0), 0)),
        )])
        .with_skolem(abst(0))
    }

    #[test]
    fn interning_shares_clones_and_preserves_content() {
        let mut interner = TyEnvInterner::default();
        let env = sample();

        let first = interner.intern(&env);
        let clone = env.clone();
        let second = interner.intern(&clone);

        // Clones of one environment intern to the same shared value...
        assert!(std::sync::Arc::ptr_eq(&first, &second));
        // ...and the stored value keeps the environment's exact content.
        assert_eq!(
            second.as_ref().get(&def(0)),
            Some(&AnnId::Type(derived_id(KeySpaceId::derive(0xEE11_EE13, 0, 0, 0), 0,)))
        );
        assert!(second.skolem_scope().contains(&abst(0)));
    }

    #[test]
    fn interning_dedupes_independently_built_equal_environments() {
        let mut interner = TyEnvInterner::default();
        let env = sample();

        let first = interner.intern(&env);
        // The same content rebuilt from scratch shares no structure, but the
        // content comparison still interns it to the same shared value.
        let rebuilt = sample();
        let second = interner.intern(&rebuilt);

        assert_eq!(env, rebuilt);
        assert!(std::sync::Arc::ptr_eq(&first, &second));
        assert_eq!(second.as_ref().get(&def(0)), first.as_ref().get(&def(0)));
    }

    #[test]
    fn environment_extensions_preserve_existing_bindings_and_skolems() {
        let base = sample();
        let added_type = derived_id(KeySpaceId::derive(0xEE11_EE13, 0, 0, 0), 1);
        let extension = (base.clone() + [(def(1), AnnId::Type(added_type))]).with_skolem(abst(1));

        assert!(extension.is_extension_of(&base));
        assert!(base.is_extension_of(&base));

        let replaced = base.clone() + [(def(0), AnnId::Type(added_type))];
        assert!(!replaced.is_extension_of(&base));

        let missing_definition = TyEnv::default().with_skolem(abst(0));
        assert!(!missing_definition.is_extension_of(&base));

        let missing_skolem = TyEnv::from_iter(base.clone());
        assert!(!missing_skolem.is_extension_of(&base));
    }
}
