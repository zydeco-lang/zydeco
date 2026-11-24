//! Environments used by the Zydeco type checker.

use super::*;
use derive_more::{Deref, DerefMut, From, Index, IndexMut, Into};
use std::ops::{Add, AddAssign};
use zydeco_foundation::prelude::With;

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

        pub(crate) fn iter(&self) -> impl Iterator<Item = AbstId> + '_ {
            self.0.iter().copied()
        }
    }

    impl FromIterator<AbstId> for SkolemScope {
        fn from_iter<I: IntoIterator<Item = AbstId>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }
}

/// Typing environment mapping definitions to annotations together with the
/// existential witnesses and enclosing local/val type witnesses visible at that point.
#[derive(Clone, Debug)]
pub struct TyEnv {
    defs: Env<AnnId>,
    skolems: SkolemScope,
    val_witnesses: rpds::VectorSync<AbstId>,
    local_witnesses: rpds::HashTrieSetSync<AbstId>,
}

mod impls_ty_env {
    use super::*;

    impl TyEnv {
        pub fn new() -> Self {
            Self {
                defs: Env::new(),
                skolems: SkolemScope::default(),
                val_witnesses: rpds::VectorSync::new_sync(),
                local_witnesses: rpds::HashTrieSetSync::new_sync(),
            }
        }

        pub fn skolem_scope(&self) -> &SkolemScope {
            &self.skolems
        }

        pub fn val_witnesses(&self) -> impl Iterator<Item = AbstId> + '_ {
            self.val_witnesses.iter().copied()
        }

        pub fn is_local_witness(&self, witness: &AbstId) -> bool {
            self.local_witnesses.contains(witness)
        }

        pub fn with_skolem(mut self, skolem: AbstId) -> Self {
            self.skolems = self.skolems.with(skolem);
            self.local_witnesses.insert_mut(skolem);
            self
        }

        pub fn with_val_witness(mut self, witness: AbstId) -> Self {
            self.val_witnesses.push_back_mut(witness);
            self.local_witnesses.insert_mut(witness);
            self
        }

        pub fn with_val_witnesses(mut self, witnesses: impl IntoIterator<Item = AbstId>) -> Self {
            for witness in witnesses {
                self.val_witnesses.push_back_mut(witness);
                self.local_witnesses.insert_mut(witness);
            }
            self
        }

        pub fn with_local_witness(mut self, witness: AbstId) -> Self {
            self.local_witnesses.insert_mut(witness);
            self
        }

        pub fn with_skolem_scope(mut self, scope: SkolemScope) -> Self {
            self.skolems = scope;
            self
        }

        /// Whether this environment preserves every binding, skolem, and local/val witness in `base`.
        pub fn is_extension_of(&self, base: &Self) -> bool {
            let preserves_definitions = self.defs.0.ptr_eq(&base.defs.0)
                || (self.defs.len() >= base.defs.len()
                    && base.defs.iter().all(|(def, ann)| self.defs.get(def) == Some(ann)));
            let preserves_skolems = self.skolems.0.ptr_eq(&base.skolems.0)
                || (self.skolems.0.size() >= base.skolems.0.size()
                    && base.skolems.0.iter().all(|skolem| self.skolems.contains(skolem)));
            let preserves_val_witnesses = self.val_witnesses.len() >= base.val_witnesses.len()
                && self
                    .val_witnesses
                    .iter()
                    .zip(base.val_witnesses.iter())
                    .all(|(left, right)| left == right);
            let preserves_local_witnesses = self.local_witnesses.ptr_eq(&base.local_witnesses)
                || (self.local_witnesses.size() >= base.local_witnesses.size()
                    && base
                        .local_witnesses
                        .iter()
                        .all(|witness| self.local_witnesses.contains(witness)));
            preserves_definitions
                && preserves_skolems
                && preserves_val_witnesses
                && preserves_local_witnesses
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
            Self {
                defs,
                skolems: ori.skolems.clone(),
                val_witnesses: ori.val_witnesses.clone(),
                local_witnesses: ori.local_witnesses.clone(),
            }
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
            Self {
                defs: iter.into_iter().collect(),
                skolems: SkolemScope::default(),
                val_witnesses: rpds::VectorSync::new_sync(),
                local_witnesses: rpds::HashTrieSetSync::new_sync(),
            }
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
    pub(crate) nominals: std::sync::Arc<
        std::collections::BTreeMap<AbstId, crate::elaborate::monadic::nominal::NominalFamily>,
    >,
    pub(crate) nominal_dictionary: Option<ValueId>,
}

/* --------------------------- Environment Interning ------------------------- */

/// Typing environments compare by structural equivalence: clones of one
/// environment share persistent allocations, so pointer equality answers the
/// common case in O(1), while independently built environments fall back to a
/// content comparison.
impl PartialEq for TyEnv {
    fn eq(&self, other: &Self) -> bool {
        let same_val_witnesses = self.val_witnesses.len() == other.val_witnesses.len()
            && self.val_witnesses.iter().zip(other.val_witnesses.iter()).all(|(a, b)| a == b);
        if !same_val_witnesses {
            return false;
        }
        if self.defs.0.ptr_eq(&other.defs.0)
            && self.skolems.0.ptr_eq(&other.skolems.0)
            && self.local_witnesses.ptr_eq(&other.local_witnesses)
        {
            return true;
        }
        self.defs.len() == other.defs.len()
            && self.defs.iter().all(|(def, ann)| other.defs.get(def) == Some(ann))
            && self.skolems.0.size() == other.skolems.0.size()
            && self.skolems.0.iter().all(|skolem| other.skolems.contains(skolem))
            && self.local_witnesses.size() == other.local_witnesses.size()
            && self.local_witnesses.iter().all(|witness| other.local_witnesses.contains(witness))
    }
}

impl Eq for TyEnv {}

#[inline]
fn mix_u64(mut value: u64) -> u64 {
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d049bb133111eb);
    value ^ (value >> 31)
}

impl std::hash::Hash for TyEnv {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use zydeco_foundation::dense::DenseId;

        // Combine unordered sets by wrapping addition and ordered `val_witnesses`
        // sequentially so equivalent environments hash equally.
        let mut acc = 0u64;
        for (def, ann) in self.defs.iter() {
            let def_mix =
                mix_u64(def.key_space().as_u64() ^ u64::from(def.raw().into_u32()).rotate_left(32));
            let ann_mix = match ann {
                | AnnId::Set => mix_u64(1),
                | AnnId::Kind(kind) => mix_u64(u64::from(kind.raw_u32()) ^ 2),
                | AnnId::Type(ty) => mix_u64(u64::from(ty.raw_u32()) ^ 3),
            };
            acc = acc.wrapping_add(mix_u64(def_mix ^ ann_mix.rotate_left(17)));
        }
        for skolem in self.skolems.0.iter() {
            let skolem_mix = mix_u64(u64::from(skolem.raw_u32()));
            acc = acc.wrapping_add(skolem_mix);
        }
        let mut local_acc = 0u64;
        for witness in self.local_witnesses.iter() {
            local_acc = local_acc.wrapping_add(mix_u64(u64::from(witness.raw_u32()) ^ 0x517cc1b7));
        }
        acc.hash(state);
        local_acc.hash(state);
        self.val_witnesses.len().hash(state);
        for witness in self.val_witnesses.iter() {
            witness.raw_u32().hash(state);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_foundation::arena::IdAllocator;
    use zydeco_foundation::dense::DenseId;

    struct Fixture {
        defs: [DefId; 2],
        absts: [AbstId; 2],
        types: [TypeId; 2],
    }

    impl Fixture {
        fn new() -> Self {
            let mut allocator = IdAllocator::<StaticsScope>::new();
            Self {
                defs: [allocator.alloc(), allocator.alloc()],
                absts: [AbstId::from_index(0), AbstId::from_index(1)],
                types: [TypeId::from_index(0), TypeId::from_index(1)],
            }
        }

        fn sample(&self) -> TyEnv {
            TyEnv::from_iter([(self.defs[0], AnnId::Type(self.types[0]))])
                .with_skolem(self.absts[0])
        }
    }

    #[test]
    fn environment_extensions_preserve_existing_bindings_and_skolems() {
        let fixture = Fixture::new();
        let base = fixture.sample();
        let added_type = fixture.types[1];
        let extension = (base.clone() + [(fixture.defs[1], AnnId::Type(added_type))])
            .with_skolem(fixture.absts[1]);

        assert!(extension.is_extension_of(&base));
        assert!(base.is_extension_of(&base));

        let replaced = base.clone() + [(fixture.defs[0], AnnId::Type(added_type))];
        assert!(!replaced.is_extension_of(&base));

        let missing_definition = TyEnv::default().with_skolem(fixture.absts[0]);
        assert!(!missing_definition.is_extension_of(&base));

        let missing_skolem = TyEnv::from_iter(base.clone());
        assert!(!missing_skolem.is_extension_of(&base));
    }

    #[test]
    fn hashing_distinguishes_same_keyspace_bindings_and_ignores_insertion_order() {
        use rustc_hash::FxHasher;
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash, Hasher},
        };

        fn hash_with<H: Hasher + Default>(env: &TyEnv) -> u64 {
            let mut hasher = H::default();
            env.hash(&mut hasher);
            hasher.finish()
        }

        let mut allocator = IdAllocator::<StaticsScope>::new();
        let def_a: DefId = allocator.alloc();
        let def_b: DefId = allocator.alloc();
        let type_0 = TypeId::from_index(0);
        let type_1 = TypeId::from_index(1);
        let kind_0 = KindId::from_index(0);
        let kind_1 = KindId::from_index(1);

        let env_type_0 = TyEnv::from_iter([(def_a, AnnId::Type(type_0))]);
        let env_type_1 = TyEnv::from_iter([(def_a, AnnId::Type(type_1))]);
        assert_ne!(hash_with::<FxHasher>(&env_type_0), hash_with::<FxHasher>(&env_type_1));
        assert_ne!(
            hash_with::<DefaultHasher>(&env_type_0),
            hash_with::<DefaultHasher>(&env_type_1)
        );

        let env_kind_0 = TyEnv::from_iter([(def_a, AnnId::Kind(kind_0))]);
        let env_kind_1 = TyEnv::from_iter([(def_a, AnnId::Kind(kind_1))]);
        assert_ne!(hash_with::<FxHasher>(&env_kind_0), hash_with::<FxHasher>(&env_kind_1));
        assert_ne!(
            hash_with::<DefaultHasher>(&env_kind_0),
            hash_with::<DefaultHasher>(&env_kind_1)
        );

        let forward =
            TyEnv::from_iter([(def_a, AnnId::Type(type_0)), (def_b, AnnId::Kind(kind_1))]);
        let reverse =
            TyEnv::from_iter([(def_b, AnnId::Kind(kind_1)), (def_a, AnnId::Type(type_0))]);
        assert_eq!(forward, reverse);
        assert_eq!(hash_with::<FxHasher>(&forward), hash_with::<FxHasher>(&reverse));
        assert_eq!(hash_with::<DefaultHasher>(&forward), hash_with::<DefaultHasher>(&reverse));
    }
}
