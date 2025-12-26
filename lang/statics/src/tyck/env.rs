//! Environments used by the Zydeco type checker.

use super::{syntax::*, *};
use derive_more::{Deref, DerefMut, From, Into};
use std::ops::{Add, AddAssign, Index};
use zydeco_utils::with::With;

#[derive(Clone, Debug, From, Into, Deref, DerefMut)]
pub struct Env<T>(im::HashMap<DefId, T>);

mod impls_env {
    use super::*;

    impl<Iter, T> Add<Iter> for Env<T>
    where
        T: Clone,
        Iter: IntoIterator<Item = (DefId, T)>,
    {
        type Output = Self;
        fn add(self, iter: Iter) -> Self {
            let Env(mut defs) = self;
            for (def, t) in iter {
                defs.insert(def, t);
            }
            Self(defs)
        }
    }
    impl<Iter, T> AddAssign<Iter> for Env<T>
    where
        T: Clone,
        Iter: IntoIterator<Item = (DefId, T)>,
    {
        fn add_assign(&mut self, iter: Iter) {
            for (def, t) in iter {
                self.0.insert(def, t);
            }
        }
    }
    impl<T> Env<T> {
        pub fn new() -> Self {
            Self(im::HashMap::new())
        }
    }
    impl<T> Default for Env<T> {
        fn default() -> Self {
            Self::new()
        }
    }
    impl<T> Index<&DefId> for Env<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.0[def]
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
    impl<T> IntoIterator for Env<T>
    where
        T: Clone,
    {
        type Item = (DefId, T);
        type IntoIter = im::hashmap::ConsumingIter<(DefId, T)>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}

pub type TyEnv = Env<AnnId>;

mod impls_ty_env {
    use super::*;

    impl TyEnv {
        pub fn monadic_new(tycker: &mut Tycker, ori: &TyEnv) -> Self {
            let mut env = Env::new();
            env += [
                (tycker.prim.vtype.get().to_owned(), VType.build(tycker, ori).into()),
                (tycker.prim.ctype.get().to_owned(), CType.build(tycker, ori).into()),
                (tycker.prim.thk.get().to_owned(), ThkTy.build(tycker, ori).into()),
                (tycker.prim.ret.get().to_owned(), RetTy.build(tycker, ori).into()),
                (tycker.prim.unit.get().to_owned(), UnitTy.build(tycker, ori).into()),
                // (tycker.prim.top.get().to_owned(), cs::TopTy.build(tycker, ori).into()),
                // (tycker.prim.monad.get().to_owned(), cs::MonadTy.build(tycker, ori).into()),
                // (tycker.prim.algebra.get().to_owned(), cs::AlgebraTy.build(tycker, ori).into()),
            ];
            for (def, ann) in ori.clone() {
                use zydeco_surface::arena::ArenaAccess;
                if tycker.statics.global_defs.get(&def).is_some() {
                    env += [(def, ann)];
                }
            }
            env
        }
        pub fn recursively_get_type(&self, tycker: &Tycker, def: &DefId) -> Option<&AnnId> {
            let ann = self.0.get(def)?;
            match ann {
                | AnnId::Set | AnnId::Kind(_) => unreachable!(),
                | AnnId::Type(ty) => {
                    let ty = tycker.statics.r#type(ty);
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
}

/// substituting types for type variables;
/// S for substitution / statics
/// PLEASE NOTE: when performing substitution, the environment should be applied one by one
///
/// `.info`: the environment of type variables; should be applied from the first to the last
// Note: should be ordered?
pub type TyEnvT<T> = With<TyEnv, T>;

pub type SubstEnv = Env<DefId>;
pub type SubstAbstEnv = im::HashMap<AbstId, AbstId>;
pub type SubstEnvT<T> = With<SubstEnv, T>;

#[derive(Clone)]
pub struct StrEnv {
    // Todo: remove this useless non-sense
    pub def_map: im::HashMap<DefId, AbstId>,
    pub absts: im::HashMap<AbstId, ValueId>,
}

mod impls_str_env {
    use super::*;

    impl StrEnv {
        pub fn new() -> Self {
            Self { def_map: im::HashMap::new(), absts: im::HashMap::new() }
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

pub type StrEnvT<T> = With<StrEnv, T>;

#[derive(Clone)]
pub struct MonEnv {
    pub ty: TyEnv,
    pub subst: SubstEnv,
    pub subst_abst: SubstAbstEnv,
    pub structure: StrEnv,
    pub monad_ty: TypeId,
    pub monad_impl: ValueId,
}
