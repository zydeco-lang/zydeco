//! Implementation of the monadic block via algebra translation.
//!
//! See the following links for details:
//! + Core Idea: [section 5.3 of oopsla25](https://dl.acm.org/doi/10.1145/3720434)
//! + Detailed Implementation: [appendix D of the extended version](https://arxiv.org/abs/2502.15031)

use crate::*;

/// Constructions that need the monadic translation's environment.
pub mod construct;
pub(crate) mod nominal;
mod projection;
mod value_functions;
use projection::ValueProjectionTranslation;
use value_functions::ValueFunctionTranslation;

mod pack_pi;
mod structure;
mod terms;
mod types;
use pack_pi::{
    PackPiAbstraction, PackPiApplication, PackPiPatternTranslation, PackPiStructureTranslation,
    PackPiWitnessLayout, PackPiWitnessLayoutEntry, PackagePatternStructure,
};
use structure::{CoDataStructureTranslation, structure_translation};
use terms::{
    computation_translation, package_pattern_translation, value_pattern_translation,
    value_translation,
};
use types::{kind_pattern_translation, type_pattern_translation, type_translation};
/// Per-block memoization for closed signature, type-lift, and shareable structure
/// algebra translations inside [`AlgebraicTranslation`].
#[derive(Default)]
pub(crate) struct MonadicMemo {
    pub(crate) signatures: rustc_hash::FxHashMap<(TypeId, TypeId, TypeId), TypeId>,
    pub(crate) type_lifts: rustc_hash::FxHashMap<(TypeId, TypeId, TypeId), TypeId>,
    pub(crate) structures: rustc_hash::FxHashMap<TypeId, (ValueId, TypeId)>,
    pub(crate) structure_bindings: Vec<(VPatId, ValueId)>,
    pub(crate) share_structures: bool,
}

pub mod syntax {
    /// signature translation
    #[derive(Clone, Copy)]
    pub struct Signature<T> {
        pub ty: T,
    }
    /// structure translation
    #[derive(Clone)]
    pub struct Structure<T> {
        pub ty: T,
    }
    /// type translation (lift)
    #[derive(Clone, Copy)]
    pub struct TypeLift<T> {
        pub ty: T,
    }
    /// term translation (lift)
    #[derive(Clone)]
    pub struct TermLift<T> {
        pub tm: T,
    }

    /// structure pattern introduction
    ///
    /// need to register the fresh var `S` as a structure of a given (abstract) type `A`;
    /// the optional variable definition `T` can be passed
    pub struct StrPat<S, A, T>(pub S, pub A, pub T);

    /// substitute abstract type from `S` to `T`
    pub struct AbstPat<S, T>(pub S, pub T);

    /// copy a type pattern's named shape around a different payload witness
    pub struct ReboundTypePattern {
        pub source: super::TPatId,
        pub witness: super::AbstId,
    }

    /// reconstruct a complete type argument from a pattern and its payload
    pub struct PatternArgument {
        pub pattern: super::TPatId,
        pub payload: super::AbstId,
    }

    /// construct a forall while retaining an explicit type-pattern shape
    pub struct PatternForall<P, T> {
        pub pattern: P,
        pub witness: super::AbstId,
        pub body: T,
    }
}

mod syntax_impl {
    use super::*;

    // SignatureTrans
    impl<T> Construct<MonEnv, TypeId> for cs::Signature<T>
    where
        T: Construct<MonEnv, TypeId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            tycker.guarded(|tycker| {
                let cs::Signature { ty } = self;
                let (env, ty) = ty.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::SignatureGen(ty.into()));
                signature_translation(tycker, env, ty)
            })
        }
    }

    // StructureTrans
    impl<T> Construct<MonEnv, CompuId> for cs::Structure<T>
    where
        T: Construct<MonEnv, TypeId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            tycker.guarded(|tycker| {
                let cs::Structure { ty } = self;
                let (env, ty) = ty.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::StructureGen(ty.into()));
                structure_translation(tycker, env, ty)
            })
        }
    }

    // TypeLift (type pattern translation)
    impl<T> Construct<MonEnv, TPatId> for cs::TypeLift<T>
    where
        T: Construct<MonEnv, TPatId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
            tycker.guarded(|tycker| {
                let cs::TypeLift { ty } = self;
                let (env, ty) = ty.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::MonadicLiftPat(ty.into()));
                type_pattern_translation(tycker, env, ty)
            })
        }
    }

    // TypeLift
    impl<T> Construct<MonEnv, TypeId> for cs::TypeLift<T>
    where
        T: Construct<MonEnv, TypeId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            tycker.guarded(|tycker| {
                let cs::TypeLift { ty } = self;
                let (env, ty) = ty.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::MonadicLiftTerm(ty.into()));
                type_translation(tycker, env, ty)
            })
        }
    }

    // TermLift (value pattern translation)
    impl<T> Construct<MonEnv, VPatId> for cs::TermLift<T>
    where
        T: Construct<MonEnv, VPatId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::MonadicLiftPat(tm.into()));
                value_pattern_translation(tycker, env, tm)
            })
        }
    }

    // TermLift (value translation)
    impl<T> Construct<MonEnv, ValueId> for cs::TermLift<T>
    where
        T: Construct<MonEnv, ValueId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::MonadicLiftTerm(tm.into()));
                value_translation(tycker, env, tm)
            })
        }
    }

    // TermLift (computation translation)
    impl<T> Construct<MonEnv, CompuId> for cs::TermLift<T>
    where
        T: Construct<MonEnv, CompuId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.construct(tycker, env)?;

                // administrative
                tycker.tasks.push(TyckTask::MonadicLiftTerm(tm.into()));
                computation_translation(tycker, env, tm)
            })
        }
    }

    // StrPat
    impl<S, A, T> Construct<MonEnv, VPatId> for cs::StrPat<S, A, T>
    where
        S: IntoName<VarName>,
        A: Construct<MonEnv, AbstId>,
        T: Construct<MonEnv, Option<DefId>>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            let cs::StrPat(var, abst, tvar) = self;
            let (env, abst) = abst.construct(tycker, env)?;
            let (env, ty) = cs::Thk(cs::Signature { ty: cs::Type(abst) }).construct(tycker, env)?;
            let def = Alloc::alloc(tycker, var.into_name(), ty.into(), &());
            let (env, tvar) = tvar.construct(tycker, env)?;
            let (env, pattern) = cs::Pat(def, ty).construct(tycker, env)?;
            let structure = pattern.reify(tycker);
            let mut env = env;
            env.structure.absts.insert_mut(abst, structure);
            if let Some(tvar) = tvar {
                env.structure.def_map.insert_mut(tvar, abst);
            }
            Ok((env, pattern))
        }
    }

    impl Construct<MonEnv, TypeId> for cs::PatternArgument {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let payload = env.abst(self.payload);
            let payload_kind = tycker.statics.annotations_abst[&payload];
            let payload = Alloc::alloc(tycker, payload, payload_kind, &());
            let argument = self.pattern.introduce_payload(tycker, payload)?;
            Ok((env, argument))
        }
    }

    // AbstPat
    impl<T> Construct<MonEnv, AbstId> for cs::AbstPat<AbstId, T>
    where
        T: Construct<MonEnv, AbstId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
            let cs::AbstPat(old, new) = self;
            let (env, new) = new.construct(tycker, env)?;
            tycker.transfer_builtin_role(old, new)?;
            let mut env = env;
            env.subst_abst.insert_mut(old, new);
            Ok((env, new))
        }
    }

    impl Construct<MonEnv, TPatId> for cs::ReboundTypePattern {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
            let witness = env.abst(self.witness);
            let payload = tycker.statics.abst_hints.get(witness).copied();
            let pattern = self.source.rebind_payload(tycker, payload);
            Ok((env, pattern))
        }
    }

    impl<P, T> Construct<MonEnv, TypeId> for cs::PatternForall<P, T>
    where
        P: Construct<MonEnv, TPatId>,
        T: Construct<MonEnv, TypeId>,
    {
        fn construct(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let (env, pattern) = self.pattern.construct(tycker, env)?;
            let witness = env.abst(self.witness);
            let (env, body) = self.body.construct(tycker, env)?;
            let (env, ctype) = CType.construct(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let forall = Alloc::alloc(tycker, Forall(binder, body), ctype, &());
            Ok((env, forall))
        }
    }
}

/// Signature Translation `Sig_K(T)`
///
/// Given the monad type `M` and the type environment, implement the signature
/// translation from input type `T` to an output computation type `CompuId`
/// which is the type of algebra of `T`. Specifically,
///
/// + `T: VType` -> `Top`
/// + `T: CType` -> `Algebra M T`
/// + `T: K_1 -> K_2` -> `forall X: K_1 . Thk (Sig_K_1(X)) -> Sig_K_2(T X)`
fn signature_translation(tycker: &mut Tycker, env: MonEnv, ty: TypeId) -> Result<(MonEnv, TypeId)> {
    zydeco_foundation::stack::maybe_grow(|| {
        let key = (env.monad_ty, env.basis.algebra, ty);
        if let Some(cached) =
            tycker.monadic_memo.as_ref().and_then(|memo| memo.signatures.get(&key).copied())
        {
            return Ok((env, cached));
        }
        let (env, res) = signature_translation_inner(tycker, env, ty)?;
        if let Some(memo) = &mut tycker.monadic_memo {
            memo.signatures.insert(key, res);
        }
        Ok((env, res))
    })
}

fn signature_translation_inner(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, TypeId)> {
    let (env, kd) = cs::TypeOf(ty).construct(tycker, env)?;
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => cs::TopTy.construct(tycker, env)?,
        | Kind::CType(CType) => cs::Algebra(env.monad_ty, ty).construct(tycker, env)?,
        | Kind::Label(Label(name, payload_kind)) => {
            let payload = ty.project_named(tycker, &name, payload_kind)?;
            signature_translation(tycker, env, payload)?
        }
        | Kind::Arrow(Arrow(kd_1, _)) => cs::Forall(cs::Ann(None, kd_1), |abst| {
            let ty_1 = cs::Ann(abst, kd_1);
            Arrow(cs::Thk(cs::Signature { ty: ty_1 }), cs::Signature { ty: App(ty, ty_1) })
        })
        .construct(tycker, env)?,
    };
    Ok(res)
}
