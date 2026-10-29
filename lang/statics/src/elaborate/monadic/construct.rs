//! Constructions that need the monadic translation's environment.
//!
//! The translation copies a closed term. Copying a binder allocates a fresh definition and records
//! the renaming of its occurrences in [`MonEnv`], and the algebra type comes from the translation's
//! basis. Computation abstraction and application over types are defined here too:
//! they share [`Abs`] and [`App`] with the value forms of [`crate::construct`], which every
//! environment uses, and are told apart by [`cs::Ty`].

use crate::*;

/* ------------------------------- TypePattern ------------------------------ */

/// Copy a type binder, renaming its occurrences in the rest of the translation.
impl<K> Construct<MonEnv, TPatId> for cs::Pat<DefId, K>
where
    K: Construct<MonEnv, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(def, kd) = self;
        let (mut env, kd) = kd.construct(tycker, env)?;
        let var = tycker.def_name(&def).clone();
        let copy = Alloc::alloc(tycker, var, kd.into(), &());
        env.subst += [(def, copy)];
        let tpat = Alloc::alloc(tycker, copy, kd, &env.ty);
        Ok((env, tpat))
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

/// Copy a value binder, renaming its occurrences in the rest of the translation.
impl<T> Construct<MonEnv, VPatId> for cs::Pat<DefId, T>
where
    T: Construct<MonEnv, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(def, ty) = self;
        let (mut env, ty) = ty.construct(tycker, env)?;
        let var = tycker.def_name(&def).clone();
        let copy = Alloc::alloc(tycker, var, ty.into(), &());
        env.subst += [(def, copy)];
        let vpat = Alloc::alloc(tycker, copy, ty, &env.ty);
        Ok((env, vpat))
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl Construct<MonEnv, TypeId> for cs::AlgebraTy {
    fn construct(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let algebra = env.basis.algebra;
        Ok((env, algebra))
    }
}
impl<M, R> Construct<MonEnv, TypeId> for cs::Algebra<M, R>
where
    M: Construct<MonEnv, TypeId>,
    R: Construct<MonEnv, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Algebra(monad_ty, carrier) = self;
        App(App(cs::AlgebraTy, monad_ty), carrier).construct(tycker, env)
    }
}

/* ------------------------------- Computation ------------------------------ */

/// Computation abstraction over a type parameter, with a fresh abstract type as its witness.
impl<P, F, T> Construct<MonEnv, CompuId> for Abs<cs::Ty<P>, F>
where
    P: Construct<MonEnv, TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: Construct<MonEnv, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty(tpat), body) = self;
        let (env, tpat) = tpat.construct(tycker, env)?;
        let (def, param_kd) = tpat.try_destruct_def(tycker);
        // make sure that the abstract type is allocated only once!
        let abst = Alloc::alloc(tycker, def, param_kd, &());
        let (env, body) = body(tpat, abst).construct(tycker, env)?;
        ForallAbstraction::alloc(tycker, env, tpat, abst, body)
    }
}
/// Computation abstraction over a type parameter whose witness is a renamed abstract type.
impl<P, F, T> Construct<MonEnv, CompuId> for Abs<cs::Ty<(P, AbstId)>, F>
where
    P: Construct<MonEnv, TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: Construct<MonEnv, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty((tpat, abst)), body) = self;
        let (env, tpat) = tpat.construct(tycker, env)?;
        let abst = env.abst(abst);
        let (env, body) = body(tpat, abst).construct(tycker, env)?;
        ForallAbstraction::alloc(tycker, env, tpat, abst, body)
    }
}

/// A computation abstraction over a type parameter, typed by its `forall`.
struct ForallAbstraction;

impl ForallAbstraction {
    fn alloc(
        tycker: &mut Tycker<'_>, env: MonEnv, pattern: TPatId, witness: AbstId, body: CompuId,
    ) -> Result<(MonEnv, CompuId)> {
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.construct(tycker, env)?;
        let binder = TypeBinder { pattern, witness };
        let ty = Alloc::alloc(tycker, Forall(binder, body_ty), ctype, &env.ty);
        let abstraction = Alloc::alloc(tycker, Abs(pattern, body), ty, &env.ty);
        Ok((env, abstraction))
    }
}

/// Computation application to a type argument.
impl<S, T> Construct<MonEnv, CompuId> for App<S, cs::Ty<T>>
where
    S: Construct<MonEnv, CompuId>,
    T: Construct<MonEnv, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let App(abs, cs::Ty(arg)) = self;
        let (env, abs) = abs.construct(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((binder, body_ty)) = abs_ty.destruct_forall_binder(tycker) else {
            return tycker.err(TyckError::TypeExpected {
                expected: "a forall type for a monadic operation".to_owned(),
                found: abs_ty,
            });
        };
        let (env, arg) = arg.construct(tycker, env)?;
        let domain_kd = binder.domain_kind(tycker);
        let arg_kd = tycker.statics.type_kind(arg);
        Lub::lub(domain_kd, arg_kd, tycker)?;
        let payload = binder.pattern.bind_argument(tycker, arg)?;
        let ty = body_ty.subst_abst(tycker, (binder.witness, payload))?;
        let application = Alloc::alloc(tycker, App(abs, arg), ty, &env.ty);
        Ok((env, application))
    }
}
