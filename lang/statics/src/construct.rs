//! Constructors for patterns, types, and terms in [`StaticsArena`].
//!
//! This module provides the [`Construct`] trait, which describes a convenient DSL for
//! writing Zydeco programs in Rust. Its semantics is as follows:
//!
//! `impl` [`Construct<T>`] for `S` means that `S` can be used to construct `T`
//!
//! The [`Construct`] API is an improvement based on [`Alloc`] in that unlike `Alloc`
//! which requires feeding annotations manually, it tries to infer the annotations
//! (types and kinds) of the constructed terms automatically, which brings two benefits:
//! + It's more convenient, because much less boilerplate annotations are needed.
//! + It's less error-prone, as the inferred types are constructed from the known context
//!   of the compiler. Manually constructed types are not guaranteed to match the terms
//!   that they are supposed to annotate.
//!
//! In conclusion, it's recommended to use `Construct` instead of `Alloc` for constructing
//! Zydeco programs in Rust. Example use cases are wrapped as legacy methods in [`Tycker`],
//! as well as static tests in `<sort>_test` modules.

use crate::{syntax::*, *};

pub trait Construct<T> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> T;
}

impl<S, T, A> Construct<T> for Ann<S, A>
where
    S: Alloc<T, Ann = A>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> T {
        let Ann { tm, ty } = self;
        Alloc::alloc(tycker, tm, ty)
    }
}

pub mod syntax {
    // use super::*;

    /// `codata end`
    pub struct TopTy;
    /// `Monad M : (VType -> CType) -> CType`
    pub struct MonadTy;
    /// generates `Monad M` where:
    /// 1. M is `monad_ty` of kind `VType -> CType`
    pub struct Monad<M>(pub M);
    /// `Algebra M R : (VType -> CType) -> CType -> CType`
    pub struct AlgebraTy;
    /// generates `Algebra M R` where:
    /// 1. M is `monad_ty` of kind `VType -> CType`
    /// 2. R is `carrier` of kind `CType`
    pub struct Algebra<M, R>(pub M, pub R);

    /// `exists X. A`
    pub struct Exists<Param, FBody>(pub Param, pub FBody);
    /// `forall X. B`
    pub struct Forall<Param, FBody>(pub Param, pub FBody);
}

/* ------------------------------- Definition ------------------------------- */

impl Construct<DefId> for DefId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> DefId {
        self
    }
}
impl Construct<DefId> for Ann<String, AnnId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> DefId {
        let Ann { tm, ty } = self;
        Ann { tm: VarName(tm), ty }.build(tycker, env)
    }
}
impl Construct<DefId> for Ann<&str, AnnId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> DefId {
        let Ann { tm, ty } = self;
        Ann { tm: tm.to_string(), ty }.build(tycker, env)
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl Construct<AbstId> for AbstId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> AbstId {
        self
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl Construct<KindId> for KindId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> KindId {
        self
    }
}
impl Construct<KindId> for VType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.vtype.get()] else { unreachable!() };
        kd
    }
}
impl Construct<KindId> for CType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.ctype.get()] else { unreachable!() };
        kd
    }
}
impl<S, T> Construct<KindId> for Arrow<S, T>
where
    S: Construct<KindId>,
    T: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let Arrow(k1, k2) = self;
        let k1 = k1.build(tycker, env);
        let k2 = k2.build(tycker, env);
        Alloc::alloc(tycker, Arrow(k1, k2), ())
    }
}

impl Tycker {
    pub fn vtype(&mut self, env: &Env<AnnId>) -> KindId {
        VType.build(self, env)
    }
    pub fn ctype(&mut self, env: &Env<AnnId>) -> KindId {
        CType.build(self, env)
    }
}

#[cfg(test)]
mod kind_test {
    use crate::{syntax::*, *};

    #[test]
    fn r#static() {
        fn _f(tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
            Arrow(VType, Arrow(CType, CType)).build(tycker, env)
        }
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl Construct<TPatId> for TPatId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> TPatId {
        self
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl Construct<TypeId> for TypeId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> TypeId {
        self
    }
}
impl<Kd> Construct<TypeId> for Ann<Hole, (Kd, su::TermId)>
where
    Kd: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Ann { tm: Hole, ty: (kd, site) } = self;
        let kd = kd.build(tycker, env);
        let fill = tycker.statics.fills.alloc(site);
        Alloc::alloc(tycker, fill, kd)
    }
}
impl Construct<TypeId> for DefId {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[&self] else { unreachable!() };
        let kd = tycker.statics.annotations_type[&ty];
        Alloc::alloc(tycker, self, kd)
    }
}
impl Construct<TypeId> for AbstId {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> TypeId {
        let kd = tycker.statics.annotations_abst[&self];
        Alloc::alloc(tycker, self, kd)
    }
}
impl<S, F, T> Construct<TypeId> for Abs<S, F>
where
    S: Construct<TPatId>,
    F: Fn(TPatId, DefId, KindId) -> T,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Abs(param, ty) = self;
        let tpat = param.build(tycker, env);
        let (def, param_kd) = tpat.destruct_def(tycker);
        let body = ty(tpat, def, param_kd).build(tycker, env);
        let body_kd = tycker.statics.annotations_type[&body];
        let kd = Arrow(param_kd, body_kd).build(tycker, env);
        Alloc::alloc(tycker, Abs(tpat, body), kd)
    }
}
impl<S, T> Construct<TypeId> for App<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let App(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let kd_1 = tycker.statics.annotations_type[&ty_1];
        let Some((kd_a, kd_b)) = kd_1.destruct_arrow(tycker) else { unreachable!() };
        let ty_2 = ty_2.build(tycker, env);
        let kd_2 = tycker.statics.annotations_type[&ty_2];
        let Ok(_) = Lub::lub(kd_a, kd_2, tycker) else { unreachable!() };
        Alloc::alloc(tycker, App(ty_1, ty_2), kd_b)
    }
}
impl Construct<TypeId> for IntTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.int.get()] else { unreachable!() };
        ty
    }
}
impl Construct<TypeId> for CharTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.char.get()] else { unreachable!() };
        ty
    }
}
impl Construct<TypeId> for StringTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.string.get()] else { unreachable!() };
        ty
    }
}
impl Construct<TypeId> for ThkTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.thk.get()] else { unreachable!() };
        ty
    }
}
impl<T> Construct<TypeId> for Thunk<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Thunk(arg) = self;
        let thk = ThkTy.build(tycker, env);
        let arg = arg.build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, App(thk, arg), vtype)
    }
}
impl Construct<TypeId> for UnitTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.unit.get()] else { unreachable!() };
        ty
    }
}
impl<S, T> Construct<TypeId> for Prod<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Prod(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let ty_2 = ty_2.build(tycker, env);
        let vtype = tycker.vtype(env);
        Alloc::alloc(tycker, Prod(ty_1, ty_2), vtype)
    }
}
impl<F, A, T> Construct<TypeId> for cs::Exists<A, F>
where
    F: Fn(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Exists(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, Exists(abst, ty), vtype)
    }
}
impl Construct<TypeId> for OSTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.os.get()] else { unreachable!() };
        ty
    }
}
impl Construct<TypeId> for cs::TopTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.top.get()] else { unreachable!() };
        ty
    }
}
impl<S, T> Construct<TypeId> for Arrow<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Arrow(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let ty_2 = ty_2.build(tycker, env);
        let ctype = tycker.ctype(env);
        Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype)
    }
}
impl<F, A, T> Construct<TypeId> for cs::Forall<A, F>
where
    F: Fn(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Forall(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, Exists(abst, ty), ctype)
    }
}
impl Construct<TypeId> for RetTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.ret.get()] else { unreachable!() };
        ty
    }
}
impl<T> Construct<TypeId> for Ret<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Ret(arg) = self;
        let ret = RetTy.build(tycker, env);
        let arg = arg.build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, App(ret, arg), ctype)
    }
}
impl Construct<TypeId> for syntax::MonadTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.monad.get()] else { unreachable!() };
        ty
    }
}
impl<M> Construct<TypeId> for cs::Monad<M>
where
    M: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Monad(monad_ty) = self;
        App(cs::MonadTy, monad_ty).build(tycker, env)
    }
}
impl Construct<TypeId> for syntax::AlgebraTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.algebra.get()] else { unreachable!() };
        ty
    }
}
impl<M, R> Construct<TypeId> for cs::Algebra<M, R>
where
    M: Construct<TypeId>,
    R: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Algebra(monad_ty, carrier) = self;
        App(App(cs::AlgebraTy, monad_ty), carrier).build(tycker, env)
    }
}

impl Tycker {
    /// generates `Thunk B`
    pub fn thk_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        Thunk(arg).build(self, env)
    }
    /// generates `Thunk _`
    pub fn thk_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        Thunk(Ann { tm: Hole, ty: (CType, site) }).build(self, env)
    }
    /// generates `Ret A`
    pub fn ret_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        Ret(arg).build(self, env)
    }
    /// generates `Ret _`
    pub fn ret_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        Ret(Ann { tm: Hole, ty: (VType, site) }).build(self, env)
    }
    pub fn type_top(&mut self, env: &Env<AnnId>) -> TypeId {
        cs::TopTy.build(self, env)
    }
    pub fn type_exists(&mut self, env: &Env<AnnId>, x: AbstId, b: TypeId) -> TypeId {
        cs::Exists(x, |_| b).build(self, env)
    }
    pub fn type_forall(&mut self, env: &Env<AnnId>, x: AbstId, b: TypeId) -> TypeId {
        cs::Forall(x, |_| b).build(self, env)
    }
    /// generates `Monad M` where:
    /// 1. M is `monad_ty` of kind `VType -> CType`
    pub fn monad_mo(&mut self, env: &Env<AnnId>, monad_ty: TypeId) -> TypeId {
        cs::Monad(monad_ty).build(self, env)
    }
    pub fn algebra_mo_car(
        &mut self, env: &Env<AnnId>, monad_ty: TypeId, carrier: TypeId,
    ) -> TypeId {
        cs::Algebra(monad_ty, carrier).build(self, env)
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl Construct<VPatId> for VPatId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> VPatId {
        self
    }
}

/* ---------------------------------- Value --------------------------------- */

impl Construct<ValueId> for ValueId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> ValueId {
        self
    }
}
impl<T> Construct<ValueId> for Thunk<T>
where
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> ValueId {
        let Thunk(body) = self;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = tycker.thk_arg(env, body_ty);
        Alloc::alloc(tycker, Thunk(body), ty)
    }
}
impl Construct<ValueId> for Triv {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> ValueId {
        let ty = UnitTy.build(tycker, env);
        Alloc::alloc(tycker, Triv, ty)
    }
}
impl<S, T> Construct<ValueId> for Cons<S, T>
where
    S: Construct<ValueId>,
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> ValueId {
        let Cons(a, b) = self;
        let a = a.build(tycker, env);
        let a_ty = tycker.statics.annotations_value[&a];
        let b = b.build(tycker, env);
        let b_ty = tycker.statics.annotations_value[&b];
        let ty = Prod(a_ty, b_ty).build(tycker, env);
        Alloc::alloc(tycker, Cons(a, b), ty)
    }
}

impl Tycker {
    pub fn value_var(&mut self, env: &Env<AnnId>, def: DefId, ty: TypeId) -> ValueId {
        Ann { tm: def, ty }.build(self, env)
    }
    pub fn value_thunk(&mut self, env: &Env<AnnId>, body: CompuId) -> ValueId {
        Thunk(body).build(self, env)
    }
    pub fn value_triv(&mut self, env: &Env<AnnId>) -> ValueId {
        Triv.build(self, env)
    }
    pub fn value_vcons(&mut self, env: &Env<AnnId>, a: ValueId, b: ValueId) -> ValueId {
        Cons(a, b).build(self, env)
    }
}

/* ---------------------------------- Compu --------------------------------- */

impl Construct<CompuId> for CompuId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> CompuId {
        self
    }
}
// impl<> Construct<CompuId> for Abs<

impl Tycker {
    pub fn compu_vabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, def_ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, def_ty);
        let body = body(self, def);
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(def_ty, body_ty), ctype);
        Alloc::alloc(self, Abs(vpat, body), ty)
    }
    pub fn try_compu_vabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> Result<CompuId>,
    ) -> Result<CompuId> {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, def_ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, def_ty);
        let body = body(self, def)?;
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(def_ty, body_ty), ctype);
        Ok(Alloc::alloc(self, Abs(vpat, body), ty))
    }
    pub fn compu_vapp(&mut self, _env: &Env<AnnId>, abs: CompuId, arg: ValueId) -> CompuId {
        let abs_ty = self.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(self) else { unreachable!() };
        let arg_ty = self.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, self) else { unreachable!() };
        Alloc::alloc(self, App(abs, arg), body_ty)
    }
    pub fn compu_tabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_kd: impl Construct<KindId>,
        body: impl Fn(&mut Self, DefId, AbstId) -> CompuId,
    ) -> CompuId {
        let def_kd = def_kd.build(self, env);
        let def = Alloc::alloc(self, name, def_kd.into());
        let abst = Alloc::alloc(self, def, def_kd);
        let tpat: TPatId = Alloc::alloc(self, def, def_kd);
        let body = body(self, def, abst);
        let body_ty = self.statics.annotations_compu[&body];
        let ctype = self.ctype(env);
        let ty = Alloc::alloc(self, Forall(abst, body_ty), ctype);
        Alloc::alloc(self, Abs(tpat, body), ty)
    }
    pub fn try_compu_tabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_kd: impl Construct<KindId>,
        body: impl Fn(&mut Self, DefId, AbstId) -> Result<CompuId>,
    ) -> Result<CompuId> {
        let def_kd = def_kd.build(self, env);
        let def = Alloc::alloc(self, name, def_kd.into());
        let abst = Alloc::alloc(self, def, def_kd);
        let tpat: TPatId = Alloc::alloc(self, def, def_kd);
        let body = body(self, def, abst)?;
        let body_ty = self.statics.annotations_compu[&body];
        let ctype = self.ctype(env);
        let ty = Alloc::alloc(self, Forall(abst, body_ty), ctype);
        Ok(Alloc::alloc(self, Abs(tpat, body), ty))
    }
    pub fn compu_tapp(&mut self, _env: &Env<AnnId>, abs: CompuId, arg: TypeId) -> CompuId {
        let abs_ty = self.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(self) else { unreachable!() };
        let param_kd = self.statics.annotations_abst[&abst];
        let arg_kd = self.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, self) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(self, (abst, arg)) else { unreachable!() };
        Alloc::alloc(self, App(abs, arg), ty)
    }
    pub fn compu_fix(
        &mut self, env: &Env<AnnId>, name: VarName, ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, ty);
        let body = body(self, def);
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(ty, body_ty), ctype);
        Alloc::alloc(self, Fix(vpat, body), ty)
    }
    pub fn compu_force(&mut self, _env: &Env<AnnId>, thk: ValueId) -> CompuId {
        let thk_ty = self.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(self) else { unreachable!() };
        Alloc::alloc(self, Force(thk), body_ty)
    }
    pub fn compu_ret(&mut self, env: &Env<AnnId>, val: ValueId) -> CompuId {
        let val_ty = self.statics.annotations_value[&val];
        let ret_ty = self.ret_arg(env, val_ty);
        Alloc::alloc(self, Ret(val), ret_ty)
    }
    pub fn compu_bind(
        &mut self, _env: &Env<AnnId>, bindee: CompuId, name: VarName,
        tail: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let bindee_ty = self.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(self) else { unreachable!() };
        let def = Alloc::alloc(self, name, def_ty.into());
        let binder = Alloc::alloc(self, def, def_ty);
        let tail = tail(self, def);
        let tail_ty = self.statics.annotations_compu[&tail];
        Alloc::alloc(self, Bind { binder, bindee, tail }, tail_ty)
    }
    pub fn compu_let(
        &mut self, _env: &Env<AnnId>, bindee: ValueId, name: VarName,
        tail: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let def_ty = self.statics.annotations_value[&bindee];
        let def = Alloc::alloc(self, name, def_ty.into());
        let binder = Alloc::alloc(self, def, def_ty);
        let tail = tail(self, def);
        let tail_ty = self.statics.annotations_compu[&tail];
        Alloc::alloc(self, PureBind { binder, bindee, tail }, tail_ty)
    }
    pub fn compu_top(&mut self, env: &Env<AnnId>) -> CompuId {
        let top = self.type_top(env);
        Alloc::alloc(self, CoMatch { arms: Vec::new() }, top)
    }
    pub fn compu_dtor(&mut self, env: &Env<AnnId>, head: CompuId, dtor: DtorName) -> CompuId {
        let head_ty = self.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, self) else { unreachable!() };
        let Some(ty) = coda.get(&dtor).cloned() else { unreachable!() };
        Alloc::alloc(self, Dtor(head, dtor), ty)
    }
}
