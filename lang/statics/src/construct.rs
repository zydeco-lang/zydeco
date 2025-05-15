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

/// Trait for constructing entities in [`Tycker`] with more type inference available.
///
/// The only method provided is [`Construct::build`], which takes `&mut` [`Tycker`]
/// and the environment of the type checked program, and returns the constructed value.
///
/// The trait is different from [`Alloc`] in that it does not require feeding annotations
/// manually, but instead infers them from the context.
pub trait Construct<T> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> T;
}

impl<S, T, A> Construct<T> for cs::Ann<S, A>
where
    S: Alloc<T, Ann = A>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> T {
        let cs::Ann(tm, ty) = self;
        Alloc::alloc(tycker, tm, ty)
    }
}

impl<T> Construct<Result<T>> for Result<T> {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<T> {
        self
    }
}

/// Syntax used by [`Construct`]. Specifically,
///
/// + Some structures that are not defined as part of the common syntax of Zydeco,
///   such as `Monad` and `Algebra`.
/// + Some existing structures that may take a more convenient syntax,
///   such as `Ann`, `Ctor`, and `Dtor`.
/// + Head-only structures (starting with `H`) that implements methods for
///   generating the whole term accordingly.
///
/// The head-only structures are the most interesting in that they are invented
/// only because of the limitation of Rust's trait constraint solver.
/// These head-only structures are the first part of their corresponding HOAS.
/// They store the positive data for parameters, and methods implementing different
/// `apply` methods will be provided to take a user-provided continuation and
/// generate the whole term.
pub mod syntax {
    /// `Ann { tm: S, ty: A }`
    #[derive(Clone, Copy)]
    pub struct Ann<S, A>(pub S, pub A);
    /// type indicator; used to resolve trait conflicts
    ///
    /// see [`super::App`] implementations on term level for examples
    pub struct Ty<T>(pub T);
    /// take the annotated type of a term, or the kind of a type
    pub struct TypeOf<T>(pub T);
    /// fresh variable [`super::DefId`] or abstract type [`super::AbstId`]
    pub struct Fresh<T>(pub T);
    // Todo: complete fresh

    /// generates an abstract type [`super::TypeId`] from [`super::AbstId`]
    pub struct AbstTy<T>(pub T);
    /// `Thk B`
    pub struct Thk<B>(pub B);
    /// `Ret A`
    pub struct Ret<A>(pub A);
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

    /// `exists X. ...`
    ///
    /// See crate level documentation [`crate::construct::syntax`] for more details.
    pub struct HExists<Param>(pub Param);
    pub struct Exists<Param, FBody>(pub Param, pub FBody);
    /// `forall X. ...`
    ///
    /// See crate level documentation [`crate::construct::syntax`] for more details.
    pub struct HForall<Param>(pub Param);
    pub struct Forall<Param, FBody>(pub Param, pub FBody);

    /// `+Ctor(tail)`
    pub struct Ctor<Name, Tail>(pub Name, pub Tail);
    /// `head .dtor`
    pub struct Dtor<Head, Name>(pub Head, pub Name);
    /// `comatch end`
    pub struct Top;

    /// `fn h -> ...`
    ///
    /// head-only abstractions that implements methods for
    /// generating the whole term accordingly
    ///
    /// See crate level documentation [`crate::construct::syntax`] for more details.
    pub struct HAbs<Param>(pub Param);

    pub use crate::monadic::syntax::*;
}

macro_rules! impl_construct_trivial {
    ($($ty:ty),*) => {
        $(
            impl Construct<$ty> for $ty {
                fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> $ty {
                    self
                }
            }
            impl Construct<Result<$ty>> for $ty {
                fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<$ty> {
                    Ok(self)
                }
            }
        )*
    }
}

impl_construct_trivial!(DefId, KindId, AbstId, TPatId, TypeId, VPatId, ValueId, CompuId);

/* ------------------------------- Definition ------------------------------- */

// VarName
impl Construct<VarName> for VarName {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> VarName {
        self
    }
}
impl Construct<VarName> for String {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> VarName {
        VarName(self).build(tycker, env)
    }
}
impl Construct<VarName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> VarName {
        self.to_string().build(tycker, env)
    }
}

// CtorName
impl Construct<CtorName> for CtorName {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> CtorName {
        self
    }
}
impl Construct<CtorName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CtorName {
        CtorName(self.to_string()).build(tycker, env)
    }
}

// DtorName
impl Construct<DtorName> for DtorName {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> DtorName {
        self
    }
}
impl Construct<DtorName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> DtorName {
        DtorName(self.to_string()).build(tycker, env)
    }
}

// DefId
impl Construct<DefId> for cs::Ann<VarName, KindId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> DefId {
        let cs::Ann(tm, ty) = self;
        Alloc::alloc(tycker, tm, ty.into())
    }
}
impl Construct<DefId> for cs::Ann<VarName, TypeId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> DefId {
        let cs::Ann(tm, ty) = self;
        Alloc::alloc(tycker, tm, ty.into())
    }
}
impl<A> Construct<DefId> for cs::Ann<String, A>
where
    A: Into<AnnId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> DefId {
        let cs::Ann(tm, ty) = self;
        cs::Ann(VarName(tm), ty.into()).build(tycker, env)
    }
}
impl<A> Construct<DefId> for cs::Ann<&str, A>
where
    A: Into<AnnId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> DefId {
        let cs::Ann(tm, ty) = self;
        cs::Ann(tm.to_string(), ty.into()).build(tycker, env)
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl Construct<KindId> for cs::TypeOf<TypeId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> KindId {
        let cs::TypeOf(ty) = self;
        tycker.statics.annotations_type[&ty]
    }
}
impl Construct<KindId> for cs::TypeOf<AbstId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> KindId {
        let cs::TypeOf(abst) = self;
        tycker.statics.annotations_abst[&abst]
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

impl Construct<TPatId> for cs::Ann<Option<DefId>, KindId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TPatId {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl Construct<TypeId> for cs::TypeOf<ValueId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> TypeId {
        let cs::TypeOf(value) = self;
        tycker.statics.annotations_value[&value]
    }
}
impl Construct<TypeId> for cs::TypeOf<CompuId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> TypeId {
        let cs::TypeOf(compu) = self;
        tycker.statics.annotations_compu[&compu]
    }
}
impl<K> Construct<TypeId> for cs::Ann<Hole, (K, su::TermId)>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Ann(Hole, (kd, site)) = self;
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
impl<T> Construct<TypeId> for cs::AbstTy<T>
where
    T: Construct<AbstId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::AbstTy(abst) = self;
        let abst = abst.build(tycker, env);
        abst.build(tycker, env)
    }
}

impl<S, F, T> Construct<TypeId> for Abs<S, F>
where
    S: Construct<TPatId>,
    F: Fn(&mut Tycker, &Env<AnnId>, TPatId, DefId, KindId) -> T,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let Abs(param, ty) = self;
        let tpat = param.build(tycker, env);
        let (def, param_kd) = tpat.destruct_def(tycker);
        let body = ty(tycker, env, tpat, def, param_kd).build(tycker, env);
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
impl<T> Construct<TypeId> for cs::Thk<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Thk(arg) = self;
        let thk = ThkTy.build(tycker, env);
        let arg = arg.build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, App(thk, arg), vtype)
    }
}
impl<T> Construct<Result<TypeId>> for cs::Thk<T>
where
    T: Construct<Result<TypeId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Thk(arg) = self;
        let thk = ThkTy.build(tycker, env);
        let arg = arg.build(tycker, env)?;
        let vtype = VType.build(tycker, env);
        Ok(Alloc::alloc(tycker, App(thk, arg), vtype))
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
// impl<P> cs::HExists<P>
// where
//     P: Construct<AbstId>,
// {
//     fn mk<F, T>(self, tycker: &mut Tycker, env: &Env<AnnId>, body: F) -> impl Construct<TypeId>
//     where
//         F: Fn(AbstId) -> T,
//         T: Construct<TypeId>,
//     {
//         let cs::HExists(abst) = self;
//         let abst = abst.build(tycker, env);
//         body(abst)
//     }
// }
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
impl<S, T> Construct<Result<TypeId>> for Arrow<S, T>
where
    S: Construct<Result<TypeId>>,
    T: Construct<Result<TypeId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let Arrow(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env)?;
        let ty_2 = ty_2.build(tycker, env)?;
        let ctype = tycker.ctype(env);
        Ok(Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype))
    }
}
impl<F, A, T> Construct<TypeId> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Forall(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, Forall(abst, ty), ctype)
    }
}
impl<F, A, T> Construct<Result<TypeId>> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<Result<TypeId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Forall(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env)?;
        let ctype = CType.build(tycker, env);
        Ok(Alloc::alloc(tycker, Forall(abst, ty), ctype))
    }
}
impl Construct<TypeId> for RetTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.ret.get()] else { unreachable!() };
        ty
    }
}
impl<T> Construct<TypeId> for cs::Ret<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> TypeId {
        let cs::Ret(arg) = self;
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
    // /// generates `Thunk B`
    // pub fn thk_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
    //     Thunk(arg).build(self, env)
    // }
    /// generates `Thunk _`
    pub fn thk_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        cs::Thk(cs::Ann(Hole, (CType, site))).build(self, env)
    }
    // /// generates `Ret A`
    // pub fn ret_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
    //     Ret(arg).build(self, env)
    // }
    /// generates `Ret _`
    pub fn ret_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        cs::Ret(cs::Ann(Hole, (VType, site))).build(self, env)
    }
    // pub fn type_top(&mut self, env: &Env<AnnId>) -> TypeId {
    //     cs::TopTy.build(self, env)
    // }
    // pub fn monad_mo(&mut self, env: &Env<AnnId>, monad_ty: TypeId) -> TypeId {
    //     cs::Monad(monad_ty).build(self, env)
    // }
    // pub fn algebra_mo_car(
    //     &mut self, env: &Env<AnnId>, monad_ty: TypeId, carrier: TypeId,
    // ) -> TypeId {
    //     cs::Algebra(monad_ty, carrier).build(self, env)
    // }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl Construct<VPatId> for cs::Ann<Option<DefId>, TypeId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> VPatId {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}

/* ---------------------------------- Value --------------------------------- */

impl Construct<ValueId> for DefId {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> ValueId {
        let AnnId::Type(ty) = tycker.statics.annotations_var[&self] else { unreachable!() };
        Alloc::alloc(tycker, self, ty)
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
        let ty = cs::Thk(body_ty).build(tycker, env);
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
    // pub fn value_var(&mut self, env: &Env<AnnId>, def: DefId, ty: TypeId) -> ValueId {
    //     cs::Ann(def, ty).build(self, env)
    // }
    // pub fn value_thunk(&mut self, env: &Env<AnnId>, body: CompuId) -> ValueId {
    //     Thunk(body).build(self, env)
    // }
    // pub fn value_triv(&mut self, env: &Env<AnnId>) -> ValueId {
    //     Triv.build(self, env)
    // }
    // pub fn value_vcons(&mut self, env: &Env<AnnId>, a: ValueId, b: ValueId) -> ValueId {
    //     Cons(a, b).build(self, env)
    // }
}

/* ------------------------------- Computation ------------------------------ */

// computation value abstraction
impl<P> cs::HAbs<P>
where
    P: Construct<VPatId>,
{
    pub fn try_vbody<F, T>(
        self, (tycker, env): (&mut Tycker, &Env<AnnId>), body: F,
    ) -> Result<CompuId>
    where
        F: Fn(&mut Tycker, &Env<AnnId>, Option<DefId>) -> Result<T>,
        T: Construct<CompuId>,
    {
        let cs::HAbs(vpat) = self;
        let vpat = vpat.build(tycker, env);
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let body = body(tycker, env, def)?;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env);
        let res = Alloc::alloc(tycker, Abs(vpat, body), ty);
        Ok(res)
    }
}
impl<V, F, T> Construct<CompuId> for Abs<cs::Ann<V, TypeId>, F>
where
    V: Construct<VarName>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> T,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Abs(cs::Ann(var, param_ty), body) = self;
        let var = var.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_ty.into());
        let vpat: VPatId = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def);
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env);
        Alloc::alloc(tycker, Abs(vpat, body), ty)
    }
}
impl<V, F, T> Construct<Result<CompuId>> for Abs<cs::Ann<V, TypeId>, F>
where
    V: Construct<VarName>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> Result<T>,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Abs(cs::Ann(var, param_ty), body) = self;
        let var = var.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_ty.into());
        let vpat: VPatId = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def)?;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env);
        Ok(Alloc::alloc(tycker, Abs(vpat, body), ty))
    }
}
// computation type abstraction
impl<P> cs::HAbs<P>
where
    P: Construct<TPatId>,
{
    pub fn try_tbody<F, T>(
        self, (tycker, env): (&mut Tycker, &Env<AnnId>), body: F,
    ) -> Result<CompuId>
    where
        F: Fn(&mut Tycker, &Env<AnnId>, Option<DefId>, AbstId) -> Result<T>,
        T: Construct<CompuId>,
    {
        let cs::HAbs(tpat) = self;
        let tpat = tpat.build(tycker, env);
        let (def, param_ty) = tpat.try_destruct_def(tycker);
        let abst = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def, abst)?;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env);
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        let res = Alloc::alloc(tycker, Abs(tpat, body), ty);
        Ok(res)
    }
}
impl<V, F, T> Construct<CompuId> for Abs<cs::Ann<V, KindId>, F>
where
    V: Construct<VarName>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId, AbstId) -> T,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Abs(cs::Ann(var, param_kd), body) = self;
        let var = var.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_kd.into());
        let abst = Alloc::alloc(tycker, def, param_kd);
        let tpat: TPatId = Alloc::alloc(tycker, def, param_kd);
        let body = body(tycker, env, def, abst);
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env);
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Alloc::alloc(tycker, Abs(tpat, body), ty)
    }
}
impl<V, F, T> Construct<Result<CompuId>> for Abs<cs::Ann<V, KindId>, F>
where
    V: Construct<VarName>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId, AbstId) -> Result<T>,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Abs(cs::Ann(var, param_kd), body) = self;
        let var = var.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_kd.into());
        let abst = Alloc::alloc(tycker, def, param_kd);
        let tpat: TPatId = Alloc::alloc(tycker, def, param_kd);
        let body = body(tycker, env, def, abst)?;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env);
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Ok(Alloc::alloc(tycker, Abs(tpat, body), ty))
    }
}
// computation value application
impl<S, T> Construct<CompuId> for App<S, T>
where
    S: Construct<CompuId>,
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let App(abs, arg) = self;
        let abs = abs.build(tycker, env);
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else { unreachable!() };
        let arg = arg.build(tycker, env);
        let arg_ty = tycker.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, tycker) else { unreachable!() };
        Alloc::alloc(tycker, App(abs, arg), body_ty)
    }
}
impl<S, T> Construct<Result<CompuId>> for App<S, T>
where
    S: Construct<Result<CompuId>>,
    T: Construct<Result<ValueId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let App(abs, arg) = self;
        let abs = abs.build(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else { unreachable!() };
        let arg = arg.build(tycker, env)?;
        let arg_ty = tycker.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, tycker) else { unreachable!() };
        Ok(Alloc::alloc(tycker, App(abs, arg), body_ty))
    }
}
// computation type application
impl<S, T> Construct<CompuId> for App<S, cs::Ty<T>>
where
    S: Construct<CompuId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let App(abs, cs::Ty(arg)) = self;
        let abs = abs.build(tycker, env);
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(tycker) else { unreachable!() };
        let param_kd = tycker.statics.annotations_abst[&abst];
        let arg = arg.build(tycker, env);
        let arg_kd = tycker.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, tycker) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(tycker, (abst, arg)) else { unreachable!() };
        Alloc::alloc(tycker, App(abs, arg), ty)
    }
}
impl<S, T> Construct<Result<CompuId>> for App<S, cs::Ty<T>>
where
    S: Construct<Result<CompuId>>,
    T: Construct<Result<TypeId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let App(abs, cs::Ty(arg)) = self;
        let abs = abs.build(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(tycker) else { unreachable!() };
        let param_kd = tycker.statics.annotations_abst[&abst];
        let arg = arg.build(tycker, env)?;
        let arg_kd = tycker.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, tycker) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(tycker, (abst, arg)) else { unreachable!() };
        Ok(Alloc::alloc(tycker, App(abs, arg), ty))
    }
}
// fixed point
impl<H> cs::HAbs<H>
where
    H: Construct<VPatId>,
{
    pub fn try_fix<F, T>(
        self, (tycker, env): (&mut Tycker, &Env<AnnId>), body: F,
    ) -> Result<CompuId>
    where
        F: Fn(&mut Tycker, &Env<AnnId>, Option<DefId>) -> Result<T>,
        T: Construct<CompuId>,
    {
        let cs::HAbs(vpat) = self;
        let vpat = vpat.build(tycker, env);
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let Some(ty) = param_ty.destruct_thk_app(tycker) else { unreachable!() };
        let body = body(tycker, env, def)?;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let Ok(_) = Lub::lub(ty, body_ty, tycker) else { unreachable!() };
        let res = Alloc::alloc(tycker, Fix(vpat, body), ty);
        Ok(res)
    }
}
impl<V, F> Construct<CompuId> for Fix<cs::Ann<V, TypeId>, F>
where
    V: Construct<VarName>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> CompuId,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Fix(cs::Ann(var, param_ty), body) = self;
        let var = var.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_ty.into());
        let Some(ty) = param_ty.destruct_thk_app(tycker) else { unreachable!() };
        let vpat: VPatId = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def);
        let body_ty = tycker.statics.annotations_compu[&body];
        let Ok(_) = Lub::lub(ty, body_ty, tycker) else { unreachable!() };
        Alloc::alloc(tycker, Fix(vpat, body), ty)
    }
}
// force
impl<T> Construct<CompuId> for Force<T>
where
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Force(thk) = self;
        let thk = thk.build(tycker, env);
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else { unreachable!() };
        Alloc::alloc(tycker, Force(thk), body_ty)
    }
}
impl<T> Construct<Result<CompuId>> for Force<T>
where
    T: Construct<Result<ValueId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Force(thk) = self;
        let thk = thk.build(tycker, env)?;
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else { unreachable!() };
        Ok(Alloc::alloc(tycker, Force(thk), body_ty))
    }
}
// return
impl<T> Construct<CompuId> for Ret<T>
where
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Ret(val) = self;
        let val = val.build(tycker, env);
        let val_ty = tycker.statics.annotations_value[&val];
        let ret_ty = cs::Ret(val_ty).build(tycker, env);
        Alloc::alloc(tycker, Ret(val), ret_ty)
    }
}
impl<T> Construct<Result<CompuId>> for Ret<T>
where
    T: Construct<Result<ValueId>>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Ret(val) = self;
        let val = val.build(tycker, env)?;
        let val_ty = tycker.statics.annotations_value[&val];
        let ret_ty = cs::Ret(val_ty).build(tycker, env);
        Ok(Alloc::alloc(tycker, Ret(val), ret_ty))
    }
}
// bind
impl<V, B, F> Construct<CompuId> for Bind<V, B, F>
where
    V: Construct<VarName>,
    B: Construct<CompuId>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> CompuId,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Bind { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env);
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(tycker) else { unreachable!() };
        let var = binder.build(tycker, env);
        let def = Alloc::alloc(tycker, var, def_ty.into());
        let binder = Alloc::alloc(tycker, def, def_ty);
        let tail = tail(tycker, env, def);
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty)
    }
}
// pure bind
impl<V, B, F> Construct<CompuId> for PureBind<V, B, F>
where
    V: Construct<VarName>,
    B: Construct<ValueId>,
    F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> CompuId,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let PureBind { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env);
        let def_ty = tycker.statics.annotations_value[&bindee];
        let var = binder.build(tycker, env);
        let def = Alloc::alloc(tycker, var, def_ty.into());
        let binder = Alloc::alloc(tycker, def, def_ty);
        let tail = tail(tycker, env, def);
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Alloc::alloc(tycker, PureBind { binder, bindee, tail }, tail_ty)
    }
}
// top
impl Construct<CompuId> for cs::Top {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let top = cs::TopTy.build(tycker, env);
        Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top)
    }
}
// dtor
impl Construct<CompuId> for Dtor<CompuId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let Dtor(head, dtor) = self;
        let head_ty = tycker.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, tycker) else { unreachable!() };
        let Some(ty) = coda.get(&dtor).cloned() else { unreachable!() };
        Alloc::alloc(tycker, Dtor(head, dtor), ty)
    }
}
impl<T, D> Construct<CompuId> for cs::Dtor<T, D>
where
    T: Construct<CompuId>,
    D: Construct<DtorName>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> CompuId {
        let cs::Dtor(head, dtor) = self;
        let head = head.build(tycker, env);
        let dtor = dtor.build(tycker, env);
        Dtor(head, dtor).build(tycker, env)
    }
}
impl Construct<Result<CompuId>> for Dtor<CompuId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Dtor(head, dtor) = self;
        let head_ty = tycker.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, tycker) else { unreachable!() };
        let Some(ty) = coda.get(&dtor).cloned() else { unreachable!() };
        Ok(Alloc::alloc(tycker, Dtor(head, dtor), ty))
    }
}
impl<T, D> Construct<Result<CompuId>> for cs::Dtor<T, D>
where
    T: Construct<Result<CompuId>>,
    D: Construct<DtorName>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let cs::Dtor(head, dtor) = self;
        let head = head.build(tycker, env)?;
        let dtor = dtor.build(tycker, env);
        Ok(Dtor(head, dtor).build(tycker, env))
    }
}

impl Tycker {
    pub fn compu_vabs<F, T>(
        &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
        param_ty: impl Construct<TypeId>, body: F,
    ) -> CompuId
    where
        F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> T,
        T: Construct<CompuId>,
    {
        let param_ty = param_ty.build(self, env);
        Abs(cs::Ann(name, param_ty), body).build(self, env)
    }
    pub fn try_compu_vabs<F, T>(
        &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
        param_ty: impl Construct<TypeId>, body: F,
    ) -> Result<CompuId>
    where
        F: Fn(&mut Self, &Env<AnnId>, DefId) -> Result<T>,
        T: Construct<CompuId>,
    {
        let param_ty = param_ty.build(self, env);
        Abs(cs::Ann(name, param_ty), body).build(self, env)
    }
    pub fn compu_tabs(
        &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
        param_kd: impl Construct<KindId>,
        body: impl Fn(&mut Self, &Env<AnnId>, DefId, AbstId) -> CompuId,
    ) -> CompuId {
        let param_kd = param_kd.build(self, env);
        Abs(cs::Ann(name, param_kd), body).build(self, env)
    }
    pub fn try_compu_tabs<F, T>(
        &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
        param_kd: impl Construct<KindId>, body: F,
    ) -> Result<CompuId>
    where
        F: Fn(&mut Self, &Env<AnnId>, DefId, AbstId) -> Result<T>,
        T: Construct<CompuId>,
    {
        let param_kd = param_kd.build(self, env);
        Abs(cs::Ann(name, param_kd), body).build(self, env)
    }
    // pub fn compu_vapp(&mut self, env: &Env<AnnId>, abs: CompuId, arg: ValueId) -> CompuId {
    //     App(abs, arg).build(self, env)
    // }
    // pub fn compu_tapp(&mut self, env: &Env<AnnId>, abs: CompuId, arg: TypeId) -> CompuId {
    //     App(abs, arg).build(self, env)
    // }
    pub fn compu_fix(
        &mut self, env: &Env<AnnId>, name: VarName, ty: TypeId,
        body: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    ) -> CompuId {
        Fix(cs::Ann(name, ty), body).build(self, env)
    }
    // pub fn compu_force(&mut self, env: &Env<AnnId>, thk: ValueId) -> CompuId {
    //     Force(thk).build(self, env)
    // }
    // pub fn compu_ret(&mut self, env: &Env<AnnId>, val: ValueId) -> CompuId {
    //     Ret(val).build(self, env)
    // }
    pub fn compu_bind(
        &mut self, env: &Env<AnnId>, bindee: CompuId, name: VarName,
        tail: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    ) -> CompuId {
        Bind { binder: name, bindee, tail }.build(self, env)
    }
    pub fn compu_let(
        &mut self, env: &Env<AnnId>, bindee: ValueId, name: VarName,
        tail: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    ) -> CompuId {
        PureBind { binder: name, bindee, tail }.build(self, env)
    }
    // pub fn compu_top(&mut self, env: &Env<AnnId>) -> CompuId {
    //     cs::Top.build(self, env)
    // }
    // pub fn compu_dtor(
    //     &mut self, env: &Env<AnnId>, head: CompuId, dtor: impl Construct<DtorName>,
    // ) -> CompuId {
    //     let dtor = dtor.build(self, env);
    //     Dtor(head, dtor).build(self, env)
    // }
}
