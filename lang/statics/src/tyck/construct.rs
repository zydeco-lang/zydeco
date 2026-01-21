//! Constructors for patterns, types, and terms in [`StaticsArena`].
//!
//! This module provides the [`Construct`] trait, which describes a convenient DSL for
//! writing Zydeco programs in Rust. If you're familiar with HOAS (High Order Abstract Syntax),
//! we're trying to achieve exactly the same in this trait.
//! [`Construct`] has the following semantics:
//!
//! `impl` [`Construct<Arena, T>`] for `S` means that `S` can be used to construct `T`
//!
//!
//! ## Comparison with [`Alloc`]
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
//!
//!
//! ## Advice on writing Zydeco programs in [`Construct`] style
//!
//! [`Construct`] works very similar to how monadic expressions work (e.g. in Haskell).
//! That is, you should think of them as a thin piece of syntax or "recipe" for
//! constructing terms that may have interactions with the `tycker`, or side effects
//! in general.
//!
//! If there's a side effect that is not currently supported by [`Construct`], you should
//! create a new structure in [`syntax`] module, and implement [`Construct`] for it.
//!
//!
//! ## Advice on compile-time debugging
//!
//! When your Zydeco program written in [`Construct`] fails to compile, it's likely
//! that you're not constructing the term correctly; if not, that's a bug in this module,
//! which is sad. (>_<)
//!
//! To find out what's happening, you should first try to break down the term that's been
//! constructed into smaller pieces, and see if you can find the problem.
//! Two useful approaches are:
//! + Insert small and trivial terms like [`cs::TopTy`] for type, [`cs::Top`] for computation,
//!   and [`Triv`] for value, and see if the program can partially compile.
//! + Break subterms into let bindings `let small = ...;`, and see if the small bindees
//!   can compile by calling `small.build(tycker, env)`.
//!
//! Finally, a word of caution:
//! + If you're having issues regarding lifetime, make sure to put `move` on the closure,
//!   since they implement [`FnOnce`] instead of [`Fn`].
//!
//! Good luck (つ´ω｀)つ

use super::syntax::*;
use crate::*;

/// Trait for constructing entities in [`Tycker`] with more type inference available.
///
/// The only method provided is [`Construct::build`], which takes `&mut` [`Tycker`]
/// and the environment of the type checked program, and returns the constructed value.
///
/// The trait is different from [`Alloc`] in that it does not require feeding annotations
/// manually, but instead infers them from the context.
pub trait Construct<Arena, T>: Sized {
    /// Build the term with the given type checker and environment.
    ///
    /// See [`Construct`] level documentation for more details.
    fn build(self, arena: &mut Arena, env: &TyEnv) -> T;
}

/// Syntax used by [`Construct`]. They work together as part of the HOAS
/// (High Order Abstract Syntax) for Zydeco in Rust. Specifically,
///
/// + Structures that carry semantic actions, such as `Ann`, `Pat`, `Ty`, `TypeOf`,
///   `Fresh`, and other structures related to the algebra translation.
/// + Some structures that are not defined as part of the common syntax of Zydeco,
///   such as `Monad` and `Algebra`.
/// + Some existing structures that may take a more convenient syntax,
///   such as `Ann`, `Ctor`, and `Dtor`.
pub mod syntax {
    /// Monadic bind for [`super::Construct`]
    pub struct CBind<T, I, F>(pub T, pub std::marker::PhantomData<I>, pub F);
    impl<T, I, F> CBind<T, I, F> {
        pub fn new(t: T, f: F) -> Self {
            Self(t, std::marker::PhantomData, f)
        }
    }

    /// `Ann { tm: S, ty: A }`
    #[derive(Clone, Copy)]
    pub struct Ann<S, A>(pub S, pub A);

    /// annotated pattern
    #[derive(Clone, Copy)]
    pub struct Pat<S, A>(pub S, pub A);

    /// type indicator; used to resolve trait conflicts
    ///
    /// see [`super::App`] implementations on term level for examples
    #[derive(Clone, Copy)]
    pub struct Ty<T>(pub T);

    /// take the annotated type of a term, or the kind of a type
    #[derive(Clone, Copy)]
    pub struct TypeOf<T>(pub T);

    /// Construct to type immediately
    #[derive(Clone, Copy)]
    pub struct Type<T>(pub T);
    /// Construct to value immediately
    #[derive(Clone, Copy)]
    pub struct Value<T>(pub T);
    /// Construct to computation immediately
    #[derive(Clone, Copy)]
    pub struct Compu<T>(pub T);

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

    /// `data ... end`
    ///
    /// `T` here should only be DataId
    pub struct Data<T, F>(pub T, pub F);
    /// `codata ... end`
    ///
    /// `T` here should only be CoDataId
    pub struct CoData<T, F>(pub T, pub F);

    /// `exists X. ...`
    pub struct Exists<Param, FBody>(pub Param, pub FBody);
    /// `forall X. ...`
    pub struct Forall<Param, FBody>(pub Param, pub FBody);

    /// `+Ctor(<tail>)`
    pub struct Ctor<Name, Tail>(pub Name, pub Tail);
    /// `match_ty <t> | <f> ... end`
    ///
    /// `Ty` here should be DataId
    pub struct Match<T, F>(pub T, pub F);
    /// `comatch_ty | <f> ... end`
    ///
    /// `Ty` here should be CoDataId
    pub struct CoMatch<Ty, F>(pub Ty, pub F);
    /// `<head> .dtor`
    pub struct Dtor<Head, Name>(pub Head, pub Name);
    /// `comatch end`
    pub struct Top;

    /// TCons
    pub struct TCons<T, F>(pub T, pub F);

    pub use crate::tyck::monadic::syntax::*;
}

/// Trivial [`Construct`] construction
macro_rules! impl_construct_trivial {
    ($($ty:ty),*) => {
        $(
            impl<'a> Construct<Tycker<'a>, $ty> for $ty {
                fn build(self, _tycker: &mut Tycker<'a>, _env: &TyEnv) -> $ty {
                    self
                }
            }
        )*
    }
}

/// [`Construct`] implementation for all [`Alloc`] implementors.
impl<'a, S, T, A, U> Construct<Tycker<'a>, T> for cs::Ann<S, U>
where
    U: Construct<Tycker<'a>, A>,
    S: Alloc<Tycker<'a>, T, Ann = A, Env = TyEnv>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> T {
        let cs::Ann(tm, ty) = self;
        let ty = ty.build(tycker, env);
        Alloc::alloc(tycker, tm, ty, env)
    }
}

/* --------------------------------- Monadic -------------------------------- */

impl<'a, T, F, I, O, R> Construct<Tycker<'a>, R> for cs::CBind<T, I, F>
where
    T: Construct<Tycker<'a>, I>,
    F: FnOnce(I) -> O,
    O: Construct<Tycker<'a>, R>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> R {
        let cs::CBind(input, _, f) = self;
        let input = input.build(tycker, env);
        f(input).build(tycker, env)
    }
}

/* ------------------------------- Identifier ------------------------------- */

impl_construct_trivial!(
    Option<DefId>,
    DefId,
    KindId,
    AbstId,
    TPatId,
    TypeId,
    VPatId,
    ValueId,
    CompuId
);

/* ------------------------------- Definition ------------------------------- */

impl_construct_trivial!(VarName, CtorName, DtorName);

// VarName
impl<'a> Construct<Tycker<'a>, VarName> for String {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VarName {
        VarName(self).build(tycker, env)
    }
}
impl<'a> Construct<Tycker<'a>, VarName> for &str {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VarName {
        self.to_string().build(tycker, env)
    }
}

// CtorName
impl<'a> Construct<Tycker<'a>, CtorName> for &str {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CtorName {
        CtorName(self.to_string()).build(tycker, env)
    }
}

// DtorName
impl<'a> Construct<Tycker<'a>, DtorName> for &str {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> DtorName {
        DtorName(self.to_string()).build(tycker, env)
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl<'a, K> Construct<Tycker<'a>, AbstId> for cs::Ann<VarName, K>
where
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> AbstId {
        let cs::Ann(var, kd) = self;
        let kd = kd.build(tycker, _env);
        let def = Alloc::alloc(tycker, var, kd.into(), &());
        Alloc::alloc(tycker, def, kd, &())
    }
}
impl<'a, K> Construct<Tycker<'a>, AbstId> for cs::Ann<String, K>
where
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> AbstId {
        let cs::Ann(tm, kd) = self;
        cs::Ann(VarName(tm), kd).build(tycker, _env)
    }
}
impl<'a, K> Construct<Tycker<'a>, AbstId> for cs::Ann<&str, K>
where
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> AbstId {
        let cs::Ann(tm, kd) = self;
        cs::Ann(tm.to_string(), kd).build(tycker, _env)
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl<'a> Construct<Tycker<'a>, KindId> for cs::TypeOf<TPatId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> KindId {
        let cs::TypeOf(ty) = self;
        tycker.statics.annotations_tpat[&ty]
    }
}
impl<'a> Construct<Tycker<'a>, KindId> for cs::TypeOf<TypeId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> KindId {
        let cs::TypeOf(ty) = self;
        tycker.statics.annotations_type[&ty]
    }
}
impl<'a> Construct<Tycker<'a>, KindId> for cs::TypeOf<AbstId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> KindId {
        let cs::TypeOf(abst) = self;
        tycker.statics.annotations_abst[&abst]
    }
}
impl<'a> Construct<Tycker<'a>, KindId> for VType {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.vtype.get()] else { unreachable!() };
        kd
    }
}
impl<'a> Construct<Tycker<'a>, KindId> for CType {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.ctype.get()] else { unreachable!() };
        kd
    }
}
impl<'a, S, T> Construct<Tycker<'a>, KindId> for Arrow<S, T>
where
    S: Construct<Tycker<'a>, KindId>,
    T: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> KindId {
        let Arrow(k1, k2) = self;
        let k1 = k1.build(tycker, env);
        let k2 = k2.build(tycker, env);
        Alloc::alloc(tycker, Arrow(k1, k2), (), &())
    }
}

#[cfg(test)]
mod kind_test {
    use super::super::{syntax::*, *};

    #[test]
    fn r#static() {
        fn _f<'a>(tycker: &mut Tycker<'a>, env: &TyEnv) -> KindId {
            // VType -> (CType -> CType)
            Arrow(VType, Arrow(CType, CType)).build(tycker, env)
        }
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl<'a> Construct<Tycker<'a>, TPatId> for cs::Ann<Option<DefId>, KindId> {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TPatId {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}
impl<'a, K> Construct<Tycker<'a>, TPatId> for cs::Ann<VarName, K>
where
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TPatId {
        let cs::Ann(var, kd) = self;
        let ty = kd.build(tycker, env);
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).build(tycker, env)
    }
}
impl<'a, V, K> Construct<Tycker<'a>, TPatId> for cs::Pat<V, K>
where
    V: Construct<Tycker<'a>, VarName>,
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TPatId {
        let cs::Pat(var, kd) = self;
        let var = var.build(tycker, env);
        let ty = kd.build(tycker, env);
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).build(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<'a, T> Construct<Tycker<'a>, TypeId> for cs::Type<T>
where
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Type(ty) = self;
        ty.build(tycker, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for cs::TypeOf<VPatId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> TypeId {
        let cs::TypeOf(vpat) = self;
        tycker.statics.annotations_vpat[&vpat]
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for cs::TypeOf<ValueId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> TypeId {
        let cs::TypeOf(value) = self;
        tycker.statics.annotations_value[&value]
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for cs::TypeOf<CompuId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> TypeId {
        let cs::TypeOf(compu) = self;
        tycker.statics.annotations_compu[&compu]
    }
}
impl<'a, K> Construct<Tycker<'a>, TypeId> for cs::Ann<Hole, (K, su::TermId)>
where
    K: Construct<Tycker<'a>, KindId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Ann(Hole, (kd, site)) = self;
        let kd = kd.build(tycker, env);
        let fill = tycker.statics.fills.alloc(site);
        Alloc::alloc(tycker, fill, kd, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for DefId {
    fn build(self, _tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        // Note: different from monadic construction, here we do not need to further substitute
        let AnnId::Type(ty) = env[&self] else { unreachable!() };
        ty
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for AbstId {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let kd = tycker.statics.annotations_abst[&self];
        Alloc::alloc(tycker, self, kd, env)
    }
}
impl<'a, T> Construct<Tycker<'a>, TypeId> for cs::Ty<T>
where
    T: Construct<Tycker<'a>, AbstId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Ty(abst) = self;
        let abst = abst.build(tycker, env);
        abst.build(tycker, env)
    }
}
impl<'a, S, F, T> Construct<Tycker<'a>, TypeId> for Abs<S, F>
where
    S: Construct<Tycker<'a>, TPatId>,
    F: FnOnce(TPatId, DefId, KindId) -> T,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let Abs(param, ty) = self;
        let tpat = param.build(tycker, env);
        let (def, param_kd) = tpat.destruct_def(tycker);
        let body = ty(tpat, def, param_kd).build(tycker, env);
        let kd = Arrow(param_kd, cs::TypeOf(body)).build(tycker, env);
        Alloc::alloc(tycker, Abs(tpat, body), kd, env)
    }
}
impl<'a, S, T> Construct<Tycker<'a>, TypeId> for App<S, T>
where
    S: Construct<Tycker<'a>, TypeId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let App(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let kd_1 = tycker.statics.annotations_type[&ty_1];
        let Some((_kd_a, kd_b)) = kd_1.destruct_arrow(tycker) else { unreachable!() };
        let ty_2 = ty_2.build(tycker, env);
        // let kd_2 = tycker.statics.annotations_type[&ty_2];
        // let Ok(_) = Lub::lub(kd_a, kd_2, tycker) else { unreachable!() };
        // Note: note that the resulting type application of [`Construct::build`] is not normalized
        Alloc::alloc(tycker, App(ty_1, ty_2), kd_b, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for IntTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.int.get()] else { unreachable!() };
        ty
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for CharTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.char.get()] else { unreachable!() };
        ty
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for StringTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.string.get()] else { unreachable!() };
        ty
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for ThkTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.thk.get()] else { unreachable!() };
        ty
    }
}
impl<'a, T> Construct<Tycker<'a>, TypeId> for cs::Thk<T>
where
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Thk(arg) = self;
        let thk = ThkTy.build(tycker, env);
        let arg = arg.build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, App(thk, arg), vtype, env)
    }
}
impl<'a, F, T> Construct<Tycker<'a>, TypeId> for cs::Data<DataId, F>
where
    F: Clone + FnOnce(CtorName, TypeId) -> T,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Data(data, f) = self;
        let arms = tycker.statics.datas[&data].clone();
        let arms_ = arms
            .into_iter()
            .map(|(ctor, ty)| {
                let ty_ = (f.clone())(ctor.clone(), ty).build(tycker, env);
                (ctor, ty_)
            })
            .collect::<im::Vector<_>>();
        let data = tycker.statics.datas.alloc(Data::new(arms_));
        let kd = VType.build(tycker, env);
        Alloc::alloc(tycker, data, kd, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for UnitTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.unit.get()] else { unreachable!() };
        ty
    }
}
impl<'a, S, T> Construct<Tycker<'a>, TypeId> for Prod<S, T>
where
    S: Construct<Tycker<'a>, TypeId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let Prod(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let ty_2 = ty_2.build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, Prod(ty_1, ty_2), vtype, env)
    }
}
impl<'a, F, A, T> Construct<Tycker<'a>, TypeId> for cs::Exists<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<Tycker<'a>, AbstId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Exists(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env);
        let vtype = VType.build(tycker, env);
        Alloc::alloc(tycker, Exists(abst, ty), vtype, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for OSTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.os.get()] else { unreachable!() };
        ty
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for cs::TopTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let ctype = CType.build(tycker, env);
        let coda = tycker.statics.codatas.alloc(CoData::new([]));
        Alloc::alloc(tycker, coda, ctype, env)
    }
}
impl<'a, F, T> Construct<Tycker<'a>, TypeId> for cs::CoData<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> T,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::CoData(coda, f) = self;
        let arms = tycker.statics.codatas[&coda].clone();
        let arms_ = arms
            .into_iter()
            .map(|(dtor, ty)| {
                let ty_ = (f.clone())(dtor.clone(), ty).build(tycker, env);
                (dtor, ty_)
            })
            .collect::<im::Vector<_>>();
        let coda = tycker.statics.codatas.alloc(CoData::new(arms_));
        let kd = CType.build(tycker, env);
        Alloc::alloc(tycker, coda, kd, env)
    }
}
impl<'a, S, T> Construct<Tycker<'a>, TypeId> for Arrow<S, T>
where
    S: Construct<Tycker<'a>, TypeId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let Arrow(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env);
        let ty_2 = ty_2.build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype, env)
    }
}
impl<'a, F, A, T> Construct<Tycker<'a>, TypeId> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<Tycker<'a>, AbstId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Forall(abst, ty) = self;
        let abst = abst.build(tycker, env);
        let ty = ty(abst).build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, Forall(abst, ty), ctype, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for RetTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.ret.get()] else { unreachable!() };
        ty
    }
}
impl<'a, T> Construct<Tycker<'a>, TypeId> for cs::Ret<T>
where
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Ret(arg) = self;
        let ret = RetTy.build(tycker, env);
        let arg = arg.build(tycker, env);
        let ctype = CType.build(tycker, env);
        Alloc::alloc(tycker, App(ret, arg), ctype, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for syntax::MonadTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.monad.get()] else { unreachable!() };
        ty
    }
}
impl<'a, M> Construct<Tycker<'a>, TypeId> for cs::Monad<M>
where
    M: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Monad(monad_ty) = self;
        App(cs::MonadTy, monad_ty).build(tycker, env)
    }
}
impl<'a> Construct<Tycker<'a>, TypeId> for syntax::AlgebraTy {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let AnnId::Type(ty) = env[tycker.prim.algebra.get()] else { unreachable!() };
        ty
    }
}
impl<'a, M, R> Construct<Tycker<'a>, TypeId> for cs::Algebra<M, R>
where
    M: Construct<Tycker<'a>, TypeId>,
    R: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> TypeId {
        let cs::Algebra(monad_ty, carrier) = self;
        App(App(cs::AlgebraTy, monad_ty), carrier).build(tycker, env)
    }
}

impl<'a> Tycker<'a> {
    /// generates `Thunk B`
    pub fn thk_arg(&mut self, env: &TyEnv, arg: TypeId) -> TypeId {
        App(ThkTy, arg).build(self, env)
    }
    /// generates `Thunk _`
    pub fn thk_hole(&mut self, env: &TyEnv, site: su::TermId) -> TypeId {
        cs::Thk(cs::Ann(Hole, (CType, site))).build(self, env)
    }
    /// generates `Ret A`
    pub fn ret_arg(&mut self, env: &TyEnv, arg: TypeId) -> TypeId {
        App(RetTy, arg).build(self, env)
    }
    /// generates `Ret _`
    pub fn ret_hole(&mut self, env: &TyEnv, site: su::TermId) -> TypeId {
        cs::Ret(cs::Ann(Hole, (VType, site))).build(self, env)
    }
    pub fn type_top(&mut self, env: &TyEnv) -> TypeId {
        cs::TopTy.build(self, env)
    }
    pub fn monad_mo(&mut self, env: &TyEnv, monad_ty: TypeId) -> TypeId {
        cs::Monad(monad_ty).build(self, env)
    }
    pub fn algebra_mo_car(&mut self, env: &TyEnv, monad_ty: TypeId, carrier: TypeId) -> TypeId {
        cs::Algebra(monad_ty, carrier).build(self, env)
    }
}

/* ------------------------------ ValuePattern ------------------------------ */
impl<'a> Construct<Tycker<'a>, VPatId> for cs::Ann<Option<DefId>, TypeId> {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VPatId {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}
impl<'a, T> Construct<Tycker<'a>, VPatId> for cs::Pat<Option<DefId>, T>
where
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VPatId {
        let cs::Pat(tm, ty) = self;
        let ty = ty.build(tycker, env);
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}
impl<'a, T> Construct<Tycker<'a>, VPatId> for cs::Ann<VarName, T>
where
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VPatId {
        let cs::Ann(var, ty) = self;
        let ty = ty.build(tycker, env);
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).build(tycker, env)
    }
}
impl<'a, V, T> Construct<Tycker<'a>, VPatId> for cs::Pat<V, T>
where
    V: Construct<Tycker<'a>, VarName>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> VPatId {
        let cs::Pat(var, ty) = self;
        let var = var.build(tycker, env);
        let ty = ty.build(tycker, env);
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).build(tycker, env)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<'a, T> Construct<Tycker<'a>, ValueId> for cs::Value<T>
where
    T: Construct<Tycker<'a>, ValueId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let cs::Value(arg) = self;
        arg.build(tycker, env)
    }
}
impl<'a> Construct<Tycker<'a>, ValueId> for DefId {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let AnnId::Type(ty) = tycker.statics.annotations_var[&self] else { unreachable!() };
        Alloc::alloc(tycker, self, ty, env)
    }
}
impl<'a> Construct<Tycker<'a>, ValueId> for Option<DefId> {
    fn build(self, tycker: &mut Tycker<'a>, _env: &TyEnv) -> ValueId {
        let Some(def) = self else { unreachable!() };
        def.build(tycker, _env)
    }
}
impl<'a, T> Construct<Tycker<'a>, ValueId> for Thunk<T>
where
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let Thunk(body) = self;
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = cs::Thk(body_ty).build(tycker, env);
        Alloc::alloc(tycker, Thunk(body), ty, env)
    }
}
impl<'a> Construct<Tycker<'a>, ValueId> for Triv {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let ty = UnitTy.build(tycker, env);
        Alloc::alloc(tycker, Triv, ty, env)
    }
}
impl<'a, S, T> Construct<Tycker<'a>, ValueId> for Cons<S, T>
where
    S: Construct<Tycker<'a>, ValueId>,
    T: Construct<Tycker<'a>, ValueId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let Cons(a, b) = self;
        let a = a.build(tycker, env);
        let a_ty = tycker.statics.annotations_value[&a];
        let b = b.build(tycker, env);
        let b_ty = tycker.statics.annotations_value[&b];
        let ty = Prod(a_ty, b_ty).build(tycker, env);
        Alloc::alloc(tycker, Cons(a, b), ty, env)
    }
}
impl<'a, S, V, T> Construct<Tycker<'a>, ValueId> for cs::Ann<Cons<cs::Ty<S>, V>, T>
where
    S: Construct<Tycker<'a>, TypeId>,
    V: Construct<Tycker<'a>, ValueId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let cs::Ann(Cons(cs::Ty(a), b), ty) = self;
        let a = a.build(tycker, env);
        let b = b.build(tycker, env);
        let ty = ty.build(tycker, env);
        cs::Ann(Cons(a, b), ty).build(tycker, env)
    }
}
impl<'a, C, V, T> Construct<Tycker<'a>, ValueId> for cs::Ann<cs::Ctor<C, V>, T>
where
    C: Construct<Tycker<'a>, CtorName>,
    V: Construct<Tycker<'a>, ValueId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> ValueId {
        let cs::Ann(cs::Ctor(ctor, body), ty) = self;
        let ctor = ctor.build(tycker, env);
        let body = body.build(tycker, env);
        let ty = ty.build(tycker, env);
        cs::Ann(Ctor(ctor, body), ty).build(tycker, env)
    }
}

impl<'a> Tycker<'a> {
    pub fn value_var(&mut self, env: &TyEnv, def: DefId, ty: TypeId) -> ValueId {
        cs::Ann(def, ty).build(self, env)
    }
    pub fn value_thunk(&mut self, env: &TyEnv, body: CompuId) -> ValueId {
        Thunk(body).build(self, env)
    }
    pub fn value_triv(&mut self, env: &TyEnv) -> ValueId {
        Triv.build(self, env)
    }
    pub fn value_vcons(&mut self, env: &TyEnv, a: ValueId, b: ValueId) -> ValueId {
        Cons(a, b).build(self, env)
    }
}

/* ------------------------------- Computation ------------------------------ */

impl<'a, T> Construct<Tycker<'a>, CompuId> for cs::Compu<T>
where
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let cs::Compu(arg) = self;
        arg.build(tycker, env)
    }
}
/// computation value abstraction
impl<'a, P, F, T> Construct<Tycker<'a>, CompuId> for Abs<P, F>
where
    P: Construct<Tycker<'a>, VPatId>,
    F: FnOnce(Option<DefId>) -> T,
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Abs(vpat, body) = self;
        let vpat: VPatId = vpat.build(tycker, env);
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let body = body(def).build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env);
        Alloc::alloc(tycker, Abs(vpat, body), ty, env)
    }
}
/// computation type abstraction
impl<'a, P, F, T> Construct<Tycker<'a>, CompuId> for Abs<cs::Ty<P>, F>
where
    P: Construct<Tycker<'a>, TPatId>,
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Abs(cs::Ty(tpat), body) = self;
        let tpat: TPatId = tpat.build(tycker, env);
        let (def, param_kd) = tpat.try_destruct_def(tycker);
        let abst = Alloc::alloc(tycker, def, param_kd, &());
        let body = body(def, abst);
        // TODO: add env when building the body
        let body = body.build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env);
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype, env);
        Alloc::alloc(tycker, Abs(tpat, body), ty, env)
    }
}
impl<'a, F, T> Construct<Tycker<'a>, CompuId> for Abs<cs::Ty<AbstId>, F>
where
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Abs(cs::Ty(abst), body) = self;
        use zydeco_utils::arena::ArenaAccess;
        let def = tycker.statics.abst_hints.get(&abst).cloned();
        let param_kd = tycker.statics.annotations_abst[&abst];
        let tpat: TPatId = cs::Ann(def, param_kd).build(tycker, env);
        // TODO: add env when building the body
        let body = body(def, abst).build(tycker, env);
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env);
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype, env);
        Alloc::alloc(tycker, Abs(tpat, body), ty, env)
    }
}
// computation value application
impl<'a, S, T> Construct<Tycker<'a>, CompuId> for App<S, T>
where
    S: Construct<Tycker<'a>, CompuId>,
    T: Construct<Tycker<'a>, ValueId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let App(abs, arg) = self;
        let abs = abs.build(tycker, env);
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else { unreachable!() };
        let arg = arg.build(tycker, env);
        let arg_ty = tycker.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, tycker) else { unreachable!() };
        Alloc::alloc(tycker, App(abs, arg), body_ty, env)
    }
}
// computation type application
impl<'a, S, T> Construct<Tycker<'a>, CompuId> for App<S, cs::Ty<T>>
where
    S: Construct<Tycker<'a>, CompuId>,
    T: Construct<Tycker<'a>, TypeId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let App(abs, cs::Ty(arg)) = self;
        let abs = abs.build(tycker, env);
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(tycker) else { unreachable!() };
        let param_kd = tycker.statics.annotations_abst[&abst];
        let arg = arg.build(tycker, env);
        let arg_kd = tycker.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, tycker) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(tycker, (abst, arg)) else { unreachable!() };
        Alloc::alloc(tycker, App(abs, arg), ty, env)
    }
}
// fixed point
impl<'a, V, T, F> Construct<Tycker<'a>, CompuId> for Fix<cs::Ann<V, T>, F>
where
    V: Construct<Tycker<'a>, VarName>,
    T: Construct<Tycker<'a>, TypeId>,
    F: FnOnce(DefId) -> CompuId,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Fix(cs::Ann(var, param_ty), body) = self;
        let var = var.build(tycker, env);
        let param_ty = param_ty.build(tycker, env);
        let def = Alloc::alloc(tycker, var, param_ty.into(), &());
        let Some(ty) = param_ty.destruct_thk_app(tycker) else { unreachable!() };
        let vpat: VPatId = Alloc::alloc(tycker, def, param_ty, env);
        let body = body(def);
        let body_ty = tycker.statics.annotations_compu[&body];
        let Ok(_) = Lub::lub(ty, body_ty, tycker) else { unreachable!() };
        Alloc::alloc(tycker, Fix(vpat, body), ty, env)
    }
}
// force
impl<'a, T> Construct<Tycker<'a>, CompuId> for Force<T>
where
    T: Construct<Tycker<'a>, ValueId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Force(thk) = self;
        let thk = thk.build(tycker, env);
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else { unreachable!() };
        Alloc::alloc(tycker, Force(thk), body_ty, env)
    }
}
// return
impl<'a, T> Construct<Tycker<'a>, CompuId> for Return<T>
where
    T: Construct<Tycker<'a>, ValueId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Return(val) = self;
        let val = val.build(tycker, env);
        let val_ty = tycker.statics.annotations_value[&val];
        let ret_ty = cs::Ret(val_ty).build(tycker, env);
        Alloc::alloc(tycker, Return(val), ret_ty, env)
    }
}
// bind
impl<'a, V, B, F, R> Construct<Tycker<'a>, CompuId> for Bind<V, B, F>
where
    V: Construct<Tycker<'a>, VarName>,
    B: Construct<Tycker<'a>, CompuId>,
    F: FnOnce(DefId) -> R,
    R: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Bind { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env);
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(tycker) else { unreachable!() };
        let var = binder.build(tycker, env);
        let def = Alloc::alloc(tycker, var, def_ty.into(), &());
        let binder = Alloc::alloc(tycker, def, def_ty, env);
        let tail = tail(def).build(tycker, env);
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty, env)
    }
}
// pure bind
impl<'a, P, B, F, R> Construct<Tycker<'a>, CompuId> for Let<P, B, F>
where
    P: Construct<Tycker<'a>, VPatId>,
    B: Construct<Tycker<'a>, ValueId>,
    F: FnOnce(VPatId) -> R,
    R: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Let { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env);
        let binder = binder.build(tycker, env);
        let tail = tail(binder).build(tycker, env);
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty, env)
    }
}
// // match
// impl<T, F, R> Construct<Tycker<'a>, CompuId> for cs::Match<DataId, T, F>
// where
//     T: Construct<Tycker<'a>, ValueId>,
//     F: Clone + FnOnce(CtorName, DefId, TypeId) -> R,
//     R: Construct<Tycker<'a>, CompuId>,
// {
//     fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> Result<CompuId> {
//         let cs::Match(data, scrut, arm) = self;
//         let scrut = scrut.build(tycker, env)?;
//         let data = tycker.statics.datas.defs[&data].to_owned();
//         let mut ty_ = None;
//         let arms = (data.into_iter())
//             .map(|(ctor, ty)| {
//                 let var = VarName(format!("{}", ctor.0.trim_start_matches("+").to_lowercase()));
//                 let def = Alloc::alloc(tycker, var, ty.into());
//                 let binder = cs::Ann(def, ty).build(tycker, env)?;
//                 let tail = (arm.clone())(ctor, def, ty).build(tycker, env)?;
//                 // Todo: consider lub (?)
//                 let tail_ty = tycker.statics.annotations_compu[&tail];
//                 ty_ = Some(tail_ty);
//                 Ok(Matcher { binder, tail })
//             })
//             .collect::<Result<Vec<_>>>()?;
//         Ok(Alloc::alloc(tycker, Match { scrut, arms }, ty_.unwrap()))
//     }
// }
// // comatch
// impl<F, R> Construct<Tycker<'a>, CompuId> for cs::CoMatch<CoDataId, F>
// where
//     F: Clone + FnOnce(DtorName, TypeId) -> R,
//     R: Construct<Tycker<'a>, CompuId>,
// {
//     fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> Result<CompuId> {
//         let cs::CoMatch(coda_id, arm) = self;
//         let coda = tycker.statics.codatas.defs[&coda_id].to_owned();
//         let arms = (coda.into_iter())
//             .map(|(dtor, ty)| {
//                 let tail = (arm.clone())(dtor.clone(), ty).build(tycker, env)?;
//                 let tail_ty = tycker.statics.annotations_compu[&tail];
//                 let Ok(_) = Lub::lub(ty, tail_ty, tycker) else { unreachable!() };
//                 Ok(CoMatcher { dtor, tail })
//             })
//             .collect::<Result<Vec<_>>>()?;
//         let ctype = CType.build(tycker, env)?;
//         let ty_ = Alloc::alloc(tycker, Type::from(coda_id), ctype);
//         Ok(Alloc::alloc(tycker, CoMatch { arms }, ty_))
//     }
// }
// dtor
impl<'a, T> Construct<Tycker<'a>, CompuId> for Dtor<T>
where
    T: Construct<Tycker<'a>, CompuId>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let Dtor(head, dtor) = self;
        let head = head.build(tycker, env);
        let head_ty = tycker.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, tycker) else { unreachable!() };
        let Some(ty) = coda.get(&dtor) else { unreachable!() };
        Alloc::alloc(tycker, Dtor(head, dtor), ty, env)
    }
}
impl<'a, T, D> Construct<Tycker<'a>, CompuId> for cs::Dtor<T, D>
where
    T: Construct<Tycker<'a>, CompuId>,
    D: Construct<Tycker<'a>, DtorName>,
{
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let cs::Dtor(head, dtor) = self;
        let dtor = dtor.build(tycker, env);
        Dtor(head, dtor).build(tycker, env)
    }
}
// top
impl<'a> Construct<Tycker<'a>, CompuId> for cs::Top {
    fn build(self, tycker: &mut Tycker<'a>, env: &TyEnv) -> CompuId {
        let top = cs::TopTy.build(tycker, env);
        Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top, env)
    }
}

impl<'a> Tycker<'a> {
    pub fn compu_vabs<F, T>(
        &mut self, env: &TyEnv, name: impl Construct<Tycker<'a>, VarName> + 'a,
        param_ty: impl Construct<Tycker<'a>, TypeId>, body: F,
    ) -> CompuId
    where
        F: FnOnce(Option<DefId>) -> T,
        T: Construct<Tycker<'a>, CompuId>,
    {
        let name = name.build(self, env);
        Abs(cs::Ann(name, param_ty), body).build(self, env)
    }
    pub fn compu_tabs(
        &mut self, env: &TyEnv, name: impl Construct<Tycker<'a>, VarName>,
        param_kd: impl Construct<Tycker<'a>, KindId>,
        body: impl FnOnce(Option<DefId>, AbstId) -> CompuId,
    ) -> CompuId {
        let name = name.build(self, env);
        Abs(cs::Ty(cs::Ann(name, param_kd)), body).build(self, env)
    }
    pub fn compu_vapp(
        &mut self, env: &TyEnv, abs: impl Construct<Tycker<'a>, CompuId>,
        arg: impl Construct<Tycker<'a>, ValueId>,
    ) -> CompuId {
        App(abs, arg).build(self, env)
    }
    pub fn compu_tapp(
        &mut self, env: &TyEnv, abs: impl Construct<Tycker<'a>, CompuId>,
        arg: impl Construct<Tycker<'a>, TypeId>,
    ) -> CompuId {
        App(abs, cs::Ty(arg)).build(self, env)
    }
    pub fn compu_fix(
        &mut self, env: &TyEnv, name: impl Construct<Tycker<'a>, VarName>,
        ty: impl Construct<Tycker<'a>, TypeId>, body: impl FnOnce(DefId) -> CompuId,
    ) -> CompuId {
        Fix(cs::Ann(name, ty), body).build(self, env)
    }
    pub fn compu_force(
        &mut self, env: &TyEnv, thk: impl Construct<Tycker<'a>, ValueId>,
    ) -> CompuId {
        Force(thk).build(self, env)
    }
    pub fn compu_ret(&mut self, env: &TyEnv, val: impl Construct<Tycker<'a>, ValueId>) -> CompuId {
        Return(val).build(self, env)
    }
    pub fn compu_bind(
        &mut self, env: &TyEnv, bindee: impl Construct<Tycker<'a>, CompuId>,
        name: impl Construct<Tycker<'a>, VarName>, tail: impl FnOnce(DefId) -> CompuId,
    ) -> CompuId {
        Bind { binder: name, bindee, tail }.build(self, env)
    }
    pub fn compu_let<R>(
        &mut self, env: &TyEnv, bindee: impl Construct<Tycker<'a>, ValueId>,
        binder: impl Construct<Tycker<'a>, VPatId>, tail: impl FnOnce(VPatId) -> R,
    ) -> CompuId
    where
        R: Construct<Tycker<'a>, CompuId>,
    {
        Let { binder, bindee, tail }.build(self, env)
    }
    pub fn compu_top(&mut self, env: &TyEnv) -> CompuId {
        cs::Top.build(self, env)
    }
    pub fn compu_dtor(
        &mut self, env: &TyEnv, head: impl Construct<Tycker<'a>, CompuId>,
        dtor: impl Construct<Tycker<'a>, DtorName>,
    ) -> CompuId {
        cs::Dtor(head, dtor).build(self, env)
    }
}
