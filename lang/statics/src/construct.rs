//! Constructors for patterns, types, and terms in [`StaticsArena`].
//!
//! This module provides the [`Construct`] trait, which describes a convenient DSL for
//! writing Zydeco programs in Rust. If you're familiar with HOAS (High Order Abstract Syntax),
//! we're trying to achieve exactly the same in this trait.
//! [`Construct`] has the following semantics:
//!
//! `impl` [`Construct<T>`] for `S` means that `S` can be used to construct `T`
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
//! + Insert small and trivial terms like `cs::TopTy` for type, `cs::Top` for computation,
//!   and `Thunk(cs::Top)` for value, and see if the program can partially compile.
//! + Break subterms into let bindings `let small = ...;`, and see if the small bindees
//!   can compile by calling `small.build(tycker, env)`.
//!
//! Finally, a word of caution:
//! + If you're having issues regarding lifetime, make sure to put `move` on the closure,
//!   since they implement [`FnOnce`] instead of [`Fn`].
//!
//! Good luck (つ´ω｀)つ

use crate::{syntax::*, *};

/// Trait for constructing entities in [`Tycker`] with more type inference available.
///
/// The only method provided is [`Construct::build`], which takes `&mut` [`Tycker`]
/// and the environment of the type checked program, and returns the constructed value.
///
/// The trait is different from [`Alloc`] in that it does not require feeding annotations
/// manually, but instead infers them from the context.
pub trait Construct<T>: Sized {
    /// Build the term with the given type checker and environment.
    ///
    /// See [`Construct`] level documentation for more details.
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<T>;

    /// Turn the result of [`Construct::build`] into a [`ResultKont`].
    /// Eaiser to use under a `_k` context.
    fn build_k(self, tycker: &mut Tycker, env: &Env<AnnId>) -> ResultKont<T> {
        let res = self.build(tycker, env);
        tycker.err_p_to_k(res)
    }
}

impl<S, T, A> Construct<T> for cs::Ann<S, A>
where
    S: Alloc<T, Ann = A>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<T> {
        let cs::Ann(tm, ty) = self;
        Ok(Alloc::alloc(tycker, tm, ty))
    }
}

impl<T> Construct<T> for Result<T> {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<T> {
        self
    }
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

    /// fresh variable [`super::DefId`] or abstract type [`super::AbstId`]
    ///
    /// currently used for new type pattern and value pattern
    pub struct Fresh<T>(pub T);

    /// Construct to abstract type immediately
    pub struct Abst<T, F>(pub T, pub F);
    /// Construct to value immediately
    pub struct Value<T>(pub T);
    /// Construct to computation immediately
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
    ///
    /// See crate level documentation [`crate::construct::syntax`] for more details.
    pub struct HExists<Param>(pub Param);
    pub struct Exists<Param, FBody>(pub Param, pub FBody);
    /// `forall X. ...`
    ///
    /// See crate level documentation [`crate::construct::syntax`] for more details.
    pub struct HForall<Param>(pub Param);
    pub struct Forall<Param, FBody>(pub Param, pub FBody);

    /// `+Ctor(<tail>)`
    pub struct Ctor<Name, Tail>(pub Name, pub Tail);
    /// `match_ty <t> | <f> ... end`
    ///
    /// `Ty` here should be DataId
    pub struct Match<Ty, T, F>(pub Ty, pub T, pub F);
    /// `comatch_ty | <f> ... end`
    ///
    /// `Ty` here should be CoDataId
    pub struct CoMatch<Ty, F>(pub Ty, pub F);
    /// `<head> .dtor`
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
                fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<$ty> {
                    Ok(self)
                }
            }
        )*
    }
}

impl_construct_trivial!(DefId, KindId, AbstId, TPatId, TypeId, VPatId, ValueId, CompuId);

/* ------------------------------- Definition ------------------------------- */

impl_construct_trivial!(VarName, CtorName, DtorName);

// VarName
impl Construct<VarName> for String {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VarName> {
        VarName(self).build(tycker, env)
    }
}
impl Construct<VarName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VarName> {
        self.to_string().build(tycker, env)
    }
}

// CtorName
impl Construct<CtorName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CtorName> {
        CtorName(self.to_string()).build(tycker, env)
    }
}

// DtorName
impl Construct<DtorName> for &str {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<DtorName> {
        DtorName(self.to_string()).build(tycker, env)
    }
}

// DefId
impl Construct<DefId> for cs::Ann<VarName, KindId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<DefId> {
        let cs::Ann(tm, ty) = self;
        Ok(Alloc::alloc(tycker, tm, ty.into()))
    }
}
impl Construct<DefId> for cs::Ann<VarName, TypeId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<DefId> {
        let cs::Ann(tm, ty) = self;
        Ok(Alloc::alloc(tycker, tm, ty.into()))
    }
}
impl<A> Construct<DefId> for cs::Ann<String, A>
where
    A: Into<AnnId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<DefId> {
        let cs::Ann(tm, ty) = self;
        cs::Ann(VarName(tm), ty.into()).build(tycker, env)
    }
}
impl<A> Construct<DefId> for cs::Ann<&str, A>
where
    A: Into<AnnId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<DefId> {
        let cs::Ann(tm, ty) = self;
        cs::Ann(tm.to_string(), ty.into()).build(tycker, env)
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl<T, F, C, R> Construct<R> for cs::Abst<T, F>
where
    T: Construct<AbstId>,
    F: FnOnce(AbstId) -> C,
    C: Construct<R>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<R> {
        let cs::Abst(abst, f) = self;
        let abst = abst.build(tycker, env)?;
        f(abst).build(tycker, env)
    }
}
impl<K> Construct<AbstId> for cs::Ann<VarName, K>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<AbstId> {
        let cs::Ann(var, kd) = self;
        let kd = kd.build(tycker, _env)?;
        let def = Alloc::alloc(tycker, var, kd.into());
        Ok(Alloc::alloc(tycker, def, kd))
    }
}
impl<K> Construct<AbstId> for cs::Ann<String, K>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<AbstId> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(VarName(tm), kd).build(tycker, _env)
    }
}
impl<K> Construct<AbstId> for cs::Ann<&str, K>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<AbstId> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(tm.to_string(), kd).build(tycker, _env)
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl Construct<KindId> for cs::TypeOf<TypeId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<KindId> {
        let cs::TypeOf(ty) = self;
        Ok(tycker.statics.annotations_type[&ty])
    }
}
impl Construct<KindId> for cs::TypeOf<AbstId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<KindId> {
        let cs::TypeOf(abst) = self;
        Ok(tycker.statics.annotations_abst[&abst])
    }
}
impl Construct<KindId> for VType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<KindId> {
        let AnnId::Kind(kd) = env[tycker.prim.vtype.get()] else { unreachable!() };
        Ok(kd)
    }
}
impl Construct<KindId> for CType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<KindId> {
        let AnnId::Kind(kd) = env[tycker.prim.ctype.get()] else { unreachable!() };
        Ok(kd)
    }
}
impl<S, T> Construct<KindId> for Arrow<S, T>
where
    S: Construct<KindId>,
    T: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<KindId> {
        let Arrow(k1, k2) = self;
        let k1 = k1.build(tycker, env)?;
        let k2 = k2.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Arrow(k1, k2), ()))
    }
}

impl Tycker {
    pub fn vtype(&mut self, env: &Env<AnnId>) -> KindId {
        let Ok(kd) = VType.build(self, env) else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self, env: &Env<AnnId>) -> KindId {
        let Ok(kd) = CType.build(self, env) else { unreachable!() };
        kd
    }
}

#[cfg(test)]
mod kind_test {
    use crate::{syntax::*, *};

    #[test]
    fn r#static() {
        fn _f(tycker: &mut Tycker, env: &Env<AnnId>) -> Result<KindId> {
            Arrow(VType, Arrow(CType, CType)).build(tycker, env)
        }
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl Construct<TPatId> for cs::Fresh<TPatId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<TPatId> {
        let cs::Fresh(tm) = self;
        let (def, kd) = tm.destruct_def(tycker);
        let tpat_ = Alloc::alloc(tycker, def, kd);
        Ok(tpat_)
    }
}
impl Construct<TPatId> for cs::Ann<Option<DefId>, KindId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TPatId> {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}
impl<K> Construct<TPatId> for cs::Ann<VarName, K>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TPatId> {
        let cs::Ann(var, kd) = self;
        let ty = kd.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).build(tycker, env)
    }
}
impl<V, K> Construct<TPatId> for cs::Pat<V, K>
where
    V: Construct<VarName>,
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TPatId> {
        let cs::Pat(var, kd) = self;
        let var = var.build(tycker, env)?;
        let ty = kd.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).build(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl Construct<TypeId> for cs::TypeOf<ValueId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<TypeId> {
        let cs::TypeOf(value) = self;
        Ok(tycker.statics.annotations_value[&value])
    }
}
impl Construct<TypeId> for cs::TypeOf<CompuId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<TypeId> {
        let cs::TypeOf(compu) = self;
        Ok(tycker.statics.annotations_compu[&compu])
    }
}
impl<K> Construct<TypeId> for cs::Ann<Hole, (K, su::TermId)>
where
    K: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Ann(Hole, (kd, site)) = self;
        let kd = kd.build(tycker, env)?;
        let fill = tycker.statics.fills.alloc(site);
        Ok(Alloc::alloc(tycker, fill, kd))
    }
}
impl Construct<TypeId> for DefId {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[&self] else { unreachable!() };
        let kd = tycker.statics.annotations_type[&ty];
        Ok(Alloc::alloc(tycker, self, kd))
    }
}
impl Construct<TypeId> for AbstId {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<TypeId> {
        let kd = tycker.statics.annotations_abst[&self];
        Ok(Alloc::alloc(tycker, self, kd))
    }
}
impl<T> Construct<TypeId> for cs::Ty<T>
where
    T: Construct<AbstId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Ty(abst) = self;
        let abst = abst.build(tycker, env)?;
        abst.build(tycker, env)
    }
}
impl<S, F, T> Construct<TypeId> for Abs<S, F>
where
    S: Construct<TPatId>,
    F: FnOnce(TPatId, DefId, KindId) -> T,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let Abs(param, ty) = self;
        let tpat = param.build(tycker, env)?;
        let (def, param_kd) = tpat.destruct_def(tycker);
        let body = ty(tpat, def, param_kd).build(tycker, env)?;
        let kd = Arrow(param_kd, cs::TypeOf(body)).build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Abs(tpat, body), kd))
    }
}
impl<S, T> Construct<TypeId> for App<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let App(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env)?;
        let kd_1 = tycker.statics.annotations_type[&ty_1];
        let Some((kd_a, kd_b)) = kd_1.destruct_arrow(tycker) else { unreachable!() };
        let ty_2 = ty_2.build(tycker, env)?;
        let kd_2 = tycker.statics.annotations_type[&ty_2];
        let Ok(_) = Lub::lub(kd_a, kd_2, tycker) else { unreachable!() };
        Ok(Alloc::alloc(tycker, App(ty_1, ty_2), kd_b))
    }
}
impl Construct<TypeId> for IntTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.int.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl Construct<TypeId> for CharTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.char.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl Construct<TypeId> for StringTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.string.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<F, T> Construct<TypeId> for cs::Data<DataId, F>
where
    F: Clone + FnOnce(CtorName, TypeId) -> T,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Data(data, f) = self;
        let arms = tycker.statics.datas.defs[&data].clone();
        let arms_ = arms
            .into_iter()
            .map(|(ctor, ty)| {
                let ty_ = (f.clone())(ctor.clone(), ty).build(tycker, env)?;
                Ok((ctor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let data_ = Data::new(arms_.iter().cloned());
        let data = tycker.statics.datas.lookup_or_alloc(arms_, data_);
        let kd = VType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, data, kd))
    }
}
impl Construct<TypeId> for ThkTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.thk.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<T> Construct<TypeId> for cs::Thk<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Thk(arg) = self;
        let thk = ThkTy.build(tycker, env)?;
        let arg = arg.build(tycker, env)?;
        let vtype = VType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, App(thk, arg), vtype))
    }
}
impl Construct<TypeId> for UnitTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.unit.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<S, T> Construct<TypeId> for Prod<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let Prod(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env)?;
        let ty_2 = ty_2.build(tycker, env)?;
        let vtype = VType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Prod(ty_1, ty_2), vtype))
    }
}
impl<F, A, T> Construct<TypeId> for cs::Exists<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Exists(abst, ty) = self;
        let abst = abst.build(tycker, env)?;
        let ty = ty(abst).build(tycker, env)?;
        let vtype = VType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Exists(abst, ty), vtype))
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
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.os.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl Construct<TypeId> for cs::TopTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.top.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<F, T> Construct<TypeId> for cs::CoData<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> T,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::CoData(coda, f) = self;
        let arms = tycker.statics.codatas.defs[&coda].clone();
        let arms_ = arms
            .into_iter()
            .map(|(dtor, ty)| {
                let ty_ = (f.clone())(dtor.clone(), ty).build(tycker, env)?;
                Ok((dtor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let coda_ = CoData::new(arms_.iter().cloned());
        let coda = tycker.statics.codatas.lookup_or_alloc(arms_, coda_);
        let kd = CType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, coda, kd))
    }
}
impl<S, T> Construct<TypeId> for Arrow<S, T>
where
    S: Construct<TypeId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let Arrow(ty_1, ty_2) = self;
        let ty_1 = ty_1.build(tycker, env)?;
        let ty_2 = ty_2.build(tycker, env)?;
        let ctype = CType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype))
    }
}
impl<F, A, T> Construct<TypeId> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: Construct<AbstId>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Forall(abst, ty) = self;
        let abst = abst.build(tycker, env)?;
        let ty = ty(abst).build(tycker, env)?;
        let ctype = CType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Forall(abst, ty), ctype))
    }
}
impl Construct<TypeId> for RetTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.ret.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<T> Construct<TypeId> for cs::Ret<T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Ret(arg) = self;
        let ret = RetTy.build(tycker, env)?;
        let arg = arg.build(tycker, env)?;
        let ctype = CType.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, App(ret, arg), ctype))
    }
}
impl Construct<TypeId> for syntax::MonadTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.monad.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<M> Construct<TypeId> for cs::Monad<M>
where
    M: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let cs::Monad(monad_ty) = self;
        App(cs::MonadTy, monad_ty).build(tycker, env)
    }
}
impl Construct<TypeId> for syntax::AlgebraTy {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let AnnId::Type(ty) = env[tycker.prim.algebra.get()] else { unreachable!() };
        Ok(ty)
    }
}
impl<M, R> Construct<TypeId> for cs::Algebra<M, R>
where
    M: Construct<TypeId>,
    R: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
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
        let Ok(ty) = cs::Thk(cs::Ann(Hole, (CType, site))).build(self, env) else { unreachable!() };
        ty
    }
    // /// generates `Ret A`
    // pub fn ret_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
    //     Ret(arg).build(self, env)
    // }
    /// generates `Ret _`
    pub fn ret_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        let Ok(ty) = cs::Ret(cs::Ann(Hole, (VType, site))).build(self, env) else { unreachable!() };
        ty
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

impl Construct<VPatId> for cs::Fresh<VPatId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VPatId> {
        let cs::Fresh(tm) = self;
        let (def, ty) = tm.try_destruct_def(tycker);
        cs::Ann(def, ty).build(tycker, env)
    }
}
impl Construct<VPatId> for cs::Ann<Option<DefId>, TypeId> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VPatId> {
        let cs::Ann(tm, ty) = self;
        match tm {
            | Some(def) => cs::Ann(def, ty).build(tycker, env),
            | None => cs::Ann(Hole, ty).build(tycker, env),
        }
    }
}
impl<T> Construct<VPatId> for cs::Ann<VarName, T>
where
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VPatId> {
        let cs::Ann(var, ty) = self;
        let ty = ty.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).build(tycker, env)
    }
}
impl<V, T> Construct<VPatId> for cs::Pat<V, T>
where
    V: Construct<VarName>,
    T: Construct<TypeId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<VPatId> {
        let cs::Pat(var, ty) = self;
        let var = var.build(tycker, env)?;
        let ty = ty.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).build(tycker, env)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<T> Construct<ValueId> for cs::Value<T>
where
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
        let cs::Value(arg) = self;
        arg.build(tycker, env)
    }
}
impl Construct<ValueId> for DefId {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<ValueId> {
        let AnnId::Type(ty) = tycker.statics.annotations_var[&self] else { unreachable!() };
        Ok(Alloc::alloc(tycker, self, ty))
    }
}
impl Construct<ValueId> for Option<DefId> {
    fn build(self, tycker: &mut Tycker, _env: &Env<AnnId>) -> Result<ValueId> {
        let Some(def) = self else { unreachable!() };
        def.build(tycker, _env)
    }
}
impl<T> Construct<ValueId> for Thunk<T>
where
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
        let Thunk(body) = self;
        let body = body.build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = cs::Thk(body_ty).build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Thunk(body), ty))
    }
}
impl Construct<ValueId> for Triv {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
        let ty = UnitTy.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Triv, ty))
    }
}
impl<S, T> Construct<ValueId> for Cons<S, T>
where
    S: Construct<ValueId>,
    T: Construct<ValueId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
        let Cons(a, b) = self;
        let a = a.build(tycker, env)?;
        let a_ty = tycker.statics.annotations_value[&a];
        let b = b.build(tycker, env)?;
        let b_ty = tycker.statics.annotations_value[&b];
        let ty = Prod(a_ty, b_ty).build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Cons(a, b), ty))
    }
}

impl Tycker {
    // pub fn value_var(&mut self, env: &Env<AnnId>, def: DefId, ty: TypeId) -> Result<ValueId> {
    //     cs::Ann(def, ty).build(self, env)
    // }
    // pub fn value_thunk(&mut self, env: &Env<AnnId>, body: CompuId) -> Result<ValueId> {
    //     Thunk(body).build(self, env)
    // }
    // pub fn value_triv(&mut self, env: &Env<AnnId>) -> Result<ValueId> {
    //     Triv.build(self, env)
    // }
    // pub fn value_vcons(&mut self, env: &Env<AnnId>, a: ValueId, b: ValueId) -> Result<ValueId> {
    //     Cons(a, b).build(self, env)
    // }
}

/* ------------------------------- Computation ------------------------------ */

impl<T> Construct<CompuId> for cs::Compu<T>
where
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let cs::Compu(arg) = self;
        arg.build(tycker, env)
    }
}
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
        let vpat = vpat.build(tycker, env)?;
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let body = body(tycker, env, def)?;
        let body = body.build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env)?;
        let res = Alloc::alloc(tycker, Abs(vpat, body), ty);
        Ok(res)
    }
}
impl<V, F, T> Construct<CompuId> for Abs<V, F>
where
    V: Construct<VPatId>,
    F: FnOnce(Option<DefId>) -> T,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Abs(vpat, body) = self;
        let vpat: VPatId = vpat.build(tycker, env)?;
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let body = body(def).build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ty = Arrow(param_ty, body_ty).build(tycker, env)?;
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
        let tpat = tpat.build(tycker, env)?;
        let (def, param_ty) = tpat.try_destruct_def(tycker);
        let abst = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def, abst)?;
        let body = body.build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env)?;
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        let res = Alloc::alloc(tycker, Abs(tpat, body), ty);
        Ok(res)
    }
}
impl<V, F, T> Construct<CompuId> for Abs<cs::Ty<V>, F>
where
    V: Construct<TPatId>,
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Abs(cs::Ty(tpat), body) = self;
        let tpat: TPatId = tpat.build(tycker, env)?;
        let (def, param_kd) = tpat.try_destruct_def(tycker);
        let abst = Alloc::alloc(tycker, def, param_kd);
        let body = body(def, abst);
        let body = body.build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env)?;
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Ok(Alloc::alloc(tycker, Abs(tpat, body), ty))
    }
}
impl<F, T> Construct<CompuId> for Abs<cs::Ty<AbstId>, F>
where
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Abs(cs::Ty(abst), body) = self;
        use zydeco_utils::arena::ArenaAccess;
        let def = tycker.statics.abst_hints.get(&abst).cloned();
        let param_kd = tycker.statics.annotations_abst[&abst];
        let tpat: TPatId = cs::Ann(def, param_kd).build(tycker, env)?;
        let body = body(def, abst).build(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let ctype = CType.build(tycker, env)?;
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
        let vpat = vpat.build(tycker, env)?;
        let (def, param_ty) = vpat.try_destruct_def(tycker);
        let Some(ty) = param_ty.destruct_thk_app(tycker) else { unreachable!() };
        let body = body(tycker, env, def)?;
        let body = body.build(tycker, env)?;
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
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Fix(cs::Ann(var, param_ty), body) = self;
        let var = var.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, param_ty.into());
        let Some(ty) = param_ty.destruct_thk_app(tycker) else { unreachable!() };
        let vpat: VPatId = Alloc::alloc(tycker, def, param_ty);
        let body = body(tycker, env, def);
        let body_ty = tycker.statics.annotations_compu[&body];
        let Ok(_) = Lub::lub(ty, body_ty, tycker) else { unreachable!() };
        Ok(Alloc::alloc(tycker, Fix(vpat, body), ty))
    }
}
// force
impl<T> Construct<CompuId> for Force<T>
where
    T: Construct<ValueId>,
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
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Ret(val) = self;
        let val = val.build(tycker, env)?;
        let val_ty = tycker.statics.annotations_value[&val];
        let ret_ty = cs::Ret(val_ty).build(tycker, env)?;
        Ok(Alloc::alloc(tycker, Ret(val), ret_ty))
    }
}
// bind
impl<V, B, F, R> Construct<CompuId> for Bind<V, B, F>
where
    V: Construct<VarName>,
    B: Construct<CompuId>,
    F: FnOnce(DefId) -> R,
    R: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Bind { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env)?;
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(tycker) else { unreachable!() };
        let var = binder.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, def_ty.into());
        let binder = Alloc::alloc(tycker, def, def_ty);
        let tail = tail(def).build(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Ok(Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty))
    }
}
// pure bind
impl<V, B, F, R> Construct<CompuId> for PureBind<V, B, F>
where
    V: Construct<VarName>,
    B: Construct<ValueId>,
    F: FnOnce(DefId) -> R,
    R: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let PureBind { binder, bindee, tail } = self;
        let bindee = bindee.build(tycker, env)?;
        let def_ty = tycker.statics.annotations_value[&bindee];
        let var = binder.build(tycker, env)?;
        let def = Alloc::alloc(tycker, var, def_ty.into());
        let binder = Alloc::alloc(tycker, def, def_ty);
        let tail = tail(def).build(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Ok(Alloc::alloc(tycker, PureBind { binder, bindee, tail }, tail_ty))
    }
}
// match
impl<T, F, R> Construct<CompuId> for cs::Match<DataId, T, F>
where
    T: Construct<ValueId>,
    F: Clone + FnOnce(CtorName, DefId, TypeId) -> R,
    R: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let cs::Match(data, scrut, arm) = self;
        let scrut = scrut.build(tycker, env)?;
        let data = tycker.statics.datas.defs[&data].to_owned();
        let mut ty_ = None;
        let arms = (data.into_iter())
            .map(|(ctor, ty)| {
                let var = VarName(format!("{}", ctor.0.trim_start_matches("+").to_lowercase()));
                let def = Alloc::alloc(tycker, var, ty.into());
                let binder = cs::Ann(def, ty).build(tycker, env)?;
                let tail = (arm.clone())(ctor, def, ty).build(tycker, env)?;
                // Todo: consider lub (?)
                let tail_ty = tycker.statics.annotations_compu[&tail];
                ty_ = Some(tail_ty);
                Ok(Matcher { binder, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Alloc::alloc(tycker, Match { scrut, arms }, ty_.unwrap()))
    }
}
// comatch
impl<F, R> Construct<CompuId> for cs::CoMatch<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> R,
    R: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let cs::CoMatch(coda_id, arm) = self;
        let coda = tycker.statics.codatas.defs[&coda_id].to_owned();
        let arms = (coda.into_iter())
            .map(|(dtor, ty)| {
                let tail = (arm.clone())(dtor.clone(), ty).build(tycker, env)?;
                let tail_ty = tycker.statics.annotations_compu[&tail];
                let Ok(_) = Lub::lub(ty, tail_ty, tycker) else { unreachable!() };
                Ok(CoMatcher { dtor, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        let ctype = CType.build(tycker, env)?;
        let ty_ = Alloc::alloc(tycker, Type::from(coda_id), ctype);
        Ok(Alloc::alloc(tycker, CoMatch { arms }, ty_))
    }
}
// dtor
impl<T> Construct<CompuId> for Dtor<T>
where
    T: Construct<CompuId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let Dtor(head, dtor) = self;
        let head = head.build(tycker, env)?;
        let head_ty = tycker.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, tycker) else { unreachable!() };
        let Some(ty) = coda.get(&dtor).cloned() else { unreachable!() };
        Ok(Alloc::alloc(tycker, Dtor(head, dtor), ty))
    }
}
impl<T, D> Construct<CompuId> for cs::Dtor<T, D>
where
    T: Construct<CompuId>,
    D: Construct<DtorName>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let cs::Dtor(head, dtor) = self;
        let head = head.build(tycker, env)?;
        let dtor = dtor.build(tycker, env)?;
        Dtor(head, dtor).build(tycker, env)
    }
}
// top
impl Construct<CompuId> for cs::Top {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
        let top = cs::TopTy.build(tycker, env)?;
        Ok(Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top))
    }
}

impl Tycker {
    // pub fn compu_vabs<F, T>(
    //     &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
    //     param_ty: impl Construct<TypeId>, body: F,
    // ) -> CompuId
    // where
    //     F: Fn(&mut Tycker, &Env<AnnId>, DefId) -> T,
    //     T: Construct<CompuId>,
    // {
    //     let param_ty = param_ty.build(self, env);
    //     Abs(cs::Ann(name, param_ty), body).build(self, env)
    // }
    // pub fn try_compu_vabs<F, T>(
    //     &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
    //     param_ty: impl Construct<TypeId>, body: F,
    // ) -> Result<CompuId>
    // where
    //     F: Fn(&mut Self, &Env<AnnId>, DefId) -> Result<T>,
    //     T: Construct<CompuId>,
    // {
    //     let param_ty = param_ty.build(self, env);
    //     Abs(cs::Ann(name, param_ty), body).build(self, env)
    // }
    // pub fn compu_tabs(
    //     &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
    //     param_kd: impl Construct<KindId>,
    //     body: impl Fn(&mut Self, &Env<AnnId>, DefId, AbstId) -> CompuId,
    // ) -> CompuId {
    //     let param_kd = param_kd.build(self, env);
    //     Abs(cs::Ann(name, param_kd), body).build(self, env)
    // }
    // pub fn try_compu_tabs<F, T>(
    //     &mut self, env: &Env<AnnId>, name: impl Construct<VarName>,
    //     param_kd: impl Construct<KindId>, body: F,
    // ) -> Result<CompuId>
    // where
    //     F: Fn(&mut Self, &Env<AnnId>, DefId, AbstId) -> Result<T>,
    //     T: Construct<CompuId>,
    // {
    //     let param_kd = param_kd.build(self, env);
    //     Abs(cs::Ann(name, param_kd), body).build(self, env)
    // }
    // pub fn compu_vapp(&mut self, env: &Env<AnnId>, abs: CompuId, arg: ValueId) -> CompuId {
    //     App(abs, arg).build(self, env)
    // }
    // pub fn compu_tapp(&mut self, env: &Env<AnnId>, abs: CompuId, arg: TypeId) -> CompuId {
    //     App(abs, arg).build(self, env)
    // }
    // pub fn compu_fix(
    //     &mut self, env: &Env<AnnId>, name: VarName, ty: TypeId,
    //     body: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    // ) -> Result<CompuId> {
    //     Fix(cs::Ann(name, ty), body).build(self, env)
    // }
    // pub fn compu_force(&mut self, env: &Env<AnnId>, thk: ValueId) -> CompuId {
    //     Force(thk).build(self, env)
    // }
    // pub fn compu_ret(&mut self, env: &Env<AnnId>, val: ValueId) -> CompuId {
    //     Ret(val).build(self, env)
    // }
    // pub fn compu_bind(
    //     &mut self, env: &Env<AnnId>, bindee: CompuId, name: VarName,
    //     tail: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    // ) -> CompuId {
    //     Bind { binder: name, bindee, tail }.build(self, env)
    // }
    // pub fn compu_let(
    //     &mut self, env: &Env<AnnId>, bindee: ValueId, name: VarName,
    //     tail: impl Fn(&mut Self, &Env<AnnId>, DefId) -> CompuId,
    // ) -> CompuId {
    //     PureBind { binder: name, bindee, tail }.build(self, env)
    // }
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
