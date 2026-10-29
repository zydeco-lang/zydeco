//! One HOAS-style builder for patterns, types, and terms in [`StaticsArena`],
//! parameterized by the environment it constructs in.
//!
//! [`Construct`] describes a convenient DSL for writing Zydeco syntax in Rust.
//! `impl` [`Construct<E, T>`] for `S` means that `S` constructs a `T` in an environment `E`.
//! As in HOAS (higher-order abstract syntax), a binder is a Rust closure that receives
//! the identifiers its construction introduces.
//!
//!
//! ## Environments
//!
//! A construction threads its environment through its subterms and returns the environment
//! after the bindings it made. It fails with a checker error when a subterm does not have
//! the shape the construction requires. [`ConstructEnv`] states what every construction reads
//! from its environment:
//!
//! + A plain [`TyEnv`] constructs nodes in place. The checker builds well-formed shapes,
//!   such as intrinsic kinds and types, through [`Build::build`], which cannot fail.
//! + [`MonEnv`] is the environment of the monadic translation, which copies a closed term.
//!   It renames the definitions and abstract types of the copied term and knows the monad and
//!   algebra types that the translation targets.
//!
//! Constructions that copy binders or need the monad basis exist only for [`MonEnv`], in
//! [`crate::elaborate::monadic::construct`]. Computation abstraction and application over types
//! are defined there as well: they share [`Abs`] and [`App`] with the value forms that every
//! environment uses and are told apart by [`cs::Ty`], and only the translation builds them.
//!
//!
//! ## Comparison with [`Alloc`]
//!
//! [`Alloc`] allocates one node with an explicit annotation. [`Construct`] builds on it and infers
//! the annotations (types and kinds) of the nodes it constructs, which brings two benefits:
//! + It's more convenient, because much less boilerplate annotations are needed.
//! + It's less error-prone, as the inferred types are constructed from the known context
//!   of the compiler. Manually constructed types are not guaranteed to match the terms
//!   that they are supposed to annotate.
//!
//! [`cs::Ann`] passes an explicit annotation through to [`Alloc`] where inference is not wanted.
//!
//!
//! ## Advice on writing constructions
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
//! When your construction fails to compile, it's likely that you're not constructing
//! the term correctly; if not, that's a bug in this module, which is sad. (>_<)
//!
//! To find out what's happening, you should first try to break down the term that's been
//! constructed into smaller pieces, and see if you can find the problem.
//! Two useful approaches are:
//! + Insert small and trivial terms like [`cs::TopTy`] for type, [`cs::Top`] for computation,
//!   and [`Triv`] for value, and see if the program can partially compile.
//! + Break subterms into let bindings `let small = ...;`, and see if the small bindees
//!   can compile by calling `small.construct(tycker, env)`.
//!
//! Finally, a word of caution:
//! + If you're having issues regarding lifetime, make sure to put `move` on the closure,
//!   since they implement [`FnOnce`] instead of [`Fn`].
//!
//! Good luck (つ´ω｀)つ

use crate::*;

/// What every construction reads from the environment it constructs in.
pub trait ConstructEnv: Clone {
    /// The typing environment of the constructed nodes.
    fn ty(&self) -> &TyEnv;
    /// The definition that `def` stands for in the constructed program.
    fn def(&self, def: DefId) -> DefId;
    /// The abstract type that `abst` stands for in the constructed program.
    fn abst(&self, abst: AbstId) -> AbstId;
}

/// A plain typing environment constructs nodes in place.
impl ConstructEnv for TyEnv {
    fn ty(&self) -> &TyEnv {
        self
    }
    fn def(&self, def: DefId) -> DefId {
        def
    }
    fn abst(&self, abst: AbstId) -> AbstId {
        abst
    }
}

/// The monadic translation renames the definitions and abstract types of the term it copies.
impl ConstructEnv for MonEnv {
    fn ty(&self) -> &TyEnv {
        &self.ty
    }
    fn def(&self, def: DefId) -> DefId {
        self.subst.get(&def).copied().unwrap_or(def)
    }
    fn abst(&self, abst: AbstId) -> AbstId {
        self.subst_abst.get(&abst).copied().unwrap_or(abst)
    }
}

/// Trait for constructing entities in [`Tycker`] with more type inference available.
///
/// The only required method is [`Construct::construct`], which takes `&mut` [`Tycker`]
/// and the environment `E` of the constructed program, and returns the environment after
/// the construction's bindings together with the constructed entity.
pub trait Construct<E, T>: Sized {
    /// Construct the entity in `env`.
    ///
    /// See [`Construct`] level documentation for more details.
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, T)>;
}

/// Builds a shape in a plain typing environment, where the checker's constructions cannot fail.
pub trait Build<T> {
    fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> T;
}

impl<S, T> Build<T> for S
where
    S: Construct<TyEnv, T>,
{
    fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> T {
        let Ok((_, built)) = self.construct(tycker, env.clone()) else {
            unreachable!("the checker builds only well-formed shapes in a plain typing environment")
        };
        built
    }
}

/// A name that a construction gives to the binder, constructor, or destructor it introduces.
pub trait IntoName<N> {
    fn into_name(self) -> N;
}

impl IntoName<VarName> for VarName {
    fn into_name(self) -> VarName {
        self
    }
}
impl IntoName<VarName> for String {
    fn into_name(self) -> VarName {
        VarName(self)
    }
}
impl IntoName<VarName> for &str {
    fn into_name(self) -> VarName {
        VarName(self.to_owned())
    }
}
impl IntoName<CtorName> for CtorName {
    fn into_name(self) -> CtorName {
        self
    }
}
impl IntoName<CtorName> for &str {
    fn into_name(self) -> CtorName {
        CtorName(self.to_owned())
    }
}
impl IntoName<DtorName> for DtorName {
    fn into_name(self) -> DtorName {
        self
    }
}
impl IntoName<DtorName> for &str {
    fn into_name(self) -> DtorName {
        DtorName(self.to_owned())
    }
}

/// Syntax used by [`Construct`]. They work together as part of the HOAS
/// (High Order Abstract Syntax) for Zydeco in Rust. Specifically,
///
/// + Structures that carry semantic actions, such as `Ann`, `Pat`, `Ty`, `TypeOf`,
///   and other structures related to the algebra translation.
/// + Some structures that are not defined as part of the common syntax of Zydeco,
///   such as `Algebra`.
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

    /// `Thk B`
    pub struct Thk<B>(pub B);
    /// `Ret A`
    pub struct Ret<A>(pub A);
    /// `codata end`
    pub struct TopTy;
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

    /// `forall X. ...`
    pub struct Forall<Param, FBody>(pub Param, pub FBody);

    /// `+Ctor(<tail>)`
    pub struct Ctor<Tag, Tail>(pub Tag, pub Tail);
    /// `comatch_ty | <f> ... end`
    ///
    /// `Ty` here should be CoDataId
    pub struct CoMatch<Ty, F>(pub Ty, pub F);
    /// `<head> .dtor`
    pub struct Dtor<Head, Tag>(pub Head, pub Tag);
    /// `comatch end`
    pub struct Top;

    pub use crate::elaborate::monadic::syntax::*;
}

/// [`Construct`] for every typed [`Alloc`] whose annotation is built in the same environment.
impl<E, S, T, A, U> Construct<E, T> for cs::Ann<S, U>
where
    E: ConstructEnv,
    U: Construct<E, A>,
    for<'a> S: Alloc<Tycker<'a>, T, Ann = A, Env = TyEnv>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, T)> {
        let cs::Ann(tm, ty) = self;
        let (env, ty) = ty.construct(tycker, env)?;
        let tm = Alloc::alloc(tycker, tm, ty, env.ty());
        Ok((env, tm))
    }
}

/* --------------------------------- Monadic -------------------------------- */

impl<E, T, F, I, O, R> Construct<E, R> for cs::CBind<T, I, F>
where
    E: ConstructEnv,
    T: Construct<E, I>,
    F: FnOnce(I) -> O,
    O: Construct<E, R>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, R)> {
        let cs::CBind(input, _, f) = self;
        let (env, input) = input.construct(tycker, env)?;
        f(input).construct(tycker, env)
    }
}

/* ------------------------------- Identifier ------------------------------- */

/// Identifiers that construct themselves.
macro_rules! identity_constructions {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<E: ConstructEnv> Construct<E, $ty> for $ty {
                fn construct(self, _tycker: &mut Tycker<'_>, env: E) -> Result<(E, $ty)> {
                    Ok((env, self))
                }
            }
        )*
    };
}

identity_constructions!(Option<DefId>, KindId, TPatId, TypeId, VPatId, ValueId, CompuId);

impl<E: ConstructEnv> Construct<E, AbstId> for AbstId {
    fn construct(self, _tycker: &mut Tycker<'_>, env: E) -> Result<(E, AbstId)> {
        let abst = env.abst(self);
        Ok((env, abst))
    }
}

/// A list constructs its items in order, threading the environment through them.
impl<E, S, T> Construct<E, Vec<T>> for Vec<S>
where
    E: ConstructEnv,
    S: Construct<E, T>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, Vec<T>)> {
        let capacity = self.len();
        self.into_iter().try_fold((env, Vec::with_capacity(capacity)), |(env, mut built), item| {
            let (env, item) = item.construct(tycker, env)?;
            built.push(item);
            Ok((env, built))
        })
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl<E, K> Construct<E, AbstId> for cs::Ann<VarName, K>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, AbstId)> {
        let cs::Ann(var, kd) = self;
        let (env, kd) = kd.construct(tycker, env)?;
        let def = Alloc::alloc(tycker, var, kd.into(), &());
        Ok((env, Alloc::alloc(tycker, def, kd, &())))
    }
}
impl<E, K> Construct<E, AbstId> for cs::Ann<String, K>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(VarName(tm), kd).construct(tycker, env)
    }
}
impl<E, K> Construct<E, AbstId> for cs::Ann<&str, K>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(tm.to_owned(), kd).construct(tycker, env)
    }
}
impl<E, K> Construct<E, AbstId> for cs::Ann<Option<DefId>, K>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, AbstId)> {
        let cs::Ann(def, kd) = self;
        let (env, kd) = kd.construct(tycker, env)?;
        let abst = Alloc::alloc(tycker, def, kd, &());
        Ok((env, abst))
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl<E: ConstructEnv> Construct<E, KindId> for cs::TypeOf<TPatId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        let cs::TypeOf(tpat) = self;
        Ok((env, tycker.statics.annotations_tpat[&tpat]))
    }
}
impl<E: ConstructEnv> Construct<E, KindId> for cs::TypeOf<TypeId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        let cs::TypeOf(ty) = self;
        Ok((env, tycker.statics.type_kind(ty)))
    }
}
impl<E: ConstructEnv> Construct<E, KindId> for cs::TypeOf<AbstId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        let cs::TypeOf(abst) = self;
        Ok((env, tycker.statics.annotations_abst[&abst]))
    }
}
impl<E: ConstructEnv> Construct<E, KindId> for VType {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        Ok((env, tycker.intrinsics.vtype))
    }
}
impl<E: ConstructEnv> Construct<E, KindId> for CType {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        Ok((env, tycker.intrinsics.ctype))
    }
}
impl<E, S, T> Construct<E, KindId> for Arrow<S, T>
where
    E: ConstructEnv,
    S: Construct<E, KindId>,
    T: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, KindId)> {
        let Arrow(k1, k2) = self;
        let (env, k1) = k1.construct(tycker, env)?;
        let (env, k2) = k2.construct(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Arrow(k1, k2), (), &())))
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl<E, K> Construct<E, TPatId> for cs::Pat<Hole, K>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TPatId)> {
        let cs::Pat(Hole, kd) = self;
        let (env, kd) = kd.construct(tycker, env)?;
        let tpat = Alloc::alloc(tycker, Hole, kd, env.ty());
        Ok((env, tpat))
    }
}
impl<E, V, K> Construct<E, TPatId> for cs::Pat<V, K>
where
    E: ConstructEnv,
    V: IntoName<VarName>,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TPatId)> {
        let cs::Pat(var, kd) = self;
        let (env, kd) = kd.construct(tycker, env)?;
        let def = Alloc::alloc(tycker, var.into_name(), kd.into(), &());
        cs::Ann(def, kd).construct(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<E, T> Construct<E, TypeId> for cs::Type<T>
where
    E: ConstructEnv,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Type(ty) = self;
        ty.construct(tycker, env)
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for cs::TypeOf<VPatId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::TypeOf(vpat) = self;
        Ok((env, tycker.statics.annotations_vpat[&vpat]))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for cs::TypeOf<ValueId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::TypeOf(value) = self;
        Ok((env, tycker.statics.annotations_value[&value]))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for cs::TypeOf<CompuId> {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::TypeOf(compu) = self;
        Ok((env, tycker.statics.annotations_compu[&compu]))
    }
}
impl<E, K> Construct<E, TypeId> for cs::Ann<Hole, (K, su::TermId)>
where
    E: ConstructEnv,
    K: Construct<E, KindId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Ann(Hole, (kd, site)) = self;
        let (env, kd) = kd.construct(tycker, env)?;
        let fill: FillId = tycker.fresh();
        tycker.statics.fills.insert_new(fill, site.into());
        let ty = Alloc::alloc(tycker, fill, kd, env.ty());
        Ok((env, ty))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for AbstId {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let kd = tycker.statics.annotations_abst[&self];
        let ty = Alloc::alloc(tycker, self, kd, env.ty());
        Ok((env, ty))
    }
}
/// A constructed type application is normalized, together with its type argument.
impl<E, S, T> Construct<E, TypeId> for App<S, T>
where
    E: ConstructEnv,
    S: Construct<E, TypeId>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let App(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.construct(tycker, env)?;
        let kd_1 = tycker.statics.type_kind(ty_1);
        let Some((_, kd_b)) = kd_1.destruct_arrow(tycker) else {
            unreachable!("a type operator has an arrow kind")
        };
        let (env, ty_2) = ty_2.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, App(ty_1, ty_2), kd_b, env.ty());
        let ty = ty.normalize(tycker, kd_b)?;
        Ok((env, ty))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for PrimitiveTy {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        Ok((env, tycker.intrinsics.primitives[&self.0]))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for ThkTy {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        Ok((env, tycker.intrinsics.thk))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for RetTy {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        Ok((env, tycker.intrinsics.ret))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for UnitTy {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        Ok((env, tycker.intrinsics.unit))
    }
}
impl<E, T> Construct<E, TypeId> for cs::Thk<T>
where
    E: ConstructEnv,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Thk(arg) = self;
        let (env, thk) = ThkTy.construct(tycker, env)?;
        let (env, arg) = arg.construct(tycker, env)?;
        let (env, vtype) = VType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, App(thk, arg), vtype, env.ty());
        Ok((env, ty))
    }
}
impl<E, T> Construct<E, TypeId> for cs::Ret<T>
where
    E: ConstructEnv,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Ret(arg) = self;
        let (env, ret) = RetTy.construct(tycker, env)?;
        let (env, arg) = arg.construct(tycker, env)?;
        let (env, ctype) = CType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, App(ret, arg), ctype, env.ty());
        Ok((env, ty))
    }
}
impl<E, F, T> Construct<E, TypeId> for cs::Data<DataId, F>
where
    E: ConstructEnv,
    F: Clone + FnOnce(CtorName, TypeId) -> T,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Data(data, f) = self;
        let arms = tycker.statics.datas[&data].clone();
        let arms = arms
            .into_iter()
            .map(|(ctor, ty)| {
                let (_, ty) = (f.clone())(ctor.clone(), ty).construct(tycker, env.clone())?;
                Ok((ctor, ty))
            })
            .collect::<Result<rpds::VectorSync<_>>>()?;
        let data: DataId = tycker.fresh();
        tycker.statics.datas.insert_new(data, Data::new(arms.iter().cloned()));
        let (env, kd) = VType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, data, kd, env.ty());
        Ok((env, ty))
    }
}
impl<E, F, T> Construct<E, TypeId> for cs::CoData<CoDataId, F>
where
    E: ConstructEnv,
    F: Clone + FnOnce(DtorName, TypeId) -> T,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::CoData(coda, f) = self;
        let arms = tycker.statics.codatas[&coda].clone();
        let arms = arms
            .into_iter()
            .map(|(dtor, ty)| {
                let (_, ty) = (f.clone())(dtor.clone(), ty).construct(tycker, env.clone())?;
                Ok((dtor, ty))
            })
            .collect::<Result<rpds::VectorSync<_>>>()?;
        let coda: CoDataId = tycker.fresh();
        tycker.statics.codatas.insert_new(coda, CoData::new(arms.iter().cloned()));
        let (env, kd) = CType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, coda, kd, env.ty());
        Ok((env, ty))
    }
}
impl<E, T> Construct<E, TypeId> for Prod<T>
where
    E: ConstructEnv,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let Prod(components) = self;
        let (env, components): (E, Vec<TypeId>) = components.construct(tycker, env)?;
        let (env, vtype) = VType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, Prod(components), vtype, env.ty());
        Ok((env, ty))
    }
}
impl<E, F, A, T> Construct<E, TypeId> for cs::Forall<A, F>
where
    E: ConstructEnv,
    F: FnOnce(AbstId) -> T,
    A: Construct<E, AbstId>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let cs::Forall(abst, ty) = self;
        let (env, abst) = abst.construct(tycker, env)?;
        let (env, ty) = ty(abst).construct(tycker, env)?;
        let (env, ctype) = CType.construct(tycker, env)?;
        let binder = TypeBinder::with_witness(tycker, abst, env.ty());
        let ty = Alloc::alloc(tycker, Forall(binder, ty), ctype, env.ty());
        Ok((env, ty))
    }
}
impl<E: ConstructEnv> Construct<E, TypeId> for cs::TopTy {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let (env, ctype) = CType.construct(tycker, env)?;
        let coda: CoDataId = tycker.fresh();
        tycker.statics.codatas.insert_new(coda, CoData::new([]));
        let ty = Alloc::alloc(tycker, coda, ctype, env.ty());
        Ok((env, ty))
    }
}
impl<E, S, T> Construct<E, TypeId> for Arrow<S, T>
where
    E: ConstructEnv,
    S: Construct<E, TypeId>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, TypeId)> {
        let Arrow(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.construct(tycker, env)?;
        let (env, ty_2) = ty_2.construct(tycker, env)?;
        let (env, ctype) = CType.construct(tycker, env)?;
        let ty = Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype, env.ty());
        Ok((env, ty))
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<E, T> Construct<E, VPatId> for cs::Pat<Hole, T>
where
    E: ConstructEnv,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, VPatId)> {
        let cs::Pat(Hole, ty) = self;
        let (env, ty) = ty.construct(tycker, env)?;
        cs::Ann(Hole, ty).construct(tycker, env)
    }
}
impl<E, V, T> Construct<E, VPatId> for cs::Pat<V, T>
where
    E: ConstructEnv,
    V: IntoName<VarName>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, VPatId)> {
        let cs::Pat(var, ty) = self;
        let (env, ty) = ty.construct(tycker, env)?;
        let def = Alloc::alloc(tycker, var.into_name(), ty.into(), &());
        cs::Ann(def, ty).construct(tycker, env)
    }
}

/// The data type that a constructed constructor belongs to.
struct DataHint(TypeId);

impl DataHint {
    fn resolve(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<DataId> {
        let view = self.0.unroll(tycker)?.subst_env(tycker, env)?;
        let Type::Data(data) = tycker.type_filled(&view)?.to_owned() else {
            unreachable!("a constructed constructor retains a data classifier")
        };
        Ok(data)
    }
}

impl<E, C, V, T> Construct<E, VPatId> for cs::Pat<cs::Ctor<C, V>, T>
where
    E: ConstructEnv,
    C: IntoName<CtorName>,
    V: Construct<E, VPatId>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, VPatId)> {
        let cs::Pat(cs::Ctor(ctor, body), ty) = self;
        let (env, body) = body.construct(tycker, env)?;
        let (env, ty) = ty.construct(tycker, env)?;
        let pattern = Alloc::alloc(tycker, Ctor(ctor.into_name(), body), ty, env.ty());
        let data = DataHint(ty).resolve(tycker, env.ty())?;
        tycker.statics.data_pat_hints.insert_new(pattern, data);
        Ok((env, pattern))
    }
}
impl<E: ConstructEnv> Construct<E, VPatId> for Triv {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, VPatId)> {
        let (env, ty) = UnitTy.construct(tycker, env)?;
        cs::Ann(Triv, ty).construct(tycker, env)
    }
}
impl<E, S, T> Construct<E, VPatId> for ConsN<S, T>
where
    E: ConstructEnv,
    S: Construct<E, VPatId>,
    T: Construct<E, VPatId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, VPatId)> {
        let ConsN(items, tail) = self;
        let (env, mut components): (E, Vec<VPatId>) = items.construct(tycker, env)?;
        let (env, tail) = tail.construct(tycker, env)?;
        components.push(tail);
        let component_tys = components
            .iter()
            .map(|component| tycker.statics.annotations_vpat[component])
            .collect::<Vec<_>>();
        let (env, ty) = Prod(component_tys).construct(tycker, env)?;
        cs::Ann(ss::ValuePattern::VCons(components), ty).construct(tycker, env)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<E, T> Construct<E, ValueId> for cs::Value<T>
where
    E: ConstructEnv,
    T: Construct<E, ValueId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let cs::Value(arg) = self;
        arg.construct(tycker, env)
    }
}
/// A value pattern constructs the value it binds.
impl<E: ConstructEnv> Construct<E, ValueId> for VPatId {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let value = self.reify(tycker);
        Ok((env, value))
    }
}
impl<E: ConstructEnv> Construct<E, ValueId> for DefId {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let def = env.def(self);
        let AnnId::Type(ty) = tycker.statics.annotations_var[&def] else {
            unreachable!("a value definition is annotated with its type")
        };
        let value = Alloc::alloc(tycker, def, ty, env.ty());
        Ok((env, value))
    }
}
impl<E, T> Construct<E, ValueId> for Thunk<T>
where
    E: ConstructEnv,
    T: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let Thunk(body) = self;
        let (env, body) = body.construct(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = cs::Thk(body_ty).construct(tycker, env)?;
        let value = Alloc::alloc(tycker, Thunk(body), ty, env.ty());
        Ok((env, value))
    }
}
impl<E: ConstructEnv> Construct<E, ValueId> for Triv {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let (env, ty) = UnitTy.construct(tycker, env)?;
        let value = Alloc::alloc(tycker, Triv, ty, env.ty());
        Ok((env, value))
    }
}
impl<E, S, T> Construct<E, ValueId> for ConsN<S, T>
where
    E: ConstructEnv,
    S: Construct<E, ValueId>,
    T: Construct<E, ValueId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let ConsN(items, tail) = self;
        let (env, mut components): (E, Vec<ValueId>) = items.construct(tycker, env)?;
        let (env, tail) = tail.construct(tycker, env)?;
        components.push(tail);
        let component_tys = components
            .iter()
            .map(|component| tycker.statics.annotations_value[component])
            .collect::<Vec<_>>();
        let (env, ty) = Prod(component_tys).construct(tycker, env)?;
        let value = Alloc::alloc(tycker, ss::Value::VCons(components), ty, env.ty());
        Ok((env, value))
    }
}
impl<E, C, V, T> Construct<E, ValueId> for cs::Ann<cs::Ctor<C, V>, T>
where
    E: ConstructEnv,
    C: IntoName<CtorName>,
    V: Construct<E, ValueId>,
    T: Construct<E, TypeId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let cs::Ann(cs::Ctor(ctor, body), ty) = self;
        let (env, body) = body.construct(tycker, env)?;
        let (env, ty) = ty.construct(tycker, env)?;
        let value = Alloc::alloc(tycker, Ctor(ctor.into_name(), body), ty, env.ty());
        let data = DataHint(ty).resolve(tycker, env.ty())?;
        tycker.statics.data_hints.insert_new(value, data);
        Ok((env, value))
    }
}
/// An administrative pure binding around a value.
impl<E, P, B, F, R> Construct<E, ValueId> for Let<P, B, F>
where
    E: ConstructEnv,
    P: Construct<E, VPatId>,
    B: Construct<E, ValueId>,
    F: FnOnce(VPatId) -> R,
    R: Construct<E, ValueId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, ValueId)> {
        let Let { binder, bindee, tail } = self;
        let (env, bindee) = bindee.construct(tycker, env)?;
        let (env, binder) = binder.construct(tycker, env)?;
        let (env, tail) = tail(binder).construct(tycker, env)?;
        let tail_ty = tycker.statics.annotations_value[&tail];
        let value = Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty, env.ty());
        Ok((env, value))
    }
}

/* ------------------------------- Computation ------------------------------ */

/// Computation abstraction over a value parameter.
impl<E, P, F, T> Construct<E, CompuId> for Abs<P, F>
where
    E: ConstructEnv,
    P: Construct<E, VPatId>,
    F: FnOnce(VPatId) -> T,
    T: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let Abs(vpat, body) = self;
        let (env, vpat) = vpat.construct(tycker, env)?;
        let param_ty = tycker.statics.annotations_vpat[&vpat];
        let (env, body) = body(vpat).construct(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = Arrow(param_ty, body_ty).construct(tycker, env)?;
        let compu = Alloc::alloc(tycker, Abs(vpat, body), ty, env.ty());
        Ok((env, compu))
    }
}
/// Computation application to a value argument.
impl<E, S, T> Construct<E, CompuId> for App<S, T>
where
    E: ConstructEnv,
    S: Construct<E, CompuId>,
    T: Construct<E, ValueId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let App(abs, arg) = self;
        let (env, abs) = abs.construct(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else {
            return tycker.err(TyckError::TypeExpected {
                expected: "a computation arrow for an application".to_owned(),
                found: abs_ty,
            });
        };
        let (env, arg) = arg.construct(tycker, env)?;
        let arg_ty = tycker.statics.annotations_value[&arg];
        Lub::lub(param_ty, arg_ty, tycker)?;
        let compu = Alloc::alloc(tycker, App(abs, arg), body_ty, env.ty());
        Ok((env, compu))
    }
}
impl<E, P, F, T> Construct<E, CompuId> for Fix<P, F>
where
    E: ConstructEnv,
    P: Construct<E, VPatId>,
    F: FnOnce(VPatId) -> T,
    T: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let Fix(vpat, body) = self;
        let (env, vpat) = vpat.construct(tycker, env)?;
        let (env, body) = body(vpat).construct(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let compu = Alloc::alloc(tycker, Fix(vpat, body), body_ty, env.ty());
        Ok((env, compu))
    }
}
impl<E, T> Construct<E, CompuId> for Force<T>
where
    E: ConstructEnv,
    T: Construct<E, ValueId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let Force(thk) = self;
        let (env, thk) = thk.construct(tycker, env)?;
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else {
            unreachable!("a forced value is a thunk")
        };
        let compu = Alloc::alloc(tycker, Force(thk), body_ty, env.ty());
        Ok((env, compu))
    }
}
/// A pure binding around a computation.
impl<E, P, B, F, R> Construct<E, CompuId> for Let<P, B, F>
where
    E: ConstructEnv,
    P: Construct<E, VPatId>,
    B: Construct<E, ValueId>,
    F: FnOnce(VPatId) -> R,
    R: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let Let { binder, bindee, tail } = self;
        let (env, bindee) = bindee.construct(tycker, env)?;
        let (env, binder) = binder.construct(tycker, env)?;
        let (env, tail) = tail(binder).construct(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        let compu = Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty, env.ty());
        Ok((env, compu))
    }
}
/// Shape-check a comatch while retaining its nominal expected classifier.
impl<E, F, R> Construct<E, CompuId> for cs::CoMatch<TypeId, F>
where
    E: ConstructEnv,
    F: Clone + FnOnce(DtorName, TypeId) -> R,
    R: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let cs::CoMatch(ty, arm) = self;
        let view = ty.unroll(tycker)?;
        let Type::CoData(coda_id) = tycker.type_filled(&view)? else {
            return tycker.err(TyckError::TypeExpected {
                expected: "codata for a comatch".to_owned(),
                found: ty,
            });
        };
        let coda = tycker.statics.codatas[&coda_id].to_owned();
        let arms = coda
            .into_iter()
            .map(|(dtor, ty)| {
                let (_, tail) = (arm.clone())(dtor.clone(), ty).construct(tycker, env.clone())?;
                Lub::lub(ty, tycker.statics.annotations_compu[&tail], tycker)?;
                Ok(CoMatcher { dtor, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        let compu = Alloc::alloc(tycker, CoMatch { arms }, ty, env.ty());
        let _ = tycker.statics.codata_hints.upsert(compu, coda_id);
        Ok((env, compu))
    }
}
impl<E, T> Construct<E, CompuId> for Dtor<T, DtorName>
where
    E: ConstructEnv,
    T: Construct<E, CompuId>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let Dtor(head, dtor) = self;
        let (env, head) = head.construct(tycker, env)?;
        let head_ty = tycker.statics.annotations_compu[&head];
        let head_view = head_ty.unroll(tycker)?.subst_env(tycker, env.ty())?;
        let Type::CoData(coda_id) = tycker.type_filled(&head_view)?.to_owned() else {
            return tycker.err(TyckError::TypeExpected {
                expected: "codata for a destructor".to_owned(),
                found: head_ty,
            });
        };
        let _ = tycker.statics.codata_hints.upsert(head, coda_id);
        let coda = tycker.statics.codatas[&coda_id].to_owned();
        let Some(ty) = coda.get(&dtor) else {
            return tycker.err(TyckError::UnknownCoDataDestructor(dtor));
        };
        let compu = Alloc::alloc(tycker, Dtor(head, dtor), ty, env.ty());
        Ok((env, compu))
    }
}
impl<E, T, D> Construct<E, CompuId> for cs::Dtor<T, D>
where
    E: ConstructEnv,
    T: Construct<E, CompuId>,
    D: IntoName<DtorName>,
{
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let cs::Dtor(head, dtor) = self;
        Dtor(head, dtor.into_name()).construct(tycker, env)
    }
}
impl<E: ConstructEnv> Construct<E, CompuId> for cs::Top {
    fn construct(self, tycker: &mut Tycker<'_>, env: E) -> Result<(E, CompuId)> {
        let (env, top) = cs::TopTy.construct(tycker, env)?;
        let Ok(Type::CoData(codata)) = tycker.type_filled(&top) else {
            unreachable!("top is an empty codata")
        };
        let compu = Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top, env.ty());
        tycker.statics.codata_hints.insert_new(compu, codata);
        Ok((env, compu))
    }
}

/* ------------------------------ Type helpers ------------------------------ */

impl Tycker<'_> {
    /// `Thk B` for a known computation type `B`.
    pub fn thk_arg(&mut self, env: &TyEnv, arg: TypeId) -> TypeId {
        cs::Thk(arg).build(self, env)
    }
    /// `Thk _`, whose computation type is inferred at `site`.
    pub fn thk_hole(&mut self, env: &TyEnv, site: su::TermId) -> TypeId {
        cs::Thk(cs::Ann(Hole, (CType, site))).build(self, env)
    }
    /// `Ret _`, whose value type is inferred at `site`.
    pub fn ret_hole(&mut self, env: &TyEnv, site: su::TermId) -> TypeId {
        cs::Ret(cs::Ann(Hole, (VType, site))).build(self, env)
    }
}

#[cfg(test)]
mod kind_test {
    use crate::*;

    #[test]
    fn r#static() {
        fn _f(tycker: &mut Tycker<'_>, env: &TyEnv) -> KindId {
            // VType -> (CType -> CType)
            Arrow(VType, Arrow(CType, CType)).build(tycker, env)
        }
    }
}
