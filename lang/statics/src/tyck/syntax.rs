pub use super::arena::*;
pub use super::env::*;
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::surface_syntax as su;
use derive_more::{From, IntoIterator};

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = su::DefId;
// PatId and TermId are unsorted, so we've got the following:
zydeco_utils::new_key_type! {
    pub struct KindId;
    pub struct KPatId;
    pub struct TPatId;
    pub struct TypeId;
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
}
// .. and here we have them defined as dispatchers
/// A dispatcher for all patterns.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum PatId {
    Kind(KPatId),
    Type(TPatId),
    Value(VPatId),
}
// .. and here too
/// A dispatcher for all terms.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum TermId {
    Kind(KindId),
    Type(TypeId),
    Value(ValueId),
    Compu(CompuId),
}
// and here, a very useful dispatcher for all terms that can show up at annotation sites
/// A dispatcher for all annotations.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum AnnId {
    Set,
    Kind(KindId),
    Type(TypeId),
}
// and there are times when we need a proper pair of annotated things
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum PatAnnId {
    Kind(KPatId),
    Type(TPatId, KindId),
    Value(VPatId, TypeId),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum TermAnnId {
    Hole(FillId),
    Kind(KindId),
    Type(TypeId, KindId),
    Value(ValueId, TypeId),
    Compu(CompuId, TypeId),
}
zydeco_utils::new_key_type! {
    /// Identifier for abstract types, including:
    /// 1. sealed types, and
    /// 2. type instantiations for forall and exists.
    pub struct AbstId;
    /// Identifier for hole-filling targets with context constraints.
    pub struct FillId;
    /// Identifier for data definitions.
    pub struct DataId;
    /// Identifier for codata definitions.
    pub struct CoDataId;
}

/// A pattern carried by an erased, manifest kind component.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum StaticPatId {
    Kind(KPatId),
    Type(TPatId),
}

/// A compile-time package component erased before dynamics.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum StaticTermId {
    Kind(KindId),
    Type(TypeId),
}

mod impls_identifiers {
    use super::{super::*, *};
    use crate::*;

    impl AnnId {
        pub fn as_type(self) -> TypeId {
            match self {
                | AnnId::Type(ty) => ty,
                | _ => unreachable!(),
            }
        }
        pub fn as_kind(self) -> KindId {
            match self {
                | AnnId::Kind(kd) => kd,
                | _ => unreachable!(),
            }
        }
    }

    impl StaticPatId {
        pub fn as_kind(self) -> Option<KPatId> {
            match self {
                | Self::Kind(pattern) => Some(pattern),
                | Self::Type(_) => None,
            }
        }

        pub fn as_type(self) -> Option<TPatId> {
            match self {
                | Self::Kind(_) => None,
                | Self::Type(pattern) => Some(pattern),
            }
        }
    }

    impl StaticTermId {
        pub fn as_kind(self) -> Option<KindId> {
            match self {
                | Self::Kind(kind) => Some(kind),
                | Self::Type(_) => None,
            }
        }

        pub fn as_type(self) -> Option<TypeId> {
            match self {
                | Self::Kind(_) => None,
                | Self::Type(ty) => Some(ty),
            }
        }
    }

    impl PatAnnId {
        pub fn as_pat(self) -> PatId {
            match self {
                | PatAnnId::Kind(pat) => PatId::Kind(pat),
                | PatAnnId::Type(pat, _) => PatId::Type(pat),
                | PatAnnId::Value(pat, _) => PatId::Value(pat),
            }
        }
        pub fn mk_hole(tycker: &mut Tycker<'_>, env: &TyEnv, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => {
                    let tm = Alloc::alloc(tycker, Hole, (), env);
                    PatAnnId::Kind(tm)
                }
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, Hole, kd, env);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, Hole, ty, env);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn mk_var(tycker: &mut Tycker<'_>, env: &TyEnv, def: DefId, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => {
                    let tm = Alloc::alloc(tycker, def, (), env);
                    PatAnnId::Kind(tm)
                }
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, def, kd, env);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, def, ty, env);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn as_type(self) -> (TPatId, KindId) {
            match self {
                | PatAnnId::Type(pat, kd) => (pat, kd),
                | PatAnnId::Kind(_) | PatAnnId::Value(_, _) => unreachable!(),
            }
        }
        pub fn as_value(self) -> (VPatId, TypeId) {
            match self {
                | PatAnnId::Value(pat, ty) => (pat, ty),
                | PatAnnId::Kind(_) | PatAnnId::Type(_, _) => unreachable!(),
            }
        }
        pub fn try_as_kind(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<KPatId> {
            match self {
                | PatAnnId::Kind(pat) => Ok(pat),
                | PatAnnId::Type(_, _) | PatAnnId::Value(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_type(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(TPatId, KindId)> {
            match self {
                | PatAnnId::Type(pat, kd) => Ok((pat, kd)),
                | PatAnnId::Kind(_) | PatAnnId::Value(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_value(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(VPatId, TypeId)> {
            match self {
                | PatAnnId::Value(pat, ty) => Ok((pat, ty)),
                | PatAnnId::Kind(_) | PatAnnId::Type(_, _) => tycker.err_k(err, blame),
            }
        }
    }

    impl TermAnnId {
        pub fn as_term(self) -> Option<TermId> {
            let res = match self {
                | TermAnnId::Kind(k) => TermId::Kind(k),
                | TermAnnId::Type(t, _) => TermId::Type(t),
                | TermAnnId::Value(v, _) => TermId::Value(v),
                | TermAnnId::Compu(c, _) => TermId::Compu(c),
                | TermAnnId::Hole(_) => None?,
            };
            Some(res)
        }
        pub fn as_term_static(self) -> AnnId {
            match self {
                | TermAnnId::Kind(k) => AnnId::Kind(k),
                | TermAnnId::Type(t, _) => AnnId::Type(t),
                | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                    unreachable!()
                }
            }
        }
        pub fn try_as_kind(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<KindId> {
            match self {
                | TermAnnId::Kind(kd) => Ok(kd),
                | TermAnnId::Hole(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_type(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(TypeId, KindId)> {
            match self {
                | TermAnnId::Type(ty, kd) => Ok((ty, kd)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_value(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(ValueId, TypeId)> {
            match self {
                | TermAnnId::Value(val, ty) => Ok((val, ty)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_compu(
            self, tycker: &mut Tycker<'_>, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(CompuId, TypeId)> {
            match self {
                | TermAnnId::Compu(com, ty) => Ok((com, ty)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _) => tycker.err_k(err, blame),
            }
        }
    }
}

/* --------------------------------- Context -------------------------------- */

pub use su::Context;

/* -------------------------------- Fillable -------------------------------- */

/// A value that is either complete or a fillable hole.
#[derive(Clone, Debug, From, Hash, PartialEq, Eq)]
pub enum Fillable<T> {
    Fill(FillId),
    #[from(ignore)]
    Done(T),
}

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VType;
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CType;

#[derive(Clone, Debug, From, Hash, PartialEq, Eq)]
pub enum Kind {
    VType(VType),
    CType(CType),
    Arrow(ArrowU<KindId>),
    Label(Label<FieldName, KindId>),
}

/* ------------------------------- KindPattern ------------------------------ */

#[derive(From, Clone, Debug)]
pub enum KindPattern {
    Hole(Hole),
    Var(DefId),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, TPatId>),
}

/// `U`
#[derive(Clone, Debug)]
pub struct ThkTy;

/// `F`
#[derive(Clone, Debug)]
pub struct RetTy;

/// `Unit`
#[derive(Clone, Debug)]
pub struct UnitTy;

/// `Int`
#[derive(Clone, Debug)]
pub struct IntTy;

/// `Char`
#[derive(Clone, Debug)]
pub struct CharTy;

/// `String`
#[derive(Clone, Debug)]
pub struct StringTy;

/// `OS`
#[derive(Clone, Debug)]
pub struct OSTy;

/// A type-level binder retains both its checked pattern and the abstract
/// payload used in the body.
///
/// Keeping the pattern is semantically relevant for named binders:
/// `(field = X)` binds the payload of a named type, while a plain `X` binds
/// the whole argument.
#[derive(Clone, Debug)]
pub struct TypeBinder {
    pub pattern: TPatId,
    pub witness: AbstId,
}

/// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub TypeBinder, pub TypeId);

/// The non-empty telescope of abstract type witnesses bound by a [`PackPi`].
///
/// The order records how the witnesses correspond to the existential binders
/// opened from the package domain.
#[derive(Clone, Debug)]
pub struct PackTelescope {
    first: AbstId,
    rest: im::Vector<AbstId>,
}

/// A package-dependent computation arrow.
///
/// `witnesses` are abstract type identities obtained by opening `domain`.
/// They are bound in `codomain`, but not in `domain`.
#[derive(Clone, Debug)]
pub struct PackPi {
    pub domain: TypeId,
    pub witnesses: PackTelescope,
    pub codomain: TypeId,
}

/// Whether an existential binder is abstract or discloses its definition.
#[derive(Clone, Debug)]
pub enum ExistsMode {
    Abstract,
    Manifest(TypeId),
}

/// `exists (X : K) . B` or `exists (X as A : K) . B`
#[derive(Clone, Debug)]
pub struct Exists {
    pub binder: TypeBinder,
    pub mode: ExistsMode,
    pub body: TypeId,
}

/// A transparent kind alias exported by a package signature.
///
/// Unlike an abstract existential type component, this entry introduces no
/// fresh identity. Its definition is substituted into the package body and
/// the component is erased before dynamics.
#[derive(Clone, Debug)]
pub struct ManifestKind {
    pub binder: KPatId,
    pub definition: KindId,
    pub body: TypeId,
}

/// data | C_1 ty | ... end
#[derive(Clone, Debug, IntoIterator)]
pub struct Data {
    #[into_iterator(owned, ref)]
    arms: im::Vector<(CtorName, TypeId)>,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug, IntoIterator)]
pub struct CoData {
    #[into_iterator(owned, ref)]
    arms: im::Vector<(DtorName, TypeId)>,
}

mod impls_structs {
    use super::*;

    impl Exists {
        pub fn new(binder: TypeBinder, body: TypeId) -> Self {
            Self { binder, mode: ExistsMode::Abstract, body }
        }

        pub fn with_manifest(binder: TypeBinder, definition: TypeId, body: TypeId) -> Self {
            Self { binder, mode: ExistsMode::Manifest(definition), body }
        }

        pub fn definition(&self) -> Option<TypeId> {
            match self.mode {
                | ExistsMode::Abstract => None,
                | ExistsMode::Manifest(definition) => Some(definition),
            }
        }
    }

    impl PackTelescope {
        pub fn new(first: AbstId, rest: impl IntoIterator<Item = AbstId>) -> Self {
            Self { first, rest: rest.into_iter().collect() }
        }

        pub fn singleton(witness: AbstId) -> Self {
            Self::new(witness, [])
        }

        pub fn iter(&self) -> impl Iterator<Item = &AbstId> {
            std::iter::once(&self.first).chain(self.rest.iter())
        }

        pub fn contains(&self, witness: &AbstId) -> bool {
            self.iter().any(|candidate| candidate == witness)
        }

        pub fn len(&self) -> usize {
            1 + self.rest.len()
        }

        pub fn is_empty(&self) -> bool {
            false
        }

        pub fn map(self, mut f: impl FnMut(AbstId) -> AbstId) -> Self {
            Self::new(f(self.first), self.rest.into_iter().map(f))
        }
    }

    impl Data {
        pub fn new(arms: impl IntoIterator<Item = (CtorName, TypeId)>) -> Self {
            Self { arms: arms.into_iter().collect() }
        }
        pub fn get(&self, ctor: &CtorName) -> Option<TypeId> {
            use std::collections::HashMap;
            self.arms.iter().cloned().collect::<HashMap<_, _>>().get(ctor).cloned()
        }
        pub fn iter(&self) -> impl Iterator<Item = &(CtorName, TypeId)> {
            self.into_iter()
        }
        pub fn len(&self) -> usize {
            self.arms.len()
        }
    }

    impl CoData {
        pub fn new(arms: impl IntoIterator<Item = (DtorName, TypeId)>) -> Self {
            Self { arms: arms.into_iter().collect() }
        }
        pub fn get(&self, dtor: &DtorName) -> Option<TypeId> {
            use std::collections::HashMap;
            self.arms.iter().cloned().collect::<HashMap<_, _>>().get(dtor).cloned()
        }
        pub fn iter(&self) -> impl Iterator<Item = &(DtorName, TypeId)> {
            self.into_iter()
        }
        pub fn len(&self) -> usize {
            self.arms.len()
        }
    }
}

#[derive(From, Clone, Debug)]
pub enum Type {
    Var(DefId),
    Abst(AbstId),
    Abs(Abs<TPatId, TypeId>),
    App(App<TypeId, TypeId>),
    Named(Named<FieldName, TypeId>),
    Label(Label<FieldName, TypeId>),
    Proj(Proj<TypeId, FieldName>),
    Thk(ThkTy),
    Ret(RetTy),
    Unit(UnitTy),
    Int(IntTy),
    Char(CharTy),
    String(StringTy),
    OS(OSTy),
    Arrow(ArrowU<TypeId>),
    Forall(Forall),
    PackPi(PackPi),
    Prod(ProdU<TypeId>),
    Exists(Exists),
    ManifestKind(ManifestKind),
    Data(DataId),
    CoData(CoDataId),
}

mod impls_types {
    use super::*;
    // use crate::err::*;

    impl Type {}
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, VPatId>),
    Ctor(Ctor<CtorName, VPatId>),
    Triv(Triv),
    VCons(ConsN<VPatId, VPatId>),
    SCons(ConsN<StaticPatId, VPatId>),
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, ValueId>),
    /// Administrative scoped binding used when a source block produces a value.
    Let(Let<VPatId, ValueId, ValueId>),
    Thunk(Thunk<CompuId>),
    Ctor(Ctor<CtorName, ValueId>),
    Triv(Triv),
    VCons(ConsN<ValueId, ValueId>),
    SCons(ConsN<StaticTermId, ValueId>),
    Proj(Proj<ValueId, ResolvedField>),
    Lit(Literal),
}

/// A statically resolved named projection.
///
/// The field name remains available for typed formatting, while `target`
/// records how the projection is erased.
#[derive(Clone, Debug)]
pub struct ResolvedField {
    pub name: FieldName,
    pub target: ProjTarget,
}

#[derive(Clone, Debug)]
pub enum ProjTarget {
    Direct,
    Product(usize),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<VPatId, CompuId>),
    VApp(App<CompuId, ValueId>),
    TAbs(Abs<TPatId, CompuId>),
    TApp(App<CompuId, TypeId>),
    Fix(Fix<VPatId, CompuId>),
    Force(Force<ValueId>),
    Ret(Return<ValueId>),
    Do(Bind<VPatId, CompuId, CompuId>),
    Let(Let<VPatId, ValueId, CompuId>),
    Match(Match<ValueId, VPatId, CompuId>),
    CoMatch(CoMatch<DtorName, CompuId>),
    Dtor(Dtor<CompuId, DtorName>),
}
