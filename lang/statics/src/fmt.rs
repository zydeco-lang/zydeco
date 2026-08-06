//! Formatters for scoped and statics entities in the type checker.

use super::syntax::*;
use crate::arena::StaticsArena;
use zydeco_surface::scoped::syntax::ScopedArena;
use zydeco_utils::arena::ArenaAccess;

/* -------------------------------- Formatter ------------------------------- */

pub use zydeco_syntax::{Pretty, Ugly};

pub struct Formatter<'arena> {
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
    pub indent: isize,
}
impl<'arena> Formatter<'arena> {
    pub fn new(scoped: &'arena ScopedArena, statics: &'arena StaticsArena) -> Self {
        Formatter { scoped, statics, indent: 2 }
    }
}

/// A source-facing equation that reveals the representation behind one
/// lexically sealed abstract type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SealedTypeEquation(AbstId);

impl SealedTypeEquation {
    pub fn new(statics: &StaticsArena, sealed: AbstId) -> Option<Self> {
        let definition = statics.seals.get(&sealed)?;
        statics.annotations_type.get(definition)?;
        Some(Self(sealed))
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let name = &f.scoped.defs[self];
        s += &name.ugly(f);
        s += &self.concise();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for KindId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let kd = &f.statics.kinds_pre[self];
        match kd {
            | Fillable::Fill(fill) => {
                format!("[fill-kd {}]", fill.concise_inner())
            }
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) => "VType".to_string(),
                | Kind::CType(CType) => "CType".to_string(),
                | Kind::Arrow(kd) => kd.ugly(f),
                | Kind::Label(kd) => kd.ugly(f),
            },
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for KPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match &f.statics.kpats[self] {
            | KindPattern::Hole(pattern) => pattern.ugly(f),
            | KindPattern::Var(definition) => definition.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let tpat = &f.statics.tpats[self];
        match tpat {
            | TypePattern::Hole(tpat) => tpat.ugly(f),
            | TypePattern::Var(def) => def.ugly(f),
            | TypePattern::Named(tpat) => tpat.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TypeId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ty = &f.statics.types_pre[self];
        match ty {
            | Fillable::Fill(fill) => {
                format!("[fill-ty {}]", fill.concise_inner())
            }
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => def.ugly(f),
                | Type::Abst(abst) => abst.ugly(f),
                | Type::Abs(ty) => ty.ugly(f),
                | Type::App(ty) => ty.ugly(f),
                | Type::Named(ty) => ty.ugly(f),
                | Type::Label(ty) => ty.ugly(f),
                | Type::Proj(ty) => ty.ugly(f),
                | Type::Thk(ThkTy) => "Thk".to_string(),
                | Type::Ret(RetTy) => "Ret".to_string(),
                | Type::Unit(UnitTy) => "Unit".to_string(),
                | Type::Int(IntTy) => "Int".to_string(),
                | Type::Char(CharTy) => "Char".to_string(),
                | Type::String(StringTy) => "String".to_string(),
                | Type::OS(OSTy) => "OS".to_string(),
                | Type::VArrow(ty) => ty.ugly(f),
                | Type::VForall(ty) => ty.ugly(f),
                | Type::VPackPi(ty) => ty.ugly(f),
                | Type::Arrow(ty) => ty.ugly(f),
                | Type::Forall(ty) => ty.ugly(f),
                | Type::PackPi(ty) => ty.ugly(f),
                | Type::Prod(ty) => ty.ugly(f),
                | Type::Exists(ty) => ty.ugly(f),
                | Type::ManifestKind(ty) => ty.ugly(f),
                | Type::Data(ty) => ty.ugly(f),
                | Type::CoData(ty) => ty.ugly(f),
            },
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let vpat = &f.statics.vpats[self];
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(vpat) => vpat.ugly(f),
            | VPat::Var(vpat) => vpat.ugly(f),
            | VPat::Named(vpat) => vpat.ugly(f),
            | VPat::Ctor(vpat) => vpat.ugly(f),
            | VPat::Triv(vpat) => vpat.ugly(f),
            | VPat::VCons(vpat) => vpat.ugly(f),
            | VPat::SCons(vpat) => vpat.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let value = &f.statics.values[self];
        match value {
            | Value::Hole(value) => value.ugly(f),
            | Value::Var(value) => value.ugly(f),
            | Value::Named(value) => value.ugly(f),
            | Value::Let(value) => value.ugly(f),
            | Value::VAbs(value) => value.ugly(f),
            | Value::VApp(value) => value.ugly(f),
            | Value::TAbs(value) => value.ugly(f),
            | Value::TApp(value) => value.ugly(f),
            | Value::Thunk(value) => value.ugly(f),
            | Value::Ctor(value) => value.ugly(f),
            | Value::Triv(value) => value.ugly(f),
            | Value::VCons(value) => value.ugly(f),
            | Value::SCons(value) => value.ugly(f),
            | Value::Proj(value) => value.ugly(f),
            | Value::Lit(value) => value.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for StaticPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | StaticPatId::Kind(pattern) => pattern.ugly(f),
            | StaticPatId::Type(pattern) => pattern.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for StaticTermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | StaticTermId::Kind(kind) => kind.ugly(f),
            | StaticTermId::Type(ty) => ty.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CompuId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let compu = &f.statics.compus[self];
        use Computation as Compu;
        match compu {
            | Compu::Hole(compu) => compu.ugly(f),
            | Compu::VAbs(compu) => compu.ugly(f),
            | Compu::VApp(compu) => compu.ugly(f),
            | Compu::TAbs(compu) => compu.ugly(f),
            | Compu::TApp(compu) => compu.ugly(f),
            | Compu::Fix(compu) => compu.ugly(f),
            | Compu::Force(compu) => compu.ugly(f),
            | Compu::Ret(compu) => compu.ugly(f),
            | Compu::Do(compu) => compu.ugly(f),
            | Compu::Let(compu) => compu.ugly(f),
            | Compu::Match(compu) => compu.ugly(f),
            | Compu::CoMatch(compu) => compu.ugly(f),
            | Compu::Dtor(compu) => compu.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for AnnId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | AnnId::Set => "Set".to_string(),
            | AnnId::Kind(kd) => kd.ugly(f),
            | AnnId::Type(ty) => ty.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | TermId::Kind(kd) => kd.ugly(f),
            | TermId::Type(ty) => ty.ugly(f),
            | TermId::Value(v) => v.ugly(f),
            | TermId::Compu(c) => c.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for AbstId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let () = &f.statics.absts[self];
        let sealed = &f.statics.seals.get(self);
        let hint = match f.statics.abst_hints.get(self) {
            | Some(hint) => {
                let hint = &f.scoped.defs[hint];
                hint.ugly(f)
            }
            | None => "".to_string(),
        };
        match sealed {
            | Some(_ty) => {
                format!("{}[sealed {}]", hint, self.concise_inner())
                // format!("[sealed ({}) {}]", self.concise_inner(), ty.ugly(f))
            }
            | None => {
                format!("{}[abst {}]", hint, self.concise_inner())
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DataId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        s += &"data".to_string();
        for (ctor, ty) in f.statics.datas[self].iter() {
            s += &format!(" | {} : {}", ctor.ugly(f), ty.ugly(f));
        }
        s += &" end".to_string();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoDataId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        s += &"codata".to_string();
        for (dtor, ty) in f.statics.codatas[self].iter() {
            s += &format!(" | {} : {}", dtor.ugly(f), ty.ugly(f));
        }
        s += &" end".to_string();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let VarName(name) = self;
        name.clone()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for FieldName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        self.plain()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CtorName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let CtorName(name) = self;
        name.clone()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DtorName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let DtorName(name) = self;
        name.clone()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Hole {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "_".to_string()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Abs<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Abs(s, t) = self;
        format!("fn {} => {}", s.ugly(f), t.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for App<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let App(s, t) = self;
        format!("({} {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Named<FieldName, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Named(name, inner) = self;
        format!("({} = {})", name.ugly(f), inner.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Label<FieldName, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Label(name, inner) = self;
        format!("{} :: {}", name.ugly(f), inner.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Proj<TypeId, FieldName> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Proj(head, name) = self;
        format!("{}/{}", head.ugly(f), name.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Arrow<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Arrow(s, t) = self;
        format!("({} -> {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueArrow {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ValueArrow(input, output) = self;
        format!("({} -> {})", input.ugly(f), output.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Triv {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "()".to_string()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for ConsN<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let ConsN(items, tail) = self;
        let mut items = items.iter().map(|item| item.ugly(f)).collect::<Vec<_>>();
        items.push(tail.ugly(f));
        format!("({})", items.join(", "))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Prod<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Prod(s, t) = self;
        format!("({} * {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Sealed<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Sealed(t) = self;
        t.ugly(f)
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Thunk<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Thunk(tm) = self;
        format!("{{ {} }}", tm.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Force<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Force(tm) = self;
        format!("! {}", tm.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Return<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Return(t) = self;
        format!("ret {}", t.ugly(f))
    }
}

impl<'a, Br, Be, Tail> Ugly<'a, Formatter<'a>> for Bind<Br, Be, Tail>
where
    Br: Ugly<'a, Formatter<'a>>,
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Bind { binder, bindee, tail } = self;
        format!("do {} <- {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a, Br, Be, Tail> Ugly<'a, Formatter<'a>> for Let<Br, Be, Tail>
where
    Br: Ugly<'a, Formatter<'a>>,
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Let { binder, bindee, tail } = self;
        format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a, P, Tm> Ugly<'a, Formatter<'a>> for Fix<P, Tm>
where
    P: Ugly<'a, Formatter<'a>>,
    Tm: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Fix(p, tm) = self;
        format!("fix {} => {}", p.ugly(f), tm.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Ctor<CtorName, Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ctor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Dtor<Tail, DtorName>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Dtor(tail, name) = self;
        format!("{} {}", tail.ugly(f), name.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ResolvedField {
    fn ugly(&self, f: &'a Formatter) -> String {
        self.name.ugly(f)
    }
}

impl<'a, Head> Ugly<'a, Formatter<'a>> for Proj<Head, ResolvedField>
where
    Head: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Proj(head, field) = self;
        format!("{}/{}", head.ugly(f), field.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Literal {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let mut s = String::new();
        match self {
            | Literal::Int(i) => s += &format!("{:?}", i),
            // Fixme: escape string
            | Literal::String(str) => s += &format!("{:?}", str),
            | Literal::Char(c) => s += &format!("{:?}", c),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TypeBinder {
    fn ugly(&self, f: &'a Formatter) -> String {
        let kind = f.statics.annotations_tpat[&self.pattern];
        format!("({} : {})", self.pattern.ugly(f), kind.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Forall {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Forall(abst, ty) = self;
        format!("(forall {} . {})", abst.ugly(f), ty.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueForall {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ValueForall(abst, ty) = self;
        format!("(forall-v {} . {})", abst.ugly(f), ty.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for PackPi {
    fn ugly(&self, f: &'a Formatter) -> String {
        let witnesses = self.witnesses.iter().map(|witness| witness.ugly(f)).collect::<Vec<_>>();
        format!(
            "(pack-pi ([{}] : {}) . {})",
            witnesses.join(", "),
            self.domain.ugly(f),
            self.codomain.ugly(f)
        )
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValuePackPi {
    fn ugly(&self, f: &'a Formatter) -> String {
        let witnesses = self.witnesses.iter().map(|witness| witness.ugly(f)).collect::<Vec<_>>();
        format!(
            "(pack-pi-v ([{}] : {}) . {})",
            witnesses.join(", "),
            self.domain.ugly(f),
            self.codomain.ugly(f)
        )
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Exists {
    fn ugly(&self, f: &'a Formatter) -> String {
        let binder = match self.mode {
            | ExistsMode::Abstract => self.binder.ugly(f),
            | ExistsMode::Manifest(definition) => {
                let kind = f.statics.annotations_tpat[&self.binder.pattern];
                format!(
                    "({} as {} : {})",
                    self.binder.pattern.ugly(f),
                    definition.ugly(f),
                    kind.ugly(f)
                )
            }
        };
        format!("(exists {} . {})", binder, self.body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ManifestKind {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!(
            "(exists ({} as {}) . {})",
            self.binder.ugly(f),
            self.definition.ugly(f),
            self.body.ugly(f)
        )
    }
}

impl<'a, Sc, Br, Tail> Ugly<'a, Formatter<'a>> for Match<Sc, Br, Tail>
where
    Sc: Ugly<'a, Formatter<'a>>,
    Br: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Match { scrut, arms } = self;
        let mut s = String::new();
        s += &format!("match {}", scrut.ugly(f));
        for Matcher { binder, tail } in arms.iter() {
            s += &format!(" | {} => {}", binder.ugly(f), tail.ugly(f));
        }
        s += &" end".to_string();
        s
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for CoMatch<DtorName, Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let CoMatch { arms } = self;
        let mut s = String::new();
        s += &"comatch".to_string();
        for CoMatcher { dtor, tail } in arms.iter() {
            s += &format!(" | {} => {}", dtor.ugly(f), tail.ugly(f));
        }
        s += &" end".to_string();
        s
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.scoped.defs[self];
        name.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for KindId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let kd = &f.statics.kinds_pre[self];
        match kd {
            | Fillable::Fill(fill) => RcDoc::text(format!("[fill-kd {}]", fill.concise_inner())),
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) => RcDoc::text("VType"),
                | Kind::CType(CType) => RcDoc::text("CType"),
                | Kind::Arrow(kd) => kd.pretty(f),
                | Kind::Label(kd) => kd.pretty(f),
            },
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for KPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match &f.statics.kpats[self] {
            | KindPattern::Hole(pattern) => pattern.pretty(f),
            | KindPattern::Var(definition) => definition.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let tpat = &f.statics.tpats[self];
        match tpat {
            | TypePattern::Hole(tpat) => tpat.pretty(f),
            | TypePattern::Var(def) => def.pretty(f),
            | TypePattern::Named(tpat) => tpat.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TypeId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ty = &f.statics.types_pre[self];
        match ty {
            | Fillable::Fill(fill) => RcDoc::text(format!("[fill-ty {}]", fill.concise_inner())),
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => def.pretty(f),
                | Type::Abst(abst) => abst.pretty(f),
                | Type::Abs(ty) => ty.pretty(f),
                | Type::App(ty) => ty.pretty(f),
                | Type::Named(ty) => ty.pretty(f),
                | Type::Label(ty) => ty.pretty(f),
                | Type::Proj(ty) => ty.pretty(f),
                | Type::Thk(ThkTy) => RcDoc::text("Thk"),
                | Type::Ret(RetTy) => RcDoc::text("Ret"),
                | Type::Unit(UnitTy) => RcDoc::text("Unit"),
                | Type::Int(IntTy) => RcDoc::text("Int"),
                | Type::Char(CharTy) => RcDoc::text("Char"),
                | Type::String(StringTy) => RcDoc::text("String"),
                | Type::OS(OSTy) => RcDoc::text("OS"),
                | Type::VArrow(ty) => ty.pretty(f),
                | Type::VForall(ty) => ty.pretty(f),
                | Type::VPackPi(ty) => ty.pretty(f),
                | Type::Arrow(ty) => ty.pretty(f),
                | Type::Forall(ty) => ty.pretty(f),
                | Type::PackPi(ty) => ty.pretty(f),
                | Type::Prod(ty) => ty.pretty(f),
                | Type::Exists(ty) => ty.pretty(f),
                | Type::ManifestKind(ty) => ty.pretty(f),
                | Type::Data(ty) => ty.pretty(f),
                | Type::CoData(ty) => ty.pretty(f),
            },
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StaticPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | StaticPatId::Kind(pattern) => pattern.pretty(f),
            | StaticPatId::Type(pattern) => pattern.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StaticTermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | StaticTermId::Kind(kind) => kind.pretty(f),
            | StaticTermId::Type(ty) => ty.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let vpat = &f.statics.vpats[self];
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(vpat) => vpat.pretty(f),
            | VPat::Var(vpat) => vpat.pretty(f),
            | VPat::Named(vpat) => vpat.pretty(f),
            | VPat::Ctor(vpat) => vpat.pretty(f),
            | VPat::Triv(vpat) => vpat.pretty(f),
            | VPat::VCons(vpat) => vpat.pretty(f),
            | VPat::SCons(vpat) => vpat.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let value = &f.statics.values[self];
        match value {
            | Value::Hole(value) => value.pretty(f),
            | Value::Var(value) => value.pretty(f),
            | Value::Named(value) => value.pretty(f),
            | Value::Let(value) => value.pretty(f),
            | Value::VAbs(value) => value.pretty(f),
            | Value::VApp(value) => value.pretty(f),
            | Value::TAbs(value) => value.pretty(f),
            | Value::TApp(value) => value.pretty(f),
            | Value::Thunk(value) => value.pretty(f),
            | Value::Ctor(value) => value.pretty(f),
            | Value::Triv(value) => value.pretty(f),
            | Value::VCons(value) => value.pretty(f),
            | Value::SCons(value) => value.pretty(f),
            | Value::Proj(value) => value.pretty(f),
            | Value::Lit(value) => value.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let compu = &f.statics.compus[self];
        use Computation as Compu;
        match compu {
            | Compu::Hole(compu) => compu.pretty(f),
            | Compu::VAbs(compu) => compu.pretty(f),
            | Compu::VApp(compu) => compu.pretty(f),
            | Compu::TAbs(compu) => compu.pretty(f),
            | Compu::TApp(compu) => compu.pretty(f),
            | Compu::Fix(compu) => compu.pretty(f),
            | Compu::Force(compu) => compu.pretty(f),
            | Compu::Ret(compu) => compu.pretty(f),
            | Compu::Do(compu) => compu.pretty(f),
            | Compu::Let(compu) => compu.pretty(f),
            | Compu::Match(compu) => compu.pretty(f),
            | Compu::CoMatch(compu) => compu.pretty(f),
            | Compu::Dtor(compu) => compu.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for AnnId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | AnnId::Set => RcDoc::text("Set"),
            | AnnId::Kind(kd) => kd.pretty(f),
            | AnnId::Type(ty) => ty.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | TermId::Kind(kd) => kd.pretty(f),
            | TermId::Type(ty) => ty.pretty(f),
            | TermId::Value(v) => v.pretty(f),
            | TermId::Compu(c) => c.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for AbstId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let () = &f.statics.absts[self];
        let hint = match f.statics.abst_hints.get(self) {
            | Some(hint) => {
                let hint = &f.scoped.defs[hint];
                hint.ugly(f)
            }
            | None => self.concise(),
        };
        RcDoc::text(hint.to_string())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SealedTypeEquation {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let sealed = self.0;
        let definition = f.statics.seals[&sealed];
        let kind = f.statics.annotations_type[&definition];
        RcDoc::concat([
            sealed.pretty(f),
            RcDoc::text(" :"),
            RcDoc::space(),
            kind.pretty(f),
            RcDoc::concat([
                RcDoc::hardline(),
                RcDoc::text("="),
                RcDoc::space(),
                definition.pretty(f).nest(f.indent),
            ])
            .nest(f.indent),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DataId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let data = &f.statics.datas[self];
        RcDoc::concat([
            RcDoc::text("data"),
            RcDoc::concat(data.iter().map(|(ctor, ty)| {
                RcDoc::concat([
                    RcDoc::hardline(),
                    RcDoc::text("|"),
                    RcDoc::space(),
                    ctor.pretty(f),
                    RcDoc::space(),
                    RcDoc::text(":"),
                    RcDoc::concat([RcDoc::line(), ty.pretty(f)]).group().nest(f.indent),
                ])
            })),
            RcDoc::hardline(),
            RcDoc::text("end"),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoDataId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let codata = &f.statics.codatas[self];
        RcDoc::concat([
            RcDoc::text("codata"),
            RcDoc::concat(codata.iter().map(|(dtor, ty)| {
                RcDoc::concat([
                    RcDoc::hardline(),
                    RcDoc::text("|"),
                    RcDoc::space(),
                    dtor.pretty(f),
                    RcDoc::space(),
                    RcDoc::text(":"),
                    RcDoc::concat([RcDoc::line(), ty.pretty(f)]).group().nest(f.indent),
                ])
            })),
            RcDoc::hardline(),
            RcDoc::text("end"),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarName {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for FieldName {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CtorName {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DtorName {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Hole {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for Abs<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Abs(binder, body) = self;
        RcDoc::concat([
            RcDoc::text("fn"),
            RcDoc::space(),
            binder.pretty(f),
            RcDoc::space(),
            RcDoc::text("=>"),
            RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent),
        ])
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for App<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let App(func, arg) = self;
        RcDoc::concat([
            RcDoc::text("("),
            func.pretty(f),
            RcDoc::concat([RcDoc::line(), arg.pretty(f)]).group().nest(f.indent),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for Arrow<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Arrow(func, arg) = self;
        RcDoc::concat([
            func.pretty(f),
            RcDoc::space(),
            RcDoc::text("->"),
            RcDoc::space(),
            arg.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueArrow {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ValueArrow(input, output) = self;
        RcDoc::concat([
            input.pretty(f),
            RcDoc::space(),
            RcDoc::text("->"),
            RcDoc::space(),
            output.pretty(f),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Named<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Named(name, inner) = self;
        RcDoc::concat([
            RcDoc::text("("),
            name.pretty(f),
            RcDoc::space(),
            RcDoc::text("="),
            RcDoc::space(),
            inner.pretty(f),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Label<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Label(name, inner) = self;
        name.pretty(f).append(RcDoc::text(" :: ")).append(inner.pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Proj<TypeId, FieldName> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Proj(head, name) = self;
        head.pretty(f).append(RcDoc::text("/")).append(name.pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for ConsN<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ConsN(items, tail) = self;
        let items = items.iter().map(|item| item.pretty(f)).chain(std::iter::once(tail.pretty(f)));
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(items, RcDoc::text(", ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for Prod<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Prod(head, tail) = self;
        RcDoc::concat([
            head.pretty(f),
            RcDoc::space(),
            RcDoc::text("*"),
            RcDoc::space(),
            tail.pretty(f),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Sealed<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Sealed(t) = self;
        t.pretty(f)
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Thunk<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Thunk(t) = self;
        RcDoc::concat([
            RcDoc::text("{"),
            RcDoc::space(),
            t.pretty(f),
            RcDoc::space(),
            RcDoc::text("}"),
        ])
        .group()
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Force<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Force(t) = self;
        RcDoc::concat([RcDoc::text("!"), RcDoc::space(), t.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Return<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Return(t) = self;
        RcDoc::concat([RcDoc::text("ret"), RcDoc::space(), t.pretty(f)])
    }
}

impl<'a, Br, Be, Tail> Pretty<'a, Formatter<'a>> for Bind<Br, Be, Tail>
where
    Br: Pretty<'a, Formatter<'a>>,
    Be: Pretty<'a, Formatter<'a>>,
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Bind { binder, bindee, tail } = self;
        RcDoc::concat([
            RcDoc::text("do"),
            RcDoc::space(),
            binder.pretty(f),
            RcDoc::space(),
            RcDoc::text("<-"),
            RcDoc::concat([RcDoc::line(), bindee.pretty(f), RcDoc::line(), RcDoc::text(";")])
                .group()
                .nest(f.indent),
            RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
        ])
    }
}

impl<'a, Br, Be, Tail> Pretty<'a, Formatter<'a>> for Let<Br, Be, Tail>
where
    Br: Pretty<'a, Formatter<'a>>,
    Be: Pretty<'a, Formatter<'a>>,
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Let { binder, bindee, tail } = self;
        RcDoc::concat([
            RcDoc::text("let"),
            RcDoc::space(),
            binder.pretty(f),
            RcDoc::space(),
            RcDoc::text("="),
            RcDoc::concat([
                RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(f.indent),
                RcDoc::line(),
                RcDoc::text("in"),
            ])
            .group(),
            RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
        ])
    }
}

impl<'a, P, Tm> Pretty<'a, Formatter<'a>> for Fix<P, Tm>
where
    P: Pretty<'a, Formatter<'a>>,
    Tm: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Fix(p, tm) = self;
        RcDoc::concat([
            RcDoc::text("fix"),
            RcDoc::space(),
            p.pretty(f),
            RcDoc::space(),
            RcDoc::text("=>"),
            RcDoc::concat([RcDoc::line(), tm.pretty(f)]).nest(f.indent).group(),
        ])
    }
}

impl<'a, Tail> Pretty<'a, Formatter<'a>> for Ctor<CtorName, Tail>
where
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text("("), tail.pretty(f), RcDoc::text(")")])
    }
}

impl<'a, Tail> Pretty<'a, Formatter<'a>> for Dtor<Tail, DtorName>
where
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Dtor(tail, name) = self;
        RcDoc::concat([tail.pretty(f), RcDoc::space(), name.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ResolvedField {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        self.name.pretty(f)
    }
}

impl<'a, Head> Pretty<'a, Formatter<'a>> for Proj<Head, ResolvedField>
where
    Head: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Proj(head, field) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text("/"), field.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Literal {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.ugly(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TypeBinder {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let kind = f.statics.annotations_tpat[&self.pattern];
        RcDoc::concat([
            RcDoc::text("("),
            self.pattern.pretty(f),
            RcDoc::text(" :"),
            RcDoc::space(),
            kind.pretty(f),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Forall {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Forall(abst, ty) = self;
        RcDoc::concat([
            RcDoc::text("forall"),
            RcDoc::space(),
            abst.pretty(f),
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), ty.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueForall {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ValueForall(abst, ty) = self;
        RcDoc::concat([
            RcDoc::text("forall-v"),
            RcDoc::space(),
            abst.pretty(f),
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), ty.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PackPi {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let witnesses = self.witnesses.iter().map(|witness| witness.pretty(f));
        RcDoc::concat([
            RcDoc::text("pack-pi"),
            RcDoc::space(),
            RcDoc::text("(["),
            RcDoc::intersperse(witnesses, RcDoc::text(", ")),
            RcDoc::text("] :"),
            RcDoc::space(),
            self.domain.pretty(f),
            RcDoc::text(")"),
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), self.codomain.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValuePackPi {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let witnesses = self.witnesses.iter().map(|witness| witness.pretty(f));
        RcDoc::concat([
            RcDoc::text("pack-pi-v"),
            RcDoc::space(),
            RcDoc::text("(["),
            RcDoc::intersperse(witnesses, RcDoc::text(", ")),
            RcDoc::text("] :"),
            RcDoc::space(),
            self.domain.pretty(f),
            RcDoc::text(")"),
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), self.codomain.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Exists {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let binder = match self.mode {
            | ExistsMode::Abstract => self.binder.pretty(f),
            | ExistsMode::Manifest(definition) => {
                let kind = f.statics.annotations_tpat[&self.binder.pattern];
                RcDoc::concat([
                    RcDoc::text("("),
                    self.binder.pattern.pretty(f),
                    RcDoc::text(" as"),
                    RcDoc::space(),
                    definition.pretty(f),
                    RcDoc::text(" :"),
                    RcDoc::space(),
                    kind.pretty(f),
                    RcDoc::text(")"),
                ])
            }
        };
        RcDoc::concat([
            RcDoc::text("exists"),
            RcDoc::space(),
            binder,
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), self.body.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ManifestKind {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("exists"),
            RcDoc::space(),
            RcDoc::text("("),
            self.binder.pretty(f),
            RcDoc::text(" as"),
            RcDoc::space(),
            self.definition.pretty(f),
            RcDoc::text(")"),
            RcDoc::space(),
            RcDoc::text("."),
            RcDoc::concat([RcDoc::line(), self.body.pretty(f)]).group().nest(f.indent),
        ])
        .group()
    }
}

impl<'a, Sc, Br, Tail> Pretty<'a, Formatter<'a>> for Match<Sc, Br, Tail>
where
    Sc: Pretty<'a, Formatter<'a>>,
    Br: Pretty<'a, Formatter<'a>>,
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Match { scrut, arms } = self;
        let _ = arms;
        RcDoc::concat([
            RcDoc::text("match"),
            RcDoc::space(),
            scrut.pretty(f),
            RcDoc::concat(arms.iter().map(|Matcher { binder, tail }| {
                RcDoc::concat([
                    RcDoc::line(),
                    RcDoc::text("|"),
                    RcDoc::space(),
                    binder.pretty(f),
                    RcDoc::space(),
                    RcDoc::text("=>"),
                    RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                ])
            })),
            RcDoc::line(),
            RcDoc::text("end"),
        ])
    }
}

impl<'a, Tail> Pretty<'a, Formatter<'a>> for CoMatch<DtorName, Tail>
where
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let CoMatch { arms } = self;
        let _ = f;
        let _ = arms;
        RcDoc::concat([
            RcDoc::text("comatch"),
            RcDoc::concat(arms.iter().map(|CoMatcher { dtor, tail }| {
                RcDoc::concat([
                    RcDoc::line(),
                    RcDoc::text("|"),
                    RcDoc::space(),
                    dtor.pretty(f),
                    RcDoc::space(),
                    RcDoc::text("=>"),
                    RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                ])
            })),
            RcDoc::line(),
            RcDoc::text("end"),
        ])
    }
}
