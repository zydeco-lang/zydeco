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

    fn def_name(&self, id: &DefId) -> &zydeco_syntax::VarName {
        self.statics.def_name(self.scoped, id)
    }
}

/// A source-facing equation that reveals the representation behind one
/// lexically sealed abstract type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SealedTypeEquation(AbstId);

impl SealedTypeEquation {
    pub fn new(statics: &StaticsArena, sealed: AbstId) -> Option<Self> {
        let definition = statics.seals.get(&sealed)?;
        statics.type_kind_at(*definition)?;
        Some(Self(sealed))
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = f.def_name(self);
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
                | Type::Opaque(OpaqueTy) => RcDoc::text("Opaque"),
                | Type::Primitive(PrimitiveTy(primitive)) => RcDoc::text(primitive.type_name()),
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
            | VPat::Alias(vpat) => vpat.pretty(f),
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
        match f.statics.abst_hints.get(self) {
            | Some(hint) => f.def_name(hint).pretty(f),
            | None => RcDoc::text(self.concise()),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SealedTypeEquation {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let sealed = self.0;
        let definition = f.statics.seals[&sealed];
        let kind = f.statics.type_kind(definition);
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
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        let VarName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for FieldName {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.plain())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CtorName {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        let CtorName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DtorName {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        let DtorName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Hole {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("_")
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

impl<'a> Pretty<'a, Formatter<'a>> for TypeAbstraction {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        Abs(self.binder.pattern, self.body).pretty(f)
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
            RcDoc::text("#"),
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
        RcDoc::text("#").append(name.pretty(f)).append(RcDoc::text(" :: ")).append(inner.pretty(f))
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

impl<'a> Pretty<'a, Formatter<'a>> for Alias<VPatId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Alias(patterns) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(patterns.iter().map(|pattern| pattern.pretty(f)), RcDoc::text("; ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Vec<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(self.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Prod<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Prod(components) = self;
        RcDoc::intersperse(
            components.iter().map(|component| component.pretty(f)),
            RcDoc::concat([RcDoc::space(), RcDoc::text("*"), RcDoc::space()]),
        )
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
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Literal::Integer(i) => RcDoc::text(format!("{i:?}")),
            | Literal::Float(value) => RcDoc::text(format!("{value:?}")),
            // Fixme: escape string
            | Literal::String(str) => RcDoc::text(format!("{str:?}")),
            | Literal::Char(c) => RcDoc::text(format!("{c:?}")),
        }
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
