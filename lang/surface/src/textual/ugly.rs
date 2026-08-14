use super::syntax::*;

pub use zydeco_syntax::{Pretty, Ugly};
/// Formatter for textual syntax using the "ugly" surface form.
pub struct Formatter<'arena> {
    // spans: SpanArenaTextual,
    arena: &'arena TextArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena TextArena) -> Self {
        Formatter { arena }
    }
}

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.arena.defs[self];
        name.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let pat = &f.arena.pats[self];
        match pat {
            | Pattern::Ann(p) => p.pretty(f),
            | Pattern::Manifest(p) => p.pretty(f),
            | Pattern::Hole(p) => p.pretty(f),
            | Pattern::Var(p) => p.pretty(f),
            | Pattern::Named(p) => p.pretty(f),
            | Pattern::Ctor(p) => p.pretty(f),
            | Pattern::Project(p) => p.pretty(f),
            | Pattern::Alias(p) => p.pretty(f),
            | Pattern::Paren(p) => p.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let copat = &f.arena.copats[self];
        match copat {
            | CoPattern::Pat(c) => c.pretty(f),
            | CoPattern::Dtor(c) => c.pretty(f),
            | CoPattern::App(c) => c.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let term = &f.arena.terms[self];
        match term {
            | Term::Meta(MetaT(meta, payload)) => {
                if matches!(&f.arena.terms[payload], Term::Hole(_)) {
                    RcDoc::concat([RcDoc::text("@("), meta.pretty(f), RcDoc::text(")")])
                } else {
                    RcDoc::concat([
                        RcDoc::text("@["),
                        meta.pretty(f),
                        RcDoc::text("] "),
                        payload.pretty(f),
                    ])
                }
            }
            | Term::SourceBoundary(SourceBoundary(t)) => t.pretty(f),
            | Term::SignatureBoundary(SignatureBoundary(t)) => t.pretty(f),
            | Term::Ann(t) => t.pretty(f),
            | Term::Hole(t) => t.pretty(f),
            | Term::Var(t) => t.pretty(f),
            | Term::Named(t) => t.pretty(f),
            | Term::Label(t) => t.pretty(f),
            | Term::Paren(t) => t.pretty(f),
            | Term::Abs(t) => t.pretty(f),
            | Term::App(t) => t.pretty(f),
            | Term::Fix(t) => t.pretty(f),
            | Term::Pi(t) => t.pretty(f),
            | Term::Arrow(t) => t.pretty(f),
            | Term::Forall(t) => t.pretty(f),
            | Term::Sigma(t) => t.pretty(f),
            | Term::Prod(t) => t.pretty(f),
            | Term::Exists(t) => t.pretty(f),
            | Term::Thunk(t) => t.pretty(f),
            | Term::Force(t) => t.pretty(f),
            | Term::Ret(t) => t.pretty(f),
            | Term::Do(t) => t.pretty(f),
            | Term::Let(t) => t.pretty(f),
            | Term::Param(t) => t.pretty(f),
            | Term::ContextBind(t) => t.pretty(f),
            | Term::Block(t) => t.pretty(f),
            | Term::Data(t) => t.pretty(f),
            | Term::CoData(t) => t.pretty(f),
            | Term::Ctor(t) => t.pretty(f),
            | Term::Match(t) => t.pretty(f),
            | Term::CoMatch(t) => t.pretty(f),
            | Term::Dtor(t) => t.pretty(f),
            | Term::Proj(t) => t.pretty(f),
            | Term::Lit(t) => t.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Meta {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.to_string())
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for Ann<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ann { tm, ty } = self;
        RcDoc::concat([
            RcDoc::text("("),
            tm.pretty(f),
            RcDoc::text(" : "),
            ty.pretty(f),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ManifestPattern {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ManifestPattern { binder, definition } = self;
        RcDoc::concat([
            RcDoc::text("("),
            binder.pretty(f),
            RcDoc::text(" as "),
            definition.pretty(f),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Hole {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("_")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for FieldName {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.plain())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarName {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        let VarName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Named<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Named(name, inner) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text(" = "), inner.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Label<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Label(name, inner) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text(" :: "), inner.pretty(f)])
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

impl<'a, T> Pretty<'a, Formatter<'a>> for Ctor<CtorName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text("("), tail.pretty(f), RcDoc::text(")")])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Paren<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Paren(ts) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(ts.iter().map(|t| t.pretty(f)), RcDoc::text(",")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Alias<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Alias(patterns) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(patterns.iter().map(|pattern| pattern.pretty(f)), RcDoc::text("; ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for ProjectionPattern<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ProjectionPattern(field, pattern) = self;
        RcDoc::concat([RcDoc::text("/"), field.pretty(f), RcDoc::text(" = "), pattern.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Dtor<T, DtorName>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Dtor(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text(" "), name.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Proj<T, FieldName>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Proj(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text("/"), name.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Appli<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Appli(ts) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(ts.iter().map(|t| t.pretty(f)), RcDoc::text(" ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Abs<CoPatId, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Abs(p, t) = self;
        RcDoc::concat([RcDoc::text("fn "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Fix<PatId, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Fix(p, t) = self;
        RcDoc::concat([RcDoc::text("fix "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Pi {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Pi(p, t) = self;
        RcDoc::concat([RcDoc::text("pi "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for ArrowU<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Arrow(t1, t2) = self;
        RcDoc::concat([t1.pretty(f), RcDoc::text(" -> "), t2.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Forall {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Forall(p, t) = self;
        RcDoc::concat([RcDoc::text("forall "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Sigma {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Sigma(p, t) = self;
        RcDoc::concat([RcDoc::text("sigma "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for ProdU<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Prod(t1, r2) = self;
        RcDoc::concat([t1.pretty(f), RcDoc::text(" * "), r2.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Exists {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Exists { parameters, body } = self;
        let parameters = RcDoc::intersperse(
            parameters.iter().map(|parameter| parameter.pretty(f)),
            RcDoc::text(" "),
        );
        RcDoc::concat([RcDoc::text("exists "), parameters, RcDoc::text(" . "), body.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ExistentialParameter {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let annotations = RcDoc::intersperse(
            self.annotations.iter().map(|annotation| {
                RcDoc::concat([RcDoc::text("@["), annotation.inner.pretty(f), RcDoc::text("]")])
            }),
            RcDoc::text(" "),
        );
        let parameter = match self.binder {
            | binder
                if matches!(
                    f.arena.pats[&binder],
                    Pattern::Ann(_) | Pattern::Manifest(_) | Pattern::Paren(_)
                ) =>
            {
                binder.pretty(f)
            }
            | binder => RcDoc::concat([RcDoc::text("("), binder.pretty(f), RcDoc::text(")")]),
        };
        if self.annotations.is_empty() {
            parameter
        } else {
            RcDoc::concat([annotations, RcDoc::text(" "), parameter])
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Thunk<TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Thunk(t) = self;
        RcDoc::concat([RcDoc::text("{ "), t.pretty(f), RcDoc::text(" }")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Force<TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Force(t) = self;
        RcDoc::concat([RcDoc::text("! "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Return<TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Return(t) = self;
        RcDoc::concat([RcDoc::text("ret "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Bind<PatId, TermId, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Bind { binder, bindee, tail } = self;
        RcDoc::concat([
            RcDoc::text("do "),
            binder.pretty(f),
            RcDoc::text(" <- "),
            bindee.pretty(f),
            RcDoc::text("; "),
            tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for GenLet {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let GenLet { binding, tail } = self;
        RcDoc::concat([RcDoc::text("let "), binding.pretty(f), RcDoc::text(" in "), tail.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Placement {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Placement::In => RcDoc::text("in"),
            | Placement::That => RcDoc::text("that"),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Param {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Param { binder, placement, tail } = self;
        RcDoc::concat([
            RcDoc::text("param "),
            binder.pretty(f),
            RcDoc::text(" "),
            placement.pretty(f),
            RcDoc::text(" "),
            tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ContextBind {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ContextBind { mode, binding, placement, tail } = self;
        let keyword = match mode {
            | DefinitionMode::Transparent => "let",
            | DefinitionMode::Nominal => "def",
        };
        RcDoc::concat([
            RcDoc::text(keyword),
            RcDoc::text(" "),
            binding.pretty(f),
            RcDoc::text(" "),
            placement.pretty(f),
            RcDoc::text(" "),
            tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Block {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Block(body) = self;
        RcDoc::concat([RcDoc::text("begin "), body.pretty(f), RcDoc::text(" end")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for GenBind<TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let GenBind { fix, comp, binder, params, ty, bindee } = self;
        let mut doc = RcDoc::nil();
        if *fix {
            doc = doc.append(RcDoc::text("fix "));
        }
        if *comp {
            doc = doc.append(RcDoc::text("! "));
        }
        doc = doc.append(binder.pretty(f));
        if let Some(params) = params {
            doc = doc.append(RcDoc::text(" ")).append(params.pretty(f));
        }
        if let Some(ty) = ty {
            doc = doc.append(RcDoc::text(" : ")).append(ty.pretty(f));
        }
        doc.append(RcDoc::text(" = ")).append(bindee.pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for GenBind<Option<TermId>> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let GenBind { fix, comp, binder, params, ty, bindee } = self;
        let mut doc = RcDoc::nil();
        if *fix {
            doc = doc.append(RcDoc::text("fix "));
        }
        if *comp {
            doc = doc.append(RcDoc::text("! "));
        }
        doc = doc.append(binder.pretty(f));
        if let Some(params) = params {
            doc = doc.append(RcDoc::text(" ")).append(params.pretty(f));
        }
        if let Some(ty) = ty {
            doc = doc.append(RcDoc::text(" : ")).append(ty.pretty(f));
        }
        if let Some(bindee) = bindee {
            doc = doc.append(RcDoc::text(" = ")).append(bindee.pretty(f));
        }
        doc
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Data {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Data { arms } = self;
        let mut doc = RcDoc::text("data");
        for DataArm { name, param } in arms {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                name.pretty(f),
                RcDoc::text(" "),
                param.pretty(f),
            ]));
        }
        doc.append(RcDoc::text(" end"))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoData {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let CoData { arms } = self;
        let mut doc = RcDoc::text("codata");
        for CoDataArm { name, params, out } in arms {
            doc = doc.append(RcDoc::concat([RcDoc::text(" | "), name.pretty(f)]));
            if let Some(params) = params {
                doc = doc.append(RcDoc::text(" ")).append(params.pretty(f));
            }
            doc = doc.append(RcDoc::text(" : ")).append(out.pretty(f));
        }
        doc.append(RcDoc::text(" end"))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Match<TermId, PatId, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Match { scrut, arms } = self;
        let mut doc = RcDoc::concat([RcDoc::text("match "), scrut.pretty(f)]);
        for Matcher { binder, tail } in arms {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                binder.pretty(f),
                RcDoc::text(" => "),
                tail.pretty(f),
            ]));
        }
        doc.append(RcDoc::text(" end"))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoMatchParam {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let CoMatchParam { arms } = self;
        let mut doc = RcDoc::text("comatch");
        for CoMatcherParam { params, tail } in arms {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                params.pretty(f),
                RcDoc::text(" => "),
                tail.pretty(f),
            ]));
        }
        doc.append(RcDoc::text(" end"))
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
