use super::syntax::*;

pub use zydeco_syntax::{Pretty, Ugly};
/// Formatter for scoped syntax (debug/ugly surface syntax).
pub struct Formatter<'arena> {
    // spans: SpanArenaTextual,
    arena: &'arena ScopedArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena ScopedArena) -> Self {
        Formatter { arena }
    }

    fn view_pattern_head_parts(&self, view: TermId) -> (TermId, Vec<TermId>) {
        let Term::App(App(function, argument)) = &self.arena.terms[&view] else {
            return (view, Vec::new());
        };
        let (head, prefix) = self.view_pattern_head_parts(*function);
        (head, prefix.into_iter().chain([*argument]).collect())
    }

    fn view_pattern_head(&'arena self, view: TermId) -> RcDoc<'arena> {
        let (head, arguments) = self.view_pattern_head_parts(view);
        if arguments.is_empty() {
            head.pretty(self)
        } else {
            RcDoc::concat([
                head.pretty(self),
                RcDoc::text("["),
                RcDoc::intersperse(
                    arguments.into_iter().map(|argument| argument.pretty(self)),
                    RcDoc::text(", "),
                ),
                RcDoc::text("]"),
            ])
        }
    }
}

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.arena.defs[self];
        RcDoc::concat([name.pretty(f), RcDoc::text(self.concise())])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let pat = &f.arena.pats[self];
        match pat {
            | Pattern::Ann(p) => p.pretty(f),
            | Pattern::Hole(p) => p.pretty(f),
            | Pattern::Var(p) => p.pretty(f),
            | Pattern::Named(p) => p.pretty(f),
            | Pattern::Ctor(p) => p.pretty(f),
            | Pattern::Project(p) => p.pretty(f),
            | Pattern::View(p) => p.pretty(f),
            | Pattern::Alias(p) => p.pretty(f),
            | Pattern::Triv(p) => p.pretty(f),
            | Pattern::Cons(p) => p.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Alias<PatId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Alias(patterns) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(patterns.iter().map(|pattern| pattern.pretty(f)), RcDoc::text("; ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProjectionPattern<FieldName, PatId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ProjectionPattern(field, pattern) = self;
        RcDoc::concat([RcDoc::text(format!("/{field} = ")), pattern.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let term = &f.arena.terms[self];
        match term {
            | Term::Meta(t) => t.pretty(f),
            | Term::SourceBoundary(SourceBoundary(t)) => t.pretty(f),
            | Term::SignatureBoundary(SignatureBoundary(t)) => t.pretty(f),
            | Term::Internal(t) => t.pretty(f),
            | Term::Sealed(t) => t.pretty(f),
            | Term::Ann(t) => t.pretty(f),
            | Term::Hole(t) => t.pretty(f),
            | Term::Var(t) => t.pretty(f),
            | Term::Named(t) => t.pretty(f),
            | Term::Label(t) => t.pretty(f),
            | Term::Triv(t) => t.pretty(f),
            | Term::Cons(t) => t.pretty(f),
            | Term::Abs(t) => t.pretty(f),
            | Term::ValAbs(Abs(pattern, body)) => RcDoc::concat([
                RcDoc::text("val "),
                pattern.pretty(f),
                RcDoc::text(" => "),
                body.pretty(f),
            ]),
            | Term::App(t) => t.pretty(f),
            | Term::Fix(t) => t.pretty(f),
            | Term::Pi(t) => t.pretty(f),
            | Term::ValPi(t) => t.pretty(f),
            | Term::Sigma(t) => t.pretty(f),
            | Term::ManifestExists(t) => t.pretty(f),
            | Term::Pack(t) => t.pretty(f),
            | Term::Thunk(t) => t.pretty(f),
            | Term::Force(t) => t.pretty(f),
            | Term::Ret(t) => t.pretty(f),
            | Term::Do(t) => t.pretty(f),
            | Term::Let(t) => t.pretty(f),
            | Term::MobileParam(_) | Term::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
            }
            | Term::Residual(t) => t.pretty(f),
            | Term::Block(t) => t.pretty(f),
            | Term::RecGroup(t) => t.pretty(f),
            | Term::MoBlock(t) => t.pretty(f),
            | Term::Data(t) => t.pretty(f),
            | Term::CoData(t) => t.pretty(f),
            | Term::Ctor(t) => t.pretty(f),
            | Term::Match(t) => t.pretty(f),
            | Term::CoMatchClauses(t) => t.pretty(f),
            | Term::CoMatch(t) => t.pretty(f),
            | Term::Dtor(t) => t.pretty(f),
            | Term::Proj(t) => t.pretty(f),
            | Term::Lit(t) => t.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Internal {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Internal::VType => RcDoc::text("VType"),
            | Internal::CType => RcDoc::text("CType"),
            | Internal::Thk => RcDoc::text("Thk"),
            | Internal::Ret => RcDoc::text("Ret"),
            | Internal::Unit => RcDoc::text("Unit"),
            | Internal::Primitive(primitive) => RcDoc::text(primitive.type_name()),
            | Internal::OS => RcDoc::text("OS"),
            | Internal::Monad => RcDoc::text("Monad"),
            | Internal::Algebra => RcDoc::text("Algebra"),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Meta {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.to_string())
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for MetaT<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let MetaT(meta, t) = self;
        RcDoc::concat([meta.pretty(f), RcDoc::text(" "), t.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Sealed<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Sealed(t) = self;
        RcDoc::concat([RcDoc::text("[sealed] "), t.pretty(f)])
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

impl<'a> Pretty<'a, Formatter<'a>> for Hole {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("_")
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

impl<'a> Pretty<'a, Formatter<'a>> for Ctor<CtorName, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text("("), tail.pretty(f), RcDoc::text(")")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Ctor<CtorName, PatId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text(" "), tail.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("()")
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

impl<'a, T> Pretty<'a, Formatter<'a>> for Named<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Named(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" = "), inner.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Label<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Label(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" :: "), inner.pretty(f)])
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

impl<'a, S, T> Pretty<'a, Formatter<'a>> for App<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let App(a, b) = self;
        RcDoc::concat([
            RcDoc::text("("),
            a.pretty(f),
            RcDoc::text(" "),
            b.pretty(f),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Abs<PatId, TermId> {
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

impl<'a> Pretty<'a, Formatter<'a>> for ValPi {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ValPi(pattern, body) = self;
        RcDoc::concat([
            RcDoc::text("val pi "),
            pattern.pretty(f),
            RcDoc::text(" . "),
            body.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Sigma {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Sigma(p, t) = self;
        RcDoc::concat([RcDoc::text("sigma "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ManifestExists {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ManifestExists { binder, definition, body } = self;
        RcDoc::concat([
            RcDoc::text("exists ("),
            binder.pretty(f),
            RcDoc::text(" as "),
            definition.pretty(f),
            RcDoc::text(") . "),
            body.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Pack {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Pack { mode, binder, definition, body } = self;
        let parameter = match mode {
            | PackMode::Disclosed => RcDoc::concat([
                RcDoc::text("("),
                binder.pretty(f),
                RcDoc::text(" as "),
                definition.pretty(f),
                RcDoc::text(")"),
            ]),
            | PackMode::Sealed => RcDoc::concat([
                RcDoc::text("("),
                binder.pretty(f),
                RcDoc::text(") is "),
                definition.pretty(f),
            ]),
        };
        match &f.arena.terms[body] {
            | Term::Pack(nested) => {
                RcDoc::concat([RcDoc::text("pack "), parameter, RcDoc::text(" "), nested.pretty(f)])
            }
            | Term::Cons(components) => RcDoc::concat([
                RcDoc::text("pack "),
                parameter,
                RcDoc::text(" where "),
                RcDoc::intersperse(components.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(" end"),
            ]),
            | _ => RcDoc::concat([
                RcDoc::text("pack "),
                parameter,
                RcDoc::text(" where "),
                body.pretty(f),
                RcDoc::text(" end"),
            ]),
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

impl<'a> Pretty<'a, Formatter<'a>> for Let<PatId, TermId, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Let { binder, bindee, tail } = self;
        RcDoc::concat([
            RcDoc::text("let "),
            binder.pretty(f),
            RcDoc::text(" = "),
            bindee.pretty(f),
            RcDoc::text(" in "),
            tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ViewPattern {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ViewPattern { function, pattern } = self;
        RcDoc::concat([f.view_pattern_head(*function), RcDoc::text(" ~> "), pattern.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Block {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Block(body) = self;
        RcDoc::concat([RcDoc::text("begin "), body.pretty(f), RcDoc::text(" end")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Residual {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Residual(body) = self;
        body.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for RecGroup {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let RecGroup { definitions, tail } = self;
        let definitions = RcDoc::intersperse(
            definitions.iter().map(|RecursiveDefinition { binder, bindee }| {
                RcDoc::concat([
                    RcDoc::text("def "),
                    binder.pretty(f),
                    RcDoc::text(" = "),
                    bindee.pretty(f),
                ])
            }),
            RcDoc::text("; "),
        );
        RcDoc::concat([RcDoc::text("rec ["), definitions, RcDoc::text("] in "), tail.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for MoBlock {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let MoBlock { body, basis: _ } = self;
        RcDoc::concat([RcDoc::text("(@[monadic] "), body.pretty(f), RcDoc::text(")")])
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
        for CoDataArm { name, out } in arms {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                name.pretty(f),
                RcDoc::text(" : "),
                out.pretty(f),
            ]));
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

impl<'a> Pretty<'a, Formatter<'a>> for CoMatch<DtorName, TermId> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let CoMatch { arms } = self;
        let mut doc = RcDoc::text("comatch");
        for CoMatcher { dtor, tail } in arms {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                dtor.pretty(f),
                RcDoc::text(" => "),
                tail.pretty(f),
            ]));
        }
        doc.append(RcDoc::text(" end"))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoPatternItem {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Pat(pattern) => pattern.pretty(f),
            | Self::Dtor(dtor) => dtor.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoPatternSpine {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|item| item.pretty(f)), RcDoc::text(" "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoMatchClauses {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let mut doc = RcDoc::text("comatch");
        for CoPatternClause { spine, tail } in &self.clauses {
            doc = doc.append(RcDoc::concat([
                RcDoc::text(" | "),
                spine.pretty(f),
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

impl<'a> Pretty<'a, Formatter<'a>> for RecursiveDefinition {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let RecursiveDefinition { binder, bindee } = self;
        RcDoc::concat([
            RcDoc::text("def "),
            binder.pretty(f),
            RcDoc::text(" = "),
            bindee.pretty(f),
            RcDoc::text(" that"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Context
where
    DefId: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let zydeco_utils::context::Context(defs) = self;
        RcDoc::intersperse(defs.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoContext
where
    DefId: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for () {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<>")
    }
}
