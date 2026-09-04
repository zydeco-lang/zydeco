//! The shared single-line debug formatter for bitter and scoped syntax.
//!
//! Bitter and scoped terms are the same node types stored in different arenas; the arenas
//! differ only in what a `Var` node carries (source names before resolution, resolved ids
//! after) and in the rendering of a few phase-residual nodes. This module maintains the
//! [`Pretty`] impls once, generic over a [`DebugArena`] adapter that each phase supplies.

use crate::bitter::syntax::*;
use pretty::RcDoc;
use zydeco_syntax::{Pretty, ViewSpine};

/// Debug formatter over one phase's arena.
pub struct Formatter<'arena, P> {
    pub(crate) arena: &'arena P,
}

impl<'arena, P: DebugArena> Formatter<'arena, P> {
    pub fn new(arena: &'arena P) -> Self {
        Formatter { arena }
    }

    fn view_pattern_head(&'arena self, view: TermId) -> RcDoc<'arena> {
        let (head, arguments) = ViewSpine::parts(view, &|view| {
            let Term::App(App(function, argument)) = self.arena.lookup_term(view) else {
                return None;
            };
            Some((*function, vec![*argument]))
        });
        ViewSpine::bracketed(head, arguments, self)
    }
}

/// The arena adapter of one debug-formatted phase.
///
/// The arena exposes node storage for lookups, names the reference stored in `Var` nodes,
/// and adapts the rendering of nodes whose debug form differs between phases.
pub trait DebugArena: Sized {
    /// What a `Var` node carries: source names before resolution, resolved ids after.
    type Ref: for<'x> Pretty<'x, Formatter<'x, Self>>;

    /// Render the definition bound by a pattern variable.
    fn pattern_var<'a>(&self, f: &'a Formatter<'a, Self>, def: DefId) -> RcDoc<'a>;

    /// The pattern stored at one pattern id.
    fn lookup_pattern(&self, pattern: PatId) -> &Pattern;

    /// The term stored at one term id.
    fn lookup_term(&self, term: TermId) -> &Term<Self::Ref>;

    /// Separator between a metadata annotation and the node it annotates.
    fn meta_separator<'a>(&self, f: &'a Formatter<'a, Self>) -> RcDoc<'a>;

    /// The tail of a `pack` layer after its parameter.
    fn pack_tail<'a>(&self, f: &'a Formatter<'a, Self>, body: TermId) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text(" where "), body.pretty(f), RcDoc::text(" end")])
    }

    /// A mobile parameter binder, present only before name resolution.
    fn mobile_param<'a>(&self, _f: &'a Formatter<'a, Self>, _node: &MobileParam) -> RcDoc<'a> {
        unreachable!("mobile syntax must be eliminated during name resolution")
    }

    /// A mobile let binder, present only before name resolution.
    fn mobile_bind<'a>(&self, _f: &'a Formatter<'a, Self>, _node: &MobileBind) -> RcDoc<'a> {
        unreachable!("mobile syntax must be eliminated during name resolution")
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for PatId {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let pat = f.arena.lookup_pattern(*self);
        match pat {
            | Pattern::Ann(p) => p.pretty(f),
            | Pattern::Hole(p) => p.pretty(f),
            | Pattern::Var(p) => f.arena.pattern_var(f, *p),
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Alias<PatId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Alias(patterns) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(patterns.iter().map(|pattern| pattern.pretty(f)), RcDoc::text("; ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for ProjectionPattern<FieldName, PatId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let ProjectionPattern(field, pattern) = self;
        RcDoc::concat([RcDoc::text(format!("/{field} = ")), pattern.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for TermId {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let term = f.arena.lookup_term(*self);
        match term {
            | Term::Meta(t) => t.pretty(f),
            | Term::TypeOf(TypeOf(operand)) => {
                RcDoc::concat([RcDoc::text("(@[typeof] "), operand.pretty(f), RcDoc::text(")")])
            }
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
            | Term::MobileParam(t) => f.arena.mobile_param(f, t),
            | Term::MobileBind(t) => f.arena.mobile_bind(f, t),
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Internal {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Meta {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::text(self.to_string())
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for MetaT<T>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let MetaT(meta, inner) = self;
        RcDoc::concat([meta.pretty(f), f.arena.meta_separator(f), inner.pretty(f)])
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Sealed<T>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Sealed(t) = self;
        RcDoc::concat([RcDoc::text("[sealed] "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena, S, T> Pretty<'a, Formatter<'a, P>> for Ann<S, T>
where
    S: Pretty<'a, Formatter<'a, P>>,
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Hole {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::text("_")
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for VarName {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let VarName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for FieldName {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::text(self.plain())
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CtorName {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let CtorName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for DtorName {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let DtorName(name) = self;
        RcDoc::text(name.clone())
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Ctor<CtorName, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text("("), tail.pretty(f), RcDoc::text(")")])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Ctor<CtorName, PatId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text(" "), tail.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Triv {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Vec<T>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(self.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Named<FieldName, T>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Named(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" = "), inner.pretty(f)])
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Label<FieldName, T>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Label(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" :: "), inner.pretty(f)])
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Dtor<T, DtorName>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Dtor(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text(" "), name.pretty(f)])
    }
}

impl<'a, P: DebugArena, T> Pretty<'a, Formatter<'a, P>> for Proj<T, FieldName>
where
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Proj(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text("/"), name.pretty(f)])
    }
}

impl<'a, P: DebugArena, S, T> Pretty<'a, Formatter<'a, P>> for App<S, T>
where
    S: Pretty<'a, Formatter<'a, P>>,
    T: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Abs<PatId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Abs(p, t) = self;
        RcDoc::concat([RcDoc::text("fn "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Fix<PatId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Fix(p, t) = self;
        RcDoc::concat([RcDoc::text("fix "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Pi {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Pi(p, t) = self;
        RcDoc::concat([RcDoc::text("pi "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for ValPi {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let ValPi(pattern, body) = self;
        RcDoc::concat([
            RcDoc::text("val pi "),
            pattern.pretty(f),
            RcDoc::text(" . "),
            body.pretty(f),
        ])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Sigma {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Sigma(p, t) = self;
        RcDoc::concat([RcDoc::text("sigma "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for ManifestExists {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Pack {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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
        RcDoc::concat([RcDoc::text("pack "), parameter, f.arena.pack_tail(f, *body)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Thunk<TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Thunk(t) = self;
        RcDoc::concat([RcDoc::text("{ "), t.pretty(f), RcDoc::text(" }")])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Force<TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Force(t) = self;
        RcDoc::concat([RcDoc::text("! "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Return<TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Return(t) = self;
        RcDoc::concat([RcDoc::text("ret "), t.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Bind<PatId, TermId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Let<PatId, TermId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for ViewPattern {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let ViewPattern { function, pattern } = self;
        RcDoc::concat([f.view_pattern_head(*function), RcDoc::text(" ~> "), pattern.pretty(f)])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Residual {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Residual(body) = self;
        body.pretty(f)
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Block {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let Block(body) = self;
        RcDoc::concat([RcDoc::text("begin "), body.pretty(f), RcDoc::text(" end")])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for RecGroup {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for MoBlock {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let MoBlock { body, basis: _ } = self;
        RcDoc::concat([RcDoc::text("(@[monadic] "), body.pretty(f), RcDoc::text(")")])
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Data {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CoData {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Match<TermId, PatId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CoMatch<DtorName, TermId> {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CoPatternItem {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | Self::Pat(pattern) => pattern.pretty(f),
            | Self::Dtor(dtor) => dtor.pretty(f),
        }
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CoPatternSpine {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|item| item.pretty(f)), RcDoc::text(" "))
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CoMatchClauses {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for Literal {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | Literal::Integer(i) => RcDoc::text(format!("{i:?}")),
            | Literal::Float(value) => RcDoc::text(format!("{value:?}")),
            // Fixme: escape string
            | Literal::String(str) => RcDoc::text(format!("{str:?}")),
            | Literal::Char(c) => RcDoc::text(format!("{c:?}")),
        }
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for RecursiveDefinition {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
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
