//! Single-line debug formatter for concrete scoped surface syntax.

use super::syntax::*;
use pretty::RcDoc;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
pub use zydeco_syntax::{Pretty, Ugly, ViewSpine};

enum DefDisplayMode {
    Raw,
    Plain,
    AlphaNormalized(RefCell<FxHashMap<DefId, usize>>),
}

/// Debug formatter over resolved surface syntax.
pub struct Formatter<'arena> {
    pub(crate) arena: &'arena ScopedArena,
    def_mode: DefDisplayMode,
}

impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena ScopedArena) -> Self {
        Self { arena, def_mode: DefDisplayMode::Raw }
    }

    pub fn plain_names(arena: &'arena ScopedArena) -> Self {
        Self { arena, def_mode: DefDisplayMode::Plain }
    }

    pub fn alpha_normalized(arena: &'arena ScopedArena) -> Self {
        Self {
            arena,
            def_mode: DefDisplayMode::AlphaNormalized(RefCell::new(FxHashMap::default())),
        }
    }

    fn view_pattern_head(&'arena self, view: TermId) -> RcDoc<'arena> {
        let (head, arguments) = ViewSpine::parts(view, &|view| {
            let Term::App(App(function, arguments)) = &self.arena.terms[&view] else {
                return None;
            };
            Some((*function, arguments.clone()))
        });
        ViewSpine::bracketed(head, arguments, self)
    }

    fn pack_tail(&'arena self, body: TermId) -> RcDoc<'arena> {
        match &self.arena.terms[&body] {
            | Term::Pack(nested) => RcDoc::concat([RcDoc::text(" "), nested.pretty(self)]),
            | Term::Cons(components) => RcDoc::concat([
                RcDoc::text(" where "),
                RcDoc::intersperse(
                    components.iter().map(|item| item.pretty(self)),
                    RcDoc::text(", "),
                ),
                RcDoc::text(" end"),
            ]),
            | _ => RcDoc::concat([RcDoc::text(" where "), body.pretty(self), RcDoc::text(" end")]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let name = &f.arena.defs[self];
        match &f.def_mode {
            | DefDisplayMode::Raw => RcDoc::concat([name.pretty(f), RcDoc::text(self.concise())]),
            | DefDisplayMode::Plain => name.pretty(f),
            | DefDisplayMode::AlphaNormalized(seen) => {
                let mut seen = seen.borrow_mut();
                let next = seen.len();
                let index = *seen.entry(*self).or_insert(next);
                RcDoc::concat([name.pretty(f), RcDoc::text(format!("#{index}"))])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PatId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match &f.arena.pats[self] {
            | Pattern::Ann(p) => p.pretty(f),
            | Pattern::Hole(p) => p.pretty(f),
            | Pattern::Var(p) => p.pretty(f),
            | Pattern::Named(p) => p.pretty(f),
            | Pattern::Ctor(p) => p.pretty(f),
            | Pattern::Lit(p) => p.pretty(f),
            | Pattern::Project(p) => p.pretty(f),
            | Pattern::View(p) => p.pretty(f),
            | Pattern::Alias(p) => p.pretty(f),
            | Pattern::Triv(p) => p.pretty(f),
            | Pattern::Cons(p) => p.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Alias<PatId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Alias(patterns) = self;
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(patterns.iter().map(|pattern| pattern.pretty(f)), RcDoc::text("; ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProjectionPattern<FieldName, PatId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let ProjectionPattern(field, pattern) = self;
        RcDoc::concat([RcDoc::text(format!("/{field} = ")), pattern.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match &f.arena.terms[self] {
            | Term::Meta(t) => t.pretty(f),
            | Term::TypeOf(TypeOf(operand)) => {
                RcDoc::concat([RcDoc::text("(@[typeof] "), operand.pretty(f), RcDoc::text(")")])
            }
            | Term::SourceBoundary(SourceBoundary(t)) => t.pretty(f),
            | Term::SignatureBoundary(SignatureBoundary(t)) => t.pretty(f),
            | Term::Internal(t) => t.pretty(f),
            | Term::Sealed(t) => t.pretty(f),
            | Term::Ann(t) => t.pretty(f),
            | Term::Seal(t) => t.pretty(f),
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
            | Term::App(App(head, arguments)) => {
                let arguments =
                    arguments.iter().map(|arg| RcDoc::concat([RcDoc::text(" "), arg.pretty(f)]));
                RcDoc::concat([
                    RcDoc::text("("),
                    head.pretty(f),
                    RcDoc::concat(arguments),
                    RcDoc::text(")"),
                ])
            }
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
            | Term::Residual(t) => t.pretty(f),
            | Term::Block(t) => t.pretty(f),
            | Term::RecGroup(t) => t.pretty(f),
            | Term::MoBlock(t) => t.pretty(f),
            | Term::CbvBlock(t) => t.pretty(f),
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
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | Internal::VType => RcDoc::text("VType"),
            | Internal::CType => RcDoc::text("CType"),
            | Internal::Thk => RcDoc::text("Thk"),
            | Internal::Ret => RcDoc::text("Ret"),
            | Internal::Unit => RcDoc::text("Unit"),
            | Internal::Primitive(primitive) => RcDoc::text(primitive.type_name()),
            | Internal::ValueInt(operation) => RcDoc::text(operation.intrinsic_name()),
            | Internal::Operation(operation) => RcDoc::text(operation.source_name()),
            | Internal::OS => RcDoc::text("OS"),
            | Internal::Monad => RcDoc::text("Monad"),
            | Internal::Algebra => RcDoc::text("Algebra"),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ResolvedMeta {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | ResolvedMeta::Builtin(role) => {
                RcDoc::text(format!("@[builtin({})]", role.source_name()))
            }
            | ResolvedMeta::Ffi(target) => RcDoc::text(format!(
                "@[ffi({}, library({:?}), symbol({:?}))]",
                target.abi.source_name(),
                target.library.as_str(),
                target.symbol.as_str(),
            )),
            | ResolvedMeta::Debug(meta) | ResolvedMeta::Opaque(meta) => {
                RcDoc::text(format!("@[{meta}]"))
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for MetaTerm {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::concat([self.meta.pretty(f), RcDoc::text(" "), self.body.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Sealed<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Sealed(t) = self;
        RcDoc::concat([RcDoc::text("[sealed] "), t.pretty(f)])
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for Ann<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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

impl<'a> Pretty<'a, Formatter<'a>> for Seal {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Seal { tm, ty } = self;
        RcDoc::concat([RcDoc::text("seal "), tm.pretty(f), RcDoc::text(" : "), ty.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Hole {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text("_")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarName {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text(self.as_str())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for FieldName {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text(self.plain_str())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CtorName {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text(self.as_str())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DtorName {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text(self.as_str())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Ctor<CtorName, TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text("("), tail.pretty(f), RcDoc::text(")")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Ctor<CtorName, PatId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Ctor(name, tail) = self;
        RcDoc::concat([name.pretty(f), RcDoc::text(" "), tail.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Vec<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Named(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" = "), inner.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Label<FieldName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Label(name, inner) = self;
        RcDoc::concat([RcDoc::text("#"), name.pretty(f), RcDoc::text(" :: "), inner.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Dtor<T, DtorName>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Dtor(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text(" "), name.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Proj<T, FieldName>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Proj(head, name) = self;
        RcDoc::concat([head.pretty(f), RcDoc::text("/"), name.pretty(f)])
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for App<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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

impl<'a> Pretty<'a, Formatter<'a>> for Abs<Vec<PatId>, TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Abs(binders, t) = self;
        let binders = RcDoc::intersperse(binders.iter().map(|p| p.pretty(f)), RcDoc::text(" "));
        RcDoc::concat([RcDoc::text("fn "), binders, RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Abs<PatId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Abs(p, t) = self;
        RcDoc::concat([RcDoc::text("fn "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Fix<PatId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Fix(p, t) = self;
        RcDoc::concat([RcDoc::text("fix "), p.pretty(f), RcDoc::text(" => "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Pi {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Pi(binders, t) = self;
        let binders = RcDoc::intersperse(binders.iter().map(|p| p.pretty(f)), RcDoc::text(" "));
        RcDoc::concat([RcDoc::text("pi "), binders, RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValPi {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Sigma(p, t) = self;
        RcDoc::concat([RcDoc::text("sigma "), p.pretty(f), RcDoc::text(" . "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ManifestExists {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
        RcDoc::concat([RcDoc::text("pack "), parameter, f.pack_tail(*body)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Thunk<TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Thunk(t) = self;
        RcDoc::concat([RcDoc::text("{ "), t.pretty(f), RcDoc::text(" }")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Force<TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Force(t) = self;
        RcDoc::concat([RcDoc::text("! "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Return<TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Return(t) = self;
        RcDoc::concat([RcDoc::text("ret "), t.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Bind<PatId, TermId, TermId> {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let ViewPattern { function, pattern } = self;
        RcDoc::concat([f.view_pattern_head(*function), RcDoc::text(" ~> "), pattern.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Residual {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Residual(body) = self;
        body.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Block {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let Block(body) = self;
        RcDoc::concat([RcDoc::text("begin "), body.pretty(f), RcDoc::text(" end")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for RecGroup {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let MoBlock { body, basis: _ } = self;
        RcDoc::concat([RcDoc::text("(@[monadic] "), body.pretty(f), RcDoc::text(")")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CbvBlock {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let CbvBlock { body } = self;
        RcDoc::concat([RcDoc::text("(@[cbv] "), body.pretty(f), RcDoc::text(")")])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Data {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | Self::Pat(pattern) => pattern.pretty(f),
            | Self::Dtor(dtor) => dtor.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoPatternSpine {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|item| item.pretty(f)), RcDoc::text(" "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoMatchClauses {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | Literal::Integer(i) => RcDoc::text(format!("{i:?}")),
            | Literal::Float(value) => RcDoc::text(format!("{value:?}")),
            | Literal::String(str) => RcDoc::text(format!("{str:?}")),
            | Literal::Char(c) => RcDoc::text(format!("{c:?}")),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for RecursiveDefinition {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
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

impl<'a> Pretty<'a, Formatter<'a>> for Context {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let zydeco_foundation::context::Context(defs) = self;
        RcDoc::intersperse(defs.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoContext {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for () {
    fn pretty(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text("<>")
    }
}
