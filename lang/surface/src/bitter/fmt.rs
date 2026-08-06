use super::syntax::*;

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    arena: &'arena BitterArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena BitterArena) -> Self {
        Formatter { arena }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let name = &f.arena.defs[self];
        name.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for PatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let pat = &f.arena.pats[self];
        match pat {
            | Pattern::Ann(p) => s += &p.ugly(f),
            | Pattern::Hole(p) => s += &p.ugly(f),
            | Pattern::Var(p) => s += &p.ugly(f),
            | Pattern::Named(p) => s += &p.ugly(f),
            | Pattern::Ctor(p) => s += &p.ugly(f),
            | Pattern::Project(p) => s += &p.ugly(f),
            | Pattern::Alias(p) => s += &p.ugly(f),
            | Pattern::Triv(p) => s += &p.ugly(f),
            | Pattern::Cons(p) => s += &p.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Alias<PatId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Alias(patterns) = self;
        format!(
            "({})",
            patterns.iter().map(|pattern| pattern.ugly(f)).collect::<Vec<_>>().join("; ")
        )
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ProjectionPattern<FieldName, PatId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ProjectionPattern(field, pattern) = self;
        format!("/{field} = {}", pattern.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let term = &f.arena.terms[self];
        match term {
            | Term::Meta(t) => s += &t.ugly(f),
            | Term::SourceBoundary(SourceBoundary(t)) => s += &t.ugly(f),
            | Term::Internal(t) => s += &t.ugly(f),
            | Term::Sealed(t) => s += &t.ugly(f),
            | Term::Ann(t) => s += &t.ugly(f),
            | Term::Hole(t) => s += &t.ugly(f),
            | Term::Var(t) => s += &t.ugly(f),
            | Term::Named(t) => s += &t.ugly(f),
            | Term::Label(t) => s += &t.ugly(f),
            | Term::Triv(t) => s += &t.ugly(f),
            | Term::Cons(t) => s += &t.ugly(f),
            | Term::Abs(t) => s += &t.ugly(f),
            | Term::App(t) => s += &t.ugly(f),
            | Term::Fix(t) => s += &t.ugly(f),
            | Term::Pi(t) => s += &t.ugly(f),
            | Term::Sigma(t) => s += &t.ugly(f),
            | Term::ManifestExists(t) => s += &t.ugly(f),
            | Term::Thunk(t) => s += &t.ugly(f),
            | Term::Force(t) => s += &t.ugly(f),
            | Term::Ret(t) => s += &t.ugly(f),
            | Term::Do(t) => s += &t.ugly(f),
            | Term::Let(t) => s += &t.ugly(f),
            | Term::MobileParam(t) => s += &t.ugly(f),
            | Term::MobileBind(t) => s += &t.ugly(f),
            | Term::Residual(t) => s += &t.ugly(f),
            | Term::Block(t) => s += &t.ugly(f),
            | Term::RecGroup(t) => s += &t.ugly(f),
            | Term::MoBlock(t) => s += &t.ugly(f),
            | Term::Data(t) => s += &t.ugly(f),
            | Term::CoData(t) => s += &t.ugly(f),
            | Term::Ctor(t) => s += &t.ugly(f),
            | Term::Match(t) => s += &t.ugly(f),
            | Term::CoMatchClauses(t) => s += &t.ugly(f),
            | Term::CoMatch(t) => s += &t.ugly(f),
            | Term::Dtor(t) => s += &t.ugly(f),
            | Term::Proj(t) => s += &t.ugly(f),
            | Term::Lit(t) => s += &t.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Internal {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let mut s = String::new();
        match self {
            | Internal::VType => s += "VType",
            | Internal::CType => s += "CType",
            | Internal::Thk => s += "Thk",
            | Internal::Ret => s += "Ret",
            | Internal::Unit => s += "Unit",
            | Internal::Int => s += "Int",
            | Internal::Char => s += "Char",
            | Internal::String => s += "String",
            | Internal::OS => s += "OS",
            | Internal::Monad => s += "Monad",
            | Internal::Algebra => s += "Algebra",
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Meta {
    fn ugly(&self, _f: &'a Formatter) -> String {
        self.to_string()
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for MetaT<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let MetaT(meta, decl) = self;
        s += &meta.ugly(f);
        s += &decl.ugly(f);
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Sealed<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Sealed(t) = self;
        s += "[sealed] ";
        s += &t.ugly(f);
        s
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Ann<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Ann { tm, ty } = self;
        s += "(";
        s += &tm.ugly(f);
        s += " : ";
        s += &ty.ugly(f);
        s += ")";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Hole {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "_".to_string()
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

impl<'a> Ugly<'a, Formatter<'a>> for Ctor<CtorName, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Ctor(name, tail) = self;
        s += &name.ugly(f);
        s += "(";
        s += &tail.ugly(f);
        s += ")";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Ctor<CtorName, PatId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Ctor(name, tail) = self;
        s += &name.ugly(f);
        s += " ";
        s += &tail.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Triv {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "()".to_string()
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for ConsN<T, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("({})", self.iter().map(|item| item.ugly(f)).collect::<Vec<_>>().join(", "))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Named<FieldName, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Named(name, inner) = self;
        format!("{} = {}", name.ugly(f), inner.ugly(f))
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

impl<'a, T> Ugly<'a, Formatter<'a>> for Dtor<T, DtorName>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Dtor(head, name) = self;
        s += &head.ugly(f);
        s += " ";
        s += &name.ugly(f);
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Proj<T, FieldName>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Proj(head, name) = self;
        format!("{}/{}", head.ugly(f), name.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for App<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let App(a, b) = self;
        s += "(";
        s += &a.ugly(f);
        s += " ";
        s += &b.ugly(f);
        s += ")";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Abs<PatId, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Abs(p, t) = self;
        s += "fn ";
        s += &p.ugly(f);
        s += " => ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Fix<PatId, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Fix(p, t) = self;
        s += "fix ";
        s += &p.ugly(f);
        s += " => ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Pi {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Pi(p, t) = self;
        s += "pi ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Sigma {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Sigma(p, t) = self;
        s += "sigma ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ManifestExists {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ManifestExists { binder, definition, body } = self;
        format!("exists ({} as {}) . {}", binder.ugly(f), definition.ugly(f), body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Thunk<TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Thunk(t) = self;
        s += "{ ";
        s += &t.ugly(f);
        s += " }";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Force<TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Force(t) = self;
        s += "! ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Return<TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Return(t) = self;
        s += "ret ";
        s += &t.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Bind<PatId, TermId, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Bind { binder, bindee, tail } = self;
        s += "do ";
        s += &binder.ugly(f);
        s += " <- ";
        s += &bindee.ugly(f);
        s += "; ";
        s += &tail.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Let<PatId, TermId, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Let { binder, bindee, tail } = self;
        s += "let ";
        s += &binder.ugly(f);
        s += " = ";
        s += &bindee.ugly(f);
        s += " in ";
        s += &tail.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for MobileParam {
    fn ugly(&self, f: &'a Formatter) -> String {
        let MobileParam { binder, tail } = self;
        format!("param {} that {}", binder.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for MobileBind {
    fn ugly(&self, f: &'a Formatter) -> String {
        let MobileBind { binder, bindee, tail } = self;
        format!("let {} = {} that {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Residual {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Residual(body) = self;
        body.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Block {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Block(body) = self;
        format!("begin {} end", body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for RecGroup {
    fn ugly(&self, f: &'a Formatter) -> String {
        let RecGroup { definitions, tail } = self;
        let definitions = definitions
            .iter()
            .map(|RecursiveDefinition { binder, bindee }| {
                format!("def {} = {}", binder.ugly(f), bindee.ugly(f))
            })
            .collect::<Vec<_>>()
            .join("; ");
        format!("rec [{}] in {}", definitions, tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for MoBlock {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let MoBlock { body, basis: _ } = self;
        s += "monadic ";
        s += &body.ugly(f);
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Data {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Data { arms } = self;
        s += "data";
        for DataArm { name, param } in arms {
            s += " | ";
            s += &name.ugly(f);
            s += " ";
            s += &param.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoData {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let CoData { arms } = self;
        s += "codata";
        for CoDataArm { name, out } in arms {
            s += " | ";
            s += &name.ugly(f);
            s += " : ";
            s += &out.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Match<TermId, PatId, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Match { scrut, arms } = self;
        s += "match ";
        s += &scrut.ugly(f);
        for Matcher { binder, tail } in arms {
            s += " | ";
            s += &binder.ugly(f);
            s += " => ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoMatch<DtorName, TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let CoMatch { arms } = self;
        s += "comatch";
        for CoMatcher { dtor, tail } in arms {
            s += " | ";
            s += &dtor.ugly(f);
            s += " => ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoPatternItem {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Self::Pat(pattern) => pattern.ugly(f),
            | Self::Dtor(dtor) => dtor.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoPatternSpine {
    fn ugly(&self, f: &'a Formatter) -> String {
        self.iter().map(|item| item.ugly(f)).collect::<Vec<_>>().join(" ")
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoMatchClauses {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::from("comatch");
        for CoPatternClause { spine, tail } in &self.clauses {
            s += " | ";
            s += &spine.ugly(f);
            s += " => ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
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

impl<'a> Ugly<'a, Formatter<'a>> for RecursiveDefinition {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let RecursiveDefinition { binder, bindee } = self;
        s += "def ";
        s += &binder.ugly(f);
        s += " = ";
        s += &bindee.ugly(f);
        s += " that";
        s
    }
}
