use super::syntax::*;

pub use zydeco_syntax::Ugly;
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
            | Pattern::Manifest(p) => s += &p.ugly(f),
            | Pattern::Hole(p) => s += &p.ugly(f),
            | Pattern::Var(p) => s += &p.ugly(f),
            | Pattern::Named(p) => s += &p.ugly(f),
            | Pattern::Ctor(p) => s += &p.ugly(f),
            | Pattern::Project(p) => s += &p.ugly(f),
            | Pattern::Alias(p) => s += &p.ugly(f),
            | Pattern::Paren(p) => s += &p.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let copat = &f.arena.copats[self];
        match copat {
            | CoPattern::Pat(c) => s += &c.ugly(f),
            | CoPattern::Dtor(c) => s += &c.ugly(f),
            | CoPattern::App(c) => s += &c.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let term = &f.arena.terms[self];
        match term {
            | Term::Meta(t) => s += &t.ugly(f),
            | Term::SourceBoundary(SourceBoundary(t)) => s += &t.ugly(f),
            | Term::SignatureBoundary(SignatureBoundary(t)) => s += &t.ugly(f),
            | Term::Ann(t) => s += &t.ugly(f),
            | Term::Hole(t) => s += &t.ugly(f),
            | Term::Var(t) => s += &t.ugly(f),
            | Term::Named(t) => s += &t.ugly(f),
            | Term::Label(t) => s += &t.ugly(f),
            | Term::Paren(t) => s += &t.ugly(f),
            | Term::Abs(t) => s += &t.ugly(f),
            | Term::App(t) => s += &t.ugly(f),
            | Term::Fix(t) => s += &t.ugly(f),
            | Term::Pi(t) => s += &t.ugly(f),
            | Term::Arrow(t) => s += &t.ugly(f),
            | Term::Forall(t) => s += &t.ugly(f),
            | Term::Sigma(t) => s += &t.ugly(f),
            | Term::Prod(t) => s += &t.ugly(f),
            | Term::Exists(t) => s += &t.ugly(f),
            | Term::Thunk(t) => s += &t.ugly(f),
            | Term::Force(t) => s += &t.ugly(f),
            | Term::Ret(t) => s += &t.ugly(f),
            | Term::Do(t) => s += &t.ugly(f),
            | Term::Let(t) => s += &t.ugly(f),
            | Term::Param(t) => s += &t.ugly(f),
            | Term::ContextBind(t) => s += &t.ugly(f),
            | Term::Block(t) => s += &t.ugly(f),
            | Term::Data(t) => s += &t.ugly(f),
            | Term::CoData(t) => s += &t.ugly(f),
            | Term::Ctor(t) => s += &t.ugly(f),
            | Term::Match(t) => s += &t.ugly(f),
            | Term::CoMatch(t) => s += &t.ugly(f),
            | Term::Dtor(t) => s += &t.ugly(f),
            | Term::Proj(t) => s += &t.ugly(f),
            | Term::Lit(t) => s += &t.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Meta {
    fn ugly(&self, _f: &'a Formatter) -> String {
        self.to_string()
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

impl<'a> Ugly<'a, Formatter<'a>> for ManifestPattern {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ManifestPattern { binder, definition } = self;
        format!("({} as {})", binder.ugly(f), definition.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Hole {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "_".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for FieldName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        self.plain()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let VarName(name) = self;
        name.clone()
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

impl<'a, T> Ugly<'a, Formatter<'a>> for Ctor<CtorName, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
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

impl<'a, T> Ugly<'a, Formatter<'a>> for Paren<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Paren(ts) = self;
        s += "(";
        s += &ts.iter().map(|t| t.ugly(f)).collect::<Vec<_>>().join(",");
        s += ")";
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Alias<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Alias(patterns) = self;
        format!(
            "({})",
            patterns.iter().map(|pattern| pattern.ugly(f)).collect::<Vec<_>>().join("; ")
        )
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for ProjectionPattern<FieldName, T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let ProjectionPattern(field, pattern) = self;
        format!("/{} = {}", field.ugly(f), pattern.ugly(f))
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

impl<'a, T> Ugly<'a, Formatter<'a>> for Appli<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Appli(ts) = self;
        s += "(";
        s += &ts.iter().map(|t| t.ugly(f)).collect::<Vec<_>>().join(" ");
        s += ")";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Abs<CoPatId, TermId> {
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

impl<'a, T> Ugly<'a, Formatter<'a>> for ArrowU<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Arrow(t1, t2) = self;
        s += &t1.ugly(f);
        s += " -> ";
        s += &t2.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Forall {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Forall(p, t) = self;
        s += "forall ";
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

impl<'a, T> Ugly<'a, Formatter<'a>> for ProdU<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Prod(t1, r2) = self;
        s += &t1.ugly(f);
        s += " * ";
        s += &r2.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Exists {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Exists { parameters, body } = self;
        let parameters =
            parameters.iter().map(|parameter| parameter.ugly(f)).collect::<Vec<_>>().join(" ");
        format!("exists {parameters} . {}", body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ExistentialParameter {
    fn ugly(&self, f: &'a Formatter) -> String {
        let annotations = self
            .annotations
            .iter()
            .map(|annotation| format!("@[{}]", annotation.inner.ugly(f)))
            .collect::<Vec<_>>()
            .join(" ");
        let parameter = match self.binder {
            | binder
                if matches!(
                    f.arena.pats[&binder],
                    Pattern::Ann(_) | Pattern::Manifest(_) | Pattern::Paren(_)
                ) =>
            {
                binder.ugly(f)
            }
            | binder => format!("({})", binder.ugly(f)),
        };
        if annotations.is_empty() { parameter } else { format!("{annotations} {parameter}") }
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

impl<'a> Ugly<'a, Formatter<'a>> for GenLet {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let GenLet { binding, tail } = self;
        s += "let ";
        s += &binding.ugly(f);
        s += " in ";
        s += &tail.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Placement {
    fn ugly(&self, _f: &'a Formatter) -> String {
        match self {
            | Placement::In => "in".to_string(),
            | Placement::That => "that".to_string(),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Param {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Param { binder, placement, tail } = self;
        format!("param {} {} {}", binder.ugly(f), placement.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ContextBind {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ContextBind { mode, binding, placement, tail } = self;
        let keyword = match mode {
            | DefinitionMode::Transparent => "let",
            | DefinitionMode::Nominal => "def",
        };
        format!("{} {} {} {}", keyword, binding.ugly(f), placement.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Block {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Block(body) = self;
        format!("begin {} end", body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for GenBind<TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let GenBind { fix, comp, binder, params, ty, bindee } = self;
        if *fix {
            s += "fix ";
        }
        if *comp {
            s += "! ";
        }
        s += &binder.ugly(f);
        if let Some(params) = params {
            s += " ";
            s += &params.ugly(f);
        }
        if let Some(ty) = ty {
            s += " : ";
            s += &ty.ugly(f);
        }
        s += " = ";
        s += &bindee.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for GenBind<Option<TermId>> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let GenBind { fix, comp, binder, params, ty, bindee } = self;
        if *fix {
            s += "fix ";
        }
        if *comp {
            s += "! ";
        }
        s += &binder.ugly(f);
        if let Some(params) = params {
            s += " ";
            s += &params.ugly(f);
        }
        if let Some(ty) = ty {
            s += " : ";
            s += &ty.ugly(f);
        }
        if let Some(bindee) = bindee {
            s += " = ";
            s += &bindee.ugly(f);
        }
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
        for CoDataArm { name, params, out } in arms {
            s += " | ";
            s += &name.ugly(f);
            if let Some(params) = params {
                s += " ";
                s += &params.ugly(f);
            }
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

impl<'a> Ugly<'a, Formatter<'a>> for CoMatchParam {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let CoMatchParam { arms } = self;
        s += "comatch";
        for CoMatcherParam { params, tail } in arms {
            s += " | ";
            s += &params.ugly(f);
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
            | Literal::Integer(i) => s += &format!("{:?}", i),
            | Literal::Float(value) => s += &format!("{:?}", value),
            // Fixme: escape string
            | Literal::String(str) => s += &format!("{:?}", str),
            | Literal::Char(c) => s += &format!("{:?}", c),
        }
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for MetaT<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let MetaT(meta, decl) = self;
        s += "@[";
        s += &meta.ugly(f);
        s += "]";
        s += " ";
        s += &(*decl).ugly(f);
        s
    }
}
