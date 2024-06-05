use crate::textual::syntax::*;

pub trait Ugly {
    fn ugly(&self, f: &Formatter) -> String;
}

pub struct Formatter<'arena> {
    // spans: SpanArenaTextual,
    arena: &'arena Arena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Formatter { arena }
    }
}

impl Ugly for TopLevel {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let TopLevel(decls) = self;
        s += &decls
            .iter()
            .map(|decl| {
                let Modifiers { public, external, inner } = &f.arena.decls[decl];
                let mut s = String::new();
                if *public {
                    s += "pub ";
                }
                if *external {
                    s += "extern ";
                }
                use Declaration as Decl;
                match inner {
                    | Decl::DataDef(d) => s += &d.ugly(f),
                    | Decl::CoDataDef(d) => s += &d.ugly(f),
                    | Decl::Define(d) => s += &d.ugly(f),
                    | Decl::Alias(d) => s += &d.ugly(f),
                    // Decl::Layer(d) => s += &d.ugly(f),
                    // Decl::UseDef(d) => s += &d.ugly(f),
                    // Decl::UseBlock(d) => s += &d.ugly(f),
                    | Decl::Main(d) => s += &d.ugly(f),
                }
                s
            })
            .collect::<Vec<_>>()
            .join("\n");
        s
    }
}

impl Ugly for DefId {
    fn ugly(&self, f: &Formatter) -> String {
        let name = &f.arena.defs[self];
        name.ugly(f)
    }
}

impl Ugly for PatId {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let pat = &f.arena.pats[self];
        match pat {
            | Pattern::Ann(p) => s += &p.ugly(f),
            | Pattern::Hole(p) => s += &p.ugly(f),
            | Pattern::Var(p) => s += &p.ugly(f),
            | Pattern::Ctor(p) => s += &p.ugly(f),
            | Pattern::Paren(p) => s += &p.ugly(f),
        }
        s
    }
}

impl Ugly for CoPatId {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for TermId {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let term = &f.arena.terms[self];
        match term {
            | Term::Ann(t) => s += &t.ugly(f),
            | Term::Hole(t) => s += &t.ugly(f),
            | Term::Var(t) => s += &t.ugly(f),
            | Term::Paren(t) => s += &t.ugly(f),
            | Term::Abs(t) => s += &t.ugly(f),
            | Term::App(t) => s += &t.ugly(f),
            | Term::Rec(t) => s += &t.ugly(f),
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
            // Term::UseLet(t) => s += &t.ugly(f),
            | Term::Data(t) => s += &t.ugly(f),
            | Term::CoData(t) => s += &t.ugly(f),
            | Term::Ctor(t) => s += &t.ugly(f),
            | Term::Match(t) => s += &t.ugly(f),
            | Term::CoMatch(t) => s += &t.ugly(f),
            | Term::Dtor(t) => s += &t.ugly(f),
            | Term::WithBlock(t) => s += &t.ugly(f),
            | Term::Lit(t) => s += &t.ugly(f),
        }
        s
    }
}

impl<S, T> Ugly for Ann<S, T>
where
    S: Ugly,
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for Hole {
    fn ugly(&self, _f: &Formatter) -> String {
        "_".to_string()
    }
}

impl Ugly for VarName {
    fn ugly(&self, _f: &Formatter) -> String {
        let VarName(name) = self;
        name.clone()
    }
}

impl<T> Ugly for NameRef<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let NameRef(root, path, name) = self;
        if *root {
            s += "root/";
        }
        for p in path {
            s += &p.ugly(f);
            s += "/";
        }
        s += &name.ugly(f);
        s
    }
}

impl Ugly for CtorName {
    fn ugly(&self, _f: &Formatter) -> String {
        let CtorName(name) = self;
        name.clone()
    }
}

impl Ugly for DtorName {
    fn ugly(&self, _f: &Formatter) -> String {
        let DtorName(name) = self;
        name.clone()
    }
}

impl<T> Ugly for Ctor<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Ctor(name, tail) = self;
        s += &name.ugly(f);
        s += "(";
        s += &tail.ugly(f);
        s += ")";
        s
    }
}

impl<T> Ugly for Paren<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Paren(ts) = self;
        s += "(";
        s += &ts.iter().map(|t| t.ugly(f)).collect::<Vec<_>>().join(",");
        s += ")";
        s
    }
}

impl<T> Ugly for Dtor<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Dtor(head, name) = self;
        s += &head.ugly(f);
        s += " ";
        s += &name.ugly(f);
        s
    }
}

impl<T> Ugly for Appli<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Appli(ts) = self;
        s += "(";
        s += &ts.iter().map(|t| t.ugly(f)).collect::<Vec<_>>().join(" ");
        s += ")";
        s
    }
}

impl Ugly for Abs<CoPatId, TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Abs(p, t) = self;
        s += "fn ";
        s += &p.ugly(f);
        s += " -> ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Rec<PatId, TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Rec(p, t) = self;
        s += "rec ";
        s += &p.ugly(f);
        s += " -> ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Pi {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Pi(p, t) = self;
        s += "pi ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl<T> Ugly for Arrow<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Arrow(t1, t2) = self;
        s += &t1.ugly(f);
        s += " -> ";
        s += &t2.ugly(f);
        s
    }
}

impl Ugly for Forall {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Forall(p, t) = self;
        s += "forall ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Sigma {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Sigma(p, t) = self;
        s += "sigma ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl<T> Ugly for Prod<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Prod(t1, r2) = self;
        s += &t1.ugly(f);
        s += " * ";
        s += &r2.ugly(f);
        s
    }
}

impl Ugly for Exists {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Exists(p, t) = self;
        s += "exists ";
        s += &p.ugly(f);
        s += " . ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Thunk<TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Thunk(t) = self;
        s += "{ ";
        s += &t.ugly(f);
        s += " }";
        s
    }
}

impl Ugly for Force<TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Force(t) = self;
        s += "! ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Ret<TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Ret(t) = self;
        s += "ret ";
        s += &t.ugly(f);
        s
    }
}

impl Ugly for Bind<PatId, TermId, TermId> {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for GenPureBind {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let GenPureBind { binding, tail } = self;
        s += "let ";
        s += &binding.ugly(f);
        s += " in ";
        s += &tail.ugly(f);
        s
    }
}

impl Ugly for GenBind<TermId> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let GenBind { rec, comp, binder, params, ty, bindee } = self;
        if *rec {
            s += "rec ";
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

impl Ugly for GenBind<Option<TermId>> {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let GenBind { rec, comp, binder, params, ty, bindee } = self;
        if *rec {
            s += "rec ";
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

impl Ugly for Data {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for CoData {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for Match {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Match { scrut, arms } = self;
        s += "match ";
        s += &scrut.ugly(f);
        for Matcher { binder, tail } in arms {
            s += " | ";
            s += &binder.ugly(f);
            s += " -> ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
    }
}

impl Ugly for CoMatch {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let CoMatch { arms } = self;
        s += "comatch";
        for CoMatcher { params, tail } in arms {
            s += " | ";
            s += &params.ugly(f);
            s += " -> ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
    }
}

impl Ugly for Import {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Import { binder, body  } = self;
        s += "import ";
        s += &binder.ugly(f);
        s += " = ";
        s += &body.ugly(f);
        s
    }
}

impl Ugly for WithBlock {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let WithBlock { monad_ty, imports, body } = self;
        s += "with ";
        s += &monad_ty.ugly(f);
        for import in imports {
            s += " ";
            s += &import.ugly(f);
        }
        s += " begin ";
        s += &body.ugly(f);
        s += " end";
        s
    }
}

impl Ugly for Literal {
    fn ugly(&self, _f: &Formatter) -> String {
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

// impl Ugly for UseBind {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         let UseBind { uses, tail } = self;
//         s += "use ";
//         s += &uses.ugly(f);
//         s += " in ";
//         s += &tail.ugly(f);
//         s
//     }
// }

// impl Ugly for UsePath {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         let UsePath(u) = self;
//         s += &u.ugly(f);
//         s
//     }
// }

// impl Ugly for UseEnum {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         match self {
//             | UseEnum::Name(n) => s += &n.ugly(f),
//             | UseEnum::Alias(UseAlias(binder, origin)) => {
//                 s += &binder.ugly(f);
//                 s += " = ";
//                 s += &origin.ugly(f);
//             }
//             | UseEnum::All(UseAll) => {
//                 s += "..";
//             }
//             | UseEnum::Cluster(Uses(u)) => {
//                 s += "( ";
//                 s += &u.iter().map(|u| u.ugly(f)).collect::<Vec<_>>().join(", ");
//                 s += " )";
//             }
//         }
//         s
//     }
// }

// impl Ugly for UseDef {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         let UseDef(u) = self;
//         s += "use ";
//         s += &u.ugly(f);
//         s += " end";
//         s
//     }
// }

// impl Ugly for UseBlock {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         let UseBlock { uses, top } = self;
//         s += "use ";
//         s += &uses.ugly(f);
//         s += " where\n";
//         s += &top.ugly(f);
//         s += "\nend";
//         s
//     }
// }

// impl Ugly for Layer {
//     fn ugly(&self, f: &Formatter) -> String {
//         let mut s = String::new();
//         let Layer { name, uses, top } = self;
//         if let Some(name) = name {
//             s += "layer ";
//             s += &name.ugly(f);
//         }
//         for Modifiers { public, external, inner } in uses {
//             if *public {
//                 s += " pub";
//             }
//             if *external {
//                 s += " extern";
//             }
//             s += " use ";
//             s += &inner.ugly(f);
//         }
//         s += " where\n";
//         s += &top.ugly(f);
//         s += "\nend";
//         s
//     }
// }

impl Ugly for Define {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Define(b) = self;
        s += "define ";
        s += &b.ugly(f);
        s += " end";
        s
    }
}

impl Ugly for Alias {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Alias(b) = self;
        s += "alias ";
        s += &b.ugly(f);
        s += " end";
        s
    }
}

impl Ugly for DataDef {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let DataDef { name, params, def: Data { arms } } = self;
        s += "data ";
        s += &name.ugly(f);
        for param in params {
            s += " ";
            s += &param.ugly(f);
        }
        s += " where";
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

impl Ugly for CoDataDef {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let CoDataDef { name, params, def: CoData { arms } } = self;
        s += "codata ";
        s += &name.ugly(f);
        for param in params {
            s += " ";
            s += &param.ugly(f);
        }
        s += " where";
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

impl Ugly for Main {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let Main(m) = self;
        s += "main ";
        s += &m.ugly(f);
        s += " end";
        s
    }
}
