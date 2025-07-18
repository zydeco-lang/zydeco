use crate::bitter::syntax::*;

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    arena: &'arena Arena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Formatter { arena }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TopLevel {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let TopLevel(decls) = self;
        s += &decls.iter().map(|decl| decl.ugly(f)).collect::<Vec<_>>().join("\n");
        s
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
            | Pattern::Ctor(p) => s += &p.ugly(f),
            | Pattern::Triv(p) => s += &p.ugly(f),
            | Pattern::Cons(p) => s += &p.ugly(f),
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
            | Term::Internal(t) => s += &t.ugly(f),
            | Term::Sealed(t) => s += &t.ugly(f),
            | Term::Ann(t) => s += &t.ugly(f),
            | Term::Hole(t) => s += &t.ugly(f),
            | Term::Var(t) => s += &t.ugly(f),
            | Term::Triv(t) => s += &t.ugly(f),
            | Term::Cons(t) => s += &t.ugly(f),
            | Term::Abs(t) => s += &t.ugly(f),
            | Term::App(t) => s += &t.ugly(f),
            | Term::Fix(t) => s += &t.ugly(f),
            | Term::Pi(t) => s += &t.ugly(f),
            | Term::Sigma(t) => s += &t.ugly(f),
            | Term::Thunk(t) => s += &t.ugly(f),
            | Term::Force(t) => s += &t.ugly(f),
            | Term::Ret(t) => s += &t.ugly(f),
            | Term::Do(t) => s += &t.ugly(f),
            | Term::Let(t) => s += &t.ugly(f),
            // Term::UseLet(t) => s += &t.ugly(f),
            | Term::MoBlock(t) => s += &t.ugly(f),
            | Term::Data(t) => s += &t.ugly(f),
            | Term::CoData(t) => s += &t.ugly(f),
            | Term::Ctor(t) => s += &t.ugly(f),
            | Term::Match(t) => s += &t.ugly(f),
            | Term::CoMatch(t) => s += &t.ugly(f),
            | Term::Dtor(t) => s += &t.ugly(f),
            | Term::Lit(t) => s += &t.ugly(f),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DeclId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Modifiers { public, external, inner } = &f.arena.decls[self];
        let mut s = String::new();
        if *public {
            s += "pub ";
        }
        if *external {
            s += "extern ";
        }
        use Declaration as Decl;
        match inner {
            | Decl::Meta(d) => s += &d.ugly(f),
            | Decl::AliasBody(d) => s += &d.ugly(f),
            | Decl::AliasHead(d) => s += &d.ugly(f),
            | Decl::Module(d) => s += &d.ugly(f),
            // Decl::Layer(d) => s += &d.ugly(f),
            // Decl::UseDef(d) => s += &d.ugly(f),
            // Decl::UseBlock(d) => s += &d.ugly(f),
            | Decl::Exec(d) => s += &d.ugly(f),
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
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Meta { stem, args } = self;
        s += &stem;
        s += "(";
        s += &args.iter().map(|a| a.ugly(f)).collect::<Vec<_>>().join(",");
        s += ")";
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

impl<'a, T> Ugly<'a, Formatter<'a>> for NameRef<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
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

impl<'a> Ugly<'a, Formatter<'a>> for Ctor<TermId> {
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

impl<'a> Ugly<'a, Formatter<'a>> for Ctor<PatId> {
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

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Cons<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Cons(a, b) = self;
        s += "(";
        s += &a.ugly(f);
        s += ", ";
        s += &b.ugly(f);
        s += ")";
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Dtor<T>
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
        s += " -> ";
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
        s += " -> ";
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

impl<'a> Ugly<'a, Formatter<'a>> for MoBlock {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let MoBlock(body) = self;
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
            s += " -> ";
            s += &tail.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoMatch<TermId> {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let CoMatch { arms } = self;
        s += "comatch";
        for CoMatcher { dtor, tail } in arms {
            s += " | ";
            s += &dtor.ugly(f);
            s += " -> ";
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

// impl<'a> Ugly<'a, Formatter<'a>> for UseBind {
//     fn ugly(&self, f: &'a Formatter) -> String {
//         let mut s = String::new();
//         let UseBind { uses, tail } = self;
//         s += "use ";
//         s += &uses.ugly(f);
//         s += " in ";
//         s += &tail.ugly(f);
//         s
//     }
// }

// impl<'a> Ugly<'a, Formatter<'a>> for UsePath {
//     fn ugly(&self, f: &'a Formatter) -> String {
//         let mut s = String::new();
//         let UsePath(u) = self;
//         s += &u.ugly(f);
//         s
//     }
// }

// impl<'a> Ugly<'a, Formatter<'a>> for UseEnum {
//     fn ugly(&self, f: &'a Formatter) -> String {
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

// impl<'a> Ugly<'a, Formatter<'a>> for UseDef {
//     fn ugly(&self, f: &'a Formatter) -> String {
//         let mut s = String::new();
//         let UseDef(u) = self;
//         s += "use ";
//         s += &u.ugly(f);
//         s += " end";
//         s
//     }
// }

// impl<'a> Ugly<'a, Formatter<'a>> for UseBlock {
//     fn ugly(&self, f: &'a Formatter) -> String {
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

impl<'a> Ugly<'a, Formatter<'a>> for Module {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Module { name, top } = self;
        s += "module";
        if let Some(name) = name {
            s += " ";
            s += &name.ugly(f);
        }
        s += " where\n";
        s += &top.ugly(f);
        s += "\nend";
        s
    }
}

// impl<'a> Ugly<'a, Formatter<'a>> for Layer {
//     fn ugly(&self, f: &'a Formatter) -> String {
//         let mut s = String::new();
//         let Layer { name, uses, top } = self;
//         if let Some(name) = name {
//             s += "layer ";
//             s += &name.ugly(f);
//         }
//         for Modifiers { public, inner } in uses {
//             if *public {
//                 s += " pub";
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

impl<'a> Ugly<'a, Formatter<'a>> for AliasBody {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let AliasBody { binder, bindee } = self;
        s += "alias ";
        s += &binder.ugly(f);
        s += " = ";
        s += &bindee.ugly(f);
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for AliasHead {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let AliasHead { binder, ty } = self;
        s += "def ";
        s += &binder.ugly(f);
        if let Some(ty) = ty {
            s += " : ";
            s += &ty.ugly(f);
        }
        s += " end";
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Exec {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Exec(m) = self;
        s += "main ";
        s += &m.ugly(f);
        s += " end";
        s
    }
}
