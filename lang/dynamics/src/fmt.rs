use crate::syntax::*;

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    arena: &'arena DynamicsArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena DynamicsArena) -> Self {
        Formatter { arena }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Declaration {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Declaration::VAliasBody(VAliasBody { binder, bindee }) => {
                format!("def {} = {} end", binder.ugly(f), bindee.ugly(f))
            }
            | Declaration::Exec(Exec(comp)) => {
                format!("exec {} end", comp.ugly(f))
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let VarName(name) = &f.arena.defs[self];
        format!("{}{}", name, self.concise())
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for RcVPat {
    fn ugly(&self, f: &'a Formatter) -> String {
        use ValuePattern as VPat;
        match self.as_ref() {
            | VPat::Hole(Hole) => "_".to_string(),
            | VPat::Var(def) => def.ugly(f),
            | VPat::Ctor(vpat) => vpat.ugly(f),
            | VPat::Triv(Triv) => "()".to_string(),
            | VPat::VCons(vpat) => vpat.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for RcValue {
    fn ugly(&self, f: &'a Formatter) -> String {
        self.as_ref().ugly(f)
    }
}
impl<'a> Ugly<'a, Formatter<'a>> for Value {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Value::Hole(Hole) => "_".to_string(),
            | Value::Var(def) => def.ugly(f),
            | Value::Thunk(Thunk(body)) => {
                format!("{{ {} }}", body.ugly(f))
            }
            | Value::Ctor(value) => value.ugly(f),
            | Value::Triv(value) => value.ugly(f),
            | Value::VCons(value) => value.ugly(f),
            | Value::Lit(lit) => {
                format!("{:?}", lit)
            }
            | Value::SemValue(sem) => {
                format!("{:?}", sem)
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for RcCompu {
    fn ugly(&self, f: &'a Formatter) -> String {
        self.as_ref().ugly(f)
    }
}
impl<'a> Ugly<'a, Formatter<'a>> for Computation {
    fn ugly(&self, f: &'a Formatter) -> String {
        use Computation as Compu;
        match self {
            | Compu::Hole(Hole) => "_".to_string(),
            | Compu::VAbs(Abs(param, body)) => {
                format!("fn {} -> {}", param.ugly(f), body.ugly(f))
            }
            | Compu::VApp(App(body, arg)) => {
                format!("({} {})", body.ugly(f), arg.ugly(f))
            }
            | Compu::Fix(Fix(param, body)) => {
                format!("fix {} -> {}", param.ugly(f), body.ugly(f))
            }
            | Compu::Force(Force(body)) => {
                format!("! {}", body.ugly(f))
            }
            | Compu::Ret(Ret(body)) => {
                format!("ret {}", body.ugly(f))
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                format!("do {} <- {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Compu::Let(PureBind { binder, bindee, tail }) => {
                format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Compu::Match(Match { scrut, arms }) => {
                let mut s = String::new();
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
            | Compu::CoMatch(CoMatch { arms }) => {
                let mut s = String::new();
                s += "comatch ";
                for CoMatcher { dtor, tail } in arms {
                    s += " | ";
                    let DtorName(name) = dtor;
                    s += name;
                    s += " -> ";
                    s += &tail.ugly(f);
                }
                s += " end";
                s
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                let DtorName(name) = dtor;
                format!("({} {})", body.ugly(f), name)
            }
            | Compu::Prim(Prim { arity, body }) => {
                format!("prim({})[{:?}]", arity, body)
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for SemValue {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | SemValue::Thunk(v) => format!("{{ {} }}", v.ugly(f)),
            | SemValue::Ctor(v) => v.ugly(f),
            | SemValue::Triv(v) => v.ugly(f),
            | SemValue::VCons(v) => v.ugly(f),
            | SemValue::Literal(v) => format!("{:?}", v),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for SemCompu {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        match self {
            | SemCompu::Kont(body, _env, vpat) => {
                s += "kont ";
                s += &body.ugly(f);
                s += " ";
                s += &vpat.ugly(f);
            }
            | SemCompu::App(arg) => {
                s += "app ";
                s += &arg.ugly(f);
            }
            | SemCompu::Dtor(dtor) => {
                s += "dtor ";
                let DtorName(name) = dtor;
                s += name;
            }
        }
        s
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Ctor<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ctor(name, args) = self;
        let CtorName(name) = name;
        let args = args.ugly(f);
        format!("{} {}", name, args)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Triv {
    fn ugly(&self, _: &'a Formatter) -> String {
        "()".to_string()
    }
}

impl<'a, A, B> Ugly<'a, Formatter<'a>> for Cons<A, B>
where
    A: Ugly<'a, Formatter<'a>>,
    B: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Cons(a, b) = self;
        let a = a.ugly(f);
        let b = b.ugly(f);
        format!("({}, {})", a, b)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for EnvThunk {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("[..]{{ {} }}", self.body.ugly(f))
    }
}
