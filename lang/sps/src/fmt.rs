use super::syntax::*;
use zydeco_utils::arena::ArenaSparse;

pub use zydeco_syntax::Ugly;

/* -------------------------------- Formatter ------------------------------- */

pub struct Formatter<'arena> {
    defs: &'arena ArenaSparse<DefId, VarName>,
}

impl<'arena> Formatter<'arena> {
    pub fn new(defs: &'arena ArenaSparse<DefId, VarName>) -> Self {
        Formatter { defs }
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let name = &f.defs[self];
        s += &name.ugly(f);
        s += &self.concise();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Block {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let Block { body } = self;
        s += &body.ugly(f);
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValuePattern {
    fn ugly(&self, f: &'a Formatter) -> String {
        use ValuePattern as VPat;
        match self {
            | VPat::Hole(value) => value.ugly(f),
            | VPat::Var(value) => value.ugly(f),
            | VPat::Ctor(value) => value.ugly(f),
            | VPat::Triv(value) => value.ugly(f),
            | VPat::VCons(value) => value.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Value {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Value::Hole(value) => value.ugly(f),
            | Value::Var(value) => value.ugly(f),
            | Value::Proc(value) => value.ugly(f),
            | Value::Ctor(value) => value.ugly(f),
            | Value::Triv(value) => value.ugly(f),
            | Value::VCons(value) => value.ugly(f),
            | Value::Lit(value) => value.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Stack {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Stack::Kont(stk) => stk.ugly(f),
            | Stack::Var(stk) => stk.ugly(f),
            | Stack::Arg(stk) => stk.ugly(f),
            | Stack::Tag(stk) => stk.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Computation {
    fn ugly(&self, f: &'a Formatter) -> String {
        use Computation as Compu;
        match self {
            | Compu::Hole(compu) => compu.ugly(f),
            | Compu::VAbs(compu) => compu.ugly(f),
            | Compu::Fix(compu) => compu.ugly(f),
            | Compu::Call(compu) => compu.ugly(f),
            | Compu::Ret(compu) => compu.ugly(f),
            | Compu::Let(compu) => compu.ugly(f),
            | Compu::Match(compu) => compu.ugly(f),
            | Compu::CoMatch(compu) => compu.ugly(f),
            | Compu::Dtor(compu) => compu.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let VarName(name) = self;
        name.clone()
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

impl<'a> Ugly<'a, Formatter<'a>> for Hole {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "_".to_string()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Abs<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Abs(s, t) = self;
        format!("fn {} -> {}", s.ugly(f), t.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for App<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let App(s, t) = self;
        format!("({} {})", s.ugly(f), t.ugly(f))
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
        let Cons(s, t) = self;
        format!("({}, {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Thunk<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Thunk(tm) = self;
        format!("{{ {} }}", tm.ugly(f))
    }
}

impl<'a, Br, Be, Tail> Ugly<'a, Formatter<'a>> for Let<Br, Be, Tail>
where
    Br: Ugly<'a, Formatter<'a>>,
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Let { binder, bindee, tail } = self;
        format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for LetAbs {
    fn ugly(&self, f: &'a Formatter) -> String {
        let LetAbs { binder, bindee, body } = self;
        format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), body.ugly(f))
    }
}

impl<'a, P, Tm> Ugly<'a, Formatter<'a>> for Fix<P, Tm>
where
    P: Ugly<'a, Formatter<'a>>,
    Tm: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Fix(p, tm) = self;
        format!("fix {} -> {}", p.ugly(f), tm.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Call {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Call { thunk, stack } = self;
        format!("{} ! {}", thunk.ugly(f), stack.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ReturnKont {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ReturnKont { stack, value } = self;
        format!("{} @ {}", stack.ugly(f), value.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Current {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "current".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Kont {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Kont { binder, body } = self;
        format!("kontinuation {} -> {}", binder.ugly(f), body.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for StackItem<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("{} :: {}", self.item.ugly(f), self.next.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Ctor<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ctor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Dtor<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Dtor(tail, name) = self;
        format!("{} {}", tail.ugly(f), name.ugly(f))
    }
}

impl<'a, Sc, Br, Tail> Ugly<'a, Formatter<'a>> for Match<Sc, Br, Tail>
where
    Sc: Ugly<'a, Formatter<'a>>,
    Br: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Match { scrut, arms } = self;
        let mut s = String::new();
        s += &format!("match {}", scrut.ugly(f));
        for Matcher { binder, tail } in arms.iter() {
            s += &format!(" | {} -> {}", binder.ugly(f), tail.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for CoMatch<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let CoMatch { arms } = self;
        let mut s = String::new();
        s += &format!("comatch");
        for CoMatcher { dtor, tail } in arms.iter() {
            s += &format!(" | {} -> {}", dtor.ugly(f), tail.ugly(f));
        }
        s += &format!(" end");
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
