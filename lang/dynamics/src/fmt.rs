use crate::syntax::*;

pub use zydeco_syntax::{Pretty, Ugly};
/// Formatter borrowing the linked program for name resolution.
pub struct Formatter<'arena> {
    program: &'arena DynamicsProgram,
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

impl<'arena> Formatter<'arena> {
    /// Create a formatter bound to the given dynamic program.
    pub fn new(program: &'arena DynamicsProgram) -> Self {
        Formatter { program }
    }
}

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let VarName(name) = &f.program.defs()[self];
        RcDoc::text(format!("{}{}", name, self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValuePattern {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        use ValuePattern as VPat;
        match self {
            | VPat::Hole(Hole) => RcDoc::text("_"),
            | VPat::Var(def) => def.pretty(f),
            | VPat::Ctor(vpat) => vpat.pretty(f),
            | VPat::Alias(Alias(patterns)) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(
                    patterns.iter().map(|pattern| pattern.pretty(f)),
                    RcDoc::text("; "),
                ),
                RcDoc::text(")"),
            ]),
            | VPat::Triv(Triv) => RcDoc::text("()"),
            | VPat::VCons(vpat) => vpat.pretty(f),
            | VPat::View(view) => RcDoc::concat([
                view.function.pretty(f),
                RcDoc::text(" ~> "),
                view.pattern.pretty(f),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Let(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in "),
                tail.pretty(f),
            ]),
            | Value::ValAbs(Abs(param, body)) => RcDoc::concat([
                RcDoc::text("val "),
                param.pretty(f),
                RcDoc::text(" => "),
                body.pretty(f),
            ]),
            | Value::ValApp(App(function, argument)) => RcDoc::concat([
                RcDoc::text("("),
                function.pretty(f),
                RcDoc::text(" "),
                argument.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Thunk(Thunk(body)) => {
                RcDoc::concat([RcDoc::text("{ "), body.pretty(f), RcDoc::text(" }")])
            }
            | Value::Ctor(value) => value.pretty(f),
            | Value::Triv(value) => value.pretty(f),
            | Value::VCons(value) => value.pretty(f),
            | Value::Proj(Proj(head, position)) => {
                RcDoc::concat([head.pretty(f), RcDoc::text(format!("[{position}]"))])
            }
            | Value::Lit(lit) => lit.pretty(f),
            | Value::SemValue(sem) => RcDoc::text(format!("{:?}", sem)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Computation {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        use Computation as Compu;
        match self {
            | Compu::Hole(Hole) => RcDoc::text("_"),
            | Compu::VAbs(Abs(param, body)) => RcDoc::concat([
                RcDoc::text("fn "),
                param.pretty(f),
                RcDoc::text(" => "),
                body.pretty(f),
            ]),
            | Compu::VApp(App(body, arg)) => RcDoc::concat([
                RcDoc::text("("),
                body.pretty(f),
                RcDoc::text(" "),
                arg.pretty(f),
                RcDoc::text(")"),
            ]),
            | Compu::Fix(Fix(param, body)) => RcDoc::concat([
                RcDoc::text("fix "),
                param.pretty(f),
                RcDoc::text(" => "),
                body.pretty(f),
            ]),
            | Compu::Force(Force(body)) => RcDoc::concat([RcDoc::text("! "), body.pretty(f)]),
            | Compu::Ret(Return(body)) => RcDoc::concat([RcDoc::text("ret "), body.pretty(f)]),
            | Compu::Do(Bind { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("do "),
                binder.pretty(f),
                RcDoc::text(" <- "),
                bindee.pretty(f),
                RcDoc::text("; "),
                tail.pretty(f),
            ]),
            | Compu::Let(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in "),
                tail.pretty(f),
            ]),
            | Compu::Match(Match { scrut, arms }) => {
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
            | Compu::CoMatch(CoMatch { arms }) => {
                let mut doc = RcDoc::text("comatch ");
                for CoMatcher { dtor, tail } in arms {
                    let DtorName(name) = dtor;
                    doc = doc.append(RcDoc::concat([
                        RcDoc::text(" | "),
                        RcDoc::text(name.clone()),
                        RcDoc::text(" => "),
                        tail.pretty(f),
                    ]));
                }
                doc.append(RcDoc::text(" end"))
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                let DtorName(name) = dtor;
                RcDoc::concat([
                    RcDoc::text("("),
                    body.pretty(f),
                    RcDoc::text(" "),
                    RcDoc::text(name.clone()),
                    RcDoc::text(")"),
                ])
            }
            | Compu::Prim(Prim { arity, role }) => RcDoc::text(format!("prim({arity})[{role}]")),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SemValue {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | SemValue::Closure(v) => {
                RcDoc::concat([RcDoc::text("<val "), v.body.pretty(f), RcDoc::text(">")])
            }
            | SemValue::Thunk(v) => {
                RcDoc::concat([RcDoc::text("{ "), v.pretty(f), RcDoc::text(" }")])
            }
            | SemValue::Ctor(v) => v.pretty(f),
            | SemValue::Triv(v) => v.pretty(f),
            | SemValue::VCons(v) => v.pretty(f),
            | SemValue::Literal(v) => v.pretty(f),
            | SemValue::Host(v) => RcDoc::text(format!("<{v:?}>")),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Literal {
    fn pretty(&self, _: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Literal::Integer(value) => RcDoc::text(format!("{value:?}")),
            | Literal::Float(value) => RcDoc::text(format!("{value:?}")),
            | Literal::String(value) => RcDoc::text(format!("{value:?}")),
            | Literal::Char(value) => RcDoc::text(format!("{value:?}")),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SemCompu {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | SemCompu::Kont(body, _env, vpat) => RcDoc::concat([
                RcDoc::text("kont "),
                body.pretty(f),
                RcDoc::text(" "),
                vpat.pretty(f),
            ]),
            | SemCompu::App(arg) => RcDoc::concat([RcDoc::text("app "), arg.pretty(f)]),
            | SemCompu::Dtor(dtor) => {
                let DtorName(name) = dtor;
                RcDoc::concat([RcDoc::text("dtor "), RcDoc::text(name.clone())])
            }
        }
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Ctor<CtorName, T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let Ctor(name, args) = self;
        let CtorName(name) = name;
        RcDoc::concat([RcDoc::text(name.clone()), RcDoc::text(" "), args.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}

impl<'a, S, T> Pretty<'a, Formatter<'a>> for ConsN<S, T>
where
    S: Pretty<'a, Formatter<'a>>,
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let ConsN(items, tail) = self;
        let mut docs = items.iter().map(|item| item.pretty(f)).collect::<Vec<_>>();
        docs.push(tail.pretty(f));
        RcDoc::concat([
            RcDoc::text("("),
            RcDoc::intersperse(docs, RcDoc::text(", ")),
            RcDoc::text(")"),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for EnvThunk {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("[..]{ "), self.body.pretty(f), RcDoc::text(" }")])
    }
}
