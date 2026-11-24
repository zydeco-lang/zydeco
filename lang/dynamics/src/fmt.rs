use crate::syntax::*;
use pretty::RcDoc;
use zydeco_ir::{CoreProgram, fmt::Formatter as CoreFormatter};
pub use zydeco_syntax::{Pretty, Ugly};

/// Formatter borrowing the executed [`CoreProgram`] to render semantic values and continuations.
pub struct Formatter<'arena> {
    core: CoreFormatter<'arena>,
}

impl<'arena> Formatter<'arena> {
    pub fn new(program: &'arena CoreProgram) -> Self {
        Self { core: CoreFormatter::new(program.arena()) }
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for [T]
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

impl<'a> Pretty<'a, Formatter<'a>> for SemValue {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        zydeco_foundation::stack::maybe_grow(|| match self {
            | SemValue::Thunk(v) => {
                RcDoc::concat([RcDoc::text("{ "), v.pretty(f), RcDoc::text(" }")])
            }
            | SemValue::Ctor(v) => v.pretty(f),
            | SemValue::Triv(v) => v.pretty(f),
            | SemValue::VCons(v) => v.pretty(f),
            | SemValue::Literal(v) => v.pretty(f),
            | SemValue::Host(v) => RcDoc::text(format!("<{v:?}>")),
        })
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ThunkBody {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | ThunkBody::Core { body, .. } => {
                RcDoc::concat([RcDoc::text("[..]{ "), body.pretty(&f.core), RcDoc::text(" }")])
            }
            | ThunkBody::Fix { param, body, .. } => RcDoc::concat([
                RcDoc::text("[fix "),
                param.pretty(&f.core),
                RcDoc::text(" => "),
                body.pretty(&f.core),
                RcDoc::text("]"),
            ]),
            | ThunkBody::Host(op) => RcDoc::text(format!("prim({})[{op}]", op.arity())),
            | ThunkBody::Foreign(import) => RcDoc::text(format!("ffi[{}]", import.target().symbol)),
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
            | SemCompu::Kont { body, binder, .. } => RcDoc::concat([
                RcDoc::text("kont "),
                body.pretty(&f.core),
                RcDoc::text(" "),
                binder.pretty(&f.core),
            ]),
            | SemCompu::App(arg) => RcDoc::concat([RcDoc::text("app "), arg.pretty(f)]),
            | SemCompu::Dtor(dtor) => {
                RcDoc::concat([RcDoc::text("dtor "), RcDoc::text(dtor.name.as_str())])
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
        RcDoc::concat([RcDoc::text(name.as_str()), RcDoc::text(" "), args.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}
