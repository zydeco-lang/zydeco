use super::{builtins::Builtin, syntax::*};
use crate::{prelude::*, rc, statics::syntax as ss};
use im::Vector;

pub trait Link<T> {
    fn link(value: T) -> Self;
}

impl<S, T> Link<Span<S>> for T
where
    T: Link<S>,
{
    fn link(value: Span<S>) -> Self {
        Link::link(value.inner())
    }
}

impl Link<ss::Program> for Program {
    fn link(ss::Program { module, entry }: ss::Program) -> Self {
        Program { module: Link::link(module), entry: todo!() }
    }
}

impl Link<ss::Module> for Module {
    fn link(ss::Module { name, data, codata, alias, define, define_ext }: ss::Module) -> Self {
        Module { name, define: todo!() }
    }
}
