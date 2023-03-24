use crate::syntax::Span;
use std::rc::Rc;

#[derive(Clone, Copy)]
pub struct Args {
    indent_unit: usize,
    pub indent: usize,
}
impl Args {
    pub fn new(indent_unit: usize) -> Self {
        Self { indent_unit, indent: 0 }
    }
    pub fn indent(&self) -> Self {
        Self { indent: self.indent + self.indent_unit, ..self.clone() }
    }
    pub fn force_space(&self) -> String {
        format!("\n{}", " ".repeat(self.indent))
    }
}

pub trait FmtArgs {
    fn fmt_args(&self, args: Args) -> String;
    fn fmt(&self) -> String {
        self.fmt_args(Args::new(2))
    }
}

impl<T: FmtArgs> FmtArgs for Box<T> {
    fn fmt_args(&self, args: Args) -> String {
        self.as_ref().fmt_args(args)
    }
}

impl<T: FmtArgs> FmtArgs for Rc<T> {
    fn fmt_args(&self, args: Args) -> String {
        self.as_ref().fmt_args(args)
    }
}

impl<T: FmtArgs> FmtArgs for Span<T> {
    fn fmt_args(&self, args: Args) -> String {
        self.inner_ref().fmt_args(args)
    }
}
