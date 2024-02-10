use crate::prelude::*;
use std::rc::Rc;

#[derive(Clone, Copy)]
pub struct Args {
    indent_unit: usize,
    allow_br: bool,
    indent: usize,
    line: usize,
}
impl Args {
    pub fn indent(&self) -> Self {
        Self { indent: self.indent + self.indent_unit, ..self.clone() }
    }
    pub fn set_allow_br(&self, allow_br: bool) -> Self {
        Self { allow_br, ..self.clone() }
    }
    pub fn br_indent(&self) -> String {
        if self.allow_br {
            format!("\n{}", " ".repeat(self.indent))
        } else {
            " ".into()
        }
    }
    pub fn max_width(&self) -> usize {
        self.line - self.indent
    }
}

pub trait FmtArgs {
    fn fmt_args(&self, fargs: Args) -> String;
    fn fmt(&self) -> String {
        self.fmt_args(Args { indent_unit: 2, allow_br: true, indent: 0, line: 80 })
    }
    fn fmt_no_br(&self) -> String {
        self.fmt_args(Args { indent_unit: 2, allow_br: false, indent: 0, line: 80 })
    }
    fn fmt_truncate(&self, n: usize) -> String {
        let s = self.fmt_no_br();
        if s.len() > n {
            format!("{}...", &s[..n].trim())
        } else {
            s
        }
    }
    fn fmt_inline_debug(&self) -> String {
        self.fmt_truncate(40)
        // uncomment the following line to debug
        // self.fmt_no_br()
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

impl<T: FmtArgs> FmtArgs for Sp<T> {
    fn fmt_args(&self, args: Args) -> String {
        self.inner_ref().fmt_args(args)
    }
}
