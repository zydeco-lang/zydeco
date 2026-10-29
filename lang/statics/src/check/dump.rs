//! Render statics entities for checker diagnostics.

use crate::{fmt::*, *};

impl Tycker<'_> {
    pub fn pretty_statics_nested<T>(&self, item: T, prefix: &str) -> String
    where
        T: for<'f> Pretty<'f, Formatter<'f>>,
    {
        let fmt = Formatter::new(self.scoped, &self.statics);
        let res = item.pretty(&fmt);
        let mut buf = String::new();
        buf += "\n";
        res.render_fmt(100, &mut buf).unwrap();
        buf.replace("\n", &format!("\n{}", prefix))
    }
}
