use crate::*;
use std::fmt;

macro_rules! impl_name {
    ($name:ident, $plain:expr) => {
        impl $name {
            pub fn plain(&self) -> &str {
                let $name(name) = self;
                $plain(name)
            }
        }
        impl From<&str> for $name {
            fn from(name: &str) -> Self {
                $name(name.to_string())
            }
        }
        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.plain())
            }
        }
    };
}
#[inline]
fn remove_prefix(name: &str) -> &str {
    &name[1..]
}

impl_name!(VarName, |name| name);
impl_name!(SymName, |name| name);
impl_name!(CtorName, remove_prefix);
impl_name!(DtorName, remove_prefix);
