use crate::*;
use std::fmt;

macro_rules! impl_name {
    ($name:ident, $plain:expr) => {
        impl $name {
            pub fn plain(&self) -> String {
                let $name(name) = self;
                $plain(name)
            }
        }
        impl<T: AsRef<str>> From<T> for $name {
            fn from(name: T) -> Self {
                $name(name.as_ref().to_string())
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
fn add_prefix(prefix: &'static str, name: &str) -> String {
    format!("{}{}", prefix, name)
}
#[inline]
fn remove_prefix(name: &str) -> String {
    name[1..].to_string()
}

impl_name!(VarName, str::to_string);
impl_name!(SymName, |name| add_prefix("%", name));
impl_name!(CtorName, remove_prefix);
impl_name!(DtorName, remove_prefix);
