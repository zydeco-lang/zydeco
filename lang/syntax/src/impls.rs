use crate::*;
use std::fmt;

impl<S, T> ConsN<S, T> {
    pub fn new(items: Vec<S>, tail: T) -> Self {
        Self(items, tail)
    }

    pub fn len(&self) -> usize {
        self.0.len() + 1
    }

    pub fn is_empty(&self) -> bool {
        false
    }
}

impl<T> ConsN<T, T> {
    pub fn from_vec(mut items: Vec<T>) -> Option<Self> {
        let tail = items.pop()?;
        Some(Self::new(items, tail))
    }

    pub fn into_vec(self) -> Vec<T> {
        let Self(mut items, tail) = self;
        items.push(tail);
        items
    }

    pub fn iter(&self) -> std::iter::Chain<std::slice::Iter<'_, T>, std::iter::Once<&T>> {
        self.0.iter().chain(std::iter::once(&self.1))
    }
}

impl<T> IntoIterator for ConsN<T, T> {
    type Item = T;
    type IntoIter = std::iter::Chain<std::vec::IntoIter<T>, std::iter::Once<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().chain(std::iter::once(self.1))
    }
}

impl<'a, T> IntoIterator for &'a ConsN<T, T> {
    type Item = &'a T;
    type IntoIter = std::iter::Chain<std::slice::Iter<'a, T>, std::iter::Once<&'a T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

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
impl_name!(FieldName, str::to_string);
impl_name!(SymName, |name| add_prefix("%", name));
impl_name!(CtorName, remove_prefix);
impl_name!(DtorName, remove_prefix);
