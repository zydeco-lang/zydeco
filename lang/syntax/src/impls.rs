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
            #[inline]
            pub fn new(name: impl AsRef<str>) -> Self {
                Self(Sym::intern(name.as_ref()))
            }

            #[inline]
            pub const fn from_sym(sym: Sym) -> Self {
                Self(sym)
            }

            #[inline]
            pub const fn sym(self) -> Sym {
                self.0
            }

            #[inline]
            pub fn as_str(&self) -> &'static str {
                self.0.as_str()
            }

            #[inline]
            pub fn plain_str(&self) -> &'static str {
                $plain(self.0.as_str())
            }

            #[inline]
            pub fn plain(&self) -> String {
                self.plain_str().to_owned()
            }
        }
        impl std::ops::Deref for $name {
            type Target = str;

            #[inline]
            fn deref(&self) -> &Self::Target {
                self.as_str()
            }
        }
        impl AsRef<str> for $name {
            #[inline]
            fn as_ref(&self) -> &str {
                self.as_str()
            }
        }
        impl PartialEq<str> for $name {
            #[inline]
            fn eq(&self, other: &str) -> bool {
                self.as_str() == other
            }
        }
        impl PartialEq<&str> for $name {
            #[inline]
            fn eq(&self, other: &&str) -> bool {
                self.as_str() == *other
            }
        }
        impl PartialEq<String> for $name {
            #[inline]
            fn eq(&self, other: &String) -> bool {
                self.as_str() == other.as_str()
            }
        }
        impl std::borrow::Borrow<str> for $name {
            #[inline]
            fn borrow(&self) -> &str {
                self.as_str()
            }
        }
        impl From<&str> for $name {
            #[inline]
            fn from(name: &str) -> Self {
                Self::new(name)
            }
        }
        impl From<String> for $name {
            #[inline]
            fn from(name: String) -> Self {
                Self::new(name)
            }
        }
        impl From<&String> for $name {
            #[inline]
            fn from(name: &String) -> Self {
                Self::new(name)
            }
        }
        impl From<Box<str>> for $name {
            #[inline]
            fn from(name: Box<str>) -> Self {
                Self::new(name)
            }
        }
        impl From<std::borrow::Cow<'_, str>> for $name {
            #[inline]
            fn from(name: std::borrow::Cow<'_, str>) -> Self {
                Self::new(name)
            }
        }
        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(self.plain_str())
            }
        }
    };
}
#[inline]
fn identity(name: &str) -> &str {
    name
}
#[inline]
fn remove_prefix(name: &str) -> &str {
    &name[1..]
}

impl_name!(VarName, identity);
impl_name!(FieldName, identity);
impl_name!(CtorName, remove_prefix);
impl_name!(DtorName, remove_prefix);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interned_names_are_four_bytes_and_copy() {
        assert_eq!(std::mem::size_of::<VarName>(), 4);
        assert_eq!(std::mem::size_of::<Option<VarName>>(), 4);
        assert_eq!(std::mem::size_of::<FieldName>(), 4);
        assert_eq!(std::mem::size_of::<CtorName>(), 4);
        assert_eq!(std::mem::size_of::<DtorName>(), 4);

        let v = VarName::new("alpha");
        let v2 = v;
        assert_eq!(v, v2);
        assert_eq!(v.as_str(), "alpha");
        assert_eq!(CtorName::new("+True").plain_str(), "True");
        assert_eq!(DtorName::new(".head").plain_str(), "head");
    }
}
