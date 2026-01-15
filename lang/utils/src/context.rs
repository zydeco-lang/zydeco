//! Context and CoContext types for ordered and unordered collections.

/// Contexts are ordered sets of elements.
#[derive(Clone, Debug)]
pub struct Context<T>(pub Vec<T>);

impl<T> FromIterator<T> for Context<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Context(iter.into_iter().collect())
    }
}

// Note: can we derive this, as well as as_ref, as_mut, deref, deref_mut, and borrow?
impl<T> IntoIterator for Context<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Context<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> Context<T> {
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn new() -> Self {
        Context(Vec::new())
    }

    pub fn singleton(item: T) -> Self {
        Context::from_iter([item])
    }
}

impl<T> Default for Context<T> {
    fn default() -> Self {
        Context::new()
    }
}

impl<T, Iter> std::ops::Add<Iter> for Context<T>
where
    T: Clone,
    Iter: IntoIterator<Item = T>,
{
    type Output = Self;
    fn add(self, other: Iter) -> Self {
        Context::from_iter(self.0.into_iter().chain(other))
    }
}

/// CoContexts are unordered sets of elements.
#[derive(Clone, Debug)]
pub struct CoContext<T: std::hash::Hash + Eq>(pub im::HashSet<T>);

impl<T> FromIterator<T> for CoContext<T>
where
    T: std::hash::Hash + Eq + Clone,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        CoContext(iter.into_iter().collect())
    }
}

impl<T> IntoIterator for CoContext<T>
where
    T: std::hash::Hash + Eq + Clone,
{
    type Item = T;
    type IntoIter = im::hashset::ConsumingIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a CoContext<T>
where
    T: std::hash::Hash + Eq,
{
    type Item = &'a T;
    type IntoIter = im::hashset::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> CoContext<T>
where
    T: std::hash::Hash + Eq,
{
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn new() -> Self {
        CoContext(im::HashSet::new())
    }

    pub fn singleton(item: T) -> Self
    where
        T: Clone,
    {
        CoContext::from_iter([item])
    }
}

impl<T> Default for CoContext<T>
where
    T: std::hash::Hash + Eq,
{
    fn default() -> Self {
        CoContext::new()
    }
}

impl<T, Iter> std::ops::Add<Iter> for CoContext<T>
where
    T: std::hash::Hash + Eq + Clone,
    Iter: IntoIterator<Item = T>,
{
    type Output = Self;
    fn add(self, other: Iter) -> Self {
        let mut set = self.0;
        set.extend(other);
        CoContext(set)
    }
}

impl<T> std::ops::Sub<&T> for CoContext<T>
where
    T: std::hash::Hash + Eq + Clone,
{
    type Output = Self;
    fn sub(self, item: &T) -> Self {
        let mut set = self.0;
        set.remove(item);
        CoContext(set)
    }
}

impl<T> std::ops::Sub<Context<T>> for CoContext<T>
where
    T: std::hash::Hash + Eq + Clone,
{
    type Output = Self;
    fn sub(mut self, ctx: Context<T>) -> Self {
        for item in ctx.0 {
            self = self - &item;
        }
        self
    }
}
