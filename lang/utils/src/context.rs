//! Context and CoContext types for ordered and unordered collections.

use derive_more::IntoIterator;

/// Contexts are ordered sets of elements.
#[derive(Clone, Debug, IntoIterator)]
pub struct Context<T>(#[into_iterator(owned, ref)] pub Vec<T>);

impl<T> FromIterator<T> for Context<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Context(iter.into_iter().collect())
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

/// CoContexts are unordered sets of elements stored as compact sorted slices.
///
/// Free-variable contexts are tiny and numerous. A persistent hash trie gives
/// each occurrence its own tree nodes and dominates the resolved arena even
/// when most sets contain zero or one element. Keeping the elements sorted and
/// unique makes storage proportional to the logical contents instead.
#[derive(Clone, Debug, IntoIterator)]
pub struct CoContext<T: Ord>(#[into_iterator(owned, ref)] Vec<T>);

impl<T> FromIterator<T> for CoContext<T>
where
    T: Ord,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut items = iter.into_iter().collect::<Vec<_>>();
        items.sort_unstable();
        items.dedup();
        CoContext(items)
    }
}

impl<T> CoContext<T>
where
    T: Ord,
{
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn new() -> Self {
        CoContext(Vec::new())
    }

    pub fn singleton(item: T) -> Self {
        CoContext::from_iter([item])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Default for CoContext<T>
where
    T: Ord,
{
    fn default() -> Self {
        CoContext::new()
    }
}

impl<T, Iter> std::ops::Add<Iter> for CoContext<T>
where
    T: Ord,
    Iter: IntoIterator<Item = T>,
{
    type Output = Self;
    fn add(self, other: Iter) -> Self {
        CoContext::from_iter(self.0.into_iter().chain(other))
    }
}

impl<T> std::ops::Sub<&T> for CoContext<T>
where
    T: Ord,
{
    type Output = Self;
    fn sub(self, item: &T) -> Self {
        let mut items = self.0;
        if let Ok(index) = items.binary_search(item) {
            items.remove(index);
        }
        CoContext(items)
    }
}

impl<T> std::ops::Sub<Context<T>> for CoContext<T>
where
    T: Ord,
{
    type Output = Self;
    fn sub(mut self, ctx: Context<T>) -> Self {
        for item in ctx.0 {
            self = self - &item;
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn co_contexts_store_sorted_unique_elements() {
        let context = CoContext::from_iter([3, 1, 2, 1]);

        assert_eq!(context.iter().copied().collect::<Vec<_>>(), [1, 2, 3]);
    }

    #[test]
    fn co_context_set_operations_preserve_the_invariant() {
        let context = CoContext::from_iter([3, 1]) + [2, 1];
        let context = context - &2;
        let context = context - Context::from_iter([3]);

        assert_eq!(context.iter().copied().collect::<Vec<_>>(), [1]);
    }
}
