//! Context and CoContext types for ordered and unordered collections.

use derive_more::IntoIterator;
use std::cmp::Ordering;

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
/// unique makes storage proportional to the logical contents and enables
/// linear two-pointer merges and intersections.
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

    pub fn as_slice(&self) -> &[T] {
        &self.0
    }

    pub fn new() -> Self {
        CoContext(Vec::new())
    }

    pub fn singleton(item: T) -> Self {
        CoContext(vec![item])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn contains(&self, item: &T) -> bool {
        self.0.binary_search(item).is_ok()
    }

    /// Two-pointer disjointness check in `O(|self| + |other|)`.
    pub fn intersects(&self, other: &Self) -> bool {
        let (mut i, mut j) = (0, 0);
        let (a, b) = (&self.0, &other.0);
        while i < a.len() && j < b.len() {
            match a[i].cmp(&b[j]) {
                | Ordering::Less => i += 1,
                | Ordering::Greater => j += 1,
                | Ordering::Equal => return true,
            }
        }
        false
    }

    /// Two-pointer sorted merge in `O(|self| + |other|)`.
    pub fn union(&self, other: &Self) -> Self
    where
        T: Clone,
    {
        if self.is_empty() {
            return other.clone();
        }
        if other.is_empty() {
            return self.clone();
        }
        let mut merged = Vec::with_capacity(self.0.len() + other.0.len());
        let (mut i, mut j) = (0, 0);
        let (a, b) = (&self.0, &other.0);
        while i < a.len() && j < b.len() {
            match a[i].cmp(&b[j]) {
                | Ordering::Less => {
                    merged.push(a[i].clone());
                    i += 1;
                }
                | Ordering::Greater => {
                    merged.push(b[j].clone());
                    j += 1;
                }
                | Ordering::Equal => {
                    merged.push(a[i].clone());
                    i += 1;
                    j += 1;
                }
            }
        }
        merged.extend_from_slice(&a[i..]);
        merged.extend_from_slice(&b[j..]);
        CoContext(merged)
    }

    /// Consume two sorted sets and merge them in `O(|self| + |other|)` without re-sorting.
    pub fn merge(self, other: Self) -> Self {
        if self.0.is_empty() {
            return other;
        }
        if other.0.is_empty() {
            return self;
        }
        let mut merged = Vec::with_capacity(self.0.len() + other.0.len());
        let mut a = self.0.into_iter().peekable();
        let mut b = other.0.into_iter().peekable();
        while let (Some(x), Some(y)) = (a.peek(), b.peek()) {
            match x.cmp(y) {
                | Ordering::Less => merged.push(a.next().unwrap()),
                | Ordering::Greater => merged.push(b.next().unwrap()),
                | Ordering::Equal => {
                    merged.push(a.next().unwrap());
                    let _ = b.next();
                }
            }
        }
        merged.extend(a);
        merged.extend(b);
        CoContext(merged)
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

impl<T> std::ops::Add<CoContext<T>> for CoContext<T>
where
    T: Ord,
{
    type Output = Self;
    #[inline]
    fn add(self, other: CoContext<T>) -> Self {
        self.merge(other)
    }
}

impl<T> std::ops::Add<&CoContext<T>> for CoContext<T>
where
    T: Ord + Clone,
{
    type Output = Self;
    #[inline]
    fn add(self, other: &CoContext<T>) -> Self {
        self.union(other)
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
        if self.0.is_empty() || ctx.0.is_empty() {
            return self;
        }
        if ctx.0.len() == 1 {
            return self - &ctx.0[0];
        }
        let mut needles = ctx.0;
        needles.sort_unstable();
        needles.dedup();
        self.0.retain(|item| needles.binary_search(item).is_err());
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
        let context = CoContext::from_iter([3, 1]) + CoContext::from_iter([2, 1]);
        assert!(context.intersects(&CoContext::from_iter([4, 2])));
        assert!(!context.intersects(&CoContext::from_iter([4, 5])));
        assert!(context.contains(&2));
        let context = context - &2;
        let context = context - Context::from_iter([3]);

        assert_eq!(context.iter().copied().collect::<Vec<_>>(), [1]);
    }
}
