use super::syntax::*;
use crate::sps::syntax::*;

#[derive(Clone, Debug)]
pub struct SubstPatMap {
    /// Ordered map of value patterns to their corresponding values.
    /// The order is from innermost to outermost, namely,
    /// - we should apply the substitution to the underlying body from the innermost to the outermost.
    /// - or equivalently, we should apply the outermost substitution to both all inner substitutions
    ///   and the underlying body, and then take one step inner, and so on.
    /// 
    /// If you feel confused, simply think of it as a bunch of reversed let bindings.
    values: indexmap::IndexMap<VPatId, ValueId>,
    /// Ordered list of stacks substitutions.
    /// The order is from innermost to outermost.
    /// 
    /// Reversed stack-let bindings similar to reversed value-let bindings [`Self::values`].
    stacks: Vec<StackId>,
}

impl SubstPatMap {
    pub fn new() -> Self {
        Self { values: indexmap::IndexMap::new(), stacks: Vec::new() }
    }
    /// Substitute a pattern for a value.
    pub fn cascade_value(&mut self, pat: VPatId, value: ValueId) {
        self.values.insert(pat, value);
    }
    /// Add another layer of stack on top of the current stack,
    /// semantically substituting the current bullet for the new (prev) stack.
    pub fn cascade_stack(&mut self, prev: StackId) {
        self.stacks.push(prev);
    }
    /// Iterate over value pattern substitutions.
    pub fn values(&self) -> impl Iterator<Item = (&VPatId, &ValueId)> {
        self.values.iter()
    }
    /// Iterate over stack substitutions.
    pub fn stacks(&self) -> impl Iterator<Item = &StackId> {
        self.stacks.iter()
    }
}

/// In-place substitution for normalized stack IR nodes.
pub trait SubstPatInPlace {
    /// Substitute the free variables in the term in place.
    ///
    /// The [`DefId`]s in the pattern part of the map are guaranteed to be free.
    fn subst_pat_in_place<T>(self, arena: &mut T, map: &SubstPatMap)
    where
        T: AsMut<StackirArena> + AsMut<SNormArena>;
}
