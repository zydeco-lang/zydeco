use super::syntax::*;

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug)]
pub struct TextArena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

#[derive(Clone, Debug, derive_more::AddAssign)]
pub struct SpanArena(ArenaSparse<EntityId, Span>);

mod impl_span_arena {
    use super::*;
    use std::ops::Index;

    impl SpanArena {
        pub fn new(allocator: IndexAlloc<usize>) -> Self {
            Self(ArenaSparse::new(allocator))
        }
        pub fn alloc(&mut self, span: Span) -> EntityId {
            self.0.alloc(span)
        }
    }

    impl Index<&EntityId> for SpanArena {
        type Output = Span;
        fn index(&self, index: &EntityId) -> &Self::Output {
            &self.0[index]
        }
    }

    impl IntoIterator for SpanArena {
        type Item = (EntityId, Span);
        type IntoIter = <ArenaSparse<EntityId, Span> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a SpanArena {
        type Item = (&'a EntityId, &'a Span);
        type IntoIter = <&'a ArenaSparse<EntityId, Span> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            (&self.0).into_iter()
        }
    }

    impl<'a> SpanArena {
        pub fn iter(&'a self) -> <&'a Self as IntoIterator>::IntoIter {
            self.into_iter()
        }
    }
}
