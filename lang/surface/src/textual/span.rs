use super::syntax::*;
use zydeco_syntax::SpanStore;

impl SpanStore<EntityId> for SpanArena {
    fn span(&self, entity: EntityId) -> &Span {
        &self[&entity]
    }
}

macro_rules! impl_span_view {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, SpanArena> for $ty {
                fn span(&self, arena: &'a SpanArena) -> &'a Span {
                    &arena[&((*self).into())]
                }
            }
            impl<'a> SpanView<'a, Parser> for $ty {
                fn span(&self, parser: &'a Parser) -> &'a Span {
                    self.span(&parser.spans)
                }
            }
        )*
    };
}

impl_span_view! {
    DefId
    PatId
    CoPatId
    MetaId
    TermId
}

mod impl_span_arena {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum Category {
        Definition,
        Pattern,
        CoPattern,
        Term,
        Metadata,
    }

    impl SpanArena {
        /// Find all entities that **includes** the given cursor.
        /// The result is sorted: smallest entity first.
        pub fn lookup_cursor(&self, cursor: usize) -> Vec<EntityId> {
            let mut hit: Vec<_> = (self.iter())
                .filter_map(|(id, s)| {
                    let span = s.range();
                    if span.start <= cursor && cursor <= span.end {
                        Some((id, span.end - span.start))
                    } else {
                        None
                    }
                })
                .collect();
            hit.sort_by_key(|a| a.1);
            hit.into_iter().map(|(id, _)| id).collect()
        }

        /// Find all entities that are **included** in the given span.
        /// The result is sorted: largest entity first.
        pub fn lookup_span(&self, span: Span) -> Vec<EntityId> {
            let range = span.range();
            let mut hit: Vec<_> = (self.iter())
                .filter_map(|(id, s)| {
                    let inner = s.range();
                    if range.start <= inner.start && inner.end <= range.end {
                        Some((id, inner.end - inner.start))
                    } else {
                        None
                    }
                })
                .collect();
            hit.sort_by_key(|item| std::cmp::Reverse(item.1));
            hit.into_iter().map(|(id, _)| id).collect()
        }
    }

    impl TextArena {
        /// Classify an entity ID by which arena it belongs to.
        fn get_category(&self, id: EntityId) -> Category {
            match id {
                | EntityId::Def(_) => Category::Definition,
                | EntityId::Pat(_) => Category::Pattern,
                | EntityId::CoPat(_) => Category::CoPattern,
                | EntityId::Meta(_) => Category::Metadata,
                | EntityId::Term(_) => Category::Term,
            }
        }

        /// Sort entities by category precision, including copatterns.
        pub fn order_entities_by_precision(&self, entities: Vec<EntityId>) -> Vec<EntityId> {
            let mut hit: Vec<_> =
                entities.into_iter().map(|id| (id, self.get_category(id))).collect();
            hit.sort_by_key(|item| std::cmp::Reverse(item.1));
            hit.into_iter().map(|(id, _)| id).collect()
        }
    }
}
