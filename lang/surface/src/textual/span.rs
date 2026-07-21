use super::syntax::*;

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
    TermId
    DeclId
}

mod impl_span_arena {
    use super::*;
    use std::path::PathBuf;
    use zydeco_utils::span::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum Category {
        Definition,
        Pattern,
        CoPattern,
        Term,
        Declaration,
    }

    impl SpanArena {
        /// Find all entities that **includes** the given cursor.
        /// The result is sorted: smallest entity first.
        pub fn lookup_cursor(&self, path: Option<&PathBuf>, cursor: Cursor1) -> Vec<EntityId> {
            let mut hit: Vec<_> = (self.iter())
                .filter_map(|(id, s)| {
                    if s.get_path() != path {
                        return None;
                    }
                    let (start, end) = s.get_cursor1();
                    if start <= cursor && cursor <= end { Some((id, end - start)) } else { None }
                })
                .collect();
            hit.sort_by_key(|a| a.1);
            hit.into_iter().map(|(id, _)| *id).collect()
        }

        /// Find all entities that are **included** in the given span.
        /// The result is sorted: largest entity first.
        pub fn lookup_span(&self, span: Span) -> Vec<EntityId> {
            let path = span.get_path();
            let range = span.get_cursor1();
            let mut hit: Vec<_> = (self.iter())
                .filter_map(|(id, s)| {
                    if s.get_path() != path {
                        return None;
                    }
                    let (start, end) = s.get_cursor1();
                    if range.0 <= start && end <= range.1 { Some((id, end - start)) } else { None }
                })
                .collect();
            hit.sort_by_key(|item| std::cmp::Reverse(item.1));
            hit.into_iter().map(|(id, _)| *id).collect()
        }
    }

    impl TextArena {
        /// Classify an entity ID by which arena it belongs to.
        fn get_category(&self, id: EntityId) -> Category {
            match id {
                | EntityId::Def(_) => Category::Definition,
                | EntityId::Pat(_) => Category::Pattern,
                | EntityId::CoPat(_) => Category::CoPattern,
                | EntityId::Term(_) => Category::Term,
                | EntityId::Decl(_) => Category::Declaration,
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
