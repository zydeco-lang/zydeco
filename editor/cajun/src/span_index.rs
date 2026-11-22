use std::ops::Range;
use zydeco_foundation::span::FileId;
use zydeco_surface::{
    scoped::{
        arena::ScopedArena,
        syntax::{DefId, Term, TermId},
    },
    textual::syntax::SpanArena,
};

use crate::cursor::RangeEnd;

#[derive(Clone, Copy, Debug)]
struct SpanEntry<T> {
    start: u32,
    end: u32,
    order: u32,
    payload: T,
}

impl<T> SpanEntry<T> {
    fn len(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    fn byte_range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

/// Static augmented interval tree over byte ranges within one source file.
///
/// Entries are sorted by `(start, end, order)` so right subtrees can be pruned
/// whenever `start > offset` and subtrees with insufficient `max_end` can be
/// skipped in `O(1)`. Tie-breaking matches `Iterator::min_by_key` by picking
/// the smallest byte length and, among equal lengths, the earliest `order`.
#[derive(Clone, Debug)]
pub(crate) struct IntervalTree<T> {
    entries: Vec<SpanEntry<T>>,
    subtree_max_end: Vec<u32>,
}

impl<T> Default for IntervalTree<T> {
    fn default() -> Self {
        Self { entries: Vec::new(), subtree_max_end: Vec::new() }
    }
}

impl<T: Copy> IntervalTree<T> {
    fn build(mut entries: Vec<SpanEntry<T>>) -> Self {
        entries.sort_unstable_by_key(|entry| (entry.start, entry.end, entry.order));
        let mut subtree_max_end = vec![0; entries.len()];
        Self::populate_max_end(&entries, &mut subtree_max_end, 0, entries.len());
        Self { entries, subtree_max_end }
    }

    fn populate_max_end(
        entries: &[SpanEntry<T>], subtree_max_end: &mut [u32], lo: usize, hi: usize,
    ) -> u32 {
        if lo >= hi {
            return 0;
        }
        let mid = lo + (hi - lo) / 2;
        let left_max = Self::populate_max_end(entries, subtree_max_end, lo, mid);
        let right_max = Self::populate_max_end(entries, subtree_max_end, mid + 1, hi);
        let max_end = entries[mid].end.max(left_max).max(right_max);
        subtree_max_end[mid] = max_end;
        max_end
    }

    pub(crate) fn innermost_containing(
        &self, offset: usize, range_end: RangeEnd,
    ) -> Option<(T, Range<usize>)> {
        let Ok(offset_u32) = u32::try_from(offset) else {
            return None;
        };
        let mut best: Option<&SpanEntry<T>> = None;
        self.query(0, self.entries.len(), offset, offset_u32, range_end, &mut best);
        best.map(|entry| (entry.payload, entry.byte_range()))
    }

    fn query<'a>(
        &'a self, lo: usize, hi: usize, offset: usize, offset_u32: u32, range_end: RangeEnd,
        best: &mut Option<&'a SpanEntry<T>>,
    ) {
        if lo >= hi {
            return;
        }
        let mid = lo + (hi - lo) / 2;
        let max_end = self.subtree_max_end[mid];
        let can_reach = match range_end {
            | RangeEnd::Exclusive => offset_u32 < max_end,
            | RangeEnd::Inclusive => offset_u32 <= max_end,
        };
        if !can_reach {
            return;
        }
        self.query(lo, mid, offset, offset_u32, range_end, best);
        let entry = &self.entries[mid];
        if entry.start <= offset_u32 {
            let range = entry.byte_range();
            if range_end.contains(&range, offset) {
                let candidate_key = (entry.len(), entry.order);
                if best.is_none_or(|current| candidate_key < (current.len(), current.order)) {
                    *best = Some(entry);
                }
            }
            self.query(mid + 1, hi, offset, offset_u32, range_end, best);
        }
    }
}

/// Pre-indexed per-file lookup structures derived once per analysis revision.
#[derive(Clone, Debug, Default)]
pub(crate) struct FileSpanIndex {
    symbols: IntervalTree<DefId>,
    terms: IntervalTree<TermId>,
    document_symbols: Vec<(DefId, Range<usize>)>,
}

impl FileSpanIndex {
    pub(crate) fn symbol_at(
        &self, offset: usize, range_end: RangeEnd,
    ) -> Option<(DefId, Range<usize>)> {
        self.symbols.innermost_containing(offset, range_end)
    }

    pub(crate) fn term_at(
        &self, offset: usize, range_end: RangeEnd,
    ) -> Option<(TermId, Range<usize>)> {
        self.terms.innermost_containing(offset, range_end)
    }

    pub(crate) fn document_symbols(&self) -> &[(DefId, Range<usize>)] {
        &self.document_symbols
    }
}

#[derive(Default)]
struct FileSpanBuilder {
    symbols: Vec<SpanEntry<DefId>>,
    terms: Vec<SpanEntry<TermId>>,
    document_symbols: Vec<(DefId, Range<usize>)>,
}

/// Partitioned per-[`FileId`] interval indexes covering all source defs, uses, and terms.
#[derive(Clone, Debug, Default)]
pub(crate) struct SpanIndex {
    files: Vec<FileSpanIndex>,
}

impl SpanIndex {
    pub(crate) fn build(spans: &SpanArena, scoped: &ScopedArena) -> Self {
        let Some(source_map) = spans.source_map() else {
            return Self::default();
        };
        let mut builders =
            (0..source_map.files().len()).map(|_| FileSpanBuilder::default()).collect::<Vec<_>>();
        let mut symbol_order = 0u32;

        for (definition, _) in scoped.defs.iter() {
            if scoped.is_prelude_definition(*definition) {
                continue;
            }
            let Some(entity) = scoped.origins.source(&(*definition).into()) else {
                continue;
            };
            let Some((file, range)) = source_map.range(spans[&entity]) else {
                continue;
            };
            let Some(builder) = builders.get_mut(file.id().0 as usize) else {
                continue;
            };
            let Ok(start) = u32::try_from(range.start) else {
                continue;
            };
            let Ok(end) = u32::try_from(range.end) else {
                continue;
            };
            builder.symbols.push(SpanEntry {
                start,
                end,
                order: symbol_order,
                payload: *definition,
            });
            symbol_order += 1;
            builder.document_symbols.push((*definition, range));
        }

        let mut term_order = 0u32;
        for (term, body) in scoped.terms.iter() {
            let Some(entity) = scoped.origins.source(&term.into()) else {
                continue;
            };
            let Some((file, range)) = source_map.range(spans[&entity]) else {
                continue;
            };
            let Some(builder) = builders.get_mut(file.id().0 as usize) else {
                continue;
            };
            let Ok(start) = u32::try_from(range.start) else {
                continue;
            };
            let Ok(end) = u32::try_from(range.end) else {
                continue;
            };
            builder.terms.push(SpanEntry { start, end, order: term_order, payload: term });
            term_order += 1;

            if let Term::Var(definition) = body {
                let is_implicit_alias = scoped.is_prelude_definition(*definition)
                    && scoped.origins.source(&(*definition).into()) == Some(entity);
                if !is_implicit_alias {
                    builder.symbols.push(SpanEntry {
                        start,
                        end,
                        order: symbol_order,
                        payload: *definition,
                    });
                    symbol_order += 1;
                }
            }
        }

        let files = builders
            .into_iter()
            .map(|builder| FileSpanIndex {
                symbols: IntervalTree::build(builder.symbols),
                terms: IntervalTree::build(builder.terms),
                document_symbols: builder.document_symbols,
            })
            .collect();
        Self { files }
    }

    pub(crate) fn file(&self, id: FileId) -> Option<&FileSpanIndex> {
        self.files.get(id.0 as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn brute_force(
        entries: &[SpanEntry<u32>], offset: usize, range_end: RangeEnd,
    ) -> Option<(u32, Range<usize>)> {
        entries
            .iter()
            .filter(|entry| range_end.contains(&entry.byte_range(), offset))
            .min_by_key(|entry| (entry.len(), entry.order))
            .map(|entry| (entry.payload, entry.byte_range()))
    }

    #[test]
    fn empty_tree_returns_none() {
        let tree: IntervalTree<u32> = IntervalTree::build(Vec::new());
        for mode in [RangeEnd::Exclusive, RangeEnd::Inclusive] {
            assert_eq!(tree.innermost_containing(0, mode), None);
            assert_eq!(tree.innermost_containing(10, mode), None);
        }
    }

    #[test]
    fn selects_innermost_nested_and_breaks_equal_length_ties_by_order() {
        let entries = vec![
            SpanEntry { start: 0, end: 20, order: 0, payload: 10 },
            SpanEntry { start: 5, end: 15, order: 1, payload: 20 },
            SpanEntry { start: 5, end: 15, order: 2, payload: 30 },
            SpanEntry { start: 8, end: 8, order: 3, payload: 40 },
        ];
        let tree = IntervalTree::build(entries.clone());

        assert_eq!(tree.innermost_containing(6, RangeEnd::Exclusive), Some((20, 5..15)));
        assert_eq!(tree.innermost_containing(15, RangeEnd::Exclusive), Some((10, 0..20)));
        assert_eq!(tree.innermost_containing(15, RangeEnd::Inclusive), Some((20, 5..15)));
        assert_eq!(tree.innermost_containing(8, RangeEnd::Exclusive), Some((20, 5..15)));
        assert_eq!(tree.innermost_containing(8, RangeEnd::Inclusive), Some((40, 8..8)));
    }

    #[test]
    fn matches_linear_scan_across_mixed_nested_and_overlapping_intervals() {
        let mut entries = Vec::new();
        let mut seed = 0x1234_5678_9abc_def0_u64;
        for order in 0..240u32 {
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            let start = ((seed >> 32) % 120) as u32;
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            let len = ((seed >> 32) % 40) as u32;
            entries.push(SpanEntry { start, end: start + len, order, payload: order + 1 });
        }
        let tree = IntervalTree::build(entries.clone());

        for offset in 0..170 {
            for mode in [RangeEnd::Exclusive, RangeEnd::Inclusive] {
                assert_eq!(
                    tree.innermost_containing(offset, mode),
                    brute_force(&entries, offset, mode),
                    "mismatch at offset {offset}, mode {mode:?}"
                );
            }
        }
    }
}
