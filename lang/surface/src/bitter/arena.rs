use super::syntax::*;
use crate::textual::syntax as t;

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and storage scope for desugared surface syntax.
#[derive(Debug)]
pub enum BitterScope {}

impl Allocates<DefId> for BitterScope {}
impl Allocates<PatId> for BitterScope {}
impl Allocates<TermId> for BitterScope {}

impl ArenaSchema<DefId> for BitterScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for BitterScope {
    type Item = Pattern;
}
impl ArenaSchema<TermId> for BitterScope {
    type Item = Term<VarName>;
}
/// One compact textual origin for every derived surface node.
///
/// A complete source program has one textual key space. Desugaring and the
/// resolver allocate derived IDs sequentially within their respective key
/// spaces, so each page stores the repeated key spaces and raw derived IDs
/// implicitly.
#[derive(Clone, Debug, Default)]
pub struct TextualOrigins {
    textual_key_space: Option<KeySpaceId>,
    pages: ArenaAssoc<KeySpaceId, OriginPage>,
}

#[derive(Clone, Debug, Default)]
struct OriginPage {
    derived_categories: Vec<DerivedCategory>,
    textual_origins: Vec<CompactTextualOrigin>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
enum DerivedCategory {
    Definition,
    Pattern,
    Term,
}

impl DerivedCategory {
    fn split(entity: EntityId) -> (Self, KeySpaceId, RawIdx) {
        match entity {
            | EntityId::Def(id) => (Self::Definition, id.key_space(), id.raw()),
            | EntityId::Pat(id) => (Self::Pattern, id.key_space(), id.raw()),
            | EntityId::Term(id) => (Self::Term, id.key_space(), id.raw()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CompactTextualOrigin {
    raw: RawIdx,
    category: t::EntityCategory,
}

impl CompactTextualOrigin {
    fn split(entity: t::EntityId) -> (KeySpaceId, Self) {
        let (category, key_space, raw) = entity.into_parts();
        (key_space, Self { raw, category })
    }

    fn join(self, key_space: KeySpaceId) -> t::EntityId {
        self.category.restore(key_space, self.raw)
    }
}

impl TextualOrigins {
    pub fn insert_new(&mut self, textual: t::EntityId, derived: EntityId) {
        let (textual_key_space, textual) = CompactTextualOrigin::split(textual);
        match self.textual_key_space {
            | Some(existing) => assert_eq!(
                existing, textual_key_space,
                "textual origins must belong to one source program",
            ),
            | None => self.textual_key_space = Some(textual_key_space),
        }

        let (category, derived_key_space, raw) = DerivedCategory::split(derived);
        let page = self.pages.entry(derived_key_space).or_default();
        assert_eq!(
            raw.into_u32() as usize,
            page.textual_origins.len(),
            "derived IDs must follow their allocation sequence",
        );
        page.derived_categories.push(category);
        page.textual_origins.push(textual);
    }

    pub fn source(&self, derived: &EntityId) -> Option<t::EntityId> {
        let (category, key_space, raw) = DerivedCategory::split(*derived);
        let page = self.pages.get(&key_space)?;
        let index = raw.into_u32() as usize;
        let origin = *page.textual_origins.get(index)?;
        (page.derived_categories.get(index) == Some(&category)).then_some(origin).map(|origin| {
            origin.join(
                self.textual_key_space
                    .expect("a nonempty textual-origin arena has a textual key space"),
            )
        })
    }
}

/// Storage for all bitter syntax nodes plus their textual origins.
#[derive(Default, Debug)]
pub struct BitterArena {
    // arenas
    pub defs: ArenaSparse<BitterScope, DefId>,
    pub pats: ArenaSparse<BitterScope, PatId>,
    pub terms: ArenaSparse<BitterScope, TermId>,

    /// Textual source origin of every derived entity.
    pub origins: TextualOrigins,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::textual::arena::TextualScope;
    use std::mem::size_of;

    #[test]
    fn textual_origins_round_trip_dense_typed_ids() {
        let mut textual = IdAllocator::<TextualScope>::new();
        let source_definition: t::DefId = textual.alloc();
        let source_pattern: t::PatId = textual.alloc();
        let source_copattern: t::CoPatId = textual.alloc();
        let source_term: t::TermId = textual.alloc();
        let mut derived = IdAllocator::<BitterScope>::new();
        let definition: DefId = derived.alloc();
        let pattern: PatId = derived.alloc();
        let copattern_term: TermId = derived.alloc();
        let source_term_term: TermId = derived.alloc();
        let mut origins = TextualOrigins::default();

        origins.insert_new(source_definition.into(), definition.into());
        origins.insert_new(source_pattern.into(), pattern.into());
        origins.insert_new(source_copattern.into(), copattern_term.into());
        origins.insert_new(source_term.into(), source_term_term.into());

        assert_eq!(size_of::<CompactTextualOrigin>(), 8);
        assert_eq!(size_of::<DerivedCategory>(), 1);
        assert_eq!(origins.source(&definition.into()), Some(source_definition.into()));
        assert_eq!(origins.source(&pattern.into()), Some(source_pattern.into()));
        assert_eq!(origins.source(&copattern_term.into()), Some(source_copattern.into()));
        assert_eq!(origins.source(&source_term_term.into()), Some(source_term.into()));

        let wrong_category: PatId = restore_id(definition.key_space(), definition.raw());
        assert_eq!(origins.source(&wrong_category.into()), None);
    }

    #[test]
    #[should_panic(expected = "derived IDs must follow their allocation sequence")]
    fn textual_origins_reject_allocation_gaps() {
        let mut textual = IdAllocator::<TextualScope>::new();
        let source: t::TermId = textual.alloc();
        let mut derived = IdAllocator::<BitterScope>::new();
        let _: TermId = derived.alloc();
        let term: TermId = derived.alloc();
        TextualOrigins::default().insert_new(source.into(), term.into());
    }

    #[test]
    #[should_panic(expected = "textual origins must belong to one source program")]
    fn textual_origins_reject_multiple_source_key_spaces() {
        let mut first_textual = IdAllocator::<TextualScope>::new();
        let mut second_textual = IdAllocator::<TextualScope>::new();
        let first_source: t::DefId = first_textual.alloc();
        let second_source: t::PatId = second_textual.alloc();
        let mut derived = IdAllocator::<BitterScope>::new();
        let definition: DefId = derived.alloc();
        let pattern: PatId = derived.alloc();
        let mut origins = TextualOrigins::default();

        origins.insert_new(first_source.into(), definition.into());
        origins.insert_new(second_source.into(), pattern.into());
    }
}
