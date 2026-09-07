//! Source identities for nominal type mismatches, independent of arena numbering.

use super::*;
use zydeco_syntax::{SpanStore, TextualBack};
use zydeco_utils::arena::ArenaAccess;

pub(super) struct NominalMismatch {
    expected: AbstId,
    found: AbstId,
}

impl NominalMismatch {
    pub(super) fn new(tycker: &Tycker<'_>, error: &TyckError) -> Option<Self> {
        let TyckError::TypeMismatch { expected, found } = error else { return None };
        let Fillable::Done(Type::Abst(expected)) = tycker.statics.types_pre.get(expected)? else {
            return None;
        };
        let Fillable::Done(Type::Abst(found)) = tycker.statics.types_pre.get(found)? else {
            return None;
        };
        (expected != found).then_some(Self { expected: *expected, found: *found })
    }

    fn name(tycker: &Tycker<'_>, identity: AbstId) -> String {
        tycker.statics.abst_hints.get(&identity).map_or_else(
            || "abstract type".to_owned(),
            |definition| tycker.statics.def_name(tycker.scoped, definition).plain().to_owned(),
        )
    }

    fn span(tycker: &Tycker<'_>, identity: AbstId) -> Option<Span> {
        let definition = tycker.statics.abst_hints.get(&identity)?;
        let source = tycker.scoped.textual_back((*definition).into())?;
        Some(*tycker.spans.span(source))
    }

    pub(super) fn message(&self, tycker: &Tycker<'_>) -> String {
        format!(
            "Type mismatch: expected {} (identity 1), found {} (identity 2)",
            Self::name(tycker, self.expected),
            Self::name(tycker, self.found),
        )
    }

    pub(super) fn labels(&self, tycker: &Tycker<'_>) -> [(Option<Span>, &'static str); 2] {
        [(self.expected, true), (self.found, false)].map(|(identity, expected)| {
            let message = if tycker.statics.seals.get(&identity).is_some() {
                if expected {
                    "expected type (identity 1) is sealed here"
                } else {
                    "found type (identity 2) is sealed here"
                }
            } else if tycker.statics.existential_skolems.get(&identity).is_some() {
                if expected {
                    "expected type (identity 1) is opened here"
                } else {
                    "found type (identity 2) is opened here"
                }
            } else if expected {
                "expected type (identity 1) is introduced here"
            } else {
                "found type (identity 2) is introduced here"
            };
            (Self::span(tycker, identity), message)
        })
    }

    pub(super) fn help() -> String {
        "these are distinct abstract types; matching names or hidden representations do not make their identities interchangeable".to_owned()
    }
}
