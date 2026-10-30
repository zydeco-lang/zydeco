//! Invariants that every successful check leaves in its finished typed arena.
//!
//! Later phases rely on them without rechecking: every inference hole is solved,
//! every node carries an annotation of its category's sort, and no residual hole survives.
//! Allocations abandoned by inference retries are held to the same rules.

use zydeco_statics::{
    ArenaAccess,
    arena::StaticsArena,
    surface_syntax as su,
    syntax::{
        CompuId, Computation, FillId, Fillable, Kind, KindId, TPatId, TermAnnId, TypeId, VPatId,
        Value, ValueId,
    },
};

/// A node whose category fixes the sort of its annotation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AnnotatedNode {
    Value(ValueId),
    Compu(CompuId),
    VPat(VPatId),
    TPat(TPatId),
    Type(TypeId),
}

/// The sort that an annotation's kind must have.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AnnotationSort {
    VType,
    CType,
}

/// Where a term annotation was recorded.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AnnotationSite {
    Root,
    Source(su::TermId),
}

/// One invariant that a finished typed arena violates.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ArenaViolation {
    UnfilledKind {
        kind: KindId,
        fill: FillId,
    },
    UnfilledType {
        ty: TypeId,
        fill: FillId,
    },
    MissingAnnotation {
        node: AnnotatedNode,
    },
    WrongSort {
        node: AnnotatedNode,
        ty: TypeId,
        expected: AnnotationSort,
    },
    /// A value hole that is not the placeholder of a foreign import.
    ResidualHoleValue {
        value: ValueId,
    },
    ResidualHoleComputation {
        compu: CompuId,
    },
    ResidualHoleAnnotation {
        site: AnnotationSite,
    },
}

/// The finished arena of one successful check.
pub struct ArenaInvariants<'a> {
    pub statics: &'a StaticsArena,
}

impl ArenaInvariants<'_> {
    /// Every violated invariant, starting from the root annotation of the check.
    pub fn violations(&self, root: TermAnnId) -> Vec<ArenaViolation> {
        [self.fill_closure(), self.annotation_presence(), self.annotation_sorts(), self.holes(root)]
            .into_iter()
            .flatten()
            .collect()
    }

    /// Every kind and type cell has left its hole form.
    fn fill_closure(&self) -> Vec<ArenaViolation> {
        let kinds = self.statics.kinds_pre.iter().filter_map(|(kind, value)| match value {
            | Fillable::Fill(fill) => {
                Some(ArenaViolation::UnfilledKind { kind: *kind, fill: *fill })
            }
            | Fillable::Done(_) => None,
        });
        let types = self.statics.types_pre.iter().filter_map(|(ty, value)| match value {
            | Fillable::Fill(fill) => Some(ArenaViolation::UnfilledType { ty, fill: *fill }),
            | Fillable::Done(_) => None,
        });
        kinds.chain(types).collect()
    }

    /// Every allocated node appears in its annotation table.
    ///
    /// Abstract-type witnesses are exempt: their kind may live in their own table,
    /// on a type node that denotes them, or in an enclosing binder.
    fn annotation_presence(&self) -> Vec<ArenaViolation> {
        let statics = self.statics;
        let missing = |node| ArenaViolation::MissingAnnotation { node };
        let values = (statics.values.iter())
            .filter(|(value, _)| statics.annotations_value.get(value).is_none())
            .map(|(value, _)| missing(AnnotatedNode::Value(*value)));
        let compus = (statics.compus.iter())
            .filter(|(compu, _)| statics.annotations_compu.get(compu).is_none())
            .map(|(compu, _)| missing(AnnotatedNode::Compu(*compu)));
        let vpats = (statics.vpats.iter())
            .filter(|(pat, _)| statics.annotations_vpat.get(pat).is_none())
            .map(|(pat, _)| missing(AnnotatedNode::VPat(*pat)));
        let tpats = (statics.tpats.iter())
            .filter(|(pat, _)| statics.annotations_tpat.get(pat).is_none())
            .map(|(pat, _)| missing(AnnotatedNode::TPat(*pat)));
        let types = (statics.types_pre.iter())
            .filter(|(ty, _)| statics.type_kind_at(*ty).is_none())
            .map(|(ty, _)| missing(AnnotatedNode::Type(ty)));
        values.chain(compus).chain(vpats).chain(tpats).chain(types).collect()
    }

    /// Value and pattern annotations are value types; computation annotations are computation types.
    fn annotation_sorts(&self) -> Vec<ArenaViolation> {
        let annotated = (self.statics.annotations_value.iter())
            .map(|(value, ty)| (AnnotatedNode::Value(*value), *ty, AnnotationSort::VType))
            .chain(
                (self.statics.annotations_compu.iter())
                    .map(|(compu, ty)| (AnnotatedNode::Compu(*compu), *ty, AnnotationSort::CType)),
            )
            .chain(
                (self.statics.annotations_vpat.iter())
                    .map(|(pat, ty)| (AnnotatedNode::VPat(*pat), *ty, AnnotationSort::VType)),
            );
        annotated
            .filter(|(_, ty, expected)| self.sort(*ty) != Some(*expected))
            .map(|(node, ty, expected)| match self.statics.type_kind_at(ty) {
                | Some(_) => ArenaViolation::WrongSort { node, ty, expected },
                | None => ArenaViolation::MissingAnnotation { node: AnnotatedNode::Type(ty) },
            })
            .collect()
    }

    fn sort(&self, ty: TypeId) -> Option<AnnotationSort> {
        match self.statics.normalized_kind_at(self.statics.type_kind_at(ty)?) {
            | Some(Kind::VType(_)) => Some(AnnotationSort::VType),
            | Some(Kind::CType(_)) => Some(AnnotationSort::CType),
            | _ => None,
        }
    }

    /// No term, recorded annotation, or root keeps a hole after a successful check.
    fn holes(&self, root: TermAnnId) -> Vec<ArenaViolation> {
        let statics = self.statics;
        let values = (statics.values.iter())
            .filter(|(value, node)| {
                matches!(node, Value::Hole(_)) && statics.foreign_imports.get(value).is_none()
            })
            .map(|(value, _)| ArenaViolation::ResidualHoleValue { value: *value });
        let compus = (statics.compus.iter())
            .filter(|(_, node)| matches!(node, Computation::Hole(_)))
            .map(|(compu, _)| ArenaViolation::ResidualHoleComputation { compu: *compu });
        let annotations = std::iter::once((AnnotationSite::Root, root))
            .chain(
                (statics.term_facts.iter())
                    .map(|(term, facts)| (AnnotationSite::Source(term), facts.annotation())),
            )
            .filter(|(_, annotation)| matches!(annotation, TermAnnId::Hole(_)))
            .map(|(site, _)| ArenaViolation::ResidualHoleAnnotation { site });
        values.chain(compus).chain(annotations).collect()
    }
}
