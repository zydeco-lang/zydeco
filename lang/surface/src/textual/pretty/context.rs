//! Typed descriptions of the parser grammar accepted at child positions.

/// Precedence levels of the `Term` nonterminal, ordered from tightest to
/// loosest. These mirror the levels documented in `parser.lalrpop`.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) enum TermPrecedence {
    Atom,
    Projection,
    Application,
    Product,
    Arrow,
    Quantifier,
    Binder,
}

/// The grammar accepted at one term child position.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum TermRequirement {
    /// Any ordinary `Term`, as accepted by `TermId`.
    Any,
    /// Any `TermAnn`, including a bare annotation, name, or label.
    Annotated,
    /// An ordinary term no looser than the given precedence.
    Through(TermPrecedence),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum RenderedTermClass {
    Term(TermPrecedence),
    AnnotatedOnly,
}

impl TermRequirement {
    pub(super) fn accepts(self, class: RenderedTermClass) -> bool {
        match (self, class) {
            | (Self::Annotated, _) => true,
            | (Self::Any, RenderedTermClass::Term(_)) => true,
            | (Self::Through(maximum), RenderedTermClass::Term(precedence)) => {
                precedence <= maximum
            }
            | (_, RenderedTermClass::AnnotatedOnly) => false,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum PatternRequirement {
    Pattern,
    Annotated,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum RenderedPatternClass {
    Pattern,
    AnnotatedOnly,
}

impl PatternRequirement {
    pub(super) fn accepts(self, class: RenderedPatternClass) -> bool {
        match (self, class) {
            | (Self::Annotated, _) => true,
            | (Self::Pattern, RenderedPatternClass::Pattern) => true,
            | (Self::Pattern, RenderedPatternClass::AnnotatedOnly) => false,
        }
    }
}
