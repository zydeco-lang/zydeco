//! Typed descriptions of the parser grammar accepted at child positions.

use super::super::syntax::*;

/// The parser-side grammar facts needed while reconstructing parentheses.
pub(super) struct GrammarContext<'arena> {
    arena: &'arena TextArena,
}

impl<'arena> GrammarContext<'arena> {
    pub(super) fn new(arena: &'arena TextArena) -> Self {
        Self { arena }
    }

    pub(super) fn accepts_term(&self, requirement: TermRequirement, term: TermId) -> bool {
        requirement.accepts(self.term_class(term))
    }

    pub(super) fn accepts_pattern(&self, requirement: PatternRequirement, pattern: PatId) -> bool {
        requirement.accepts(self.pattern_class(pattern))
    }

    fn term_class(&self, term: TermId) -> RenderedTermClass {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner)) => self.term_class(*inner),
            | Term::SignatureBoundary(SignatureBoundary(inner)) => self.term_class(*inner),
            | Term::Named(_) | Term::Label(_) => RenderedTermClass::AnnotatedOnly,
            // These constructors either are atoms in the grammar or are
            // deliberately rendered with their own delimiters.
            | Term::Ann(_)
            | Term::Hole(_)
            | Term::Var(_)
            | Term::Paren(_)
            | Term::Thunk(_)
            | Term::Block(_)
            | Term::Data(_)
            | Term::CoData(_)
            | Term::Match(_)
            | Term::CoMatch(_)
            | Term::Pack(_)
            | Term::Lit(_) => RenderedTermClass::Term(TermPrecedence::Atom),
            | Term::Proj(_) => RenderedTermClass::Term(TermPrecedence::Projection),
            | Term::Force(_) | Term::Ret(_) | Term::Ctor(_) => {
                RenderedTermClass::Term(TermPrecedence::Prefix)
            }
            | Term::App(_) | Term::Dtor(_) => RenderedTermClass::Term(TermPrecedence::Application),
            | Term::Prod(_) => RenderedTermClass::Term(TermPrecedence::Product),
            | Term::Arrow(_) => RenderedTermClass::Term(TermPrecedence::Arrow),
            | Term::Pipeline(Pipeline { direction: PipelineDirection::Forward, .. }) => {
                RenderedTermClass::Term(TermPrecedence::ForwardCut)
            }
            | Term::Pipeline(Pipeline { direction: PipelineDirection::Backward, .. }) => {
                RenderedTermClass::Term(TermPrecedence::BackwardCut)
            }
            | Term::Pi(_) | Term::ValPi(_) | Term::Forall(_) | Term::Sigma(_) | Term::Exists(_) => {
                RenderedTermClass::Term(TermPrecedence::Quantifier)
            }
            | Term::Meta(_)
            | Term::Abs(_)
            | Term::ValAbs(_)
            | Term::Fix(_)
            | Term::Do(_)
            | Term::Let(_)
            | Term::Param(_)
            | Term::ContextBind(_) => RenderedTermClass::Term(TermPrecedence::Binder),
        }
    }

    fn pattern_class(&self, pattern: PatId) -> RenderedPatternClass {
        match &self.arena.pats[&pattern] {
            | Pattern::Named(_) | Pattern::Project(_) => RenderedPatternClass::AnnotatedOnly,
            // Annotations and manifest patterns include their own parentheses
            // in canonical output, so their rendered form is an ordinary
            // `Pattern` even though their payload grammar is `PatternAnn`.
            | Pattern::Ann(_)
            | Pattern::Manifest(_)
            | Pattern::Hole(_)
            | Pattern::Var(_)
            | Pattern::Ctor(_)
            | Pattern::View(_)
            | Pattern::Alias(_)
            | Pattern::Paren(_) => RenderedPatternClass::Pattern,
        }
    }
}

/// Precedence levels of the `Term` nonterminal, ordered from tightest to
/// loosest. These mirror the levels documented in `parser.lalrpop`.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) enum TermPrecedence {
    Atom,
    Projection,
    Prefix,
    Application,
    Product,
    Arrow,
    ForwardCut,
    BackwardCut,
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
enum RenderedTermClass {
    Term(TermPrecedence),
    AnnotatedOnly,
}

impl TermRequirement {
    fn accepts(self, class: RenderedTermClass) -> bool {
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
enum RenderedPatternClass {
    Pattern,
    AnnotatedOnly,
}

impl PatternRequirement {
    fn accepts(self, class: RenderedPatternClass) -> bool {
        match (self, class) {
            | (Self::Annotated, _) => true,
            | (Self::Pattern, RenderedPatternClass::Pattern) => true,
            | (Self::Pattern, RenderedPatternClass::AnnotatedOnly) => false,
        }
    }
}
