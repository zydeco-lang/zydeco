//! Configurable pretty printing for canonical textual syntax.

mod context;
mod punning;
mod forms;
mod layout;
mod patterns;
mod terms;
#[cfg(test)]
mod corpus;
#[cfg(test)]
mod tests;

use crate::{
    config::{LayoutIntentions, Parentheses, PrettyOptions},
    intention::*,
    presentation::FormatPresentation,
    trivia::*,
};
use context::{GrammarContext, PatternRequirement, TermPrecedence, TermRequirement};
use layout::*;
use pretty::{DocAllocator, RcAllocator, RcDoc};
use punning::{PunnedPatternPayload, PunnedTermPayload, Punning};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use zydeco_surface::{metadata::FormatMeta, textual::syntax::*};
use zydeco_syntax::{Pretty, ViewSpine};

static DOC_ALLOCATOR: RcAllocator = RcAllocator;

/// A pretty printer over one parsed textual arena.
pub struct PrettyFormatter<'arena> {
    arena: &'arena TextArena,
    spans: &'arena SpanArena,
    presentation: &'arena FormatPresentation,
    source: Option<&'arena str>,
    grammar: GrammarContext<'arena>,
    punning: Punning<'arena>,
    options: PrettyOptions,
    documents: Rc<LayoutDocuments<'arena>>,
}

/// Alternative layouts share child documents instead of rebuilding their subtrees.
/// Policy is part of the key because format annotations can change it locally.
#[derive(Default)]
struct LayoutDocuments<'arena> {
    terms: RefCell<HashMap<(TermId, TermRequirement, PrettyOptions), RcDoc<'arena>>>,
    patterns: RefCell<HashMap<(PatId, PatternRequirement, PrettyOptions), RcDoc<'arena>>>,
}

impl<'arena> PrettyFormatter<'arena> {
    pub fn new(
        arena: &'arena TextArena, spans: &'arena SpanArena,
        presentation: &'arena FormatPresentation,
    ) -> Self {
        Self::with_options(arena, spans, presentation, PrettyOptions::default())
    }

    pub fn with_options(
        arena: &'arena TextArena, spans: &'arena SpanArena,
        presentation: &'arena FormatPresentation, options: PrettyOptions,
    ) -> Self {
        Self {
            arena,
            spans,
            presentation,
            source: None,
            grammar: GrammarContext::new(arena),
            punning: Punning::new(arena, &presentation.trivia),
            options,
            documents: Rc::default(),
        }
    }

    pub fn with_source(
        arena: &'arena TextArena, spans: &'arena SpanArena,
        presentation: &'arena FormatPresentation, source: &'arena str,
    ) -> Self {
        Self::with_options_source(arena, spans, presentation, PrettyOptions::default(), source)
    }

    pub fn with_options_source(
        arena: &'arena TextArena, spans: &'arena SpanArena,
        presentation: &'arena FormatPresentation, options: PrettyOptions, source: &'arena str,
    ) -> Self {
        Self {
            arena,
            spans,
            presentation,
            source: Some(source),
            grammar: GrammarContext::new(arena),
            punning: Punning::new(arena, &presentation.trivia),
            options,
            documents: Rc::default(),
        }
    }

    /// Render one subtree with different policy over the same arena.
    fn scoped(&self, options: PrettyOptions) -> Self {
        Self {
            arena: self.arena,
            spans: self.spans,
            presentation: self.presentation,
            source: self.source,
            grammar: GrammarContext::new(self.arena),
            punning: Punning::new(self.arena, &self.presentation.trivia),
            options,
            documents: self.documents.clone(),
        }
    }

    fn indent(&self) -> isize {
        self.options.indent.nesting()
    }

    /// Render one complete source unit with a trailing newline.
    pub fn render_unit(&self, unit: SourceUnit) -> String {
        let document = self.with_trailing_comments(unit.root.into(), self.term(unit.root));
        self.render_doc(document.append(RcDoc::hardline()))
    }

    fn render_doc(&self, document: RcDoc<'arena>) -> String {
        let mut output = String::new();
        document.render_fmt(self.options.line_width, &mut output).unwrap();
        output
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for SourceUnit {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.term(self.root)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for DefId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.definition(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for PatId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.pattern(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for CoPatId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.copattern(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for TermId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.term(*self)
    }
}
