use super::{
    AnalysisError, CompilerSession, QuerySourceProvider, SourceInput, SourceQueryDb, source_text,
};
use crate::source::{
    BitterProgram, SourceLoadError, SourceTemplate,
    loader::{SourceGraphLoader, SourceProvider},
};
use std::{ops::Range, path::Path, sync::Arc};
use thiserror::Error;
use zydeco_statics::{
    arena::StaticsArena,
    check::{AnnotationCompatibility, CompletionTyping},
    query::{CompletionInput, CompletionTyckOutput, ScopedData, check_completion},
    syntax::{AnnId, DefId},
};
use zydeco_surface::{
    scoped::{CompletionResolution, Resolver, VisibleDefinition, arena::ScopedArena},
    textual::{
        CompletionCursor, CompletionCursorError, LexicalTokenKind, ParsedHole, RecoveringParser,
        TokenKind, syntax::Parser,
    },
};
use zydeco_utils::{arena::ArenaAccess, span::FileMap};

/// Visible names and optional classifiers owned by one current-source completion query.
#[derive(Debug)]
pub struct CompletionAnalysis {
    pub source: String,
    pub replacement: Range<usize>,
    pub candidates: Vec<VisibleDefinition>,
    pub semantics: Option<CompletionSemantics>,
}

/// The arenas owning completion annotations, never an older strict analysis's IDs.
#[derive(Debug)]
pub struct CompletionSemantics {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub typing: CompletionTyping,
}

impl CompletionSemantics {
    pub fn annotation(&self, definition: DefId) -> Option<AnnId> {
        self.statics.annotations_var.get(&definition).copied()
    }

    pub fn compatibility(&self, definition: DefId) -> AnnotationCompatibility {
        self.typing.compatibility(definition)
    }
}

#[derive(Clone, Debug, Error)]
pub enum CompletionError {
    #[error(transparent)]
    Cursor(#[from] CompletionCursorError),
    #[error(transparent)]
    Analysis(#[from] AnalysisError),
}

impl CompilerSession {
    /// Complete ordinary names at a byte cursor without changing strict source inputs or analyses.
    /// An unavailable or non-reference syntactic site yields no result.
    pub fn complete(
        &self, root: impl AsRef<Path>, offset: usize,
    ) -> Result<Option<Arc<CompletionAnalysis>>, CompletionError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        complete_source(self, root, offset)
    }
}

/// Only the edited root uses recovered syntax. Dependencies use the ordinary
/// provider, including overlays, companion signatures, and hygienic import edges.
struct CompletionSourceProvider<'db> {
    root: Arc<SourceTemplate>,
    ordinary: QuerySourceProvider<'db>,
}

impl SourceProvider for CompletionSourceProvider<'_> {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError> {
        if path == self.root.path { Ok(Arc::clone(&self.root)) } else { self.ordinary.load(path) }
    }

    fn load_optional(
        &mut self, path: &Path,
    ) -> Result<Option<Arc<SourceTemplate>>, SourceLoadError> {
        if path == self.root.path {
            Ok(Some(Arc::clone(&self.root)))
        } else {
            self.ordinary.load_optional(path)
        }
    }
}

// A completion owns transient arenas. Retain only the most recent result while
// leaving strict parsing and analysis query identities untouched.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
fn complete_source(
    db: &dyn SourceQueryDb, root: SourceInput, offset: usize,
) -> Result<Option<Arc<CompletionAnalysis>>, CompletionError> {
    let Some(source) = source_text(db, root) else {
        return Ok(None);
    };
    let cursor = CompletionCursor::at(&source, offset)?;
    let prefix = match cursor.token_kind() {
        | None
        | Some(
            LexicalTokenKind::UpperIdentifier
            | LexicalTokenKind::LowerIdentifier
            | LexicalTokenKind::Keyword,
        ) => cursor.prefix().to_owned(),
        | Some(LexicalTokenKind::Hole) => String::new(),
        | Some(_) => return Ok(None),
    };
    let mut parser = Parser::new();
    let parsed = RecoveringParser::with_completion(cursor).source(&mut parser);
    let Some(unit) = parsed.syntax else {
        return Ok(None);
    };
    let Some(site) = parsed.completion else {
        return Ok(None);
    };
    // Recovery can pop a projection or other restricted-name position into a
    // term hole. Require a hole to have been legal at the original cursor too.
    if !site.expected.contains(&TokenKind::Hole) {
        return Ok(None);
    }
    let Some(ParsedHole::Term(target)) = site.hole.map(|hole| hole.entity) else {
        return Ok(None);
    };
    let path = root.path(db);
    let file = FileMap::local(source.as_str(), Some(Arc::new(path.clone())));
    let template = SourceTemplate::with_syntax(path.clone(), source.clone(), file, parser, unit)
        .map_err(|error| AnalysisError::Source {
            error: Arc::new(SourceLoadError::Parse(error)),
        })?;
    let provider =
        CompletionSourceProvider { root: Arc::new(template), ordinary: QuerySourceProvider { db } };
    let graph = SourceGraphLoader::with_provider(provider)
        .load_root(&path)
        .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
    let (program, target) =
        graph.parse_completion(target).map_err(|error| AnalysisError::TextualProgram { error })?;
    let Some(target) = target else {
        return Ok(None);
    };
    let BitterProgram { spans, arena, prim, root } = program.desugar().map_err(|failure| {
        AnalysisError::Desugar { error: failure.error, spans: Arc::new(failure.spans.into_inner()) }
    })?;
    let CompletionResolution { site: resolved, program, .. } =
        Resolver::new(&spans, arena, prim).run_completion(root, target);
    let Some(resolved) = resolved else {
        return Ok(None);
    };
    let mut candidates = resolved
        .scope
        .definitions
        .into_iter()
        .filter(|definition| definition.name.0.starts_with(&prefix))
        .collect::<Vec<_>>();
    let semantics = program.ok().filter(|_| !candidates.is_empty()).map(|program| {
        let data = ScopedData::new(
            db,
            Arc::new(spans.into_inner()),
            program.prim,
            Arc::new(program.arena.into_inner()),
            program.root,
        );
        let request = CompletionInput::new(
            db,
            resolved.target,
            candidates.iter().map(|candidate| candidate.definition).collect::<Vec<_>>(),
        );
        let CompletionTyckOutput { source, typing } = check_completion(db, data, request);
        CompletionSemantics { scoped: source.scoped, statics: source.outcome.statics_arc(), typing }
    });
    candidates.retain(|candidate| {
        !semantics.as_ref().is_some_and(|semantics| {
            semantics.compatibility(candidate.definition) == AnnotationCompatibility::Mismatch
        })
    });
    candidates.sort_by(|left, right| {
        let compatibility = |candidate: &VisibleDefinition| {
            semantics
                .as_ref()
                .map(|semantics| semantics.compatibility(candidate.definition))
                .unwrap_or_default()
        };
        (left.name.0 != prefix, compatibility(left), left.distance, &left.name.0).cmp(&(
            right.name.0 != prefix,
            compatibility(right),
            right.distance,
            &right.name.0,
        ))
    });
    Ok(Some(Arc::new(CompletionAnalysis {
        source,
        replacement: site.replacement,
        candidates,
        semantics,
    })))
}

#[cfg(test)]
mod tests;
