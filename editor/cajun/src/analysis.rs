use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DocumentSymbol, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, NumberOrString, Position,
    PrepareRenameResponse, Range, SemanticToken, SymbolKind, Url, WorkspaceEdit,
};
use zydeco_session::{
    AnalysisError, CompilerSession, ProgramAnalysis, SourceDiagnosticSite, SourceLoadError,
};
use zydeco_statics::{
    arena::StaticsArena,
    fmt::{Formatter, SealedTypeEquation},
    syntax::{AbstId, AnnId, TermAnnId, TermId as TypedTermId},
};
use zydeco_surface::{
    scoped::arena::ScopedArena,
    scoped::syntax::{DefId, Term, TermId},
    textual::syntax::EntityId,
};
use zydeco_syntax::Pretty;
use zydeco_utils::{
    arena::ArenaAccess,
    span::{FileMap, LineCol, Span},
};

use crate::{
    hover::{
        HoverLineWidth, HoverSignature, SealedTypeEquationPreview, TypeDefinitionLink,
        TypeDefinitionPreview,
    },
    progress::{AnalysisProgress, SourceDiscovery},
    rename::{RenameRejection, Renamer},
    semantic::SemanticHighlighter,
    type_links::TypeReferenceCollector,
};

/// Compiler analysis state for one editor root.
pub(crate) struct ProjectState {
    analysis: Arc<ProgramAnalysis>,
    /// The materialized typed arena of the open root. The project is a live
    /// consumer of its root, so it holds the occurrence payload while the
    /// project lives; the session and its salsa memo only retain the latest
    /// root on demand.
    statics: Arc<StaticsArena>,
    /// The analyzed root, for fact lookups against the session's memoized
    /// queries.
    root: PathBuf,
    file_maps: HashMap<PathBuf, FileMap>,
    semantic_path: PathBuf,
    semantic_tokens: Vec<SemanticToken>,
}

#[derive(Copy, Clone)]
struct SymbolOccurrence {
    definition: DefId,
    range: Range,
}

#[derive(Copy, Clone, Debug)]
enum ProjectFailureOrigin {
    Compiler,
    Cajun,
}

impl ProjectFailureOrigin {
    fn source(self) -> &'static str {
        match self {
            | Self::Compiler => "zydeco",
            | Self::Cajun => "cajun",
        }
    }
}

/// A failed project refresh that retains compiler source provenance.
#[derive(Debug)]
pub(crate) struct ProjectFailure {
    message: String,
    site: Option<SourceDiagnosticSite>,
    origin: ProjectFailureOrigin,
}

impl ProjectFailure {
    fn compiler(message: impl Into<String>, site: Option<SourceDiagnosticSite>) -> Self {
        Self { message: message.into(), site, origin: ProjectFailureOrigin::Compiler }
    }

    fn from_source_error(error: &SourceLoadError) -> Self {
        Self::compiler(error.to_string(), error.diagnostic_site())
    }

    fn from_analysis_error(error: &AnalysisError) -> Self {
        Self::compiler(error.to_string(), error.diagnostic_site())
    }

    pub(crate) fn internal(message: impl Into<String>) -> Self {
        Self { message: message.into(), site: None, origin: ProjectFailureOrigin::Cajun }
    }

    fn range(&self, published_path: &Path, source: &str) -> Option<Range> {
        let site = self.site.as_ref()?;
        if ProjectState::normalize_path(site.path()) != ProjectState::normalize_path(published_path)
        {
            return None;
        }
        let file = FileMap::local(source, None);
        Some(Range::new(
            ProjectState::position(file.line_col_utf16(site.range().start)?),
            ProjectState::position(file.line_col_utf16(site.range().end)?),
        ))
    }

    pub(crate) fn diagnostic(
        &self, published_path: Option<&Path>, source: Option<&str>,
    ) -> Diagnostic {
        let range = published_path
            .zip(source)
            .and_then(|(path, source)| self.range(path, source))
            .unwrap_or_default();
        Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some(self.origin.source().to_owned()),
            message: self.message.clone(),
            ..Diagnostic::default()
        }
    }
}

impl ProjectState {
    #[cfg(test)]
    pub(crate) fn load(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> Result<(Self, CompilerSession), ProjectFailure> {
        Self::load_with_progress(source_path, overrides, |_| {})
    }

    #[cfg(test)]
    pub(crate) fn load_with_progress(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
        progress: impl FnMut(AnalysisProgress),
    ) -> Result<(Self, CompilerSession), ProjectFailure> {
        let mut session = CompilerSession::default();
        overrides.iter().try_for_each(|(path, source)| {
            session
                .set_overlay(path, source.clone())
                .map_err(|error| ProjectFailure::from_source_error(&error))
        })?;
        Self::load_from_session(source_path, &session, progress).map(|project| (project, session))
    }

    pub(crate) fn load_from_session(
        source_path: &Path, session: &CompilerSession, mut progress: impl FnMut(AnalysisProgress),
    ) -> Result<Self, ProjectFailure> {
        let graph = session
            .graph(source_path)
            .map_err(|error| ProjectFailure::from_source_error(&error))?;
        let source_count = graph.sources.len();
        graph.sources.iter().enumerate().for_each(|(index, (_, source))| {
            progress(AnalysisProgress::Loading(SourceDiscovery {
                path: source.path.clone(),
                discovered: index + 1,
            }))
        });
        progress(AnalysisProgress::Parsing { source_count });
        progress(AnalysisProgress::Desugaring { source_count });
        progress(AnalysisProgress::Resolving { source_count });
        progress(AnalysisProgress::Tycking { source_count });
        let analysis = session
            .analyze(source_path)
            .map_err(|error| ProjectFailure::from_analysis_error(&error))?;
        let statics = session
            .materialize_arena(&analysis)
            .map_err(|error| ProjectFailure::from_analysis_error(&error))?;
        let scoped = analysis.scoped();
        let file_maps = analysis
            .sources()
            .map(|(path, source)| {
                let path = Self::normalize_path(path);
                (path.clone(), FileMap::local(source, Some(Arc::new(path))))
            })
            .collect();
        let source_path = Self::normalize_path(source_path);
        let source = analysis.source(&source_path).ok_or_else(|| {
            ProjectFailure::internal(format!(
                "the parsed program omitted `{}`",
                source_path.display()
            ))
        })?;
        progress(AnalysisProgress::Highlighting { path: source_path.clone() });
        let tokens = SemanticHighlighter::compiler_refined(
            source,
            &source_path,
            analysis.spans(),
            scoped,
            Some(&statics),
        );
        let semantic_path = source_path;
        let semantic_tokens = tokens;

        Ok(Self {
            analysis,
            statics,
            root: semantic_path.clone(),
            file_maps,
            semantic_path,
            semantic_tokens,
        })
    }

    pub(crate) fn definition(&self, file_path: &Path, position: Position) -> Option<Location> {
        let occurrence = self.symbol_at(file_path, position)?;
        self.definition_location(occurrence.definition)
    }

    pub(crate) fn references(
        &self, file_path: &Path, position: Position, include_declaration: bool,
    ) -> Option<Vec<Location>> {
        let definition = self.symbol_at(file_path, position)?.definition;
        let declaration =
            include_declaration.then(|| self.definition_location(definition)).into_iter().flatten();
        let uses = self
            .scoped()
            .users
            .forth(&definition)
            .iter()
            .filter_map(|term| self.term_location(*term));
        Some(Self::ordered_locations(declaration.chain(uses).collect()))
    }

    /// Offer the identifier under the position for interactive renaming.
    ///
    /// Names introduced by data, codata, and comatch arms resolve during type
    /// checking rather than name resolution, so they have no resolver-backed
    /// occurrence set and are not offered.
    pub(crate) fn prepare_rename(
        &self, file_path: &Path, position: Position,
    ) -> Option<PrepareRenameResponse> {
        let occurrence = self.symbol_at(file_path, position)?;
        self.definition_location(occurrence.definition)?;
        let placeholder = self.scoped().defs[&occurrence.definition].0.clone();
        Some(PrepareRenameResponse::RangeWithPlaceholder { range: occurrence.range, placeholder })
    }

    /// Rewrite the definition and every resolved use of the symbol under the
    /// position, across all files of the analyzed import closure.
    pub(crate) fn rename(
        &self, file_path: &Path, position: Position, new_name: &str,
    ) -> Result<WorkspaceEdit, RenameRejection> {
        let occurrence = self.symbol_at(file_path, position).ok_or(RenameRejection::Unresolved)?;
        let current = self.scoped().defs[&occurrence.definition].0.clone();
        let renamer = Renamer::adopt(&current, new_name)?;
        let declaration =
            self.definition_location(occurrence.definition).ok_or(RenameRejection::Synthesized)?;
        let uses = self
            .scoped()
            .users
            .forth(&occurrence.definition)
            .iter()
            .filter_map(|term| self.term_location(*term));
        let locations = Self::ordered_locations(std::iter::once(declaration).chain(uses).collect());
        Ok(renamer.apply(locations))
    }

    pub(crate) fn hover(
        &self, session: &CompilerSession, file_path: &Path, position: Position,
        line_width: HoverLineWidth,
    ) -> Option<Hover> {
        self.symbol_at(file_path, position)
            .and_then(|occurrence| self.symbol_hover(session, &occurrence, line_width))
            .or_else(|| self.term_hover(file_path, position, line_width))
    }

    fn symbol_hover(
        &self, session: &CompilerSession, occurrence: &SymbolOccurrence, line_width: HoverLineWidth,
    ) -> Option<Hover> {
        let name = &self.scoped().defs[&occurrence.definition];
        let annotation =
            session.annotation_of_def(&self.root, occurrence.definition).ok().flatten()?;
        let formatter = Formatter::new(self.scoped(), self.statics());
        let definition_type =
            session.type_definition_of_def(&self.root, occurrence.definition).ok().flatten();
        let annotation_width =
            HoverSignature::annotation_width(&name.0, line_width, definition_type.is_some());
        let mut annotation_text = String::new();
        annotation.pretty(&formatter).render_fmt(annotation_width, &mut annotation_text).ok()?;
        let definition = definition_type.and_then(|definition| {
            let mut rendered = String::new();
            definition
                .pretty(&formatter)
                .render_fmt(HoverSignature::nested_width(line_width), &mut rendered)
                .ok()?;
            Some(TypeDefinitionPreview::new(rendered))
        });
        let displayed_definition =
            definition.as_ref().filter(|definition| definition.is_expanded()).and(definition_type);
        let references =
            TypeReferenceCollector::collect(self.statics(), annotation, displayed_definition);
        let definitions = references
            .definitions()
            .filter(|definition| *definition != occurrence.definition)
            .filter_map(|definition| self.type_definition_link(definition));
        let sealed_types = references
            .sealed_types()
            .filter(|sealed| {
                self.statics().abst_hints.get(sealed).copied() != Some(occurrence.definition)
            })
            .filter_map(|sealed| self.sealed_type_equation(sealed, &formatter, line_width));
        let signature = HoverSignature::with_definitions(&name.0, &annotation_text, definitions)
            .with_definition(definition)
            .with_sealed_types(sealed_types)
            .markdown();
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: signature,
            }),
            range: Some(occurrence.range),
        })
    }

    /// The checked annotation of the innermost term enclosing the position.
    ///
    /// Compound terms carry no resolver-backed symbol of their own — projection
    /// field names, applications, and literals all resolve during type checking
    /// rather than name resolution — so this hover reports the annotation the
    /// checker recorded for the term itself. Resolved symbols take precedence,
    /// so hovering a variable or definition still reports the definition.
    fn term_hover(
        &self, file_path: &Path, position: Position, line_width: HoverLineWidth,
    ) -> Option<Hover> {
        let file_path = Self::normalize_path(file_path);
        let offset = self.offset(&file_path, position)?;
        let terms = self.scoped().terms.iter().filter_map(|(term, _)| {
            let entity = self.scoped().origins.source(&term.into())?;
            Some((term, &self.analysis.spans()[&entity]))
        });
        let (_, range, term) = terms
            .filter_map(|(term, span)| {
                self.containing_range(&file_path, offset, span)
                    .map(|(length, range)| (length, range, term))
            })
            .min_by_key(|(length, _, _)| *length)?;
        let (checked, annotation) = match self.statics().term_annotation(term)? {
            | TermAnnId::Value(term, annotation) => {
                (TypedTermId::Value(term), AnnId::Type(annotation))
            }
            | TermAnnId::Compu(term, annotation) => {
                (TypedTermId::Compu(term), AnnId::Type(annotation))
            }
            | TermAnnId::Type(term, kind) => (TypedTermId::Type(term), AnnId::Kind(kind)),
            | TermAnnId::Kind(_) | TermAnnId::Hole(_) => return None,
        };
        let formatter = Formatter::new(self.scoped(), self.statics());
        let mut rendered = String::new();
        checked.pretty(&formatter).render_fmt(line_width.columns(), &mut rendered).ok()?;
        let label = Self::term_label(rendered, line_width);
        let mut annotation_text = String::new();
        annotation
            .pretty(&formatter)
            .render_fmt(
                HoverSignature::annotation_width(&label, line_width, false),
                &mut annotation_text,
            )
            .ok()?;
        let references = TypeReferenceCollector::collect(self.statics(), annotation, None);
        let definitions =
            references.definitions().filter_map(|definition| self.type_definition_link(definition));
        let sealed_types = references
            .sealed_types()
            .filter_map(|sealed| self.sealed_type_equation(sealed, &formatter, line_width));
        let signature = HoverSignature::with_definitions(&label, &annotation_text, definitions)
            .with_sealed_types(sealed_types)
            .markdown();
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: signature,
            }),
            range: Some(range),
        })
    }

    /// The columns a term label leaves for the annotation it introduces.
    const MIN_ANNOTATION_COLUMNS: usize = 20;

    /// Label a hovered term by its rendered form, eliding to `…` when the
    /// rendering spans lines or crowds out the annotation. The editor already
    /// shows the hovered source, so the hover keeps only the type.
    fn term_label(rendered: String, line_width: HoverLineWidth) -> String {
        let budget = line_width.columns().saturating_sub(Self::MIN_ANNOTATION_COLUMNS + 3);
        let fits = !rendered.contains('\n') && rendered.chars().count() <= budget;
        if fits { rendered } else { "…".to_owned() }
    }

    pub(crate) fn document_symbols(&self, file_path: &Path) -> Vec<DocumentSymbol> {
        let file_path = Self::normalize_path(file_path);
        let mut symbols = self
            .scoped()
            .defs
            .iter()
            .filter_map(|(definition, name)| {
                let entity = self.scoped().origins.source(&(*definition).into())?;
                let span = &self.analysis.spans()[&entity];
                (self.span_file(span) == Some(file_path.clone()))
                    .then(|| {
                        let range = self.span_range(span)?;
                        Some(Self::document_symbol(name.0.clone(), range))
                    })
                    .flatten()
            })
            .collect::<Vec<_>>();
        symbols.sort_by_key(|symbol| {
            (
                symbol.range.start.line,
                symbol.range.start.character,
                symbol.range.end.line,
                symbol.range.end.character,
            )
        });
        symbols
    }

    pub(crate) fn semantic_tokens(&self, file_path: &Path) -> Option<Vec<SemanticToken>> {
        (self.semantic_path == Self::normalize_path(file_path))
            .then(|| self.semantic_tokens.clone())
    }

    pub(crate) fn diagnostics(&self, file_path: &Path) -> Vec<Diagnostic> {
        let file_path = Self::normalize_path(file_path);
        let mut diagnostics = self
            .analysis
            .warnings()
            .into_iter()
            .filter(|site| Self::normalize_path(site.path()) == file_path)
            .filter_map(|site| {
                Some(Diagnostic {
                    range: self.byte_range(&file_path, site.warning.range().clone())?,
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String(site.warning.code().to_owned())),
                    source: Some("zydeco".to_owned()),
                    message: format!("{}. {}", site.warning.message(), site.warning.note()),
                    ..Diagnostic::default()
                })
            })
            .collect::<Vec<_>>();
        if let Some(type_diagnostics) = self.analysis.outcome().diagnostics() {
            diagnostics.extend(type_diagnostics.iter().filter_map(|diagnostic| {
                // LSP diagnostics require a real per-file range. An internal failure without a
                // source anchor remains available to non-file frontends instead of being
                // misrepresented at the beginning of the root document.
                let primary = diagnostic.primary.as_ref()?;
                let diagnostic_path = self.span_file(&primary.span)?;
                if diagnostic_path != file_path {
                    return None;
                }
                let range = self.span_range(&primary.span)?;
                let related_information = diagnostic
                    .related
                    .iter()
                    .filter_map(|related| {
                        let path = self.span_file(&related.span)?;
                        Some(DiagnosticRelatedInformation {
                            location: Location {
                                uri: Url::from_file_path(path).ok()?,
                                range: self.span_range(&related.span)?,
                            },
                            message: related.message.clone(),
                        })
                    })
                    .collect::<Vec<_>>();
                let message = match diagnostic.help.as_slice() {
                    | [] => diagnostic.message.clone(),
                    | help => format!("{}\n\nHelp: {}", diagnostic.message, help.join("\nHelp: ")),
                };
                Some(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(diagnostic.code.to_string())),
                    source: Some("zydeco".to_owned()),
                    message,
                    related_information: (!related_information.is_empty())
                        .then_some(related_information),
                    ..Diagnostic::default()
                })
            }));
        }
        diagnostics
    }

    fn symbol_at(&self, file_path: &Path, position: Position) -> Option<SymbolOccurrence> {
        let file_path = Self::normalize_path(file_path);
        let offset = self.offset(&file_path, position)?;
        let definitions = self.scoped().defs.iter().filter_map(|(definition, _)| {
            let entity = self.scoped().origins.source(&(*definition).into())?;
            Some((*definition, &self.analysis.spans()[&entity]))
        });
        let uses = self.scoped().terms.iter().filter_map(|(term, body)| {
            let Term::Var(definition) = body else {
                return None;
            };
            let entity = self.scoped().origins.source(&term.into())?;
            Some((*definition, &self.analysis.spans()[&entity]))
        });
        definitions
            .chain(uses)
            .filter_map(|(definition, span)| {
                self.containing_occurrence(&file_path, offset, definition, span)
            })
            .min_by_key(|(length, _)| *length)
            .map(|(_, occurrence)| occurrence)
    }

    fn containing_occurrence(
        &self, file_path: &Path, offset: usize, definition: DefId, span: &Span,
    ) -> Option<(usize, SymbolOccurrence)> {
        self.containing_range(file_path, offset, span)
            .map(|(length, range)| (length, SymbolOccurrence { definition, range }))
    }

    /// The LSP range of a span naming a token of `file_path` that contains the
    /// byte `offset`, paired with its byte length so callers can prefer the
    /// innermost enclosing entity.
    fn containing_range(
        &self, file_path: &Path, offset: usize, span: &Span,
    ) -> Option<(usize, Range)> {
        let (_, bytes) = self.analysis.spans().source_map().and_then(|map| map.range(*span))?;
        (self.span_file(span) == Some(file_path.to_path_buf())
            && bytes.start <= offset
            && offset < bytes.end)
            .then(|| self.span_range(span).map(|hit| (bytes.end.saturating_sub(bytes.start), hit)))
            .flatten()
    }

    fn definition_location(&self, definition: DefId) -> Option<Location> {
        let entity = self.scoped().origins.source(&definition.into())?;
        self.entity_location(&entity)
    }

    fn type_definition_link(&self, definition: DefId) -> Option<TypeDefinitionLink> {
        let name = self.scoped().defs.get(&definition)?.0.clone();
        let location = self.definition_location(definition)?;
        let mut target = location.uri;
        target.set_fragment(Some(&format!("L{}", location.range.start.line + 1)));
        Some(TypeDefinitionLink { name, target })
    }

    fn sealed_type_equation(
        &self, sealed: AbstId, formatter: &Formatter<'_>, line_width: HoverLineWidth,
    ) -> Option<SealedTypeEquationPreview> {
        let mut rendered = String::new();
        SealedTypeEquation::new(self.statics(), sealed)?
            .pretty(formatter)
            .render_fmt(line_width.columns(), &mut rendered)
            .ok()?;
        Some(SealedTypeEquationPreview::new(rendered))
    }

    fn term_location(&self, term: TermId) -> Option<Location> {
        let entity = self.scoped().origins.source(&term.into())?;
        self.entity_location(&entity)
    }

    fn entity_location(&self, entity: &EntityId) -> Option<Location> {
        let span = &self.analysis.spans()[entity];
        let path = self.span_file(span)?;
        Some(Location { uri: Url::from_file_path(path).ok()?, range: self.span_range(span)? })
    }

    fn ordered_locations(mut locations: Vec<Location>) -> Vec<Location> {
        locations.sort_by(|left, right| {
            (
                left.uri.as_str(),
                left.range.start.line,
                left.range.start.character,
                left.range.end.line,
                left.range.end.character,
            )
                .cmp(&(
                    right.uri.as_str(),
                    right.range.start.line,
                    right.range.start.character,
                    right.range.end.line,
                    right.range.end.character,
                ))
        });
        locations.dedup();
        locations
    }

    fn offset(&self, file_path: &Path, position: Position) -> Option<usize> {
        self.file_maps
            .get(file_path)?
            .offset_utf16(LineCol { line: position.line, column: position.character })
    }

    /// The normalized path of the file a merged span belongs to.
    fn span_file(&self, span: &Span) -> Option<PathBuf> {
        let (file, _) = self.analysis.spans().source_map()?.range(*span)?;
        Some(Self::normalize_path(&file.path()))
    }

    fn span_range(&self, span: &Span) -> Option<Range> {
        let (file, range) = self.analysis.spans().source_map()?.range(*span)?;
        let path = Self::normalize_path(&file.path());
        self.byte_range(&path, range)
    }

    fn byte_range(&self, path: &Path, range: std::ops::Range<usize>) -> Option<Range> {
        let file_map = self.file_maps.get(path)?;
        Some(Range::new(
            Self::position(file_map.line_col_utf16(range.start)?),
            Self::position(file_map.line_col_utf16(range.end)?),
        ))
    }

    fn position(cursor: LineCol) -> Position {
        Position::new(cursor.line, cursor.column)
    }

    fn normalize_path(path: &Path) -> PathBuf {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }

    fn scoped(&self) -> &ScopedArena {
        self.analysis.scoped()
    }

    fn statics(&self) -> &StaticsArena {
        &self.statics
    }

    #[allow(deprecated)]
    fn document_symbol(name: String, range: Range) -> DocumentSymbol {
        DocumentSymbol {
            name,
            detail: None,
            kind: SymbolKind::VARIABLE,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ProjectFailure, ProjectState};
    use crate::{
        hover::HoverLineWidth,
        progress::{AnalysisProgress, SourceDiscovery},
        rename::{NameClass, RenameRejection},
        semantic::SemanticHighlighter,
    };
    use std::{collections::HashMap, path::Path};
    use tower_lsp::lsp_types::{
        DiagnosticSeverity, HoverContents, NumberOrString, Position, PrepareRenameResponse, Range,
        SemanticToken, SemanticTokensLegend, Url,
    };
    use zydeco_session::SourceDiagnosticSite;
    use zydeco_utils::span::{FileMap, LineCol};

    #[test]
    fn failed_analysis_ranges_are_measured_in_utf16() {
        let path = Path::new("unicode-error.zy");
        let failure = ProjectFailure::compiler(
            "parse error",
            Some(SourceDiagnosticSite::new(path.to_path_buf(), 5..6)),
        );

        let diagnostic = failure.diagnostic(Some(path), Some("😀 ?"));

        assert_eq!(diagnostic.range, Range::new(Position::new(0, 3), Position::new(0, 4)));
    }

    fn fenced_zydeco_sources(markdown: &str) -> Vec<&str> {
        markdown
            .split("```zydeco\n")
            .skip(1)
            .map(|fence| fence.split_once("\n```").expect("Zydeco fence should be closed").0)
            .collect()
    }

    fn source_position(source: &str, needle: &str) -> Position {
        let byte = source.find(needle).unwrap_or_else(|| panic!("missing source text: {needle}"));
        let before = &source[..byte];
        let line = before.bytes().filter(|byte| *byte == b'\n').count() as u32;
        let line_start = before.rfind('\n').map_or(0, |newline| newline + 1);
        let character = source[line_start..byte].encode_utf16().count() as u32;
        Position::new(line, character)
    }

    fn definition_url(path: &Path, position: Position) -> Url {
        let mut definition = Url::from_file_path(path).unwrap();
        definition.set_fragment(Some(&format!("L{}", position.line + 1)));
        definition
    }

    struct DecodedToken {
        text: String,
        token_type: String,
        modifiers: Vec<String>,
    }

    struct SemanticTokenDecoder<'source> {
        source: &'source str,
        map: FileMap,
        legend: SemanticTokensLegend,
    }

    impl<'source> SemanticTokenDecoder<'source> {
        fn new(source: &'source str) -> Self {
            Self {
                source,
                map: FileMap::local(source, None),
                legend: SemanticHighlighter::legend(),
            }
        }

        fn decode(&self, tokens: &[SemanticToken]) -> Vec<DecodedToken> {
            tokens
                .iter()
                .scan((0_u32, 0_u32), |previous, token| {
                    let line = previous.0 + token.delta_line;
                    let start = if token.delta_line == 0 {
                        previous.1 + token.delta_start
                    } else {
                        token.delta_start
                    };
                    *previous = (line, start);
                    let byte_start = self.map.offset_utf16(LineCol { line, column: start })?;
                    let byte_end =
                        self.map.offset_utf16(LineCol { line, column: start + token.length })?;
                    let token_type =
                        self.legend.token_types[token.token_type as usize].as_str().to_owned();
                    let modifiers = self
                        .legend
                        .token_modifiers
                        .iter()
                        .enumerate()
                        .filter(|(index, _)| token.token_modifiers_bitset & (1 << index) != 0)
                        .map(|(_, modifier)| modifier.as_str().to_owned())
                        .collect();
                    Some(DecodedToken {
                        text: self.source[byte_start..byte_end].to_owned(),
                        token_type,
                        modifiers,
                    })
                })
                .collect()
        }
    }

    #[test]
    fn analysis_reports_discovery_before_whole_program_phases() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "()\n").unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let library = library.canonicalize().unwrap();
        let root = root.canonicalize().unwrap();
        let mut progress = Vec::new();

        ProjectState::load_with_progress(&root, &HashMap::new(), |update| progress.push(update))
            .unwrap();

        assert_eq!(
            progress,
            vec![
                AnalysisProgress::Loading(SourceDiscovery { path: root.clone(), discovered: 1 }),
                AnalysisProgress::Loading(SourceDiscovery { path: library, discovered: 2 }),
                AnalysisProgress::Parsing { source_count: 2 },
                AnalysisProgress::Desugaring { source_count: 2 },
                AnalysisProgress::Resolving { source_count: 2 },
                AnalysisProgress::Tycking { source_count: 2 },
                AnalysisProgress::Highlighting { path: root },
            ]
        );
    }

    #[test]
    fn resolved_symbols_support_definition_references_and_type_hover_across_imports() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "begin\n  let answer = () that\n  (answer, answer)\nend\n")
            .unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let (project, session) = ProjectState::load(&root, &HashMap::new()).unwrap();

        let definition = project.definition(&library, Position::new(1, 7)).unwrap();
        assert_eq!(definition.uri.to_file_path().unwrap(), library.canonicalize().unwrap());
        assert_eq!(definition.range.start, Position::new(1, 6));

        let references = project.references(&library, Position::new(2, 4), true).unwrap();
        assert_eq!(references.len(), 3);
        assert_eq!(references[0].range.start, Position::new(1, 6));
        assert_eq!(references[1].range.start, Position::new(2, 3));
        assert_eq!(references[2].range.start, Position::new(2, 11));

        let uses = project.references(&library, Position::new(1, 7), false).unwrap();
        assert_eq!(uses.len(), 2);
        let hover = project
            .hover(&session, &library, Position::new(2, 4), HoverLineWidth::default())
            .unwrap();
        assert_eq!(hover.range.unwrap().start, Position::new(2, 3));
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        assert_eq!(contents.value, "```zydeco\nanswer : Unit\n```");
    }

    #[test]
    fn rename_rewrites_each_resolved_occurrence_across_imports() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "begin\n  let answer = () that\n  (answer, answer)\nend\n")
            .unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let (project, _session) = ProjectState::load(&root, &HashMap::new()).unwrap();

        let from_binder = project.rename(&library, Position::new(1, 7), "result").unwrap();
        let from_use = project.rename(&library, Position::new(2, 4), "result").unwrap();
        assert_eq!(from_binder, from_use);

        let changes = from_binder.changes.unwrap();
        let library = library.canonicalize().unwrap();
        assert_eq!(changes.len(), 1);
        let edits = &changes[&Url::from_file_path(&library).unwrap()];
        assert_eq!(edits.len(), 3);
        assert!(edits.iter().all(|edit| edit.new_text == "result"));
        assert_eq!(
            edits.iter().map(|edit| edit.range.start).collect::<Vec<_>>(),
            vec![Position::new(1, 6), Position::new(2, 3), Position::new(2, 11)]
        );
    }

    #[test]
    fn rename_preserves_name_classes_and_refuses_grammar_words() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "begin\n  let answer = () that\n  (answer, answer)\nend\n")
            .unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let (project, _session) = ProjectState::load(&root, &HashMap::new()).unwrap();

        assert_eq!(
            project.rename(&library, Position::new(1, 7), "Answer"),
            Err(RenameRejection::Lexical {
                proposed: "Answer".to_owned(),
                class: NameClass::Lower
            })
        );
        assert_eq!(
            project.rename(&library, Position::new(1, 7), "let"),
            Err(RenameRejection::Reserved { proposed: "let".to_owned() })
        );
        assert_eq!(project.rename(&library, Position::new(1, 7), ""), Err(RenameRejection::Empty));

        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/recursive-data.zy")
            .canonicalize()
            .unwrap();
        let (project, _session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let edit = project.rename(&path, Position::new(1, 7), "Nat2").unwrap();
        let edits = &edit.changes.unwrap()[&Url::from_file_path(&path).unwrap()];
        assert_eq!(edits.len(), 5, "the definition plus four type uses: {edits:?}");
        assert!(edits.iter().all(|edit| edit.new_text == "Nat2"));
        assert_eq!(
            project.rename(&path, Position::new(1, 7), "nat"),
            Err(RenameRejection::Lexical { proposed: "nat".to_owned(), class: NameClass::Upper })
        );
    }

    #[test]
    fn prepare_rename_offers_the_identifier_and_placeholder() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "begin\n  let answer = () that\n  (answer, answer)\nend\n")
            .unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let (project, _session) = ProjectState::load(&root, &HashMap::new()).unwrap();

        assert_eq!(
            project.prepare_rename(&library, Position::new(2, 4)),
            Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: Range::new(Position::new(2, 3), Position::new(2, 9)),
                placeholder: "answer".to_owned(),
            })
        );
        assert_eq!(project.prepare_rename(&library, Position::new(0, 1)), None);
    }

    #[test]
    fn rename_refuses_unresolved_and_arm_introduced_names() {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join("library.zy");
        let root = directory.path().join("main.zy");
        std::fs::write(&library, "begin\n  let answer = () that\n  (answer, answer)\nend\n")
            .unwrap();
        std::fs::write(&root, "@[import(\"library.zy\")] _\n").unwrap();
        let (project, _session) = ProjectState::load(&root, &HashMap::new()).unwrap();

        assert_eq!(
            project.rename(&library, Position::new(1, 15), "result"),
            Err(RenameRejection::Unresolved)
        );

        // Constructor names resolve during type checking, not name resolution,
        // so a data arm occurrence has no resolver-backed occurrence set yet.
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/recursive-data.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, _session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let constructor = source_position(&source, "Z();");
        assert_eq!(project.rename(&path, constructor, "Z2"), Err(RenameRejection::Unresolved));
        assert_eq!(project.prepare_rename(&path, constructor), None);
    }

    #[test]
    fn type_hover_uses_source_names_for_polymorphic_types() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let value = source_position(&source, "value : A");
        let hover = project.hover(&session, &path, value, HoverLineWidth::default()).unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let definition = definition_url(&path, source_position(&source, "A : VType"));

        assert_eq!(
            contents.value,
            format!("```zydeco\nvalue : A\n```\n\nTypes:\n\n- [`A` ↗](<{definition}>)")
        );
    }

    #[test]
    fn type_hover_links_through_context_reordered_type_aliases() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/compile/uniform.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let copy = source_position(&source, "copy : Number");
        let hover = project.hover(&session, &path, copy, HoverLineWidth::default()).unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let definition = definition_url(&path, source_position(&source, "forall (A : VType)"));

        assert_eq!(
            contents.value,
            format!("```zydeco\ncopy : A\n```\n\nTypes:\n\n- [`A` ↗](<{definition}>)")
        );
    }

    #[test]
    fn type_hover_expands_short_definitions_and_collapses_long_ones() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/data/package.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let option = source_position(&source, "Option (A : VType)");
        let parameter = definition_url(&path, option);

        let short = project.hover(&session, &path, option, HoverLineWidth::default()).unwrap();
        let HoverContents::Markup(short) = short.contents else {
            panic!("type hover should use markup content")
        };
        assert_eq!(
            short.value,
            format!(
                concat!(
                    "```zydeco\n",
                    "Option : VType -> VType =\n",
                    "  fn A =>\n",
                    "    data\n",
                    "    | +None : Unit\n",
                    "    | +Some : A\n",
                    "    end\n",
                    "```\n\n",
                    "Types:\n\n",
                    "- [`A` ↗](<{parameter}>)"
                ),
                parameter = parameter
            )
        );

        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/data/package.type.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let long = project
            .hover(
                &session,
                &path,
                source_position(&source, "OptionModule"),
                HoverLineWidth::default(),
            )
            .unwrap();
        let HoverContents::Markup(long) = long.contents else {
            panic!("type hover should use markup content")
        };
        assert_eq!(
            long.value,
            "```zydeco\nOptionModule : VType -> VType -> VType -> VType =\n  ...\n```"
        );
    }

    #[test]
    fn type_hover_pretty_prints_within_the_client_column_budget() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/data/package.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let line_width = HoverLineWidth::new(30).unwrap();
        let hover = project
            .hover(&session, &path, source_position(&source, "zip (A : VType)"), line_width)
            .unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let sources = fenced_zydeco_sources(&contents.value);

        assert!(
            sources
                .iter()
                .flat_map(|source| source.lines())
                .all(|line| line.chars().count() <= line_width.columns()),
            "every rendered line should fit the client column budget:\n{}",
            contents.value
        );
        assert!(
            sources.first().is_some_and(|source| source.lines().count() > 1),
            "the narrow hover should wrap:\n{}",
            contents.value
        );
    }

    #[test]
    fn type_hover_breaks_result_constructor_signatures_at_72_columns() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/data/package.zy")
            .canonicalize()
            .unwrap();
        let source = concat!(
            "param (\n",
            "  (/core) :\n",
            "  @[import(\"../builtin.zy\")] _\n",
            ") in\n",
            "begin\n",
            "  let (/VType; /CType; /Thk; /Ret) = core that\n",
            "  def Result (A : VType) (E : VType) =\n",
            "    data\n",
            "    | +Ok : A\n",
            "    | +Err : E\n",
            "    end\n",
            "  that\n",
            "  def ! ok (A : VType) (E : VType) (value : A) : Ret (Result A E) =\n",
            "    ret +Ok(value)\n",
            "  that\n",
            "  ()\n",
            "end\n",
        );
        let overrides = HashMap::from([(path.clone(), source.to_owned())]);
        let (project, session) = ProjectState::load(&path, &overrides).unwrap();
        let line_width = HoverLineWidth::new(72).unwrap();
        let hover = project
            .hover(&session, &path, source_position(source, "ok (A : VType)"), line_width)
            .unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let sources = fenced_zydeco_sources(&contents.value);
        let signature = sources.first().expect("type hover should contain a Zydeco code fence");

        assert!(
            signature.lines().count() > 1,
            "the signature should be pretty-printed:\n{}",
            contents.value
        );
        assert!(
            sources
                .iter()
                .flat_map(|source| source.lines())
                .all(|line| line.chars().count() <= line_width.columns()),
            "the result hover should fit without Zed soft-wrapping it:\n{}",
            contents.value
        );
        assert_eq!(sources.len(), 2, "the declaration and equation should use separate fences");
        assert!(contents.value.contains("```\n\nwhere\n\n```zydeco\n"));
    }

    #[test]
    fn type_hover_recovers_recursive_definition_bodies() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/recursive-data.zy")
            .canonicalize()
            .unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover =
            project.hover(&session, &path, Position::new(1, 7), HoverLineWidth::default()).unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };

        assert_eq!(
            contents.value,
            concat!(
                "```zydeco\n",
                "Nat : VType =\n",
                "  data\n",
                "  | +Z : Unit\n",
                "  | +S : Nat\n",
                "  end\n",
                "```"
            )
        );
    }

    #[test]
    fn term_hover_explains_sealed_types_with_where_equations() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/recursive-data.zy")
            .canonicalize()
            .unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover = project
            .hover(&session, &path, Position::new(13, 19), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let mut definition = Url::from_file_path(&path).unwrap();
        definition.set_fragment(Some("L2"));

        assert_eq!(
            contents.value,
            format!(
                concat!(
                    "```zydeco\n",
                    "value : Nat\n",
                    "```\n\n",
                    "where\n\n",
                    "```zydeco\n",
                    "Nat : VType\n",
                    "  = data\n",
                    "    | +Z : Unit\n",
                    "    | +S : Nat\n",
                    "    end\n",
                    "```\n\n",
                    "Types:\n\n",
                    "- [`Nat` ↗](<{definition}>)"
                ),
                definition = definition
            )
        );
    }

    #[test]
    fn term_hover_reports_the_projected_type_of_field_projections() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();

        let field = project
            .hover(&session, &path, source_position(&source, "/exit"), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = field.contents else {
            panic!("projection hover should use markup content")
        };
        let builtin = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/builtin.zy")
            .canonicalize()
            .unwrap();
        let definition = definition_url(&builtin, Position::new(13, 0));

        assert_eq!(
            contents.value,
            format!(
                concat!(
                    "```zydeco\n",
                    "process/exit : Thk (Int64 -> SystemOS)\n",
                    "```\n\n",
                    "Types:\n\n",
                    "- [`SystemOS` ↗](<{definition}>)"
                ),
                definition = definition
            )
        );
        assert_eq!(field.range.unwrap().start, source_position(&source, "process/exit"));

        let head = project
            .hover(
                &session,
                &path,
                source_position(&source, "process/exit"),
                HoverLineWidth::default(),
            )
            .unwrap();
        let HoverContents::Markup(head_contents) = head.contents else {
            panic!("symbol hover should use markup content")
        };
        assert!(
            head_contents.value.starts_with("```zydeco\nprocess :"),
            "the projected head should keep its symbol hover:\n{}",
            head_contents.value
        );
    }

    #[test]
    fn term_hover_links_type_definitions_of_module_projections() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();

        let module = project
            .hover(&session, &path, source_position(&source, "/i64"), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = module.contents else {
            panic!("projection hover should use markup content")
        };
        let representations = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/builtin/representations.zy")
            .canonicalize()
            .unwrap();
        let definition = definition_url(&representations, Position::new(6, 0));

        assert_eq!(
            contents.value,
            format!(
                concat!(
                    "```zydeco\n",
                    "representations/i64 : exists (= Int64 as Int64 : VType) . Unit\n",
                    "```\n\n",
                    "Types:\n\n",
                    "- [`Int64` ↗](<{definition}>)"
                ),
                definition = definition
            )
        );
    }

    #[test]
    fn term_hover_reports_types_of_subexpressions_under_the_cursor() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();

        let application = project
            .hover(&session, &path, source_position(&source, " Int64 0"), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = application.contents else {
            panic!("term hover should use markup content")
        };
        assert_eq!(
            fenced_zydeco_sources(&contents.value),
            vec!["(! id Int64) : Int64 -> Ret Int64"],
            "hovering between the parts of an application should report the innermost term:\n{}",
            contents.value
        );
        assert_eq!(application.range.unwrap().start, source_position(&source, "! id Int64"));

        let literal = project
            .hover(&session, &path, source_position(&source, "0;"), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = literal.contents else {
            panic!("term hover should use markup content")
        };
        assert_eq!(
            fenced_zydeco_sources(&contents.value),
            vec!["0 : Int64"],
            "hovering a literal should report its type:\n{}",
            contents.value
        );

        let block = project
            .hover(&session, &path, source_position(&source, "end"), HoverLineWidth::default())
            .unwrap();
        let HoverContents::Markup(contents) = block.contents else {
            panic!("term hover should use markup content")
        };
        assert!(
            contents.value.starts_with("```zydeco\n… :"),
            "a term whose rendering spans lines should collapse to its type:\n{}",
            contents.value
        );
    }

    #[test]
    fn semantic_tokens_refine_resolved_names_with_cbpv_classes() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, _session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let encoded = project.semantic_tokens(&path).unwrap();
        let decoded = SemanticTokenDecoder::new(&source).decode(&encoded);
        let has = |text: &str, token_type: &str, modifier: &str| {
            decoded.iter().any(|token| {
                token.text == text
                    && token.token_type == token_type
                    && token.modifiers.iter().any(|found| found == modifier)
            })
        };

        assert!(has("VType", "type", "kind"));
        assert!(has("A", "typeParameter", "valueType"));
        assert!(has("Int64", "type", "valueType"));
        assert!(has("OS", "type", "computationType"));
        assert!(has("x", "variable", "value"));
        assert!(has("process", "variable", "value"));
        assert!(
            decoded.iter().any(|token| { token.text == "exit" && token.token_type == "property" })
        );
        assert!(decoded.iter().any(|token| {
            token.text == "x" && token.modifiers.iter().any(|modifier| modifier == "declaration")
        }));
    }

    #[test]
    fn semantic_tokens_mark_value_pi_and_value_abstraction_binders_as_parameters() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/compile/value-views.zy")
            .canonicalize()
            .unwrap();
        let source = concat!(
            "begin\n",
            "  param (\n",
            "    (/core; /system) :\n",
            "    @(import(\"../../std/builtin.zy\"))\n",
            "  ) that\n",
            "  let (/VType) = core that\n",
            "  let (/process) = system that\n",
            "  let identity : val pi (A : VType) (value : A) . A =\n",
            "    val (B : VType) (item : B) => item\n",
            "  that\n",
            "  ! (process/exit) 0\n",
            "end\n",
        );
        let overrides = HashMap::from([(path.clone(), source.to_owned())]);
        let (project, _session) = ProjectState::load(&path, &overrides).unwrap();
        let encoded = project.semantic_tokens(&path).unwrap();
        let decoded = SemanticTokenDecoder::new(source).decode(&encoded);
        let has = |text: &str, token_type: &str, modifier: &str| {
            decoded.iter().any(|token| {
                token.text == text
                    && token.token_type == token_type
                    && token.modifiers.iter().any(|found| found == modifier)
            })
        };

        assert!(has("A", "typeParameter", "valueType"));
        assert!(has("value", "parameter", "value"));
        assert!(has("B", "typeParameter", "valueType"));
        assert!(has("item", "parameter", "value"));
    }

    #[test]
    fn type_errors_surface_as_error_diagnostics() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let broken = source.replace("! id~ OS { ! (process/exit) x }", "x x");
        assert_ne!(source, broken);
        let overrides = HashMap::from([(path.clone(), broken.clone())]);
        let (project, _session) = ProjectState::load(&path, &overrides).unwrap();
        let diagnostics = project.diagnostics(&path);
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| {
                diagnostic.code == Some(NumberOrString::String("tyck.type-expected".to_owned()))
            })
            .expect("the invalid application should have a type diagnostic");

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.source.as_deref(), Some("zydeco"));
        let application = source_position(&broken, "x x");
        assert_eq!(
            diagnostic.range,
            Range::new(
                application,
                Position::new(application.line, application.character + "x x".len() as u32),
            )
        );
        assert!(diagnostic.related_information.is_none());
    }

    #[test]
    fn missing_annotation_diagnostics_use_the_innermost_source_span() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/fail/annotation.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, _session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let diagnostics = project.diagnostics(&path);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("expected one focused diagnostic, got {diagnostics:?}")
        };

        assert_eq!(
            diagnostic.code,
            Some(NumberOrString::String("tyck.missing-annotation".to_owned()))
        );
        assert_eq!(diagnostic.range.start, source_position(&source, "+True()\n"));
        assert!(diagnostic.message.contains("constructor `+True`"));
        assert!(diagnostic.message.contains("Help: add a type ascription"));
        assert!(diagnostic.related_information.is_none());
    }

    #[test]
    fn semantic_tokens_retain_established_static_classes_after_type_errors() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/exec/forall.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let broken = source.replace("! id~ OS { ! (process/exit) x }", "x x");
        assert_ne!(source, broken);
        let overrides = HashMap::from([(path.clone(), broken.clone())]);
        let (project, _session) = ProjectState::load(&path, &overrides).unwrap();
        let encoded = project.semantic_tokens(&path).unwrap();
        let decoded = SemanticTokenDecoder::new(&broken).decode(&encoded);

        assert!(decoded.iter().any(|token| {
            token.text == "Int64" && token.modifiers.iter().any(|modifier| modifier == "valueType")
        }));
        assert!(decoded.iter().any(|token| {
            token.text == "OS"
                && token.modifiers.iter().any(|modifier| modifier == "computationType")
        }));
    }
}
