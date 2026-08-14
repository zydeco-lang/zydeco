use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, DocumentSymbol, Hover, HoverContents, Location, MarkupContent,
    MarkupKind, NumberOrString, Position, Range, SemanticToken, SymbolKind, Url,
};
use zydeco_session::{CompilerSession, ProgramAnalysis};
use zydeco_statics::{
    arena::StaticsArena,
    fmt::{Formatter, SealedTypeEquation},
    syntax::AbstId,
};
use zydeco_surface::{
    scoped::arena::ScopedArena,
    scoped::syntax::{DefId, Term, TermId},
    textual::syntax::EntityId,
};
use zydeco_syntax::Pretty;
use zydeco_utils::{
    arena::ArenaAccess,
    span::{Cursor2, FileInfo, Span},
};

use crate::{
    hover::{
        HoverLineWidth, HoverSignature, SealedTypeEquationPreview, TypeDefinitionLink,
        TypeDefinitionPreview,
    },
    progress::{AnalysisProgress, SourceDiscovery},
    semantic::SemanticHighlighter,
    type_links::TypeReferenceCollector,
};

/// Compiler analysis state for one editor root.
pub(crate) struct ProjectState {
    analysis: Arc<ProgramAnalysis>,
    /// The analyzed root, for fact lookups against the session's memoized
    /// queries.
    root: PathBuf,
    file_infos: HashMap<PathBuf, FileInfo>,
    semantic_path: PathBuf,
    semantic_tokens: Vec<SemanticToken>,
}

#[derive(Copy, Clone)]
struct SymbolOccurrence {
    definition: DefId,
    range: Range,
}

impl ProjectState {
    #[cfg(test)]
    pub(crate) fn load(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> Result<(Self, CompilerSession), String> {
        Self::load_with_progress(source_path, overrides, |_| {})
    }

    #[cfg(test)]
    pub(crate) fn load_with_progress(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
        progress: impl FnMut(AnalysisProgress),
    ) -> Result<(Self, CompilerSession), String> {
        let mut session = CompilerSession::default();
        overrides.iter().try_for_each(|(path, source)| {
            session.set_overlay(path, source.clone()).map_err(|error| error.to_string())
        })?;
        Self::load_from_session(source_path, &session, progress).map(|project| (project, session))
    }

    pub(crate) fn load_from_session(
        source_path: &Path, session: &CompilerSession, mut progress: impl FnMut(AnalysisProgress),
    ) -> Result<Self, String> {
        let graph = session.graph(source_path).map_err(|error| error.to_string())?;
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
        let analysis = session.analyze(source_path).map_err(|error| error.to_string())?;
        let scoped = analysis.scoped();
        let statics = analysis.statics();
        let file_infos = analysis
            .sources()
            .map(|(path, source)| {
                let path = Self::normalize_path(path);
                let info = FileInfo::new(source, Some(Arc::new(path.clone())));
                (path, info)
            })
            .collect();
        let source_path = Self::normalize_path(source_path);
        let source = analysis
            .source(&source_path)
            .ok_or_else(|| format!("the parsed program omitted `{}`", source_path.display()))?;
        progress(AnalysisProgress::Highlighting { path: source_path.clone() });
        let tokens = SemanticHighlighter::compiler_refined(
            source,
            &source_path,
            analysis.spans(),
            scoped,
            Some(statics),
        );
        let semantic_path = source_path;
        let semantic_tokens = tokens;

        Ok(Self {
            analysis,
            root: semantic_path.clone(),
            file_infos,
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

    pub(crate) fn hover(
        &self, session: &CompilerSession, file_path: &Path, position: Position,
        line_width: HoverLineWidth,
    ) -> Option<Hover> {
        let occurrence = self.symbol_at(file_path, position)?;
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

    pub(crate) fn document_symbols(&self, file_path: &Path) -> Vec<DocumentSymbol> {
        let file_path = Self::normalize_path(file_path);
        let mut symbols = self
            .scoped()
            .defs
            .iter()
            .filter_map(|(definition, name)| {
                let entity = self.scoped().textual.back(&(*definition).into())?;
                let span = &self.analysis.spans()[entity];
                (span.get_path().map(|path| Self::normalize_path(path)) == Some(file_path.clone()))
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
        if let Some(reports) = self.analysis.outcome().reports() {
            diagnostics.extend(
                reports
                    .spans
                    .iter()
                    .flatten()
                    .filter(|(path, _, _)| Self::normalize_path(path.as_path()) == file_path)
                    .filter_map(|(_, range, message)| {
                        Some(Diagnostic {
                            range: self.byte_range(&file_path, range.clone())?,
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("zydeco".to_owned()),
                            message: message.clone(),
                            ..Diagnostic::default()
                        })
                    }),
            );
        }
        diagnostics
    }

    fn symbol_at(&self, file_path: &Path, position: Position) -> Option<SymbolOccurrence> {
        let file_path = Self::normalize_path(file_path);
        let offset = self.offset(&file_path, position)?;
        let definitions = self.scoped().defs.iter().filter_map(|(definition, _)| {
            let entity = self.scoped().textual.back(&(*definition).into())?;
            Some((*definition, &self.analysis.spans()[entity]))
        });
        let uses = self.scoped().terms.iter().filter_map(|(term, body)| {
            let Term::Var(definition) = body else {
                return None;
            };
            let entity = self.scoped().textual.back(&(*term).into())?;
            Some((*definition, &self.analysis.spans()[entity]))
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
        let same_file =
            span.get_path().map(|path| Self::normalize_path(path)) == Some(file_path.to_path_buf());
        let (start, end) = span.get_cursor1();
        (same_file && start <= offset && offset < end)
            .then(|| {
                self.span_range(span).map(|range| {
                    (end.saturating_sub(start), SymbolOccurrence { definition, range })
                })
            })
            .flatten()
    }

    fn definition_location(&self, definition: DefId) -> Option<Location> {
        let entity = self.scoped().textual.back(&definition.into())?;
        self.entity_location(entity)
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
        let entity = self.scoped().textual.back(&term.into())?;
        self.entity_location(entity)
    }

    fn entity_location(&self, entity: &EntityId) -> Option<Location> {
        let span = &self.analysis.spans()[entity];
        let path = Self::normalize_path(span.get_path()?);
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
        let source = self.analysis.source(file_path)?;
        self.file_infos.get(file_path)?.trans_span1_utf16(
            source,
            Cursor2 { line: position.line as usize, column: position.character as usize },
        )
    }

    fn span_range(&self, span: &Span) -> Option<Range> {
        let path = Self::normalize_path(span.get_path()?);
        let (start, end) = span.get_cursor1();
        self.byte_range(&path, start..end)
    }

    fn byte_range(&self, path: &Path, range: std::ops::Range<usize>) -> Option<Range> {
        let source = self.analysis.source(path)?;
        let file_info = self.file_infos.get(path)?;
        Some(Range::new(
            Self::position(file_info.trans_span2_utf16(source, range.start)?),
            Self::position(file_info.trans_span2_utf16(source, range.end)?),
        ))
    }

    fn position(cursor: Cursor2) -> Position {
        Position::new(cursor.line as u32, cursor.column as u32)
    }

    fn normalize_path(path: &Path) -> PathBuf {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }

    fn scoped(&self) -> &ScopedArena {
        self.analysis.scoped()
    }

    fn statics(&self) -> &StaticsArena {
        self.analysis.statics()
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
    use super::ProjectState;
    use crate::{
        hover::HoverLineWidth,
        progress::{AnalysisProgress, SourceDiscovery},
        semantic::SemanticHighlighter,
    };
    use std::{collections::HashMap, path::Path};
    use tower_lsp::lsp_types::{HoverContents, Position, SemanticToken, SemanticTokensLegend, Url};
    use zydeco_utils::span::{Cursor2, FileInfo};

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
        info: FileInfo,
        legend: SemanticTokensLegend,
    }

    impl<'source> SemanticTokenDecoder<'source> {
        fn new(source: &'source str) -> Self {
            Self {
                source,
                info: FileInfo::new(source, None),
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
                    let byte_start = self.info.trans_span1_utf16(
                        self.source,
                        Cursor2 { line: line as usize, column: start as usize },
                    )?;
                    let byte_end = self.info.trans_span1_utf16(
                        self.source,
                        Cursor2 { line: line as usize, column: (start + token.length) as usize },
                    )?;
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
            .join("../../lib/std/data/option.zy")
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
            .join("../../lib/std/data/option.type.zy")
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
            .join("../../lib/std/data/option.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let (project, session) = ProjectState::load(&path, &HashMap::new()).unwrap();
        let line_width = HoverLineWidth::new(30).unwrap();
        let hover = project
            .hover(&session, &path, source_position(&source, "map (A : VType)"), line_width)
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
            .join("../../lib/std/data/result.zy")
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
            .hover(&session, &path, Position::new(9, 19), HoverLineWidth::default())
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

        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.severity == Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR)
                && diagnostic.source.as_deref() == Some("zydeco")
        }));
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
