use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use tower_lsp::lsp_types::{
    DocumentSymbol, Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, Range,
    SemanticToken, SymbolKind, Url,
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
    hover::{HoverSignature, SealedTypeEquationPreview, TypeDefinitionLink, TypeDefinitionPreview},
    progress::{AnalysisProgress, SourceDiscovery},
    semantic::SemanticHighlighter,
    type_links::TypeReferenceCollector,
};

/// Compiler analysis state for one editor root.
pub(crate) struct ProjectState {
    analysis: Arc<ProgramAnalysis>,
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
    ) -> Result<Self, String> {
        Self::load_with_progress(source_path, overrides, |_| {})
    }

    #[cfg(test)]
    pub(crate) fn load_with_progress(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
        progress: impl FnMut(AnalysisProgress),
    ) -> Result<Self, String> {
        let mut session = CompilerSession::default();
        overrides.iter().try_for_each(|(path, source)| {
            session.set_overlay(path, source.clone()).map_err(|error| error.to_string())
        })?;
        Self::load_from_session(source_path, &session, progress)
    }

    pub(crate) fn load_from_session(
        source_path: &Path, session: &CompilerSession, mut progress: impl FnMut(AnalysisProgress),
    ) -> Result<Self, String> {
        let graph = session.graph(source_path).map_err(|error| error.to_string())?;
        let source_count = graph.sources.len();
        graph.sources.iter().enumerate().for_each(|(index, (_, source))| {
            progress(AnalysisProgress::Parsing(SourceDiscovery {
                path: source.path.clone(),
                discovered: index + 1,
            }))
        });
        progress(AnalysisProgress::Assembling { source_count });
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
            .ok_or_else(|| format!("assembled source graph omitted `{}`", source_path.display()))?;
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

        Ok(Self { analysis, file_infos, semantic_path, semantic_tokens })
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

    pub(crate) fn hover(&self, file_path: &Path, position: Position) -> Option<Hover> {
        let occurrence = self.symbol_at(file_path, position)?;
        let name = &self.scoped().defs[&occurrence.definition];
        let annotation = self.statics().annotations_var.get(&occurrence.definition)?;
        let formatter = Formatter::new(self.scoped(), self.statics());
        let mut annotation_text = String::new();
        annotation.pretty(&formatter).render_fmt(100, &mut annotation_text).ok()?;
        let definition_type = self.statics().type_definitions.get(&occurrence.definition).copied();
        let definition = definition_type.and_then(|definition| {
            let mut rendered = String::new();
            definition.pretty(&formatter).render_fmt(90, &mut rendered).ok()?;
            Some(TypeDefinitionPreview::new(rendered))
        });
        let displayed_definition =
            definition.as_ref().filter(|definition| definition.is_expanded()).and(definition_type);
        let references =
            TypeReferenceCollector::collect(self.statics(), *annotation, displayed_definition);
        let definitions = references
            .definitions()
            .filter(|definition| *definition != occurrence.definition)
            .filter_map(|definition| self.type_definition_link(definition));
        let sealed_types = references
            .sealed_types()
            .filter(|sealed| {
                self.statics().abst_hints.get(sealed).copied() != Some(occurrence.definition)
            })
            .filter_map(|sealed| self.sealed_type_equation(sealed, &formatter));
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
        &self, sealed: AbstId, formatter: &Formatter<'_>,
    ) -> Option<SealedTypeEquationPreview> {
        let mut rendered = String::new();
        SealedTypeEquation::new(self.statics(), sealed)?
            .pretty(formatter)
            .render_fmt(90, &mut rendered)
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
        let source = self.analysis.source(&path)?;
        let file_info = self.file_infos.get(&path)?;
        let (start, end) = span.get_cursor1();
        Some(Range::new(
            Self::position(file_info.trans_span2_utf16(source, start)?),
            Self::position(file_info.trans_span2_utf16(source, end)?),
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
        progress::{AnalysisProgress, SourceDiscovery},
        semantic::SemanticHighlighter,
    };
    use std::{collections::HashMap, path::Path};
    use tower_lsp::lsp_types::{HoverContents, Position, SemanticToken, SemanticTokensLegend, Url};
    use zydeco_utils::span::{Cursor2, FileInfo};

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
                AnalysisProgress::Parsing(SourceDiscovery { path: root.clone(), discovered: 1 }),
                AnalysisProgress::Parsing(SourceDiscovery { path: library, discovered: 2 }),
                AnalysisProgress::Assembling { source_count: 2 },
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
        let project = ProjectState::load(&root, &HashMap::new()).unwrap();

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
        let hover = project.hover(&library, Position::new(2, 4)).unwrap();
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
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover = project.hover(&path, Position::new(7, 9)).unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let mut definition = Url::from_file_path(&path).unwrap();
        definition.set_fragment(Some("L7"));

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
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover = project.hover(&path, Position::new(17, 11)).unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("type hover should use markup content")
        };
        let mut definition = Url::from_file_path(&path).unwrap();
        definition.set_fragment(Some("L15"));

        assert_eq!(
            contents.value,
            format!("```zydeco\ncopy : A\n```\n\nTypes:\n\n- [`A` ↗](<{definition}>)")
        );
    }

    #[test]
    fn type_hover_expands_short_definitions_and_collapses_long_ones() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/option.zy")
            .canonicalize()
            .unwrap();
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let mut parameter = Url::from_file_path(&path).unwrap();
        parameter.set_fragment(Some("L6"));

        let short = project.hover(&path, Position::new(5, 7)).unwrap();
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

        let long = project.hover(&path, Position::new(29, 7)).unwrap();
        let HoverContents::Markup(long) = long.contents else {
            panic!("type hover should use markup content")
        };
        assert_eq!(long.value, "```zydeco\nInterface : VType =\n  ...\n```");
    }

    #[test]
    fn type_hover_recovers_recursive_definition_bodies() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/recursive-data.zy")
            .canonicalize()
            .unwrap();
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover = project.hover(&path, Position::new(1, 7)).unwrap();
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
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let hover = project.hover(&path, Position::new(14, 12)).unwrap();
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
                    "where\n",
                    "  Nat : VType\n",
                    "    = data\n",
                    "      | +Z : Unit\n",
                    "      | +S : Nat\n",
                    "      end\n",
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
            .join("../../lib/tests/builtin/tuple.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let project = ProjectState::load(&path, &HashMap::new()).unwrap();
        let encoded = project.semantic_tokens(&path).unwrap();
        let decoded = SemanticTokenDecoder::new(&source).decode(&encoded);
        let has = |text: &str, token_type: &str, modifier: &str| {
            decoded.iter().any(|token| {
                token.text == text
                    && token.token_type == token_type
                    && token.modifiers.iter().any(|found| found == modifier)
            })
        };

        assert!(has("VType", "typeParameter", "kind"));
        assert!(has("Int", "typeParameter", "valueType"));
        assert!(has("OS", "typeParameter", "computationType"));
        assert!(has("pair", "variable", "value"));
        assert!(decoded.iter().any(|token| token.text == "exit" && token.token_type == "property"));
        assert!(decoded.iter().any(|token| {
            token.text == "pair" && token.modifiers.iter().any(|modifier| modifier == "declaration")
        }));
    }

    #[test]
    fn semantic_tokens_retain_established_static_classes_after_type_errors() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/builtin/tuple.zy")
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&path).unwrap();
        let broken = source.replace("! (api/exit) status", "pair pair");
        assert_ne!(source, broken);
        let overrides = HashMap::from([(path.clone(), broken.clone())]);
        let project = ProjectState::load(&path, &overrides).unwrap();
        let encoded = project.semantic_tokens(&path).unwrap();
        let decoded = SemanticTokenDecoder::new(&broken).decode(&encoded);

        assert!(decoded.iter().any(|token| {
            token.text == "Int" && token.modifiers.iter().any(|modifier| modifier == "valueType")
        }));
        assert!(decoded.iter().any(|token| {
            token.text == "OS"
                && token.modifiers.iter().any(|modifier| modifier == "computationType")
        }));
    }
}
