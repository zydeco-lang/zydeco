use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use tower_lsp::lsp_types::{
    DocumentSymbol, Location, Position, Range, SemanticToken, SymbolKind, Url,
};
use zydeco_driver::source::{SourceGraph, SourceScoped};
use zydeco_statics::tyck::{HoleSolutionOutput, Tycker};
use zydeco_surface::scoped::syntax::{DefId, Term};
use zydeco_syntax::SpanView;
use zydeco_utils::span::{Cursor2, FileInfo, Span};

use crate::semantic::SemanticHighlighter;

/// Compiler analysis state for one editor root.
pub(crate) struct ProjectState {
    scoped: SourceScoped,
    file_infos: HashMap<PathBuf, FileInfo>,
    semantic_path: PathBuf,
    semantic_tokens: Vec<SemanticToken>,
}

impl ProjectState {
    pub(crate) fn load(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> Result<Self, String> {
        let mut scoped = SourceGraph::load_with_overrides(source_path, overrides)
            .map_err(|error| format!("Source error: {error}"))?
            .assemble()
            .map_err(|error| format!("Assembly error: {error}"))?
            .desugar()
            .map_err(|error| format!("Desugaring error: {error}"))?
            .resolve()
            .map_err(|error| format!("Resolution error: {error}"))?;
        let root = scoped.root;
        let statics = Tycker::new(&scoped.spans, &scoped.prim, &mut scoped.arena)
            .with_hole_solution_output(HoleSolutionOutput::Silent)
            .check_source_outcome(root)
            .into_statics();
        let file_infos = scoped
            .sources
            .iter()
            .map(|(path, source)| {
                let path = Self::normalize_path(path);
                let info = FileInfo::new(source, Some(Arc::new(path.clone())));
                (path, info)
            })
            .collect();
        let source_path = Self::normalize_path(source_path);
        let source = scoped
            .sources
            .get(&source_path)
            .or_else(|| {
                scoped.sources.iter().find_map(|(path, source)| {
                    (Self::normalize_path(path) == source_path).then_some(source)
                })
            })
            .ok_or_else(|| format!("assembled source graph omitted `{}`", source_path.display()))?;
        let tokens = SemanticHighlighter::compiler_refined(
            source,
            &source_path,
            &scoped.spans,
            &scoped.arena,
            Some(&statics),
        );
        let semantic_path = source_path;
        let semantic_tokens = tokens;

        Ok(Self { scoped, file_infos, semantic_path, semantic_tokens })
    }

    pub(crate) fn definition(&self, file_path: &Path, position: Position) -> Option<Location> {
        let definition = self.definition_at(file_path, position)?;
        self.definition_location(definition)
    }

    pub(crate) fn document_symbols(&self, file_path: &Path) -> Vec<DocumentSymbol> {
        let file_path = Self::normalize_path(file_path);
        let mut symbols = self
            .scoped
            .arena
            .defs
            .iter()
            .filter_map(|(definition, name)| {
                let entity = self.scoped.arena.textual.back(&(*definition).into())?;
                let span = &self.scoped.spans[entity];
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

    fn definition_at(&self, file_path: &Path, position: Position) -> Option<DefId> {
        let file_path = Self::normalize_path(file_path);
        let offset = self.offset(&file_path, position)?;
        let spans = (&self.scoped.spans, &self.scoped.arena);
        self.scoped
            .arena
            .terms
            .iter()
            .filter_map(|(term, body)| {
                let Term::Var(definition) = body else {
                    return None;
                };
                let span = term.span(&spans);
                let (start, end) = span.get_cursor1();
                let same_file = span.get_path().map(|path| Self::normalize_path(path))
                    == Some(file_path.clone());
                (same_file && start <= offset && offset < end)
                    .then_some((end.saturating_sub(start), *definition))
            })
            .min_by_key(|(length, _)| *length)
            .map(|(_, definition)| definition)
    }

    fn definition_location(&self, definition: DefId) -> Option<Location> {
        let entity = self.scoped.arena.textual.back(&definition.into())?;
        let span = &self.scoped.spans[entity];
        let path = Self::normalize_path(span.get_path()?);
        Some(Location { uri: Url::from_file_path(path).ok()?, range: self.span_range(span)? })
    }

    fn offset(&self, file_path: &Path, position: Position) -> Option<usize> {
        let source = self.scoped.sources.get(file_path)?;
        self.file_infos.get(file_path)?.trans_span1_utf16(
            source,
            Cursor2 { line: position.line as usize, column: position.character as usize },
        )
    }

    fn span_range(&self, span: &Span) -> Option<Range> {
        let path = Self::normalize_path(span.get_path()?);
        let source = self.scoped.sources.get(&path)?;
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
    use crate::semantic::SemanticHighlighter;
    use std::{collections::HashMap, path::Path};
    use tower_lsp::lsp_types::{SemanticToken, SemanticTokensLegend};
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
