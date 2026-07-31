use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use tower_lsp::lsp_types::{DocumentSymbol, Location, Position, Range, SymbolKind, Url};
use zydeco_driver::source::{SourceGraph, SourceScoped};
use zydeco_surface::scoped::syntax::{DefId, Term};
use zydeco_syntax::SpanView;
use zydeco_utils::span::{Cursor2, FileInfo, Span};

/// Name-resolved source state for one editor root.
pub(crate) struct ProjectState {
    scoped: SourceScoped,
    file_infos: HashMap<PathBuf, FileInfo>,
}

impl ProjectState {
    pub(crate) fn load(
        source_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> Result<Self, String> {
        let scoped = SourceGraph::load_with_overrides(source_path, overrides)
            .map_err(|error| format!("Source error: {error}"))?
            .assemble()
            .map_err(|error| format!("Assembly error: {error}"))?
            .desugar()
            .map_err(|error| format!("Desugaring error: {error}"))?
            .resolve()
            .map_err(|error| format!("Resolution error: {error}"))?;
        let file_infos = scoped
            .sources
            .iter()
            .map(|(path, source)| {
                let path = Self::normalize_path(path);
                let info = FileInfo::new(source, Some(Arc::new(path.clone())));
                (path, info)
            })
            .collect();

        Ok(Self { scoped, file_infos })
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
