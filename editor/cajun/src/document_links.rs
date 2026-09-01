use std::path::{Path, PathBuf};

use tower_lsp::lsp_types::{DocumentLink, Position, Range, Url};
use zydeco_session::{SourceGraph, source::SourceFile};
use zydeco_surface::textual::{
    ImportSite, ImportTarget, LexicalToken, LexicalTokenKind, LexicalTokens,
};
use zydeco_utils::span::{FileMap, LineCol};

/// Filesystem import links derived from one compiler source graph.
pub(crate) struct ImportDocumentLinks<'graph> {
    graph: &'graph SourceGraph,
}

impl<'graph> ImportDocumentLinks<'graph> {
    pub(crate) fn new(graph: &'graph SourceGraph) -> Self {
        Self { graph }
    }

    pub(crate) fn for_file(&self, path: &Path) -> Vec<DocumentLink> {
        let path = Self::normalize_path(path);
        let Some((source, file)) =
            self.graph.sources.iter().find(|(_, file)| Self::normalize_path(&file.path) == path)
        else {
            return Vec::new();
        };
        let strings = LexicalTokens::new(&file.source)
            .filter(|token| token.kind == LexicalTokenKind::String)
            .collect::<Vec<_>>();

        file.imports
            .iter()
            .filter_map(|import| {
                let edge = &self.graph.imports[import];
                debug_assert_eq!(edge.importer, source);
                let site = file.import_sites.iter().find(|site| site.term == edge.term)?;
                let ImportTarget::Path(_) = &site.directive.target else {
                    return None;
                };
                let range = Self::argument_range(file, site, &strings)?;
                let target = Url::from_file_path(&self.graph.sources[&edge.imported].path).ok()?;
                Some(DocumentLink { range, target: Some(target), tooltip: None, data: None })
            })
            .collect()
    }

    fn argument_range(
        file: &SourceFile, site: &ImportSite, strings: &[LexicalToken],
    ) -> Option<Range> {
        let directive = site.directive.span.range();
        let literal = strings.iter().find(|token| {
            directive.start <= token.range.start && token.range.end <= directive.end
        })?;
        let content = literal.range.start.checked_add(1)?..literal.range.end.checked_sub(1)?;
        Self::byte_range(&file.file, content)
    }

    fn byte_range(file: &FileMap, range: std::ops::Range<usize>) -> Option<Range> {
        Some(Range::new(
            Self::position(file.line_col_utf16(range.start)?),
            Self::position(file.line_col_utf16(range.end)?),
        ))
    }

    fn position(cursor: LineCol) -> Position {
        Position::new(cursor.line, cursor.column)
    }

    fn normalize_path(path: &Path) -> PathBuf {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }
}

#[cfg(test)]
mod tests {
    use super::ImportDocumentLinks;
    use tower_lsp::lsp_types::{Position, Range, Url};
    use zydeco_session::CompilerSession;
    use zydeco_surface::textual::SourceNumber;
    use zydeco_utils::span::{FileMap, LineCol};

    fn source_range(source: &str, text: &str) -> Range {
        let start = source.find(text).unwrap();
        let end = start + text.len();
        let file = FileMap::local(source, None);
        let position = |offset| {
            let LineCol { line, column } = file.line_col_utf16(offset).unwrap();
            Position::new(line, column)
        };
        Range::new(position(start), position(end))
    }

    #[test]
    fn path_imports_link_their_unquoted_arguments_to_canonical_sources() {
        let directory = tempfile::tempdir().unwrap();
        let nested = directory.path().join("nested");
        std::fs::create_dir(&nested).unwrap();
        let library = directory.path().join("library.zy");
        let leaf = nested.join("leaf.zy");
        let root = directory.path().join("main.zy");
        let source = concat!(
            "(\"🦀\", @[import(1)] _, @[import(\"library.zy\")] _, ",
            "@(import(\"nested/leaf.zy\")))\n",
        );
        std::fs::write(&library, "()\n").unwrap();
        std::fs::write(&leaf, "()\n").unwrap();
        std::fs::write(&root, source).unwrap();

        let mut session = CompilerSession::default();
        let input = SourceNumber::new(1).unwrap().overlay_path(directory.path());
        session.set_overlay(input, "()\n".to_owned()).unwrap();
        let graph = session.graph(&root).unwrap();
        let links = ImportDocumentLinks::new(&graph).for_file(&root);

        assert_eq!(links.len(), 2);
        assert_eq!(links[0].range, source_range(source, "library.zy"));
        assert_eq!(
            links[0].target,
            Some(Url::from_file_path(library.canonicalize().unwrap()).unwrap())
        );
        assert_eq!(links[1].range, source_range(source, "nested/leaf.zy"));
        assert_eq!(
            links[1].target,
            Some(Url::from_file_path(leaf.canonicalize().unwrap()).unwrap())
        );
        assert!(links.iter().all(|link| link.tooltip.is_none() && link.data.is_none()));
    }
}
