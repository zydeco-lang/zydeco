use std::path::{Path, PathBuf};

use tower_lsp::lsp_types::{DocumentLink, Position, Range, Url};
use zydeco_session::{
    SourceGraph,
    source::{SourceFile, SourceId},
};
use zydeco_surface::textual::{
    ImportSite, ImportTarget, LexicalTokenKind, LexicalTokens, syntax as t,
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
        let mut links = self
            .graph
            .sources
            .iter()
            .filter(|(_, file)| Self::normalize_path(&file.path) == path)
            .flat_map(|(source, file)| self.source_links(source, file))
            .collect::<Vec<_>>();
        links.sort_by_key(|link| {
            (
                link.range.start.line,
                link.range.start.character,
                link.range.end.line,
                link.range.end.character,
            )
        });
        links.dedup_by(|left, right| left.range == right.range);
        links
    }

    fn source_links(&self, source: SourceId, file: &SourceFile) -> Vec<DocumentLink> {
        let strings = LexicalTokens::new(&file.source)
            .filter(|token| token.kind == LexicalTokenKind::String)
            .collect::<Vec<_>>();

        let imports = file.imports.iter().filter_map(|import| {
            let edge = &self.graph.imports[import];
            debug_assert_eq!(edge.importer, source);
            let site = file.import_sites.iter().find(|site| site.term == edge.term)?;
            let target = match &site.directive.target {
                | ImportTarget::Source(_) => self.graph.sources[&edge.imported].path.clone(),
                | ImportTarget::Input(_) => return None,
            };
            let range = Self::argument_range(file, site)?;
            let target = Url::from_file_path(target).ok()?;
            Some(DocumentLink { range, target: Some(target), tooltip: None, data: None })
        });
        let relationships = file
            .package_sites
            .iter()
            .filter(|site| file.contains_range(&site.span.range()))
            .flat_map(|site| &site.relations)
            .filter_map(|relation| {
                let span = relation.info.range();
                // Each relationship has one source-reference string.
                let literal = strings
                    .iter()
                    .find(|token| span.start <= token.range.start && token.range.end <= span.end)?;
                let range =
                    Self::byte_range(&file.file, literal.range.start + 1..literal.range.end - 1)?;
                let zydeco_surface::metadata::SourceReference::Path(path) = &relation.target else {
                    return None;
                };
                let path = Self::normalize_path(&file.path.parent()?.join(path));
                Some(DocumentLink {
                    range,
                    target: Some(Url::from_file_path(path).ok()?),
                    tooltip: Some(format!("{} relationship", relation.kind)),
                    data: None,
                })
            });
        imports.chain(relationships).collect()
    }

    fn argument_range(file: &SourceFile, site: &ImportSite) -> Option<Range> {
        let t::Term::Meta(t::MetaTerm(meta, _)) = file.arena.terms[&site.term] else {
            unreachable!("validated import site")
        };
        let argument = file.arena.metas[&meta].arguments()[0];
        let mut range = file.spans[&t::EntityId::Meta(argument)].range();
        if matches!(
            site.directive.target,
            ImportTarget::Source(zydeco_surface::metadata::SourceReference::Path(_))
        ) {
            range = range.start + 1..range.end - 1;
        }
        Self::byte_range(&file.file, range)
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
            "(\"🦀\", @(import(1)), @(import(\"library.zy\")), ",
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

    #[test]
    fn package_import_links_its_source_without_treating_the_package_name_as_a_path() {
        let directory = tempfile::tempdir().unwrap();
        let packages = directory.path().join("package.zy");
        let root = directory.path().join("main.zy");
        let source = r#"@(import(main))"#;
        std::fs::write(&packages, r#"(#main = @[package(library, name(main))] 1)"#).unwrap();
        std::fs::write(&root, source).unwrap();
        let session = CompilerSession::default();
        let catalog = session.package_catalog(std::slice::from_ref(&packages)).unwrap();
        let analysis = session
            .analyze_package(
                &zydeco_session::source::PackageId { path: root.clone(), name: None },
                catalog.bindings,
            )
            .unwrap();
        let links = ImportDocumentLinks::new(analysis.graph()).for_file(&root);
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].range, source_range(source, "main"));
        assert_eq!(
            links[0].target,
            Some(Url::from_file_path(packages.canonicalize().unwrap()).unwrap())
        );
    }

    #[test]
    fn package_links_cover_multiple_roots_and_unloaded_relationship_targets() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path().join("packages.zy");
        let first = directory.path().join("one.zy");
        let second = directory.path().join("two.zy");
        let source = r#"(
            #one = @[package(library, test("missing.zy"), name(one))] @(import("one.zy")),
            #two = @[package(library, name(two))] @(import("two.zy"))
        )"#;
        std::fs::write(&root, source).unwrap();
        std::fs::write(&first, "1").unwrap();
        std::fs::write(&second, "2").unwrap();
        let graph = CompilerSession::default().graph(&root).unwrap();
        let links = ImportDocumentLinks::new(&graph).for_file(&root);
        assert_eq!(links.len(), 3);
        for (index, text) in ["missing.zy", "one.zy", "two.zy"].into_iter().enumerate() {
            assert_eq!(links[index].range, source_range(source, text));
        }
        assert_eq!(links[0].tooltip.as_deref(), Some("test relationship"));
        assert!(links[0].target.as_ref().unwrap().path().ends_with("missing.zy"));
        let main = directory.path().join("main.zy");
        std::fs::write(&main, r#"@(import(one))"#).unwrap();
        let session = CompilerSession::default();
        let catalog = session.package_catalog(std::slice::from_ref(&root)).unwrap();
        let analysis = session
            .analyze_package(
                &zydeco_session::source::PackageId { path: main, name: None },
                catalog.bindings,
            )
            .unwrap();
        let links = ImportDocumentLinks::new(analysis.graph()).for_file(&root);
        assert_eq!(links.len(), 2, "a selected root does not link unrelated registrations");
        assert_eq!(links[0].range, source_range(source, "missing.zy"));
        assert_eq!(links[1].range, source_range(source, "one.zy"));
    }

    #[test]
    fn test_subjects_link_each_address_without_loading_subjects_or_discovery() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path().join("test.zy");
        let source = r#"@[discover(include("*.zy"))] @[package(test(of("one.zy", "two.zy", namespace/library)))] 1"#;
        std::fs::write(&root, source).unwrap();
        let graph = CompilerSession::default().graph(&root).unwrap();
        let links = ImportDocumentLinks::new(&graph).for_file(&root);
        assert_eq!(links.len(), 2);
        for (link, text) in links.iter().zip(["one.zy", "two.zy"]) {
            assert_eq!(link.range, source_range(source, text));
            assert_eq!(link.tooltip.as_deref(), Some("of relationship"));
            assert!(link.target.as_ref().unwrap().path().ends_with(text));
        }
    }
}
