use crate::source::{SourceFile, SourceGraph, SourceId};
use std::path::Path;
use zydeco_surface::textual::{DocumentationSite, syntax as t};

/// One documentation attachment together with the source arena needed to
/// interpret and eventually render its term.
#[derive(Clone, Copy, Debug)]
pub struct RepositoryDocumentationEntry<'graph> {
    pub source: SourceId,
    pub file: &'graph SourceFile,
    pub site: &'graph DocumentationSite,
}

impl RepositoryDocumentationEntry<'_> {
    pub fn path(&self) -> &Path {
        &self.file.path
    }

    pub fn term(&self) -> &t::Term {
        &self.file.arena.terms[&self.site.payload]
    }

    pub fn term_source(&self) -> &str {
        let span = &self.file.spans[&t::EntityId::Term(self.site.payload)];
        let (start, end) = span.get_cursor1();
        &self.file.source[start..end]
    }
}

impl SourceGraph {
    /// Return every explicit documentation attachment in deterministic
    /// provider-before-consumer and source order.
    pub fn documentation(&self) -> Vec<RepositoryDocumentationEntry<'_>> {
        self.provider_order()
            .into_iter()
            .flat_map(|source| {
                let file = &self.sources[&source];
                file.documentation.iter().map(move |site| RepositoryDocumentationEntry {
                    source,
                    file,
                    site,
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;
    use zydeco_surface::textual::syntax::{Hole, Meta, MetaT, Term};

    #[test]
    fn source_graph_collects_documented_terms_across_imports() {
        let directory = tempdir().unwrap();
        let provider = directory.path().join("provider.zy");
        let root = directory.path().join("root.zy");
        fs::write(&provider, "--| Provider term\n@[doc(module,\"provider\")] _\n").unwrap();
        fs::write(
            &root,
            concat!(
                "(\n",
                "  --| Imported provider\n",
                "  @[doc(import)] @[import(\"provider.zy\")] _,\n",
                "  --| Root literal\n",
                "  @[doc(example)] 1\n",
                ")\n",
            ),
        )
        .unwrap();

        let graph = SourceGraph::load(&root).unwrap();
        let documentation = graph.documentation();
        let [provider_doc, import_doc, literal_doc] = documentation.as_slice() else {
            panic!("expected documentation from both repository sources")
        };

        assert_eq!(provider_doc.path(), provider.canonicalize().unwrap());
        assert_eq!(provider_doc.site.directive.comment.as_ref().unwrap().markdown, "Provider term");
        assert_eq!(
            provider_doc.site.directive.meta.arguments,
            [Meta::ident("module"), Meta::string("provider")]
        );
        assert_eq!(provider_doc.term_source(), "_");
        assert!(matches!(provider_doc.term(), Term::Hole(Hole)));

        assert_eq!(import_doc.path(), root.canonicalize().unwrap());
        assert_eq!(
            import_doc.site.directive.comment.as_ref().unwrap().markdown,
            "Imported provider"
        );
        assert_eq!(import_doc.term_source(), "@[import(\"provider.zy\")] _");
        assert!(matches!(import_doc.term(), Term::Meta(MetaT(meta, _)) if meta.is("import")));

        assert_eq!(literal_doc.site.directive.comment.as_ref().unwrap().markdown, "Root literal");
        assert_eq!(literal_doc.term_source(), "1");
    }
}
