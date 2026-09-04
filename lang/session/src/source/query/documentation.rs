use super::{CompilerSession, SourceInput, SourceQueryDb, source_text};
use crate::source::{DocumentationIndex, SourceLoadError};
use std::{path::Path, sync::Arc};

impl CompilerSession {
    pub(crate) fn documentation_inputs(&self) -> Vec<crate::source::DocumentationExampleInput> {
        self.files
            .iter()
            .filter_map(|entry| {
                source_text(self, *entry).map(|source| crate::source::DocumentationExampleInput {
                    path: entry.key().clone(),
                    source,
                })
            })
            .collect()
    }

    /// Current authored documentation, available independently of imports,
    /// name resolution, and checking. Unrecoverable attachments are omitted.
    pub fn source_documentation(
        &self, path: impl AsRef<Path>,
    ) -> Result<Arc<DocumentationIndex>, SourceLoadError> {
        let input = self.source_input(path.as_ref().to_path_buf())?;
        Ok(source_documentation(self, input))
    }
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 16)]
fn source_documentation(db: &dyn SourceQueryDb, input: SourceInput) -> Arc<DocumentationIndex> {
    Arc::new(source_text(db, input).map_or_else(DocumentationIndex::default, |text| {
        DocumentationIndex::recover(&input.path(db), &text)
    }))
}
