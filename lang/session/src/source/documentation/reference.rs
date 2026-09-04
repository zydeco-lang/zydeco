use super::{
    DocumentationContent, DocumentationExposure, DocumentationExposureError,
    DocumentationExposureQuery, DocumentationLinkTarget, DocumentationPath,
};
use crate::{AnalysisError, CompilerSession, ProgramAnalysis};
use std::{path::Path, sync::Arc};
use thiserror::Error;
use zydeco_statics::{
    arena::StaticsArena,
    syntax::{AnnId, TermAnnId},
};

/// One public reference and the exact immutable analysis owning its identities.
pub struct DocumentationReference {
    pub analysis: Arc<ProgramAnalysis>,
    pub statics: Arc<StaticsArena>,
    pub entries: Vec<DocumentationExposure>,
}

#[derive(Debug, Error)]
pub enum DocumentationReferenceError {
    #[error(transparent)]
    Analysis(#[from] AnalysisError),
    #[error("public documentation requires a successfully checked entry root")]
    Rejected,
    #[error(transparent)]
    Exposure(#[from] DocumentationExposureError),
}

impl CompilerSession {
    pub fn documentation_reference(
        &self, root: impl AsRef<Path>,
    ) -> Result<DocumentationReference, DocumentationReferenceError> {
        let analysis = self.analyze(root)?;
        let program =
            self.checked_program(&analysis).ok_or(DocumentationReferenceError::Rejected)?;
        let classifier = match program.root {
            | TermAnnId::Type(ty, _) | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => {
                AnnId::Type(ty)
            }
            | TermAnnId::Kind(kind) => AnnId::Kind(kind),
            | TermAnnId::Hole(_) => return Err(DocumentationReferenceError::Rejected),
        };
        let root = DocumentationExposure {
            path: DocumentationPath::default(),
            classifier,
            declaration: Some(analysis.scoped_root()),
            recursive_to: None,
        };
        let members = DocumentationExposureQuery::new(&program.statics).collect(classifier)?;
        Ok(DocumentationReference {
            analysis,
            statics: program.statics,
            entries: std::iter::once(root).chain(members).collect(),
        })
    }
}

impl DocumentationReference {
    pub fn content(&self, entry: &DocumentationExposure) -> DocumentationContent<'_> {
        entry.declaration.map_or_else(DocumentationContent::default, |term| {
            self.analysis.documentation().for_term(term)
        })
    }

    pub fn get(&self, path: &DocumentationPath) -> Option<&DocumentationExposure> {
        self.entries.iter().find(|entry| entry.path == *path)
    }

    pub fn search(&self, query: &str) -> Vec<&DocumentationExposure> {
        let words = query.split_whitespace().map(str::to_lowercase).collect::<Vec<_>>();
        self.entries
            .iter()
            .filter(|entry| {
                let text =
                    format!("{}\n{}", entry.path, self.content(entry).markdown()).to_lowercase();
                words.iter().all(|word| text.contains(word))
            })
            .collect()
    }

    /// Reuse an exposed route only when provenance identifies a unique page.
    pub fn target_path(&self, target: &DocumentationLinkTarget) -> Option<&DocumentationPath> {
        if let DocumentationLinkTarget::Member { declaration: Some(declaration), .. } = target {
            let mut exact =
                self.entries.iter().filter(|entry| entry.declaration == Some(*declaration));
            if let Some(first) = exact.next() {
                return exact.next().is_none().then_some(&first.path);
            }
        }
        let content = match target {
            | DocumentationLinkTarget::Definition(definition) => {
                self.analysis.documentation().for_definition(*definition)
            }
            | DocumentationLinkTarget::Member { declaration: Some(term), .. } => {
                self.analysis.documentation().for_term(*term)
            }
            | DocumentationLinkTarget::Member { declaration: None, .. } => return None,
        };
        let mut candidates = self.entries.iter().filter(|entry| {
            self.content(entry)
                .entries()
                .any(|doc| content.entries().any(|origin| origin.id == doc.id))
        });
        let first = candidates.next()?;
        candidates.next().is_none().then_some(&first.path)
    }
}
