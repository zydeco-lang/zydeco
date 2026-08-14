use crate::source::{SourceFile, SourceGraph, SourceId};
use std::{ops::Range, path::Path};
use zydeco_surface::textual::UnattachedTextWarning;

/// A non-fatal issue discovered while interpreting one parsed source file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceWarning {
    UnattachedText(UnattachedTextWarning),
}

impl SourceWarning {
    /// Stable tooling identifier for this warning kind.
    pub fn code(&self) -> &'static str {
        match self {
            | Self::UnattachedText(_) => "unattached-text-block",
        }
    }

    /// Primary user-facing warning text.
    pub fn message(&self) -> &'static str {
        match self {
            | Self::UnattachedText(_) => {
                "text block is not attached to an annotation and has no effect"
            }
        }
    }

    /// Actionable guidance shared by diagnostic frontends.
    pub fn note(&self) -> &'static str {
        match self {
            | Self::UnattachedText(_) => {
                "Place `@[doc]` or `@[literal]` immediately after the block, or use `--` for an ordinary comment."
            }
        }
    }

    /// Byte range occupied by the ineffective source construct.
    pub fn range(&self) -> &Range<usize> {
        match self {
            | Self::UnattachedText(warning) => &warning.range,
        }
    }
}

impl From<UnattachedTextWarning> for SourceWarning {
    fn from(warning: UnattachedTextWarning) -> Self {
        Self::UnattachedText(warning)
    }
}

/// One source warning together with its source-graph provenance.
#[derive(Clone, Copy, Debug)]
pub struct SourceWarningSite<'graph> {
    pub source: SourceId,
    pub file: &'graph SourceFile,
    pub warning: &'graph SourceWarning,
}

impl SourceWarningSite<'_> {
    pub fn path(&self) -> &Path {
        &self.file.path
    }

    pub fn warning_source(&self) -> &str {
        &self.file.source[self.warning.range().clone()]
    }
}

impl SourceGraph {
    /// Return every source warning in deterministic provider-before-consumer
    /// and source order.
    pub fn warnings(&self) -> Vec<SourceWarningSite<'_>> {
        self.provider_order()
            .into_iter()
            .flat_map(|source| {
                let file = &self.sources[&source];
                file.warnings.iter().map(move |warning| SourceWarningSite { source, file, warning })
            })
            .collect()
    }
}
