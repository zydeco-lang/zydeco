use crate::source::SourceImportId;
use std::{
    error::Error,
    fmt::{Display, Formatter},
    io,
    path::PathBuf,
    sync::Arc,
};
use thiserror::Error;
use zydeco_statics::syntax::TermAnnId;
use zydeco_surface::textual::{
    BuiltinDirectiveError, ImportDirectiveError, IntrinsicDirectiveError,
};
use zydeco_utils::span::Span;

/// A deterministic source-template error suitable for memoized parsing.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum SourceParseError {
    #[error("cannot parse source `{}`: {message}", path.display())]
    Parse { path: PathBuf, message: String },
    #[error("invalid source directive in `{}`: {error}", path.display())]
    Directive {
        path: PathBuf,
        #[source]
        error: ImportDirectiveError,
    },
    #[error("invalid Builtin directive in `{}`: {error}", path.display())]
    BuiltinDirective {
        path: PathBuf,
        #[source]
        error: BuiltinDirectiveError,
    },
    #[error("invalid intrinsic directive in `{}`: {error}", path.display())]
    IntrinsicDirective {
        path: PathBuf,
        #[source]
        error: IntrinsicDirectiveError,
    },
}

#[derive(Clone, Debug, Error)]
pub enum SourceLoadError {
    #[error("cannot resolve root source `{}`: {source}", path.display())]
    RootPath {
        path: PathBuf,
        #[source]
        source: Arc<io::Error>,
    },
    #[error(
        "cannot resolve import `{}` from `{}` at {span}: {source}",
        requested.display(),
        importer.display()
    )]
    ImportPath {
        importer: PathBuf,
        requested: PathBuf,
        span: Span,
        #[source]
        source: Arc<io::Error>,
    },
    #[error("cannot read source `{}`: {source}", path.display())]
    Read {
        path: PathBuf,
        #[source]
        source: Arc<io::Error>,
    },
    #[error(transparent)]
    Parse(#[from] SourceParseError),
    #[error(transparent)]
    Cycle(#[from] ImportCycle),
}

#[derive(Clone, Debug, Error)]
pub enum ProgramAssemblyError {
    #[error("source `{}` has no import edge for term {term:?}", path.display())]
    MissingImport { path: PathBuf, term: zydeco_surface::textual::syntax::TermId },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CheckedRootSort {
    Hole,
    Kind,
    Type,
    Value,
    Computation,
}

impl From<TermAnnId> for CheckedRootSort {
    fn from(root: TermAnnId) -> Self {
        match root {
            | TermAnnId::Hole(_) => Self::Hole,
            | TermAnnId::Kind(_) => Self::Kind,
            | TermAnnId::Type(_, _) => Self::Type,
            | TermAnnId::Value(_, _) => Self::Value,
            | TermAnnId::Compu(_, _) => Self::Computation,
        }
    }
}

impl Display for CheckedRootSort {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Hole => write!(f, "an unclassified hole"),
            | Self::Kind => write!(f, "a kind"),
            | Self::Type => write!(f, "a type"),
            | Self::Value => write!(f, "a value"),
            | Self::Computation => write!(f, "a computation"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportCycleStep {
    pub import: SourceImportId,
    pub importer: PathBuf,
    pub imported: PathBuf,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportCycle {
    pub steps: Vec<ImportCycleStep>,
}

impl Display for ImportCycle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "cyclic source imports")?;
        write!(
            f,
            "{}",
            self.steps
                .iter()
                .map(|step| format!(
                    "  `{}` imports `{}` at {}",
                    step.importer.display(),
                    step.imported.display(),
                    step.span
                ))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Error for ImportCycle {}
