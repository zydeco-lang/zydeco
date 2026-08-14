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
    BuiltinDirectiveError, ImportDirectiveError, IntrinsicDirectiveError, LiteralDirectiveError,
    SourceNumber,
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
    #[error("invalid literal directive in `{}`: {error}", path.display())]
    LiteralDirective {
        path: PathBuf,
        #[source]
        error: LiteralDirectiveError,
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
    #[error("cannot resolve REPL input [{input}] imported from `{}` at {span}: {source}", importer.display())]
    ImportInput {
        importer: PathBuf,
        input: SourceNumber,
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
    Cycle(#[from] SourceCycle),
}

#[derive(Clone, Debug, Error)]
pub enum TextualProgramError {
    #[error("source `{}` has no import edge for term {term:?}", path.display())]
    MissingImport { path: PathBuf, term: zydeco_surface::textual::syntax::TermId },
    #[error("source `{}` has no attached text block for literal term {term:?}", path.display())]
    MissingLiteralText { path: PathBuf, term: zydeco_surface::textual::syntax::TermId },
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

/// The kind of edge participating in a source dependency cycle.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SourceDependencyKind {
    Import(SourceImportId),
    Signature,
}

/// One dependency edge in a reported source cycle.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceCycleStep {
    pub kind: SourceDependencyKind,
    pub dependent: PathBuf,
    pub dependency: PathBuf,
    pub span: Span,
}

/// A cycle containing imports, implementation-signature pairs, or both.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceCycle {
    pub steps: Vec<SourceCycleStep>,
}

impl Display for SourceCycle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "cyclic source dependencies")?;
        write!(
            f,
            "{}",
            self.steps
                .iter()
                .map(|step| match step.kind {
                    | SourceDependencyKind::Import(_) => format!(
                        "  `{}` imports `{}` at {}",
                        step.dependent.display(),
                        step.dependency.display(),
                        step.span
                    ),
                    | SourceDependencyKind::Signature => format!(
                        "  `{}` uses companion signature `{}` at {}",
                        step.dependent.display(),
                        step.dependency.display(),
                        step.span
                    ),
                })
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Error for SourceCycle {}
