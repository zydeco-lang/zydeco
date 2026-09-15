use crate::source::SourceImportId;
use std::{
    fmt::{Display, Formatter},
    io,
    ops::Range,
    path::PathBuf,
    sync::Arc,
};
use thiserror::Error;
use zydeco_statics::syntax::TermAnnId;
use zydeco_surface::textual::{
    BuiltinDirectiveError, ImportDirectiveError, IntrinsicDirectiveError, LiteralDirectiveError,
    PackageDirectiveError, ParseError, SourceNumber, syntax::SpanArena,
};
use zydeco_utils::span::Span;

/// One presentation-ready source failure, retaining its own primary location.
#[derive(Clone, Debug)]
pub struct SourceDiagnostic {
    pub message: String,
    pub site: Option<SourceDiagnosticSite>,
}

/// One compiler failure's primary location in file-relative byte coordinates.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceDiagnosticSite {
    path: PathBuf,
    range: Range<usize>,
}

impl SourceDiagnosticSite {
    pub fn new(path: PathBuf, range: Range<usize>) -> Self {
        Self { path, range }
    }

    pub fn from_span(spans: &SpanArena, span: Span) -> Option<Self> {
        let (file, range) = spans.source_map()?.range(span)?;
        Some(Self::new(file.path(), range))
    }

    pub fn path(&self) -> &std::path::Path {
        &self.path
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }
}

impl Display for SourceDiagnosticSite {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}:{}..{}", self.path.display(), self.range.start, self.range.end)
    }
}

pub type SourceParseErrors = zydeco_surface::diagnostic::Diagnostics<SourceParseError>;

/// Every independent failure of a source graph load. A rejected load has no graph product.
#[derive(Clone, Debug, Error)]
#[error(transparent)]
pub struct SourceLoadErrors(zydeco_surface::diagnostic::Diagnostics<SourceLoadError>);

impl SourceLoadErrors {
    pub(crate) fn with_errors(errors: Vec<SourceLoadError>) -> Option<Self> {
        zydeco_surface::diagnostic::Diagnostics::with_errors(errors).map(Self)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, SourceLoadError> {
        self.0.iter()
    }

    pub fn diagnostics(&self) -> Vec<SourceDiagnostic> {
        self.iter().flat_map(SourceLoadError::diagnostics).collect()
    }

    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        self.iter().find_map(SourceLoadError::diagnostic_site)
    }
}

impl From<SourceLoadError> for SourceLoadErrors {
    fn from(error: SourceLoadError) -> Self {
        Self(error.into())
    }
}

/// One deterministic source-template error suitable for memoized parsing.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum SourceParseError {
    #[error("invalid discovery declaration in `{}`: {error}", path.display())]
    DiscoveryDirective {
        path: PathBuf,
        error: Box<zydeco_surface::textual::DiscoveryDirectiveError>,
    },
    #[error("invalid package declaration in `{}`: {error}", path.display())]
    PackageDirective {
        path: PathBuf,
        #[source]
        error: Box<PackageDirectiveError>,
    },
    #[error("cannot parse source: {error}")]
    Parse {
        #[source]
        error: Box<ParseError>,
    },
    #[error("invalid source directive in `{}`: {error}", path.display())]
    Directive {
        path: PathBuf,
        #[source]
        error: Box<ImportDirectiveError>,
    },
    #[error("invalid Builtin directive in `{}`: {error}", path.display())]
    BuiltinDirective {
        path: PathBuf,
        #[source]
        error: Box<BuiltinDirectiveError>,
    },
    #[error("invalid intrinsic directive in `{}`: {error}", path.display())]
    IntrinsicDirective {
        path: PathBuf,
        #[source]
        error: Box<IntrinsicDirectiveError>,
    },
    #[error("invalid literal directive in `{}`: {error}", path.display())]
    LiteralDirective {
        path: PathBuf,
        #[source]
        error: Box<LiteralDirectiveError>,
    },
}

impl SourceParseError {
    pub fn diagnostics(&self) -> Vec<SourceDiagnostic> {
        match self {
            | Self::Parse { error } => {
                error
                    .diagnostics()
                    .map(|diagnostic| SourceDiagnostic {
                        message: diagnostic.to_string(),
                        site: diagnostic.issue.range.clone().map(|range| {
                            SourceDiagnosticSite::new(diagnostic.file_map.path(), range)
                        }),
                    })
                    .collect()
            }
            | _ => {
                vec![SourceDiagnostic { message: self.to_string(), site: self.diagnostic_site() }]
            }
        }
    }

    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        let (path, range) = match self {
            | Self::Parse { error } => {
                return Some(SourceDiagnosticSite::new(
                    error.file_map.path(),
                    error.source_range()?,
                ));
            }
            | Self::Directive { path, error } => (path, error.span().range()),
            | Self::BuiltinDirective { path, error } => (path, error.span().range()),
            | Self::IntrinsicDirective { path, error } => (path, error.span().range()),
            | Self::LiteralDirective { path, error } => (path, error.span().range()),
            | Self::DiscoveryDirective { path, error } => (path, error.span().range()),
            | Self::PackageDirective { path, error } => (path, error.span().range()),
        };
        Some(SourceDiagnosticSite::new(path.clone(), range))
    }
}

#[derive(Clone, Debug, Error)]
pub enum SourceLoadError {
    #[error(transparent)]
    Package(Box<super::PackageError>),
    #[error("cannot resolve package import from `{}` at {span}: {error}", importer.display())]
    PackageImport {
        importer: PathBuf,
        span: Span,
        #[source]
        error: Box<SourceLoadError>,
    },
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
        span: Box<Span>,
        #[source]
        source: Arc<io::Error>,
    },
    #[error("cannot resolve REPL input [{input}] imported from `{}` at {span}: {source}", importer.display())]
    ImportInput {
        importer: PathBuf,
        input: SourceNumber,
        span: Box<Span>,
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
    Parse(#[from] SourceParseErrors),
    #[error(transparent)]
    Cycle(#[from] SourceCycle),
}

impl SourceLoadError {
    pub fn diagnostics(&self) -> Vec<SourceDiagnostic> {
        match self {
            | Self::Parse(errors) => {
                errors.iter().flat_map(SourceParseError::diagnostics).collect()
            }
            | Self::PackageImport { importer, span, error } => error
                .diagnostics()
                .into_iter()
                .map(|mut diagnostic| {
                    diagnostic.site = diagnostic.site.or_else(|| {
                        Some(SourceDiagnosticSite::new(importer.clone(), span.range()))
                    });
                    diagnostic
                })
                .collect(),
            | _ => {
                vec![SourceDiagnostic { message: self.to_string(), site: self.diagnostic_site() }]
            }
        }
    }

    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        match self {
            | Self::Package(error) => error.diagnostic_site(),
            | Self::PackageImport { importer, span, error } => error
                .diagnostic_site()
                .or_else(|| Some(SourceDiagnosticSite::new(importer.clone(), span.range()))),
            | Self::RootPath { .. } | Self::Read { .. } => None,
            | Self::ImportPath { importer, span, .. }
            | Self::ImportInput { importer, span, .. } => {
                Some(SourceDiagnosticSite::new(importer.clone(), span.range()))
            }
            | Self::Parse(errors) => errors.iter().find_map(SourceParseError::diagnostic_site),
            | Self::Cycle(cycle) => cycle.steps.first().map(|step| {
                let path = match step.kind {
                    | SourceDependencyKind::Import(_) => &step.dependent,
                    | SourceDependencyKind::Signature => &step.dependency,
                };
                SourceDiagnosticSite::new(path.clone(), step.span.range())
            }),
        }
    }
}

impl From<super::PackageError> for SourceLoadError {
    fn from(error: super::PackageError) -> Self {
        Self::Package(Box::new(error))
    }
}

#[derive(Clone, Debug, Error)]
pub enum TextualProgramError {
    #[error("source `{}` has no import edge for term {term:?}", path.display())]
    MissingImport { path: PathBuf, term: zydeco_surface::textual::syntax::TermId, span: Span },
    #[error("source `{}` has no attached text block for literal term {term:?}", path.display())]
    MissingLiteralText { path: PathBuf, term: zydeco_surface::textual::syntax::TermId, span: Span },
}

impl TextualProgramError {
    pub fn diagnostic_site(&self) -> SourceDiagnosticSite {
        match self {
            | Self::MissingImport { path, span, .. }
            | Self::MissingLiteralText { path, span, .. } => {
                SourceDiagnosticSite::new(path.clone(), span.range())
            }
        }
    }
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
#[derive(Clone, Debug, PartialEq, Eq, Error)]
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
