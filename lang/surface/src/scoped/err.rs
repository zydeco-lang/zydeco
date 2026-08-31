use crate::bitter::syntax::*;
use crate::textual::syntax::SpanArena;
use ariadne::{Label, Report, ReportKind};
use std::ops::Range;
use thiserror::Error;
use zydeco_utils::span::{PathDisplay, Span, internal_ariadne_span};

/// Errors reported during name resolution.
#[derive(Error, Debug, Clone)]
pub enum ResolveError {
    #[error("Unbound variable: {0}")]
    UnboundVar(Sp<VarName>),
    #[error("Duplicate definition: {0} and {1}")]
    DuplicateDefinition(Sp<VarName>, Sp<VarName>),
    #[error("`that` requires an enclosing `begin` block")]
    UnenclosedThat(Span),
    #[error("A recursive context component cannot contain a parameter")]
    RecursiveParameter(Span),
}

impl ResolveError {
    /// Primary source span of this resolution failure.
    pub fn primary_span(&self) -> Span {
        match self {
            | Self::UnboundVar(variable) => variable.info,
            | Self::DuplicateDefinition(_, duplicate) => duplicate.info,
            | Self::UnenclosedThat(span) | Self::RecursiveParameter(span) => *span,
        }
    }

    /// Create an Ariadne report for this resolve error.
    ///
    /// `spans` is the merged program's span arena; its attached source map
    /// resolves each span into a file and byte range.
    pub fn to_report(&self, spans: &SpanArena) -> Report<'static, (PathDisplay, Range<usize>)> {
        let resolve = |span: &Span| {
            spans
                .source_map()
                .and_then(|map| map.ariadne_range(*span))
                .unwrap_or_else(internal_ariadne_span)
        };
        match self {
            | ResolveError::UnboundVar(var) => {
                let (file_path, range) = resolve(&var.info);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Unbound variable")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message(format!("variable `{}` is not defined", var.inner)),
                    )
                    .finish()
            }
            | ResolveError::DuplicateDefinition(var1, var2) => {
                let (file_path1, range1) = resolve(&var1.info);
                let (file_path2, range2) = resolve(&var2.info);
                let primary_span = (file_path1.clone(), range1.clone());
                let mut report = Report::build(ReportKind::Error, primary_span)
                    .with_message("Duplicate definition")
                    .with_label(
                        Label::new((file_path1.clone(), range1))
                            .with_message(format!("first definition of `{}`", var1.inner)),
                    );
                let message = if file_path1 == file_path2 {
                    format!("second definition of `{}`", var2.inner)
                } else {
                    format!("duplicate definition of `{}`", var2.inner)
                };
                report = report.with_label(Label::new((file_path2, range2)).with_message(message));
                report.finish()
            }
            | ResolveError::UnenclosedThat(span) => {
                let (file_path, range) = resolve(span);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Mobile binding without a block")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message("`that` contributes to the nearest `begin` block"),
                    )
                    .finish()
            }
            | ResolveError::RecursiveParameter(span) => {
                let (file_path, range) = resolve(span);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Recursive parameter component")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message("parameters must remain acyclic"),
                    )
                    .finish()
            }
        }
    }
}

/// Name-resolution result with boxed error for cheap cloning.
pub type Result<T> = std::result::Result<T, Box<ResolveError>>;
