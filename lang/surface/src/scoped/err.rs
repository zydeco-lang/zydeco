use crate::bitter::syntax::*;
use ariadne::{Label, Report, ReportKind};
use std::ops::Range;
use thiserror::Error;

/// Errors reported during name resolution.
#[derive(Error, Debug, Clone)]
pub enum ResolveError {
    #[error("Unbound variable: {0}")]
    UnboundVar(Sp<NameRef<VarName>>),
    #[error("Duplicate definition: {0} and {1}")]
    DuplicateDefinition(Sp<VarName>, Sp<VarName>),
    #[error("Undefined primitive: {0}")]
    UndefinedPrimitive(Sp<VarName>),
    #[error("Duplicate primitive: {0} and {1}")]
    DuplicatePrimitive(Sp<VarName>, Sp<VarName>),
    #[error("Missing primitive: {0}")]
    MissingPrim(&'static str),
    #[error("No such module found: {0}")]
    ModuleNotFound(Sp<NameRef<VarName>>),
}

impl ResolveError {
    /// Create an Ariadne report for this resolve error.
    pub fn to_report(&self) -> Report<'static, (String, Range<usize>)> {
        match self {
            | ResolveError::UnboundVar(var) => {
                let (file_path, range) = var.info.to_ariadne_span();
                Report::build(ReportKind::Error, file_path.clone(), range.start)
                    .with_message("Unbound variable")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message(format!("variable `{}` is not defined", var.inner)),
                    )
                    .finish()
            }
            | ResolveError::DuplicateDefinition(var1, var2) => {
                let (file_path1, range1) = var1.info.to_ariadne_span();
                let (file_path2, range2) = var2.info.to_ariadne_span();
                let (primary_file, primary_offset) = if file_path1 == file_path2 {
                    (file_path1.clone(), range1.start)
                } else {
                    (file_path1.clone(), range1.start)
                };
                let mut report = Report::build(ReportKind::Error, primary_file, primary_offset)
                    .with_message("Duplicate definition")
                    .with_label(
                        Label::new((file_path1.clone(), range1))
                            .with_message(format!("first definition of `{}`", var1.inner)),
                    );
                if file_path1 == file_path2 {
                    report = report.with_label(
                        Label::new((file_path2, range2))
                            .with_message(format!("second definition of `{}`", var2.inner)),
                    );
                } else {
                    report = report.with_label(
                        Label::new((file_path2, range2))
                            .with_message(format!("duplicate definition of `{}`", var2.inner)),
                    );
                }
                report.finish()
            }
            | ResolveError::UndefinedPrimitive(var) => {
                let (file_path, range) = var.info.to_ariadne_span();
                Report::build(ReportKind::Error, file_path.clone(), range.start)
                    .with_message("Undefined primitive")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message(format!("primitive `{}` is not defined", var.inner)),
                    )
                    .finish()
            }
            | ResolveError::DuplicatePrimitive(var1, var2) => {
                let (file_path1, range1) = var1.info.to_ariadne_span();
                let (file_path2, range2) = var2.info.to_ariadne_span();
                let mut report =
                    Report::build(ReportKind::Error, file_path1.clone(), range1.start)
                        .with_message("Duplicate primitive")
                        .with_label(Label::new((file_path1.clone(), range1)).with_message(
                            format!("first definition of primitive `{}`", var1.inner),
                        ));
                if file_path1 == file_path2 {
                    report =
                        report.with_label(Label::new((file_path2, range2)).with_message(format!(
                            "second definition of primitive `{}`",
                            var2.inner
                        )));
                } else {
                    report = report.with_label(Label::new((file_path2, range2)).with_message(
                        format!("duplicate definition of primitive `{}`", var2.inner),
                    ));
                }
                report.finish()
            }
            | ResolveError::MissingPrim(name) => {
                Report::build(ReportKind::Error, "<internal>".to_string(), 0)
                    .with_message(format!("Missing primitive: {}", name))
                    .with_note(format!("The primitive `{}` must be defined but is missing", name))
                    .finish()
            }
            | ResolveError::ModuleNotFound(module_ref) => {
                let (file_path, range) = module_ref.info.to_ariadne_span();
                Report::build(ReportKind::Error, file_path.clone(), range.start)
                    .with_message("Module not found")
                    .with_label(
                        Label::new((file_path, range)).with_message(format!(
                            "module `{}` could not be found",
                            module_ref.inner
                        )),
                    )
                    .finish()
            }
        }
    }
}

/// Name-resolution result with boxed error for cheap cloning.
pub type Result<T> = std::result::Result<T, Box<ResolveError>>;
