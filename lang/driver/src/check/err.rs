use ariadne::Report;
use std::{collections::HashMap, ops::Range, path::PathBuf};
use thiserror::Error;
use zydeco_utils::span::PathDisplay;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Resolve error:\n\t{0}")]
    ResolveError(String),
    #[error("Tyck error:\n{0}")]
    TyckErrors(String),
    /// Ariadne reports for better error display (replaces TyckErrors)
    #[error("Type checking errors")]
    TyckErrorReports {
        reports: Vec<Report<'static, (PathDisplay, Range<usize>)>>,
        sources: HashMap<PathBuf, String>,
    },
    /// Ariadne report for resolve error (replaces ResolveError)
    #[error("Resolve error")]
    ResolveErrorReport {
        report: Report<'static, (PathDisplay, Range<usize>)>,
        sources: HashMap<PathBuf, String>,
    },
}

pub type Result<T> = std::result::Result<T, CompileError>;
