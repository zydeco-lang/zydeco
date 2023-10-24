use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum SurfaceError {
    #[error("Path not found: `{}`", .path.display())]
    PathNotFound { path: PathBuf },
    #[error("Path invalid: `{}`", .path.display())]
    PathInvalid { path: PathBuf },
    #[error("Invalid project setting; valid options are `managed`, `root` or `root_no_std`")]
    ProjectInvalid,
    #[error("Project name mismatch: `{}` != `{}`", .name, .config_name)]
    ProjectNameMismatch { name: String, config_name: String },
    #[error("Ambiguous module: `{}` in path: {}", .name, .path.display() )]
    AmbiguousModule { path: PathBuf, name: String },
    #[error("Module not found: `{}` in path: {}", .name, .path.display() )]
    ModuleNotFound { name: String, path: PathBuf },
    // Todo: use codespan-reporting
    #[error("Parse error:\n{0}")]
    ParseError(String),
    #[error("Resolve errors:\n{0}")]
    ResolveErrors(String),
}
