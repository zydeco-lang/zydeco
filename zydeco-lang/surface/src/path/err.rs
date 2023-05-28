use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum SurfaceError {
    #[error("Path not found: `{}`. Searched: {searched:?}", .path.display(), searched = .searched)]
    PathNotFound { searched: Vec<PathBuf>, path: PathBuf },
    // Todo: use codespan-reporting
    #[error("Parse error:\n{0}")]
    ParseError(String),
    #[error("Invalid project setting; valid options are `managed`, `root` or `root_no_std`")]
    InvalidProject,
    #[error("Resolve errors:\n{0}")]
    ResolveErrors(String),
}
