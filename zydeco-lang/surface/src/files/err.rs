use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum SurfaceError {
    #[error("Path not found: {}. Searched: {searched:?}", .path.display(), searched = .searched)]
    PathNotFound { searched: Vec<PathBuf>, path: PathBuf },
    #[error("Path is not a file or is not under root: {path:?}")]
    PathNotFileOrUnderRoot { path: PathBuf },
    // Todo: use codespan-reporting
    #[error("Parse error: {0}")]
    ParseError(String),
}
