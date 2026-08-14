use std::{
    fs, io,
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_surface::textual::{
    Lexer, ParseError, SourceUnitParser, fmt::PrettyFormatter, syntax::Parser,
};
use zydeco_utils::span::{FileInfo, LocationCtx};

/// Whether formatting changed the source file on disk.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SourceFormatOutcome {
    Changed,
    Unchanged,
}

/// A filesystem or syntax failure while formatting one source file.
#[derive(Debug, Error)]
pub enum SourceFormatError {
    #[error("cannot read source `{}`: {source}", path.display())]
    Read {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("cannot format source `{}`: {message}", path.display())]
    Parse { path: PathBuf, message: String },
    #[error("cannot write formatted source `{}`: {source}", path.display())]
    Write {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
}

/// Filesystem adapter for the surface pretty printer.
pub struct SourceFormatter;

impl SourceFormatter {
    pub fn format_path(&self, path: &Path) -> Result<SourceFormatOutcome, SourceFormatError> {
        let (source, formatted) = self.render_source(path)?;
        if formatted == source {
            return Ok(SourceFormatOutcome::Unchanged);
        }
        fs::write(path, formatted)
            .map_err(|source| SourceFormatError::Write { path: path.to_path_buf(), source })?;
        Ok(SourceFormatOutcome::Changed)
    }

    /// Format the file in memory and report whether writing it would change it.
    pub fn check_path(&self, path: &Path) -> Result<SourceFormatOutcome, SourceFormatError> {
        let (source, formatted) = self.render_source(path)?;
        Ok(if formatted == source {
            SourceFormatOutcome::Unchanged
        } else {
            SourceFormatOutcome::Changed
        })
    }

    fn render_source(&self, path: &Path) -> Result<(String, String), SourceFormatError> {
        let source = fs::read_to_string(path)
            .map_err(|source| SourceFormatError::Read { path: path.to_path_buf(), source })?;
        let formatted = self.render(path, &source)?;
        Ok((source, formatted))
    }

    fn render(&self, path: &Path, source: &str) -> Result<String, SourceFormatError> {
        let file_info = FileInfo::new(source, Some(Arc::new(path.to_path_buf())));
        let location = LocationCtx::File(file_info.clone());
        let mut parser = Parser::new();
        let unit = SourceUnitParser::new()
            .parse(source, &location, &mut parser, Lexer::new(source))
            .map_err(|error| SourceFormatError::Parse {
                path: path.to_path_buf(),
                message: ParseError { error, file_info: &file_info }.to_string(),
            })?;
        Ok(PrettyFormatter::new(&parser.arena).render_unit(unit))
    }
}
