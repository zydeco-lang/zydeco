//! Documentation book validation, building, serving, and testing via Python.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::{Error, Result};

/// Action forwarded to `docs/scripts/build-books.py`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocsAction {
    /// Validate book layout, theme parity, and internal/cross-book/repo links.
    Check,
    /// Build both reference mdBooks and the combined landing site.
    Build,
    /// Build and serve the combined site locally.
    Serve,
    /// Run unit tests for the documentation scripts.
    Test,
}

impl DocsAction {
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            | "check" => Ok(Self::Check),
            | "build" => Ok(Self::Build),
            | "serve" => Ok(Self::Serve),
            | "test" => Ok(Self::Test),
            | other => Err(Error::Usage(format!(
                "unknown `docs` action `{other}` (expected `check`, `build`, `serve`, or `test`)"
            ))),
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            | Self::Check => "check",
            | Self::Build => "build",
            | Self::Serve => "serve",
            | Self::Test => "test",
        }
    }
}

/// One `cargo xtask docs` invocation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocsInvocation {
    pub action: DocsAction,
    /// Extra arguments forwarded verbatim to `docs/scripts/build-books.py`.
    pub args: Vec<OsString>,
}

impl DocsInvocation {
    /// Resolves the workspace root and executes `docs/scripts/build-books.py`.
    pub fn run(self) -> Result<u8> {
        let workspace_root = workspace_root()?;
        let script = workspace_root.join("docs/scripts/build-books.py");
        if !script.is_file() {
            return Err(Error::Docs(format!("missing script `{}`", script.display())));
        }
        let python = std::env::var_os("PYTHON").unwrap_or_else(|| OsString::from("python3"));
        let status = Command::new(&python)
            .arg(&script)
            .arg(self.action.as_str())
            .args(&self.args)
            .current_dir(&workspace_root)
            .status()
            .map_err(|error| {
                Error::Docs(format!("cannot run `{}`: {error}", python.to_string_lossy()))
            })?;
        Ok(match status.code() {
            | Some(0) => 0,
            | Some(code) => u8::try_from(code).unwrap_or(1),
            | None => 1,
        })
    }
}

fn workspace_root() -> Result<PathBuf> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| Error::Docs("cannot locate workspace root from `CARGO_MANIFEST_DIR`".into()))
}
