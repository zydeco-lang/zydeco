//! Repository task runner for the Zydeco workspace.
//!
//! The task of interest is `cargo test-resume`: it builds the workspace test
//! targets with Cargo, runs the tests target by target, and records every
//! outcome under the Cargo target directory. A later invocation reuses those
//! records, so fixing a failure and rerunning continues from the failure
//! instead of repeating the whole suite.

mod cargo;
mod cli;
mod runner;
mod state;

use std::process::ExitCode;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

/// Failure that ends an xtask invocation.
#[derive(Debug, Error)]
pub enum Error {
    /// The command line cannot be interpreted.
    #[error("{0}")]
    Usage(String),
    /// An external Cargo invocation failed.
    #[error("cargo failed: {0}")]
    Cargo(String),
    /// Reading or writing resumable-execution state failed.
    #[error("{0}")]
    State(String),
    /// A test executable could not be listed or started.
    #[error("{0}")]
    Test(String),
    /// A file operation failed.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// A Cargo message or state file is not valid JSON.
    #[error(transparent)]
    Json(#[from] serde_json::Error),
}

fn main() -> ExitCode {
    match cli::Invocation::parse(std::env::args_os().skip(1)) {
        | Ok(cli::Parsed::Help) => {
            print!("{}", cli::USAGE);
            ExitCode::SUCCESS
        }
        | Ok(cli::Parsed::Invocation(invocation)) => match runner::run(invocation) {
            | Ok(outcome) => ExitCode::from(outcome.exit_code()),
            | Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        | Err(error) => {
            eprintln!("error: {error}\n");
            eprint!("{}", cli::USAGE);
            ExitCode::from(2)
        }
    }
}
