//! Repository task runner for the Zydeco workspace.
//!
//! Supported tasks:
//! - `cargo test-resume`: build workspace test targets with Cargo, run them
//!   target by target, and reuse saved passes across invocations.
//! - `cargo xtask docs <check|build|serve|test>`: validate, build, serve, and
//!   test the mdBook language and compiler references via `docs/scripts/build-books.py`.

mod cargo;
mod cli;
mod docs;
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
    /// A documentation script invocation failed to start.
    #[error("docs task failed: {0}")]
    Docs(String),
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
    match cli::Parsed::parse(std::env::args_os().skip(1)) {
        | Ok(cli::Parsed::Help(usage)) => {
            print!("{usage}");
            ExitCode::SUCCESS
        }
        | Ok(cli::Parsed::Task(cli::Task::TestResume(invocation))) => {
            match runner::run(invocation) {
                | Ok(outcome) => ExitCode::from(outcome.exit_code()),
                | Err(error) => {
                    eprintln!("error: {error}");
                    ExitCode::FAILURE
                }
            }
        }
        | Ok(cli::Parsed::Task(cli::Task::Docs(invocation))) => match invocation.run() {
            | Ok(code) => ExitCode::from(code),
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
