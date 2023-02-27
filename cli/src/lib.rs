pub mod repl;

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run a zydeco program
    Run {
        /// Path to the file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,
        #[arg(long, default_value_t = false)]
        dry: bool,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a zydeco program
    Check {
        /// Path to the file to check
        #[arg(value_name = "FILE")]
        file: PathBuf,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
    },
    /// Start a REPL
    Repl {
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
    },
    /// Run tests
    Test {},
}
