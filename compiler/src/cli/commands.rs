use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run a zydeco program
    Run {
        /// Path to the file to run
        #[arg(short, long, value_name = "FILE")]
        file: PathBuf,
        #[arg(long, default_value_t = false)]
        dry: bool,
    },
    /// Check a zydeco program
    Check {
        /// Path to the file to check
        #[arg(short, long, value_name = "FILE")]
        file: PathBuf,
    },
    /// Start a REPL
    Repl {},
    /// Run tests
    Test {},
}
