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
        files: Vec<PathBuf>,
        /// Name of the binary
        #[arg(long)]
        bin: Option<String>,
        /// Dry run (don't execute)
        #[arg(long, default_value_t = false)]
        dry: bool,
        /// Level of verbosity
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        /// Environmental arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a zydeco program
    Check {
        /// Path to the file to check
        #[arg(value_name = "FILE")]
        files: Vec<PathBuf>,
        /// Level of verbosity
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
    },
    // /// Start a REPL
    // Repl {
    //     /// Level of verbosity
    //     #[arg(short, long, default_value_t = false)]
    //     verbose: bool,
    // },
}
