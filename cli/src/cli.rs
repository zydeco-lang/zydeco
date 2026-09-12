use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, ValueEnum)]
pub enum BuildTarget {
    Zir,
    Zasm,
    Asm,
    WasmAm,
    WasmSps,
    #[default]
    Exe,
}

/// Command-line spelling, translated to the compiler's representation policy at entry.
#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum RepresentationChoice {
    Boxed,
    Direct,
    Local,
    Shared,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum TargetArchitecture {
    #[value(alias = "x86", alias = "amd64")]
    X86_64,
    #[value(alias = "arm64")]
    Aarch64,
}

impl TargetArchitecture {
    pub fn host() -> Result<Self, &'static str> {
        match std::env::consts::ARCH {
            | "x86" | "x86_64" => Ok(Self::X86_64),
            | "aarch64" => Ok(Self::Aarch64),
            | architecture => Err(architecture),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum TargetOs {
    Linux,
    #[value(alias = "darwin")]
    Macos,
}

impl TargetOs {
    pub fn host() -> Result<Self, &'static str> {
        match std::env::consts::OS {
            | "linux" => Ok(Self::Linux),
            | "macos" => Ok(Self::Macos),
            | operating_system => Err(operating_system),
        }
    }
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Re-validate the finished typed arena after every successful check,
    /// reporting internal compiler errors (debugging aid)
    #[arg(long, global = true)]
    pub lint_types: bool,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// List optional compiler passes or explain a selected high-SPS plan
    Passes {
        /// Explain `default`, `none`, or a comma-separated list such as normalize,normalize
        #[arg(long, value_name = "PLAN")]
        sps_passes: Option<String>,
    },
    #[command(name = "__doc-example-worker", hide = true)]
    DocumentationExampleWorker,
    /// Read, search, generate, or verify project documentation
    Doc {
        #[command(subcommand)]
        command: DocumentationCommand,
    },
    /// Format Zydeco source files in place
    Fmt {
        /// Paths to the files to format
        #[arg(value_name = "FILE", required = true)]
        files: Vec<PathBuf>,
        /// Report files that would change without writing them, and exit
        /// unsuccessfully when at least one file would change
        #[arg(long)]
        check: bool,
    },
    /// Run a zydeco program
    Run {
        /// Path to the file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Dry run (don't execute)
        #[arg(long, default_value_t = false)]
        dry: bool,
        /// Environmental arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a zydeco program
    Check {
        /// Path to the file to check
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Start the declaration-free terminal REPL
    Repl,
    Build {
        /// Path to the file to compile
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Target OS (defaults to host OS)
        #[arg(long)]
        target_os: Option<TargetOs>,
        /// Target architecture (defaults to host architecture)
        #[arg(long)]
        target_arch: Option<TargetArchitecture>,
        /// Target backend
        #[arg(short, long, default_value = "exe")]
        target: BuildTarget,
        /// Local representation policy for zasm, asm, exe, or wasm-am
        #[arg(long)]
        representation: Option<RepresentationChoice>,
        #[command(flatten)]
        pipeline: PipelineOptions,
        /// Build Directory
        #[arg(short = 'b', long)]
        build_dir: Option<PathBuf>,
        /// Runtime directory
        #[arg(short = 'r', long)]
        runtime_dir: Option<PathBuf>,
        /// Run the program after building
        #[arg(short = 'x', long, default_value_t = false)]
        execute: bool,
    },
}

/// External text is parsed into phase-owned plan types before source loading.
#[derive(Args, Debug, Default)]
pub struct PipelineOptions {
    /// High-SPS passes: default, none, or a comma-separated list; order and duplicates are preserved
    #[arg(long, value_name = "PLAN")]
    pub sps_passes: Option<String>,
    /// Trace each selected high-SPS pass and its execution time on stderr
    #[arg(long)]
    pub trace_passes: bool,
    /// Verify high-SPS invariants before and after each selected pass
    #[arg(long)]
    pub verify_passes: bool,
    /// Print high-SPS IR before and after each selected pass on stderr
    #[arg(long)]
    pub dump_passes: bool,
}

#[derive(Subcommand)]
pub enum DocumentationCommand {
    /// Show a public subject's classifier and complete documentation
    Show {
        #[arg(value_name = "ROOT")]
        file: PathBuf,
        /// Field path; use `()` for a function/computation result and `.` for the entry
        #[arg(default_value = ".")]
        subject: String,
    },
    /// Search exposed names and documentation prose
    Search {
        #[arg(value_name = "ROOT")]
        file: PathBuf,
        query: String,
    },
    /// Generate a self-contained searchable HTML reference without executing examples
    Build {
        #[arg(value_name = "ROOT")]
        file: PathBuf,
        #[arg(short, long, value_name = "HTML")]
        output: PathBuf,
        #[arg(long)]
        title: Option<String>,
        /// Explicit guide pages; public links use `zydeco:member:./field`
        #[arg(long, value_name = "MARKDOWN")]
        guide: Vec<PathBuf>,
    },
    /// Check links and explicitly verified examples in the entry and its dependencies
    Check {
        #[arg(value_name = "ROOT")]
        file: PathBuf,
        #[arg(long, value_name = "MARKDOWN")]
        guide: Vec<PathBuf>,
    },
}
