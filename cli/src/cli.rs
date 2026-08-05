use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, ValueEnum)]
pub enum BuildTarget {
    Zir,
    Zasm,
    Asm,
    Llvm,
    #[default]
    Exe,
    LlvmExe,
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
    // /// Start a REPL
    // Repl {
    //     /// Level of verbosity
    //     #[arg(short, long, default_value_t = false)]
    //     verbose: bool,
    // },
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
        /// Build Directory
        #[arg(short = 'b', long)]
        build_dir: Option<PathBuf>,
        /// Runtime directory
        #[arg(short = 'r', long)]
        runtime_dir: Option<PathBuf>,
        /// Run the program after building
        #[arg(short = 'x', long, default_value_t = false)]
        execute: bool,
        /// Skip CPS translation in the StackIR pipeline
        #[arg(long, default_value_t = false)]
        no_cps: bool,
    },
}
