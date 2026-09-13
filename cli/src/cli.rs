use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;
use zydeco_surface::metadata::SourceReference;

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

/// Backends that can execute a program with the host Builtin package.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, ValueEnum)]
pub enum ExecutionTarget {
    #[default]
    Interpreter,
    Exe,
    WasmAm,
    WasmSps,
}

impl std::fmt::Display for ExecutionTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_possible_value().expect("execution target has a CLI name").get_name())
    }
}

/// A test selection expands to concrete backends before execution.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TestTarget {
    One(ExecutionTarget),
    All,
}

impl TestTarget {
    pub fn parser() -> impl clap::builder::TypedValueParser<Value = Self> {
        use clap::builder::{PossibleValue, PossibleValuesParser, TypedValueParser};
        PossibleValuesParser::new(
            ExecutionTarget::value_variants()
                .iter()
                .filter_map(ValueEnum::to_possible_value)
                .chain([PossibleValue::new("all")]),
        )
        .map(|value| match value.as_str() {
            | "all" => Self::All,
            | _ => Self::One(ExecutionTarget::from_str(&value, false).expect("validated backend")),
        })
    }

    pub fn expand(&self) -> &[ExecutionTarget] {
        match self {
            | Self::One(target) => std::slice::from_ref(target),
            | Self::All => ExecutionTarget::value_variants(),
        }
    }
}

#[derive(Args)]
pub struct ExecutionOptions {
    /// Native runtime sources, used by exe
    #[arg(short = 'r', long, default_value = "runtime")]
    pub runtime_dir: PathBuf,
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
#[command(version, about, long_about = None,
    after_help = "Packages are loaded from package.zy and packages.zy in the working directory.")]
pub struct Cli {
    /// Re-validate the finished typed arena after every successful check,
    /// reporting internal compiler errors (debugging aid)
    #[arg(long, global = true)]
    pub lint_types: bool,

    /// Add a package file alongside automatic discovery; repeat for multiple files
    #[arg(short = 'p', long = "pkg", visible_alias = "package", value_name = "FILE")]
    pub packages: Vec<PathBuf>,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// List the project's packages and relationships without checking or executing code
    Show,
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
        /// Package name, or source path (use ./ for an extensionless file)
        #[arg(value_name = "SOURCE")]
        file: SourceReference,
        /// Execution backend
        #[arg(short, long, default_value = "interpreter")]
        target: ExecutionTarget,
        #[command(flatten)]
        execution: ExecutionOptions,
        /// Dry run (don't execute)
        #[arg(long, default_value_t = false)]
        dry: bool,
        /// Environmental arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a source package and its code dependencies, including its declared executable role
    Check {
        /// Package name, or source path (use ./ for an extensionless file)
        #[arg(value_name = "SOURCE")]
        file: SourceReference,
    },
    /// Run a test package or the selected package's direct test companions with empty stdin
    Test {
        /// Package name, or source path (use ./ for an extensionless file)
        #[arg(value_name = "SOURCE")]
        file: SourceReference,
        /// Execution backend or all; repeat to test multiple backends in order
        #[arg(short, long = "target", default_value = "interpreter", value_parser = TestTarget::parser())]
        targets: Vec<TestTarget>,
        #[command(flatten)]
        execution: ExecutionOptions,
    },
    /// Start the declaration-free terminal REPL
    Repl,
    /// Build a Zydeco program for the selected target
    Build {
        /// Package name, or source path (use ./ for an extensionless file)
        #[arg(value_name = "SOURCE")]
        file: SourceReference,
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
