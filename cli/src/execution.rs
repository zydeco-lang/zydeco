use std::{
    collections::HashSet,
    io::{Seek, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::{Mutex, OnceLock},
};
use tempfile::TempDir;
use thiserror::Error;
use zydeco_dynamics::ProgKont;
use zydeco_session::ExecutableProgram;

use crate::{
    BuildOptions, CommandCompiler, CompileError, Executable, ExecutionTarget, NativeError,
    TargetArchitecture, TargetOs, TestInteraction,
};

const NODE_WASI_SCRIPT: &str = "\
const fs = require('node:fs');\n\
const { WASI } = require('node:wasi');\n\
const preopens = JSON.parse(process.env.ZYDECO_WASI_PREOPENS || '{}');\n\
const wasi = new WASI({ version: 'preview1', args: process.argv.slice(1), env: process.env, preopens });\n\
const bytes = fs.readFileSync(process.argv[1]);\n\
WebAssembly.compile(bytes)\n\
  .then((module) => WebAssembly.instantiate(module, wasi.getImportObject()))\n\
  .then((instance) => wasi.start(instance))\n\
  .catch((error) => { process.stderr.write(`${error?.stack ?? error}\\n`); process.exit(1); });";

/// WebAssembly WASI runtime selected for executing standalone `.sps.wasm` binaries.
#[derive(Clone, Debug)]
enum WasmRunner {
    Wasmtime(PathBuf),
    NodeWasi(PathBuf),
}

impl WasmRunner {
    fn discover() -> Result<Self, ExecutionError> {
        if let Some(wasmtime) = std::env::var_os("WASMTIME") {
            let path = PathBuf::from(wasmtime);
            verify_runtime(&path)?;
            return Ok(Self::Wasmtime(path));
        }
        let default_wasmtime = PathBuf::from("wasmtime");
        if verify_runtime(&default_wasmtime).is_ok() {
            return Ok(Self::Wasmtime(default_wasmtime));
        }
        let node = PathBuf::from(std::env::var_os("NODE").unwrap_or_else(|| "node".into()));
        verify_runtime(&node)?;
        Ok(Self::NodeWasi(node))
    }

    fn command(&self, module_path: &Path) -> Command {
        let preopens = wasi_preopens();
        match self {
            | Self::Wasmtime(wasmtime) => {
                let mut command = Command::new(wasmtime);
                command.args(["run", "-W", "tail-call=y"]);
                for dir in &preopens {
                    command.arg("--dir").arg(format!("{dir}::{dir}"));
                }
                command.arg("--").arg(module_path);
                command
            }
            | Self::NodeWasi(node) => {
                let preopen_map = preopens
                    .iter()
                    .map(|dir| (dir.as_str(), dir.as_str()))
                    .collect::<std::collections::BTreeMap<_, _>>();
                let json = serde_json::to_string(&preopen_map)
                    .expect("serialize WASI preopen directory map");
                let mut command = Command::new(node);
                command
                    .args(["--no-warnings", "-e", NODE_WASI_SCRIPT, "--"])
                    .arg(module_path)
                    .env("ZYDECO_WASI_PREOPENS", json);
                command
            }
        }
    }
}

/// Returns a [`Command`] configured to execute a linked standalone `.sps.wasm` binary.
pub fn wasm_command(module_path: &Path) -> Result<Command, ExecutionError> {
    Ok(WasmRunner::discover()?.command(module_path))
}

fn wasi_preopens() -> Vec<String> {
    let mut dirs = Vec::new();
    let mut push_dir = |candidate: String| {
        if !candidate.is_empty() && !dirs.contains(&candidate) {
            dirs.push(candidate);
        }
    };
    push_dir(".".to_owned());
    let mut push_path = |path: &Path| {
        if let Some(text) = normalize_dir_string(path) {
            push_dir(text);
        }
        if let Ok(canonical) = std::fs::canonicalize(path)
            && let Some(text) = normalize_dir_string(&canonical)
        {
            push_dir(text);
        }
    };
    if let Ok(cwd) = std::env::current_dir() {
        push_path(&cwd);
    }
    let temp = std::env::temp_dir();
    push_path(&temp);
    let tmp = Path::new("/tmp");
    if tmp.is_dir() {
        push_path(tmp);
    }
    dirs
}

fn normalize_dir_string(path: &Path) -> Option<String> {
    let text = path.to_str()?;
    let trimmed = if text == "/" { text } else { text.trim_end_matches('/') };
    (!trimmed.is_empty()).then(|| trimmed.to_owned())
}

/// Execution policy shared by source commands and the Rust fixture harness.
pub struct ExecutionRunner<'a> {
    compiler: &'a CommandCompiler,
    targets: Vec<ExecutionTarget>,
    runtime_dir: PathBuf,
    wasm: Option<WasmRunner>,
    libraries: crate::library::LinkedLibraries,
}

impl<'a> ExecutionRunner<'a> {
    pub fn with_libraries(mut self, libraries: crate::library::LinkedLibraries) -> Self {
        self.libraries = libraries;
        self
    }

    pub fn validate(&self, executable: &ExecutableProgram) -> Result<(), ExecutionError> {
        let imports = executable
            .core
            .values
            .iter()
            .filter_map(|(_, value)| match value {
                | zydeco_ir::Value::Foreign(zydeco_syntax::CheckedImport::C(import)) => {
                    Some(import.clone())
                }
                | _ => None,
            })
            .collect::<Vec<_>>();
        let units = executable
            .core
            .values
            .iter()
            .filter_map(|(_, value)| match value {
                | zydeco_ir::Value::Foreign(zydeco_syntax::CheckedImport::Zydeco(import)) => {
                    Some(import.clone())
                }
                | _ => None,
            })
            .collect::<Vec<_>>();
        if self.libraries.is_empty() && units.is_empty() {
            return Ok(());
        }
        for target in &self.targets {
            if (!units.is_empty() || !self.libraries.units.is_empty())
                && *target != ExecutionTarget::Exe
            {
                return Err(crate::library::LibraryError::UnitTarget.into());
            }
            if *target == ExecutionTarget::WasmSps {
                return Err(crate::library::LibraryError::Target.into());
            }
            let interpreter = *target == ExecutionTarget::Interpreter;
            let architecture = if interpreter {
                TargetArchitecture::host().map_err(NativeError::UnsupportedHostArchitecture)?
            } else {
                TargetArchitecture::X86_64
            };
            let platform = crate::library::LibraryPlatform::for_target(
                architecture,
                TargetOs::host().map_err(NativeError::UnsupportedHostOperatingSystem)?,
            )?;
            let runtime = if interpreter {
                None
            } else {
                Some(crate::library::LibraryDigest::runtime(&self.runtime_dir)?)
            };
            self.libraries.validate_imports(&imports, platform, runtime.as_deref(), interpreter)?;
            self.libraries.validate_units(&units, platform, runtime.as_deref(), !interpreter)?;
        }
        Ok(())
    }

    pub fn new(
        compiler: &'a CommandCompiler, targets: impl IntoIterator<Item = ExecutionTarget>,
        runtime_dir: PathBuf,
    ) -> Result<Self, ExecutionError> {
        let mut unique = Vec::new();
        for target in targets {
            if !unique.contains(&target) {
                unique.push(target);
            }
        }
        let wasm = if unique.contains(&ExecutionTarget::WasmSps) {
            Some(WasmRunner::discover()?)
        } else {
            None
        };
        Ok(Self { compiler, targets: unique, runtime_dir, wasm, libraries: Default::default() })
    }

    /// Reuse the checked source and one lowering across all requested backends.
    /// Preparing a program never executes its code.
    pub fn prepare(
        &self, executable: ExecutableProgram,
    ) -> Result<Vec<PreparedExecution>, ExecutionError> {
        self.validate(&executable)?;
        let backend = self
            .targets
            .iter()
            .any(|target| *target != ExecutionTarget::Interpreter)
            .then(|| self.compiler.lower_executable(executable.clone()))
            .transpose()?;
        self.targets
            .iter()
            .map(|&target| {
                if target == ExecutionTarget::Interpreter {
                    return Ok(PreparedExecution {
                        target,
                        program: ExecutionProgram::Interpreter {
                            executable: executable.clone(),
                            libraries: self.libraries.paths(),
                        },
                    });
                }
                let backend = backend.as_ref().expect("compiled target requested lowering");
                let directory = tempfile::tempdir()?;
                let command = match target {
                    | ExecutionTarget::Interpreter => unreachable!(),
                    | ExecutionTarget::Exe => {
                        let operating_system = TargetOs::host()
                            .map_err(NativeError::UnsupportedHostOperatingSystem)?;
                        let options = BuildOptions::new(
                            directory.path().to_owned(),
                            self.runtime_dir.clone(),
                            TargetArchitecture::X86_64,
                            operating_system,
                        );
                        let native = backend.emit_amd64(operating_system);
                        let executable = options.link_amd64_resolved(
                            "program",
                            &native.assembly,
                            &native.foreign_libraries,
                            &self.libraries,
                        )?;
                        Command::new(executable.path())
                    }
                    | ExecutionTarget::WasmSps => {
                        let options = BuildOptions::host(
                            directory.path().to_owned(),
                            self.runtime_dir.clone(),
                        )?;
                        let object = backend.emit_wasm_sps()?;
                        let artifact = options.link_wasm("program", &object)?;
                        self.wasm
                            .as_ref()
                            .expect("WASM runner initialized for wasm-sps")
                            .command(artifact.path())
                    }
                };
                Ok(PreparedExecution {
                    target,
                    program: ExecutionProgram::Process { command, _directory: directory },
                })
            })
            .collect()
    }
}

pub struct PreparedExecution {
    pub target: ExecutionTarget,
    program: ExecutionProgram,
}

enum ExecutionProgram {
    Interpreter {
        executable: ExecutableProgram,
        libraries: std::collections::BTreeMap<zydeco_syntax::ForeignLibraryName, PathBuf>,
    },
    Process {
        command: Command,
        _directory: TempDir,
    },
}

impl PreparedExecution {
    /// Inherit the caller's terminal streams.
    pub fn run(self, arguments: &[String]) -> Result<i32, ExecutionError> {
        match self.program {
            | ExecutionProgram::Interpreter { executable, libraries } => {
                match CommandCompiler::interpret_with_libraries(executable, arguments, libraries)? {
                    | ProgKont::ExitCode(code) => Ok(code),
                    | _ => unreachable!("an executable program exits or reports a runtime error"),
                }
            }
            | ExecutionProgram::Process { mut command, _directory } => {
                let status = command.args(arguments).status().map_err(|source| {
                    ExecutionError::Start { program: command.get_program().into(), source }
                })?;
                Ok(Executable::exit_code(status))
            }
        }
    }

    /// Capture output with explicit input; tests never inherit the terminal.
    pub fn test(
        self, arguments: &[String], input: &str,
    ) -> Result<TestInteraction, ExecutionError> {
        match self.program {
            | ExecutionProgram::Interpreter { executable, libraries } => Ok(
                CommandCompiler::test_io_with_libraries(executable, arguments, input, libraries)?,
            ),
            | ExecutionProgram::Process { mut command, _directory } => {
                let stdin = if input.is_empty() {
                    Stdio::null()
                } else {
                    // A file avoids pipe backpressure between a child reading input and writing output.
                    let mut file = tempfile::tempfile()?;
                    file.write_all(input.as_bytes())?;
                    file.rewind()?;
                    Stdio::from(file)
                };
                let output = command.args(arguments).stdin(stdin).output().map_err(|source| {
                    ExecutionError::Start { program: command.get_program().into(), source }
                })?;
                Ok(TestInteraction {
                    output: String::from_utf8_lossy(&output.stdout).into_owned(),
                    stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                    code: Executable::exit_code(output.status),
                })
            }
        }
    }
}

fn verify_runtime(program: &Path) -> Result<(), ExecutionError> {
    static VERIFIED: OnceLock<Mutex<HashSet<PathBuf>>> = OnceLock::new();
    let cache = VERIFIED.get_or_init(|| Mutex::new(HashSet::new()));
    if cache.lock().unwrap_or_else(|poison| poison.into_inner()).contains(program) {
        return Ok(());
    }
    let output = Command::new(program)
        .arg("--version")
        .output()
        .map_err(|source| ExecutionError::Start { program: program.to_path_buf(), source })?;
    if !output.status.success() {
        return Err(ExecutionError::Host {
            program: program.to_path_buf(),
            status: output.status,
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        });
    }
    cache.lock().unwrap_or_else(|poison| poison.into_inner()).insert(program.to_path_buf());
    Ok(())
}

#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error(transparent)]
    Library(#[from] crate::library::LibraryError),
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Native(#[from] NativeError),
    #[error("execution I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("cannot start `{}`: {source}", program.display())]
    Start { program: PathBuf, source: std::io::Error },
    #[error("WebAssembly host `{}` failed ({status}): {stderr}", program.display())]
    Host { program: PathBuf, status: std::process::ExitStatus, stderr: String },
}
