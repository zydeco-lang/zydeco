use std::{
    io::{Seek, Write},
    path::PathBuf,
    process::{Command, Stdio},
};
use tempfile::TempDir;
use thiserror::Error;
use zydeco_dynamics::ProgKont;
use zydeco_session::ExecutableProgram;

use crate::{
    BuildOptions, CommandCompiler, CompileError, Executable, ExecutionTarget, NativeError,
    TargetArchitecture, TargetOs, TestInteraction,
};

/// Execution policy shared by source commands and the Rust fixture harness.
pub struct ExecutionRunner<'a> {
    compiler: &'a CommandCompiler,
    targets: Vec<ExecutionTarget>,
    runtime_dir: PathBuf,
    node: PathBuf,
}

impl<'a> ExecutionRunner<'a> {
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
        let node = PathBuf::from(std::env::var_os("NODE").unwrap_or_else(|| "node".into()));
        if unique
            .iter()
            .any(|target| matches!(target, ExecutionTarget::WasmAm | ExecutionTarget::WasmSps))
        {
            let output = Command::new(&node)
                .arg("--version")
                .output()
                .map_err(|source| ExecutionError::Start { program: node.clone(), source })?;
            if !output.status.success() {
                return Err(ExecutionError::Host {
                    program: node,
                    status: output.status,
                    stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                });
            }
        }
        Ok(Self { compiler, targets: unique, runtime_dir, node })
    }

    /// Reuse the checked source and one lowering across all requested backends.
    /// Preparing a program never executes its code.
    pub fn prepare(
        &self, executable: ExecutableProgram,
    ) -> Result<Vec<PreparedExecution>, ExecutionError> {
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
                        program: ExecutionProgram::Interpreter(executable.clone()),
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
                        let executable = options.link_amd64(
                            "program",
                            &native.assembly,
                            &native.foreign_libraries,
                        )?;
                        Command::new(executable.path())
                    }
                    | ExecutionTarget::WasmAm | ExecutionTarget::WasmSps => {
                        let module = if target == ExecutionTarget::WasmAm {
                            backend.emit_wasm_am()?
                        } else {
                            backend.emit_wasm_sps()?
                        };
                        let path = directory.path().join("program.wasm");
                        std::fs::write(&path, module)?;
                        for (name, source) in [
                            ("wasm-host.mjs", include_str!("../wasm/wasm-host.mjs")),
                            ("wasm-numeric.mjs", include_str!("../wasm/wasm-numeric.mjs")),
                            ("wasm-memory.mjs", include_str!("../wasm/wasm-memory.mjs")),
                        ] {
                            std::fs::write(directory.path().join(name), source)?;
                        }
                        let mut command = Command::new(&self.node);
                        command.arg(directory.path().join("wasm-host.mjs")).arg(path);
                        command
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
    Interpreter(ExecutableProgram),
    Process { command: Command, _directory: TempDir },
}

impl PreparedExecution {
    /// Inherit the caller's terminal streams.
    pub fn run(self, arguments: &[String]) -> Result<i32, ExecutionError> {
        match self.program {
            | ExecutionProgram::Interpreter(executable) => {
                match CommandCompiler::interpret_program(executable, arguments, false)? {
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
            | ExecutionProgram::Interpreter(executable) => {
                Ok(CommandCompiler::test_io_program(executable, arguments, input)?)
            }
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

#[derive(Debug, Error)]
pub enum ExecutionError {
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
