use clap::Parser;
use std::path::{Path, PathBuf};
use thiserror::Error;
use zydeco_cli::{
    BackendProgram, BuildOptions, BuildTarget, Cli, CommandCompiler, Commands, CompileError,
    DiagnosticRenderer, NativeError, SourceFormatError, SourceFormatOutcome, SourceFormatter,
    TargetArchitecture, TargetOs, WasmBackendKind,
};
use zydeco_dynamics::ProgKont;
use zydeco_tui::{Repl, ReplError};

fn main() {
    let result = Application::default().run(Cli::parse().command);
    match result {
        | Ok(code) => std::process::exit(code),
        | Err(error) => {
            error.render();
            std::process::exit(1);
        }
    }
}

#[derive(Default)]
struct Application {
    compiler: CommandCompiler,
}

impl Application {
    fn run(&self, command: Commands) -> Result<i32, ApplicationError> {
        match command {
            | Commands::Fmt { files, check } => self.format_sources(&files, check),
            | Commands::Run { file, dry, args } => self.run_source(&file, dry, &args),
            | Commands::Check { file } => self.check_source(&file),
            | Commands::Repl => Repl::launch().map_err(ApplicationError::Repl),
            | Commands::Build {
                file,
                target_os,
                target_arch,
                target,
                build_dir,
                runtime_dir,
                execute,
            } => self.build_source(
                &file,
                target,
                BuildOptions::new(
                    build_dir.unwrap_or_else(|| PathBuf::from("build")),
                    runtime_dir.unwrap_or_else(|| PathBuf::from("runtime")),
                    target_arch
                        .map_or_else(TargetArchitecture::host, Ok)
                        .map_err(NativeError::UnsupportedHostArchitecture)?,
                    target_os
                        .map_or_else(TargetOs::host, Ok)
                        .map_err(NativeError::UnsupportedHostOperatingSystem)?,
                ),
                execute,
            ),
        }
    }

    fn format_sources(&self, paths: &[PathBuf], check: bool) -> Result<i32, ApplicationError> {
        let formatter = SourceFormatter;
        let mut changed = false;
        for path in paths {
            let outcome =
                if check { formatter.check_path(path) } else { formatter.format_path(path) }?;
            if outcome == SourceFormatOutcome::Changed {
                changed = true;
                if check {
                    println!("{}", path.display());
                }
            }
        }
        Ok(i32::from(check && changed))
    }

    fn analyze(
        &self, path: &Path,
    ) -> Result<std::sync::Arc<zydeco_session::ProgramAnalysis>, ApplicationError> {
        let analysis = self.compiler.analyze(path)?;
        DiagnosticRenderer::warnings(&analysis);
        let program = self
            .compiler
            .checked_program(&analysis)
            .expect("a checked CLI analysis has an owned program");
        DiagnosticRenderer::observations(&analysis, &program.statics);
        Ok(analysis)
    }

    fn check_source(&self, path: &Path) -> Result<i32, ApplicationError> {
        self.analyze(path)?;
        Ok(0)
    }

    fn run_source(
        &self, path: &Path, dry: bool, arguments: &[String],
    ) -> Result<i32, ApplicationError> {
        let analysis = self.analyze(path)?;
        let executable = self.compiler.executable_program(&analysis)?;
        match CommandCompiler::interpret_program(executable, arguments, dry)? {
            | ProgKont::Dry => Ok(0),
            | ProgKont::ExitCode(code) => Ok(code),
            | ProgKont::Error(_) => unreachable!("runtime errors are promoted to CompileError"),
            | ProgKont::Ret(_) => unreachable!("an executable source root must return `OS`"),
        }
    }

    fn build_source(
        &self, path: &Path, target: BuildTarget, options: BuildOptions, execute: bool,
    ) -> Result<i32, ApplicationError> {
        let analysis = self.analyze(path)?;
        let executable = self.compiler.executable_program(&analysis)?;
        let backend = BackendProgram::lower(executable)?;
        match target {
            | BuildTarget::Zir => println!("{}", backend.render_sps_low()),
            | BuildTarget::Zasm if execute => println!("{}", backend.execute_assembly()?),
            | BuildTarget::Zasm => println!("{}", backend.render_assembly()),
            | BuildTarget::Asm => {
                if options.architecture != TargetArchitecture::X86_64 {
                    return Err(
                        NativeError::UnsupportedAmd64Architecture(options.architecture).into()
                    );
                }
                println!("{}", backend.emit_amd64(options.operating_system));
            }
            | BuildTarget::Llvm => {
                println!("{}", backend.emit_llvm(options.architecture, options.operating_system)?)
            }
            | BuildTarget::WasmAm => {
                if execute {
                    return Err(NativeError::WasmExecutionRequiresHost.into());
                }
                let artifact = Self::artifact_name(path)?;
                let module = backend.emit_wasm_am()?;
                let module =
                    options.write_wasm(&artifact, WasmBackendKind::AbstractMachine, &module)?;
                println!("{}", module.path().display());
            }
            | BuildTarget::WasmSps => {
                if execute {
                    return Err(NativeError::WasmExecutionRequiresHost.into());
                }
                let artifact = Self::artifact_name(path)?;
                let module = backend.emit_wasm_sps()?;
                let module = options.write_wasm(&artifact, WasmBackendKind::SpsLow, &module)?;
                println!("{}", module.path().display());
            }
            | BuildTarget::Exe => {
                let artifact = Self::artifact_name(path)?;
                let assembly = backend.emit_amd64(options.operating_system);
                let foreign_libraries = backend.foreign_libraries();
                let executable = options.link_amd64(&artifact, &assembly, &foreign_libraries)?;
                if execute {
                    return Ok(executable.run(&[])?.code().unwrap_or(0));
                }
            }
            | BuildTarget::LlvmExe => {
                let artifact = Self::artifact_name(path)?;
                let ir = backend.emit_llvm(options.architecture, options.operating_system)?;
                let executable = options.link_llvm(&artifact, &ir)?;
                if execute {
                    return Ok(executable.run(&[])?.code().unwrap_or(0));
                }
            }
        }
        Ok(0)
    }

    fn artifact_name(path: &Path) -> Result<String, ApplicationError> {
        path.file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_owned)
            .ok_or_else(|| ApplicationError::InvalidArtifactName(path.to_path_buf()))
    }
}

#[derive(Debug, Error)]
enum ApplicationError {
    #[error(transparent)]
    Format(#[from] SourceFormatError),
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Native(#[from] NativeError),
    #[error(transparent)]
    Repl(#[from] ReplError),
    #[error("source root path `{}` does not have a valid UTF-8 artifact name", .0.display())]
    InvalidArtifactName(PathBuf),
}

impl ApplicationError {
    fn render(&self) {
        match self {
            | Self::Compile(error) => DiagnosticRenderer::error(error),
            | _ => eprintln!("{self}"),
        }
    }
}
