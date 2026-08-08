use clap::Parser;
// use zydeco_cli::Repl;
use std::path::{Path, PathBuf};
use thiserror::Error;
use zydeco_cli::{
    BackendProgram, BuildOptions, BuildTarget, Cli, CommandCompiler, Commands, CompileError,
    DiagnosticRenderer, NativeError, SourceFormatError, SourceFormatter, TargetArchitecture,
    TargetOs,
};
use zydeco_dynamics::ProgKont;
use zydeco_stackir::CpsMode;

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
            | Commands::Fmt { files } => self.format_sources(&files),
            | Commands::Run { file, dry, args } => self.run_source(&file, dry, &args),
            | Commands::Check { file } => self.check_source(&file),
            // | Commands::Repl { .. } => Repl::launch().map_err(ApplicationError::Repl),
            | Commands::Build {
                file,
                target_os,
                target_arch,
                target,
                build_dir,
                runtime_dir,
                execute,
                no_cps,
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
                if no_cps { CpsMode::Disabled } else { CpsMode::Enabled },
            ),
        }
    }

    fn format_sources(&self, paths: &[PathBuf]) -> Result<i32, ApplicationError> {
        let formatter = SourceFormatter::default();
        paths.iter().try_for_each(|path| formatter.format_path(path).map(|_| ()))?;
        Ok(0)
    }

    fn analyze(
        &self, path: &Path,
    ) -> Result<std::sync::Arc<zydeco_session::ProgramAnalysis>, ApplicationError> {
        let analysis = self.compiler.analyze(path)?;
        DiagnosticRenderer::warnings(&analysis);
        DiagnosticRenderer::observations(&analysis);
        Ok(analysis)
    }

    fn check_source(&self, path: &Path) -> Result<i32, ApplicationError> {
        self.analyze(path)?;
        Ok(0)
    }

    fn run_source(
        &self, path: &Path, dry: bool, arguments: &[String],
    ) -> Result<i32, ApplicationError> {
        let executable =
            self.analyze(path)?.executable_program().map_err(CompileError::Executable)?;
        match CommandCompiler::interpret_program(executable, arguments, dry)? {
            | ProgKont::Dry => Ok(0),
            | ProgKont::ExitCode(code) => Ok(code),
            | ProgKont::Ret(_) => unreachable!("an executable source root must return `OS`"),
        }
    }

    fn build_source(
        &self, path: &Path, target: BuildTarget, options: BuildOptions, execute: bool, cps: CpsMode,
    ) -> Result<i32, ApplicationError> {
        let executable =
            self.analyze(path)?.executable_program().map_err(CompileError::Executable)?;
        let backend = BackendProgram::lower(executable, cps)?;
        match target {
            | BuildTarget::Zir => println!("{}", backend.render_stackir()),
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
            | BuildTarget::Exe => {
                let artifact = Self::artifact_name(path)?;
                let assembly = backend.emit_amd64(options.operating_system);
                let executable = options.link_amd64(&artifact, &assembly)?;
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
    #[error("source root path `{}` does not have a valid UTF-8 artifact name", .0.display())]
    InvalidArtifactName(PathBuf),
    // #[error("REPL error: {0}")]
    // Repl(String),
}

impl ApplicationError {
    fn render(&self) {
        match self {
            | Self::Compile(error) => DiagnosticRenderer::error(error),
            | _ => eprintln!("{self}"),
        }
    }
}
