use clap::Parser;
use std::path::{Path, PathBuf};
use thiserror::Error;
use zydeco_cli::{
    BuildOptions, BuildTarget, Cli, CommandCompiler, Commands, CompileError, DiagnosticRenderer,
    DocumentationCommand, HighSpsInspection, HighSpsPass, HighSpsPlan, HighSpsPlanError,
    NativeError, RepresentationStrategy, SourceFormatError, SourceFormatOutcome, SourceFormatter,
    TargetArchitecture, TargetOs, WasmBackendKind,
    documentation::{DocumentationRenderError, DocumentationRenderer},
};
use zydeco_dynamics::ProgKont;
use zydeco_tui::{Repl, ReplError};

fn main() {
    let cli = Cli::parse();
    let compiler = CommandCompiler::default().with_lint_types(cli.lint_types);
    let result = Application { compiler }.run(cli.command);
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
    fn run(mut self, command: Commands) -> Result<i32, ApplicationError> {
        match command {
            | Commands::Passes { sps_passes } => {
                if let Some(text) = sps_passes {
                    print!("{}", text.parse::<HighSpsPlan>()?.explain());
                } else {
                    println!("Optional high-SPS passes:");
                    for pass in HighSpsPass::ALL {
                        println!("  {pass}: {}", pass.description());
                    }
                    println!("Selections: default, none, or a comma-separated pass list.");
                    println!("Use --sps-passes PLAN to explain the selected sequence.");
                }
                Ok(0)
            }
            | Commands::DocumentationExampleWorker => {
                zydeco_session::source::DocumentationExampleWorker::serve()
                    .map_err(ApplicationError::DocumentationWorker)?;
                Ok(0)
            }
            | Commands::Doc { command } => self.documentation(command),
            | Commands::Fmt { files, check } => self.format_sources(&files, check),
            | Commands::Run { file, dry, args } => self.run_source(&file, dry, &args),
            | Commands::Check { file } => self.check_source(&file),
            | Commands::Repl => Repl::launch().map_err(ApplicationError::Repl),
            | Commands::Build {
                file,
                target_os,
                target_arch,
                target,
                representation,
                pipeline,
                build_dir,
                runtime_dir,
                execute,
            } => {
                let plan = pipeline
                    .sps_passes
                    .as_deref()
                    .map(str::parse::<HighSpsPlan>)
                    .transpose()?
                    .unwrap_or_default();
                self.compiler =
                    self.compiler.with_sps_passes(plan).with_pass_inspection(HighSpsInspection {
                        trace: pipeline.trace_passes,
                        verify: pipeline.verify_passes,
                        dump: pipeline.dump_passes,
                    });
                self.build_source(
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
                    representation.map(Into::into),
                )
            }
        }
    }

    fn documentation(&self, command: DocumentationCommand) -> Result<i32, ApplicationError> {
        use zydeco_session::source::DocumentationPath;
        let file = match &command {
            | DocumentationCommand::Show { file, .. }
            | DocumentationCommand::Search { file, .. }
            | DocumentationCommand::Build { file, .. }
            | DocumentationCommand::Check { file, .. } => file,
        };
        self.analyze(file)?;
        let reference = self.compiler.documentation_reference(file)?;
        let renderer = DocumentationRenderer { reference: &reference };
        match command {
            | DocumentationCommand::Show { subject, .. } => {
                println!("{}", renderer.show(&DocumentationPath::parse(&subject))?)
            }
            | DocumentationCommand::Search { query, .. } => println!("{}", renderer.search(&query)),
            | DocumentationCommand::Build { output, title, guide, file } => {
                let guides = renderer.guides(&guide)?;
                let title = title.unwrap_or_else(|| {
                    file.file_name().unwrap_or_default().to_string_lossy().into_owned()
                });
                renderer.write(&output, &title, &guides)?;
                println!("{}", output.display());
            }
            | DocumentationCommand::Check { guide, .. } => {
                let guides = renderer.guides(&guide)?;
                renderer.check_links(&guides)?;
                let examples = renderer.examples(&guides);
                let executable =
                    std::env::current_exe().map_err(ApplicationError::DocumentationWorker)?;
                let failures = examples
                    .iter()
                    .filter_map(|example| {
                        let request = match self.compiler.documentation_example_request(example) {
                            | Ok(request) => request,
                            | Err(error) => {
                                return Some(format!("{}: {error}", example.path.display()));
                            }
                        };
                        let verification =
                            zydeco_session::source::DocumentationExampleWorker::verify(
                                &executable,
                                &["__doc-example-worker"],
                                &request,
                                std::time::Duration::from_secs(30),
                            );
                        if verification.status.is_passed() {
                            return None;
                        }
                        let details = verification
                            .diagnostics
                            .iter()
                            .map(|diagnostic| {
                                let (path, range) =
                                    if diagnostic.path.as_deref() == Some(request.path.as_path()) {
                                        (
                                            Some(example.path.as_path()),
                                            diagnostic
                                                .range
                                                .clone()
                                                .and_then(|range| example.source_range(range)),
                                        )
                                    } else {
                                        (diagnostic.path.as_deref(), diagnostic.range.clone())
                                    };
                                let path = path.unwrap_or(&example.path);
                                let source =
                                    reference.analysis.source(path).map(str::to_owned).or_else(
                                        || {
                                            guides
                                                .iter()
                                                .find(|guide| guide.path == path)
                                                .map(|guide| guide.markdown.clone())
                                        },
                                    );
                                let position = source
                                    .zip(range)
                                    .map(|(source, range)| {
                                        zydeco_utils::span::FileMap::local(source, None)
                                            .line_col(range.start)
                                            .to_string()
                                    })
                                    .unwrap_or_default();
                                format!(
                                    "{}:{position}: {}: {}",
                                    path.display(),
                                    diagnostic.code.as_deref().unwrap_or("source"),
                                    diagnostic.message
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        Some(format!(
                            "{}: {:?}\n{details}",
                            example.path.display(),
                            verification.status
                        ))
                    })
                    .collect::<Vec<_>>();
                if !failures.is_empty() {
                    return Err(
                        DocumentationRenderError::InvalidExamples(failures.join("\n")).into()
                    );
                }
                println!("Documentation links and {} examples checked.", examples.len());
            }
        }
        Ok(0)
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
        representation: Option<RepresentationStrategy>,
    ) -> Result<i32, ApplicationError> {
        if representation.is_some() && matches!(target, BuildTarget::Zir | BuildTarget::WasmSps) {
            return Err(ApplicationError::RepresentationTarget);
        }
        let analysis = self.analyze(path)?;
        let executable = self.compiler.executable_program(&analysis)?;
        let backend = self
            .compiler
            .lower_executable(executable)?
            .with_representation(representation.unwrap_or_default());
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
                println!("{}", backend.emit_amd64(options.operating_system).assembly);
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
                let native = backend.emit_amd64(options.operating_system);
                let executable =
                    options.link_amd64(&artifact, &native.assembly, &native.foreign_libraries)?;
                if execute {
                    return Ok(Self::process_exit_code(executable.run(&[])?));
                }
            }
        }
        Ok(0)
    }

    fn process_exit_code(status: std::process::ExitStatus) -> i32 {
        if let Some(code) = status.code() {
            return code;
        }
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            if let Some(signal) = status.signal() {
                return 128 + signal;
            }
        }
        1
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
    PipelinePlan(#[from] HighSpsPlanError),
    #[error(
        "--representation applies to zasm, asm, exe, and wasm-am; this target does not use assembly representation analysis"
    )]
    RepresentationTarget,
    #[error("documentation worker failed: {0}")]
    DocumentationWorker(std::io::Error),
    #[error(transparent)]
    Documentation(#[from] DocumentationRenderError),
    #[error(transparent)]
    DocumentationReference(#[from] zydeco_session::source::DocumentationReferenceError),
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
            | Self::Format(error) => DiagnosticRenderer::format_error(error),
            | _ => eprintln!("{self}"),
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::Application;
    use std::{os::unix::process::ExitStatusExt, process::ExitStatus};

    #[test]
    fn native_exit_codes_preserve_normal_exits_and_report_signals_as_failures() {
        for code in [0, 7, 134, 255] {
            assert_eq!(Application::process_exit_code(ExitStatus::from_raw(code << 8)), code);
        }
        for signal in [6, 9, 15] {
            let status = ExitStatus::from_raw(signal);
            assert_eq!(status.code(), None);
            assert_eq!(Application::process_exit_code(status), 128 + signal);
        }
    }
}
