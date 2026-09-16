use clap::Parser;
use std::{collections::BTreeSet, path::PathBuf};
use thiserror::Error;
use zydeco_cli::library::{LibraryArtifactKind, LibraryBuilder, LibraryError, LinkedLibraries};
use zydeco_cli::{
    BuildOptions, BuildTarget, Cli, CommandCompiler, Commands, CompileError, DiagnosticRenderer,
    Executable, ExecutionError, ExecutionRunner, ExecutionTarget, HighSpsInspection, HighSpsPass,
    HighSpsPlan, HighSpsPlanError, NativeError, RepresentationStrategy, SourceFormatError,
    SourceFormatOutcome, SourceFormatter, SourceSelection, TargetArchitecture, TargetOs,
    TestTarget, WasmBackendKind,
};
use zydeco_session::source::{
    Package, PackageId, PackagePath, PackageRole, SourceLoadError, SourceReference,
};
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
        if !matches!(command, Commands::Fmt { .. } | Commands::Passes { .. }) {
            let roots = ["package.zy", "workspace.zy"]
                .into_iter()
                .map(PathBuf::from)
                .filter_map(|path| match path.try_exists() {
                    | Ok(true) => Some(Ok(path)),
                    | Ok(false) => None,
                    | Err(source) => {
                        Some(Err(SourceLoadError::Read { path, source: source.into() }))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            let project = self.compiler.session().project(&roots)?;
            self.compiler = self.compiler.with_project(project);
        }
        match command {
            | Commands::Show { packages } => {
                self.show_packages(packages)?;
                Ok(0)
            }
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
            | Commands::Fmt { files, check } => self.format_sources(&files, check),
            | Commands::Run { selection, target, execution, dry, args } => {
                let sources = self.sources(selection)?;
                let [source] = sources.as_slice() else {
                    return Err(ApplicationError::SingleExecution);
                };
                self.run_source(
                    source,
                    target,
                    execution.runtime_dir,
                    dry,
                    &args,
                    LinkedLibraries::load(&execution.link_libraries)?,
                )
            }
            | Commands::Check { selection } => {
                for source in self.sources(selection)? {
                    self.check_source(&source)?;
                }
                Ok(0)
            }
            | Commands::Test { selection, targets, execution } => self.test_packages(
                self.sources(selection)?,
                targets,
                execution.runtime_dir,
                LinkedLibraries::load(&execution.link_libraries)?,
            ),
            | Commands::Repl => {
                Repl::launch(self.compiler.project().clone()).map_err(ApplicationError::Repl)
            }
            | Commands::Build {
                selection,
                target_os,
                target_arch,
                target,
                representation,
                pipeline,
                build_dir,
                runtime_dir,
                link_libraries,
                execute,
            } => {
                let plan = pipeline
                    .sps_passes
                    .as_deref()
                    .map(str::parse::<HighSpsPlan>)
                    .transpose()?
                    .unwrap_or_default();
                self.compiler = self
                    .compiler
                    .with_representation(representation.map(Into::into).unwrap_or_default())
                    .with_sps_passes(plan)
                    .with_pass_inspection(HighSpsInspection {
                        trace: pipeline.trace_passes,
                        verify: pipeline.verify_passes,
                        dump: pipeline.dump_passes,
                    });
                let libraries = LinkedLibraries::load(&link_libraries)?;
                self.build_sources(
                    self.sources(selection)?,
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
                    libraries,
                )
            }
        }
    }

    fn resolve(&self, source: &SourceReference) -> Result<PackageId, ApplicationError> {
        Ok(self.compiler.session().selection(source, self.compiler.project())?)
    }

    fn sources(
        &self, selection: SourceSelection,
    ) -> Result<Vec<SourceReference>, ApplicationError> {
        let mut seen = BTreeSet::new();
        selection
            .file
            .into_iter()
            .chain(selection.packages.into_iter().map(SourceReference::Package))
            .map(|source| Ok(seen.insert(self.resolve(&source)?).then_some(source)))
            .collect::<Result<Vec<_>, ApplicationError>>()
            .map(|sources| sources.into_iter().flatten().collect())
    }

    fn selected_analysis(
        &self, source: &SourceReference,
    ) -> Result<std::sync::Arc<zydeco_session::ProgramAnalysis>, ApplicationError> {
        let id = self.resolve(source)?;
        if matches!(source, SourceReference::Package(_)) {
            self.compiler.package(&id)?.require_role(PackageRole::Binary)?;
        }
        let analysis = self.compiler.analyze_package(&id)?;
        Ok(self.report_analysis(analysis))
    }

    fn show_packages(&self, paths: Vec<PackagePath>) -> Result<(), ApplicationError> {
        let context = zydeco_session::source::PackageContext::at_root(self.compiler.project().root);
        let declarations = if paths.is_empty() {
            self.compiler.session().declarations(self.compiler.project())?
        } else {
            let mut seen = BTreeSet::new();
            paths
                .into_iter()
                .map(|path| {
                    let namespace = context
                        .resolve(&path)
                        .map_err(zydeco_session::source::PackageError::from)?;
                    if !seen.insert(namespace.clone()) {
                        return Ok(None);
                    }
                    let id = self.resolve(&SourceReference::Package(path))?;
                    let package = self.compiler.package(&id)?;
                    Ok(Some((namespace, package)))
                })
                .collect::<Result<Vec<_>, ApplicationError>>()?
                .into_iter()
                .flatten()
                .collect()
        };
        for (namespace, package) in declarations {
            println!("{} {} ({})", package.role, namespace, package.origin);
            for import in &package.imports {
                println!("  import {}", import.directive.target);
            }
            for relation in &package.relations {
                println!("  {} {}", relation.kind, relation.target);
            }
        }
        Ok(())
    }

    fn check_package(
        &self, package: &Package,
    ) -> Result<Option<zydeco_session::ExecutableProgram>, ApplicationError> {
        let analysis = self.report_analysis(self.compiler.analyze_package(&package.id)?);
        if package.role == PackageRole::Library(zydeco_surface::metadata::LibraryRole::Source) {
            return Ok(None);
        }
        if package.role == PackageRole::Library(zydeco_surface::metadata::LibraryRole::Zydeco) {
            self.compiler.unit_program(&analysis)?;
            return Ok(None);
        }
        if let PackageRole::Library(zydeco_surface::metadata::LibraryRole::Compiled(contract)) =
            &package.role
        {
            self.compiler.library_program(&analysis, contract)?;
            return Ok(None);
        }
        let executable = self.compiler.executable_program(&analysis)?;
        zydeco_statics::BuiltinPackagePlan::for_executable(
            &executable.statics,
            &executable.signature,
        )
        .map_err(|error| CompileError::BuiltinLink(error.into()))?;
        Ok(Some(executable))
    }

    fn test_packages(
        &self, sources: Vec<SourceReference>, targets: Vec<TestTarget>, runtime_dir: PathBuf,
        libraries: LinkedLibraries,
    ) -> Result<i32, ApplicationError> {
        let packages = sources
            .iter()
            .map(|source| {
                self.compiler.package(&self.resolve(source)?).map_err(ApplicationError::from)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let programs = packages
            .iter()
            .map(|package| {
                package.require_role(PackageRole::Test)?;
                let executable =
                    self.check_package(package)?.expect("test packages are executable");
                Ok((&package.id, executable))
            })
            .collect::<Result<Vec<_>, ApplicationError>>()?;
        let runner = ExecutionRunner::new(
            &self.compiler,
            targets.iter().flat_map(TestTarget::expand).copied(),
            runtime_dir,
        )?
        .with_libraries(libraries);
        let runs = programs
            .into_iter()
            .map(|(id, executable)| {
                runner.prepare(executable).map(|runs| runs.into_iter().map(move |run| (id, run)))
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let count = runs.len();
        let mut failed = 0;
        for (id, run) in runs {
            let target = run.target;
            match run.test(&[], "") {
                | Ok(result) if result.code == 0 => println!("PASS [{target}] {id}"),
                | Ok(result) => {
                    failed += 1;
                    println!("FAIL [{target}] {id} (exit {})", result.code);
                    print!("{}", result.output);
                    eprint!("{}", result.stderr);
                }
                | Err(error) => {
                    failed += 1;
                    println!("FAIL [{target}] {id}");
                    ApplicationError::Execution(error).render();
                }
            }
        }
        println!("{} passed; {failed} failed.", count - failed);
        Ok(i32::from(failed != 0))
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

    fn report_analysis(
        &self, analysis: std::sync::Arc<zydeco_session::ProgramAnalysis>,
    ) -> std::sync::Arc<zydeco_session::ProgramAnalysis> {
        DiagnosticRenderer::warnings(&analysis);
        let program = self
            .compiler
            .checked_program(&analysis)
            .expect("a checked CLI analysis has an owned program");
        DiagnosticRenderer::observations(&analysis, &program.statics);
        analysis
    }

    fn check_source(&self, source: &SourceReference) -> Result<i32, ApplicationError> {
        let id = self.resolve(source)?;
        self.check_package(&self.compiler.package(&id)?)?;
        Ok(0)
    }

    fn run_source(
        &self, path: &SourceReference, target: ExecutionTarget, runtime_dir: PathBuf, dry: bool,
        arguments: &[String], libraries: LinkedLibraries,
    ) -> Result<i32, ApplicationError> {
        let analysis = self.selected_analysis(path)?;
        let executable = self.compiler.executable_program(&analysis)?;
        if dry && libraries.is_empty() {
            return Ok(0);
        }
        if !libraries.is_empty()
            && matches!(target, ExecutionTarget::WasmAm | ExecutionTarget::WasmSps)
        {
            return Err(LibraryError::Target.into());
        }
        let runner =
            ExecutionRunner::new(&self.compiler, [target], runtime_dir)?.with_libraries(libraries);
        runner.validate(&executable)?;
        if dry {
            return Ok(0);
        }
        let run = runner.prepare(executable)?.pop().expect("run selects one backend");
        Ok(run.run(arguments)?)
    }

    fn build_sources(
        &self, sources: Vec<SourceReference>, target: BuildTarget, options: BuildOptions,
        execute: bool, representation: Option<RepresentationStrategy>, libraries: LinkedLibraries,
    ) -> Result<i32, ApplicationError> {
        let kind = match target {
            | BuildTarget::Object => Some(LibraryArtifactKind::Object),
            | BuildTarget::Staticlib => Some(LibraryArtifactKind::Staticlib),
            | BuildTarget::Sharedlib => Some(LibraryArtifactKind::Sharedlib),
            | _ => None,
        };
        if let Some(kind) = kind {
            if execute {
                return Err(ApplicationError::LibraryExecution);
            }
            zydeco_cli::library::LibraryPlatform::for_target(
                options.architecture,
                options.operating_system,
            )?;
            let programs = sources
                .iter()
                .map(|source| {
                    let package = self.compiler.package(&self.resolve(source)?)?;
                    if !matches!(
                        &package.role,
                        PackageRole::Library(
                            zydeco_surface::metadata::LibraryRole::Compiled(_)
                                | zydeco_surface::metadata::LibraryRole::Zydeco
                        )
                    ) {
                        return Err(ApplicationError::LibraryTarget);
                    }
                    let analysis =
                        self.report_analysis(self.compiler.analyze_package(&package.id)?);
                    Ok((package, analysis))
                })
                .collect::<Result<Vec<_>, ApplicationError>>()?;
            for (package, analysis) in programs {
                let source = package
                    .id
                    .name
                    .clone()
                    .or(package.name.clone())
                    .map(SourceReference::Package)
                    .unwrap_or_else(|| SourceReference::Path(package.source.path.clone()));
                let name = match &source {
                    | SourceReference::Package(path) => self.package_name(path)?,
                    | SourceReference::Path(_) => {
                        self.artifact_name(&source)?.parse().map_err(|_| {
                            ApplicationError::InvalidArtifactName(package.source.path.clone())
                        })?
                    }
                };
                let manifest = match &package.role {
                    | PackageRole::Library(zydeco_surface::metadata::LibraryRole::Compiled(
                        contract,
                    )) => {
                        let program = self.compiler.library_program(&analysis, contract)?;
                        LibraryBuilder {
                            compiler: &self.compiler,
                            options: &options,
                            dependencies: &libraries,
                        }
                        .build(&name, contract, &analysis, &program, kind)?
                    }
                    | PackageRole::Library(zydeco_surface::metadata::LibraryRole::Zydeco) => {
                        if kind != LibraryArtifactKind::Object {
                            return Err(LibraryError::UnitTarget.into());
                        }
                        let program = self.compiler.unit_program(&analysis)?;
                        zydeco_cli::unit::UnitBuilder {
                            compiler: &self.compiler,
                            options: &options,
                            dependencies: &libraries,
                        }
                        .build(&name, &analysis, &program)?
                    }
                    | _ => return Err(ApplicationError::LibraryTarget),
                };
                println!("{}", manifest.display());
            }
            return Ok(0);
        }
        if !libraries.is_empty() && target != BuildTarget::Exe {
            return Err(ApplicationError::LibraryLinkTarget);
        }
        if representation.is_some() && matches!(target, BuildTarget::Zir) {
            return Err(ApplicationError::RepresentationTarget);
        }
        if execute && sources.len() != 1 {
            return Err(ApplicationError::SingleExecution);
        }
        let backends = sources
            .iter()
            .map(|source| {
                let package = self.compiler.package(&self.resolve(source)?)?;
                let analysis = self.report_analysis(self.compiler.analyze_package(&package.id)?);
                let executable = self.compiler.executable_program(&analysis)?;
                Ok(self
                    .compiler
                    .lower_executable(executable)?
                    .with_representation(representation.unwrap_or_default()))
            })
            .collect::<Result<Vec<_>, ApplicationError>>()?;
        for (path, backend) in sources.iter().zip(backends) {
            match target {
                | BuildTarget::Object | BuildTarget::Staticlib | BuildTarget::Sharedlib => {
                    unreachable!()
                }
                | BuildTarget::Zir => println!("{}", backend.render_sps_low()),
                | BuildTarget::Zasm if execute => println!("{}", backend.execute_assembly()?),
                | BuildTarget::Zasm => println!("{}", backend.render_assembly()),
                | BuildTarget::Asm => {
                    if options.architecture != TargetArchitecture::X86_64 {
                        return Err(NativeError::UnsupportedAmd64Architecture(
                            options.architecture,
                        )
                        .into());
                    }
                    println!("{}", backend.emit_amd64(options.operating_system).assembly);
                }
                | BuildTarget::WasmAm => {
                    if execute {
                        return Err(NativeError::WasmBuildExecution.into());
                    }
                    let artifact = self.artifact_name(path)?;
                    let module = backend.emit_wasm_am()?;
                    let module =
                        options.write_wasm(&artifact, WasmBackendKind::AbstractMachine, &module)?;
                    println!("{}", module.path().display());
                }
                | BuildTarget::WasmSps => {
                    if execute {
                        return Err(NativeError::WasmBuildExecution.into());
                    }
                    let artifact = self.artifact_name(path)?;
                    let module = backend.emit_wasm_sps()?;
                    let module = options.write_wasm(&artifact, WasmBackendKind::SpsLow, &module)?;
                    println!("{}", module.path().display());
                }
                | BuildTarget::Exe => {
                    let artifact = self.artifact_name(path)?;
                    let native = backend.emit_amd64(options.operating_system);
                    libraries.validate_imports(
                        &native.foreign_imports,
                        zydeco_cli::library::LibraryPlatform::for_target(
                            options.architecture,
                            options.operating_system,
                        )?,
                        Some(&zydeco_cli::library::LibraryDigest::runtime(&options.runtime_dir)?),
                        false,
                    )?;
                    libraries.validate_units(
                        &native.unit_imports,
                        zydeco_cli::library::LibraryPlatform::for_target(
                            options.architecture,
                            options.operating_system,
                        )?,
                        Some(&zydeco_cli::library::LibraryDigest::runtime(&options.runtime_dir)?),
                        true,
                    )?;
                    let executable = options.link_amd64_resolved(
                        &artifact,
                        &native.assembly,
                        &native.foreign_libraries,
                        &libraries,
                    )?;
                    if execute {
                        return Ok(Executable::exit_code(executable.run(&[])?));
                    }
                }
            }
        }
        Ok(0)
    }

    fn artifact_name(&self, source: &SourceReference) -> Result<String, ApplicationError> {
        match source {
            | SourceReference::Package(name) => {
                Ok(self.package_name(name)?.to_string().replace('/', "."))
            }
            | SourceReference::Path(path) => path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .map(str::to_owned)
                .ok_or_else(|| ApplicationError::InvalidArtifactName(path.clone())),
        }
    }

    fn package_name(
        &self, path: &PackagePath,
    ) -> Result<zydeco_surface::metadata::PackageName, ApplicationError> {
        let context = zydeco_session::source::PackageContext::at_root(self.compiler.project().root);
        let namespace =
            context.resolve(path).map_err(zydeco_session::source::PackageError::from)?.to_string();
        let name = namespace.trim_start_matches('/');
        Ok(if name.is_empty() { "root" } else { name }
            .parse()
            .expect("normalized named package path"))
    }
}

#[derive(Debug, Error)]
enum ApplicationError {
    #[error(transparent)]
    Library(#[from] LibraryError),
    #[error(
        "library artifacts require library(c, export(...), ...), or library(zydeco) with --target object"
    )]
    LibraryTarget,
    #[error(
        "a compiled library is entered through its exports and cannot be executed as a process"
    )]
    LibraryExecution,
    #[error("--link-library requires --target exe, object, staticlib, or sharedlib")]
    LibraryLinkTarget,
    #[error("run and build --execute require exactly one selected package")]
    SingleExecution,
    #[error("cannot read the working directory: {0}")]
    WorkingDirectory(#[from] std::io::Error),
    #[error(transparent)]
    PackageContract(#[from] zydeco_session::source::PackageError),
    #[error(transparent)]
    Package(#[from] zydeco_session::source::SourceLoadError),
    #[error(transparent)]
    PipelinePlan(#[from] HighSpsPlanError),
    #[error(
        "--representation applies to zasm, asm, exe, wasm-am, wasm-sps, object, staticlib, and sharedlib; this target does not select physical representations"
    )]
    RepresentationTarget,
    #[error(transparent)]
    Format(#[from] SourceFormatError),
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Execution(#[from] ExecutionError),
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
            | Self::Compile(error) | Self::Execution(ExecutionError::Compile(error)) => {
                DiagnosticRenderer::error(error)
            }
            | Self::Package(error) => DiagnosticRenderer::source_error(error),
            | Self::Format(error) => DiagnosticRenderer::format_error(error),
            | _ => eprintln!("{self}"),
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::Executable;
    use std::{os::unix::process::ExitStatusExt, process::ExitStatus};

    #[test]
    fn native_exit_codes_preserve_normal_exits_and_report_signals_as_failures() {
        for code in [0, 7, 134, 255] {
            assert_eq!(Executable::exit_code(ExitStatus::from_raw(code << 8)), code);
        }
        for signal in [6, 9, 15] {
            let status = ExitStatus::from_raw(signal);
            assert_eq!(status.code(), None);
            assert_eq!(Executable::exit_code(status), 128 + signal);
        }
    }
}
