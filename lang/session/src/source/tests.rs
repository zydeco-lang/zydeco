use super::*;
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};
use zydeco_utils::{arena::ArenaAccess, pass::CompilerPass};

type SourceScoped = ScopedProgram;

#[derive(Clone)]
struct SourceChecked {
    spans: zydeco_surface::textual::syntax::SpanArena,
    scoped: zydeco_surface::scoped::arena::ScopedArena,
    statics: zydeco_statics::arena::StaticsArena,
    root: zydeco_statics::syntax::TermAnnId,
}

struct SourceDynamics {
    program: zydeco_dynamics::syntax::DynamicsProgram,
}

struct SourceStack {
    spans: zydeco_surface::textual::syntax::SpanArena,
    scoped: zydeco_surface::scoped::arena::ScopedArena,
    statics: zydeco_statics::arena::StaticsArena,
    stackir: zydeco_stackir::BranchJoinProgram,
}

struct SourceSpsLow {
    spans: zydeco_surface::textual::syntax::SpanArena,
    scoped: zydeco_surface::scoped::arena::ScopedArena,
    statics: zydeco_statics::arena::StaticsArena,
    sps_low: zydeco_stackir::SpsLowProgram,
}

struct SourceAssembly {
    spans: zydeco_surface::textual::syntax::SpanArena,
    scoped: zydeco_surface::scoped::arena::ScopedArena,
    statics: zydeco_statics::arena::StaticsArena,
    sps_low: zydeco_stackir::SpsLowProgram,
    assembly: zydeco_assembly::syntax::AssemblyProgram,
}

struct NativePackage {
    name: String,
    assembly: String,
}

#[derive(Debug, thiserror::Error)]
enum TestPipelineError {
    #[error(transparent)]
    Analysis(AnalysisError),
    #[error("type checking rejected the test source")]
    Rejected,
    #[error(transparent)]
    Executable(ExecutableError),
    #[error(transparent)]
    Dynamic(zydeco_dynamics::BuiltinPackageError),
    #[error(transparent)]
    Stack(zydeco_stackir::BuiltinPackageLowerError),
}

impl ScopedProgram {
    fn check(self) -> Result<SourceChecked, zydeco_statics::TyckReports> {
        let Self { spans, arena, prim, root } = self;
        let session = super::CompilerSession::default();
        let output = session.check_resolved(spans.clone(), prim, arena, root);
        let checked = output.outcome.into_result()?;
        Ok(SourceChecked {
            spans,
            scoped: output.scoped,
            statics: checked.statics,
            root: checked.root,
        })
    }
}

impl SourceChecked {
    fn dynamics(self) -> Result<SourceDynamics, TestPipelineError> {
        let zydeco_statics::syntax::TermAnnId::Compu(root, _) = self.root else {
            return Err(TestPipelineError::Executable(ExecutableError::NonComputation {
                found: self.root.into(),
            }));
        };
        Ok(SourceDynamics {
            program: zydeco_dynamics::RootLinker {
                scoped: self.scoped,
                statics: self.statics,
                root,
            }
            .run(),
        })
    }

    fn dynamics_with_builtin(self) -> Result<SourceDynamics, TestPipelineError> {
        use zydeco_statics::syntax::{Fillable, TermAnnId, Type};

        let TermAnnId::Compu(root, ty) = self.root else {
            return Err(TestPipelineError::Executable(ExecutableError::NonComputation {
                found: self.root.into(),
            }));
        };
        let Fillable::Done(Type::PackPi(signature)) = self.statics.types_pre[&ty].clone() else {
            return Err(TestPipelineError::Executable(ExecutableError::NonBuiltinExecutable {
                found: ty,
            }));
        };
        let arena = zydeco_dynamics::BuiltinRootLinker {
            scoped: self.scoped,
            statics: self.statics,
            root,
            signature,
        }
        .run()
        .map_err(TestPipelineError::Dynamic)?;
        Ok(SourceDynamics { program: arena })
    }

    fn stackir(self) -> Result<SourceStack, TestPipelineError> {
        let zydeco_statics::syntax::TermAnnId::Compu(root, _) = self.root else {
            return Err(TestPipelineError::Executable(ExecutableError::NonComputation {
                found: self.root.into(),
            }));
        };
        let Self { spans, mut scoped, statics, root: _ } = self;
        let stackir =
            match zydeco_stackir::RootLowerer::new(&spans, &mut scoped, &statics, root).run() {
                | Ok(stackir) => stackir,
                | Err(never) => match never {},
            };
        Ok(SourceStack { spans, scoped, statics, stackir })
    }

    fn stackir_with_builtin(self) -> Result<SourceStack, TestPipelineError> {
        use zydeco_statics::syntax::{Fillable, TermAnnId, Type};

        let TermAnnId::Compu(root, ty) = self.root else {
            return Err(TestPipelineError::Executable(ExecutableError::NonComputation {
                found: self.root.into(),
            }));
        };
        let Fillable::Done(Type::PackPi(signature)) = self.statics.types_pre[&ty].clone() else {
            return Err(TestPipelineError::Executable(ExecutableError::NonBuiltinExecutable {
                found: ty,
            }));
        };
        let Self { spans, mut scoped, statics, root: _ } = self;
        let stackir =
            zydeco_stackir::BuiltinRootLowerer::new(&spans, &mut scoped, &statics, root, signature)
                .run()
                .map_err(TestPipelineError::Stack)?;
        Ok(SourceStack { spans, scoped, statics, stackir })
    }
}

impl SourceStack {
    fn convert(self) -> SourceSpsLow {
        let Self { spans, mut scoped, statics, stackir } = self;
        let sps_low = zydeco_stackir::SpsLowPipeline::new(&mut scoped).run(stackir);
        SourceSpsLow { spans, scoped, statics, sps_low }
    }
}

impl SourceSpsLow {
    fn assemble(self) -> SourceAssembly {
        let Self { spans, scoped, statics, sps_low } = self;
        let assembly =
            zydeco_assembly::LoweringPipeline::new(&spans, &scoped, &statics, &sps_low).run();
        SourceAssembly { spans, scoped, statics, sps_low, assembly }
    }
}

struct TestPipeline;

/// One session shared by every test in this module.
///
/// Salsa memoizes the standard library's sub-analyses across roots, so tests
/// that import `lib/std` only pay for their own root after the first one.
/// The mutex serializes the analysis phase, which is also what keeps the
/// heavy tests from saturating every core at once.
static SHARED_SESSION: LazyLock<Mutex<CompilerSession>> =
    LazyLock::new(|| Mutex::new(CompilerSession::default()));

impl TestPipeline {
    fn check(path: impl AsRef<Path>) -> Result<SourceChecked, TestPipelineError> {
        let session = SHARED_SESSION.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        let analysis = session.analyze(path).map_err(TestPipelineError::Analysis)?;
        let checked = analysis.checked_program().ok_or(TestPipelineError::Rejected)?;
        Ok(SourceChecked {
            spans: checked.spans,
            scoped: checked.scoped,
            statics: checked.statics,
            root: checked.root,
        })
    }

    /// Lower an already-checked program to zasm without re-analyzing it.
    fn zasm_from_checked(
        checked: SourceChecked, _: crate::TestOutput,
    ) -> Result<SourceAssembly, TestPipelineError> {
        Ok(checked.stackir_with_builtin()?.convert().assemble())
    }

    /// Emit amd64 assembly from an already-checked program without
    /// re-analyzing it.
    fn amd64_from_checked(
        path: impl AsRef<Path>, checked: SourceChecked, verbosity: crate::TestOutput,
    ) -> Result<NativePackage, TestPipelineError> {
        let path = path.as_ref();
        let name = path.file_stem().and_then(|stem| stem.to_str()).unwrap().to_owned();
        let lowered = Self::zasm_from_checked(checked, verbosity)?;
        let target = if cfg!(target_os = "macos") {
            zydeco_amd64::TargetFormat::MachO
        } else {
            zydeco_amd64::TargetFormat::Elf
        };
        let assembly = match zydeco_amd64::Emitter::new(
            &lowered.spans,
            &lowered.scoped,
            &lowered.statics,
            &lowered.assembly,
            target,
        )
        .run()
        {
            | Ok(assembly) => assembly.to_string(),
            | Err(never) => match never {},
        };
        Ok(NativePackage { name, assembly })
    }

    fn zasm(
        path: impl AsRef<Path>, _: crate::TestOutput,
    ) -> Result<SourceAssembly, TestPipelineError> {
        Ok(Self::check(path)?.stackir_with_builtin()?.convert().assemble())
    }

    fn amd64(
        path: impl AsRef<Path>, _: crate::TestBuildOptions, verbosity: crate::TestOutput,
    ) -> Result<NativePackage, TestPipelineError> {
        let path = path.as_ref();
        let name = path.file_stem().and_then(|stem| stem.to_str()).unwrap().to_owned();
        let lowered = Self::zasm(path, verbosity)?;
        let target = if cfg!(target_os = "macos") {
            zydeco_amd64::TargetFormat::MachO
        } else {
            zydeco_amd64::TargetFormat::Elf
        };
        let assembly = match zydeco_amd64::Emitter::new(
            &lowered.spans,
            &lowered.scoped,
            &lowered.statics,
            &lowered.assembly,
            target,
        )
        .run()
        {
            | Ok(assembly) => assembly.to_string(),
            | Err(never) => match never {},
        };
        Ok(NativePackage { name, assembly })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TestSourceDiscovery {
    path: PathBuf,
    discovered: usize,
}

impl SourceGraph {
    pub(crate) fn load(path: impl AsRef<Path>) -> Result<Self, SourceLoadError> {
        CompilerSession::default()
            .graph(path)
            .map(|graph| (*graph).clone())
            .map_err(|error| (*error).clone())
    }

    fn load_with_progress(
        path: impl AsRef<Path>, mut progress: impl FnMut(TestSourceDiscovery),
    ) -> Result<Self, SourceLoadError> {
        let graph = Self::load(path)?;
        graph.sources.iter().enumerate().for_each(|(index, (_, source))| {
            progress(TestSourceDiscovery { path: source.path.clone(), discovered: index + 1 })
        });
        Ok(graph)
    }

    fn source_by_path(&self, path: impl AsRef<Path>) -> Option<SourceId> {
        let path = path.as_ref().canonicalize().ok()?;
        self.sources.iter().find_map(|(source, file)| (file.path == path).then_some(source))
    }
}

struct SourceFixture {
    directory: tempfile::TempDir,
}

struct RepositorySourceFiles;

impl RepositorySourceFiles {
    fn all() -> Vec<PathBuf> {
        let repository =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap();
        ["cli", "docs", "lib"]
            .into_iter()
            .flat_map(|directory| Self::under(&repository.join(directory)).unwrap())
            .collect()
    }

    fn under(directory: &Path) -> std::io::Result<Vec<PathBuf>> {
        std::fs::read_dir(directory)?
            .map(|entry| {
                let entry = entry?;
                let path = entry.path();
                if entry.file_type()?.is_dir() {
                    Self::under(&path)
                } else if matches!(
                    path.extension().and_then(|extension| extension.to_str()),
                    Some("zy" | "zyi" | "zydeco")
                ) {
                    Ok(vec![path])
                } else {
                    Ok(Vec::new())
                }
            })
            .collect::<std::io::Result<Vec<_>>>()
            .map(|paths| paths.into_iter().flatten().collect())
    }

    fn assert_pure_package(relative: impl AsRef<Path>) {
        use zydeco_statics::syntax::{Fillable, TermAnnId, Type};

        let checked = TestPipeline::check(repository_source(relative)).unwrap();
        let TermAnnId::Value(_, root_type) = checked.root else {
            panic!("expected a pure package value")
        };
        let classifier = &checked.statics.types_pre[&root_type];
        assert!(
            matches!(classifier, Fillable::Done(Type::VArrow(_) | Type::VPackPi(_))),
            "expected a pure package factory, got {classifier:?}"
        );
    }
}

impl SourceFixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }

    fn path(&self, relative: impl AsRef<Path>) -> PathBuf {
        self.directory.path().join(relative)
    }

    fn write(&self, relative: impl AsRef<Path>, source: impl AsRef<str>) -> PathBuf {
        let path = self.path(relative);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(&path, source.as_ref()).unwrap();
        path
    }
}

fn resolve_program(
    program: TextualProgram,
) -> Result<SourceScoped, Box<zydeco_surface::scoped::ResolveError>> {
    program.desugar().unwrap().resolve()
}

fn checked_trivial_computation() -> SourceChecked {
    use zydeco_statics::{arena::StaticsArena, syntax as ss};
    use zydeco_utils::prelude::IdAllocator;

    let mut allocator = IdAllocator::<zydeco_statics::arena::StaticsScope>::new();
    let value = allocator.alloc();
    let root = allocator.alloc();
    let ty = allocator.alloc();
    let mut statics = StaticsArena::default();
    statics.values.insert_new(value, ss::Triv.into());
    statics.compus.insert_new(root, ss::Return(value).into());
    SourceChecked {
        spans: Default::default(),
        scoped: Default::default(),
        statics,
        root: ss::TermAnnId::Compu(root, ty),
    }
}

fn builtin_add_exit_source() -> &'static str {
    r#"
begin
  let Int64 = @[intrinsic(i64)] _ that
  param (
    (/OS; /int64; /process) :
    exists
      @[builtin(os)] (OS : @[intrinsic(ctype)] _)
    .
      (int64 ::
        (@[builtin(int64_add)] (add ::
          (@[intrinsic(thk)] _) (Int64 -> Int64 -> (@[intrinsic(ret)] _) Int64))) *
        (@[builtin(int64_sub)] (sub ::
          (@[intrinsic(thk)] _) (Int64 -> Int64 -> (@[intrinsic(ret)] _) Int64)))) *
      (process ::
        @[builtin(exit)] (exit :: (@[intrinsic(thk)] _) (Int64 -> OS)))
  ) in
    do sum <- ! (int64/add) 1 2;
    ! (process/exit) sum
end
"#
}

fn repository_source(relative: impl AsRef<Path>) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib").join(relative)
}

fn assert_source_program_exits_zero_and_reaches_amd64(relative: impl AsRef<Path>) {
    let root = repository_source(relative);
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

fn assert_source_io_program(relative: impl AsRef<Path>, source_input: &str, expected_output: &str) {
    let root = repository_source(relative);
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::Cursor::new(source_input);
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert_eq!(String::from_utf8(output).unwrap(), expected_output);
    assert!(!native.assembly.is_empty());
}

fn assert_source_io_program_reaches_zasm(
    relative: impl AsRef<Path>, source_input: &str, expected_output: &str,
) {
    let root = repository_source(relative);
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::Cursor::new(source_input);
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert_eq!(String::from_utf8(output).unwrap(), expected_output);
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

fn assert_source_program_exits_zero_and_reaches_zasm(relative: impl AsRef<Path>) {
    let root = repository_source(relative);
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

fn assert_source_program_reaches_amd64(relative: impl AsRef<Path>) {
    let native = TestPipeline::amd64(
        repository_source(relative),
        crate::TestBuildOptions::default(),
        crate::TestOutput::quiet(),
    )
    .unwrap();

    assert!(!native.assembly.is_empty());
}

#[test]
fn source_graph_loads_nested_relative_imports_in_provider_order() {
    let fixture = SourceFixture::new();
    fixture.write("leaf.zy", "1");
    fixture.write("nested/library.zy", r#"@[import("../leaf.zy")] _"#);
    let root = fixture.write("main.zy", r#"@[import("nested/library.zy")] _"#);

    let graph = SourceGraph::load(&root).unwrap();
    let order = graph
        .provider_order()
        .into_iter()
        .map(|source| graph.sources[&source].path.file_name().unwrap().to_owned())
        .collect::<Vec<_>>();

    assert_eq!(graph.sources.len(), 3);
    assert_eq!(graph.imports.len(), 2);
    assert_eq!(order, ["leaf.zy", "library.zy", "main.zy"].map(std::ffi::OsString::from));
    assert!(graph.sources.iter().all(|(_, source)| source.path.is_absolute()));
}

#[test]
fn source_graph_accepts_parenthesized_metadata_sugar_for_hole_payloads() {
    let fixture = SourceFixture::new();
    fixture.write("leaf.zy", "1");
    fixture.write("nested/library.zy", r#"@(import("../leaf.zy"))"#);
    let root = fixture.write("main.zy", r#"@(import("nested/library.zy"))"#);

    let graph = SourceGraph::load(&root).unwrap();
    let order = graph
        .provider_order()
        .into_iter()
        .map(|source| graph.sources[&source].path.file_name().unwrap().to_owned())
        .collect::<Vec<_>>();

    assert_eq!(graph.sources.len(), 3);
    assert_eq!(graph.imports.len(), 2);
    assert_eq!(order, ["leaf.zy", "library.zy", "main.zy"].map(std::ffi::OsString::from));
}

#[test]
fn source_graph_discovers_an_adjacent_signature_before_its_implementation() {
    let fixture = SourceFixture::new();
    fixture.write("main.zyi", "@[intrinsic(unit)] _");
    let root = fixture.write("main.zy", "()");

    let graph = SourceGraph::load(&root).unwrap();
    let implementation = &graph.sources[&graph.root];
    let signature = implementation.signature.expect("expected a companion signature");
    let order = graph
        .provider_order()
        .into_iter()
        .map(|source| graph.sources[&source].path.file_name().unwrap().to_owned())
        .collect::<Vec<_>>();

    assert_eq!(graph.sources.len(), 2);
    assert_eq!(graph.sources[&signature].kind(), SourceKind::Signature);
    assert_eq!(order, ["main.zyi", "main.zy"].map(std::ffi::OsString::from));
}

#[test]
fn source_graph_does_not_pair_program_roots_with_signatures() {
    let fixture = SourceFixture::new();
    fixture.write("main.zyi", "@[intrinsic(i64)] _");
    let root = fixture.write("main.zydeco", "()");

    let graph = SourceGraph::load(root).unwrap();

    assert_eq!(graph.sources.len(), 1);
    assert_eq!(graph.sources[&graph.root].kind(), SourceKind::Program);
    assert!(graph.sources[&graph.root].signature.is_none());
}

#[test]
fn source_graph_reports_unique_sources_as_they_are_discovered() {
    let fixture = SourceFixture::new();
    fixture.write("leaf.zy", "1");
    fixture.write("nested/library.zy", r#"@[import("../leaf.zy")] _"#);
    let root = fixture.write("main.zy", r#"@[import("nested/library.zy")] _"#);
    let mut progress = Vec::new();

    let graph = SourceGraph::load_with_progress(&root, |update| progress.push(update)).unwrap();
    let progress = progress
        .into_iter()
        .map(|update| (update.path.file_name().unwrap().to_owned(), update.discovered))
        .collect::<Vec<_>>();

    assert_eq!(graph.sources.len(), 3);
    assert_eq!(
        progress,
        [
            (std::ffi::OsString::from("main.zy"), 1),
            (std::ffi::OsString::from("library.zy"), 2),
            (std::ffi::OsString::from("leaf.zy"), 3),
        ]
    );
}

#[test]
fn source_graph_orders_a_diamond_after_its_shared_provider() {
    let fixture = SourceFixture::new();
    fixture.write("leaf.zy", "1");
    fixture.write("left.zy", r#"@[import("leaf.zy")] _"#);
    fixture.write("right.zy", r#"@[import("leaf.zy")] _"#);
    let root = fixture.write("main.zy", r#"(@[import("left.zy")] _, @[import("right.zy")] _)"#);

    let graph = SourceGraph::load(&root).unwrap();
    let order = graph
        .provider_order()
        .into_iter()
        .enumerate()
        .map(|(position, source)| {
            (graph.sources[&source].path.file_name().unwrap().to_owned(), position)
        })
        .collect::<std::collections::HashMap<_, _>>();

    assert_eq!(graph.sources.len(), 4);
    assert_eq!(graph.imports.len(), 4);
    let position = |name| order[std::ffi::OsStr::new(name)];
    assert!(position("leaf.zy") < position("left.zy"));
    assert!(position("leaf.zy") < position("right.zy"));
    assert!(position("left.zy") < position("main.zy"));
    assert!(position("right.zy") < position("main.zy"));
}

#[test]
fn source_graph_deduplicates_files_but_preserves_import_occurrences() {
    let fixture = SourceFixture::new();
    let library = fixture.write("library.zy", "1");
    let root =
        fixture.write("main.zy", r#"(@[import("library.zy")] _, @[import("library.zy")] _)"#);

    let graph = SourceGraph::load(&root).unwrap();
    let library = graph.source_by_path(library).unwrap();
    let root_imports = &graph.sources[&graph.root].imports;
    let imported =
        root_imports.iter().map(|import| graph.imports[import].imported).collect::<Vec<_>>();

    assert_eq!(graph.sources.len(), 2);
    assert_eq!(root_imports.len(), 2);
    assert_eq!(imported, [library, library]);
    assert_ne!(root_imports[0], root_imports[1]);
    assert_ne!(graph.imports[&root_imports[0]].term, graph.imports[&root_imports[1]].term);
}

#[cfg(unix)]
#[test]
fn source_graph_deduplicates_symlinked_source_paths() {
    use std::os::unix::fs::symlink;

    let fixture = SourceFixture::new();
    fixture.write("library.zy", "1");
    symlink(fixture.path("library.zy"), fixture.path("alias.zy")).unwrap();
    let root = fixture.write("main.zy", r#"(@[import("library.zy")] _, @[import("alias.zy")] _)"#);

    let graph = SourceGraph::load(root).unwrap();
    let imports = &graph.sources[&graph.root].imports;

    assert_eq!(graph.sources.len(), 2);
    assert_eq!(imports.len(), 2);
    assert_eq!(graph.imports[&imports[0]].imported, graph.imports[&imports[1]].imported);
}

#[test]
fn source_graph_accepts_absolute_import_paths() {
    let fixture = SourceFixture::new();
    let library = fixture.write("library.zy", "1").canonicalize().unwrap();
    let root = fixture.write("main.zy", format!(r#"@[import("{}")] _"#, library.display()));

    let graph = SourceGraph::load(root).unwrap();
    let [import] = graph.sources[&graph.root].imports.as_slice() else {
        panic!("expected one import")
    };

    assert_eq!(graph.sources[&graph.imports[import].imported].path, library);
}

#[test]
fn source_graph_rejects_cycles_with_every_import_site() {
    let fixture = SourceFixture::new();
    let first = fixture.write("first.zy", r#"@[import("second.zy")] _"#);
    fixture.write("second.zy", r#"@[import("first.zy")] _"#);

    let SourceLoadError::Cycle(cycle) = SourceGraph::load(first).unwrap_err() else {
        panic!("expected an import cycle")
    };
    let names = cycle
        .steps
        .iter()
        .map(|step| {
            (
                step.dependent.file_name().unwrap().to_owned(),
                step.dependency.file_name().unwrap().to_owned(),
            )
        })
        .collect::<Vec<_>>();

    assert_eq!(
        names,
        [("first.zy".into(), "second.zy".into()), ("second.zy".into(), "first.zy".into()),]
    );
    assert!(cycle.steps.iter().all(|step| step.span.get_path().is_some()));
}

#[test]
fn source_graph_rejects_a_self_import_at_its_site() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", r#"@[import("main.zy")] _"#);

    let SourceLoadError::Cycle(cycle) = SourceGraph::load(root).unwrap_err() else {
        panic!("expected a self-import cycle")
    };
    let [step] = cycle.steps.as_slice() else { panic!("expected one self-import step") };

    assert_eq!(step.dependent, step.dependency);
    assert_eq!(
        step.span.get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("main.zy"))
    );
}

#[test]
fn source_graph_rejects_cycles_through_a_companion_signature() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "()");
    fixture.write("main.zyi", r#"@[import("main.zy")] _"#);

    let SourceLoadError::Cycle(cycle) = SourceGraph::load(root).unwrap_err() else {
        panic!("expected a source dependency cycle")
    };

    assert_eq!(cycle.steps.len(), 2);
    assert!(cycle.steps.iter().any(|step| step.kind == SourceDependencyKind::Signature));
    assert!(cycle.steps.iter().any(|step| matches!(step.kind, SourceDependencyKind::Import(_))));
}

#[test]
fn source_graph_reports_a_missing_import_at_its_source_site() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", r#"@[import("missing.zy")] _"#);

    let SourceLoadError::ImportPath { importer, requested, span, .. } =
        SourceGraph::load(root).unwrap_err()
    else {
        panic!("expected a missing import")
    };

    assert_eq!(importer.file_name().unwrap(), "main.zy");
    assert_eq!(requested.file_name().unwrap(), "missing.zy");
    assert_eq!(
        span.get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("main.zy"))
    );
}

#[test]
fn source_graph_rejects_a_legacy_declaration_sequence() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "def value = 1 end main value end");

    assert!(matches!(
        SourceGraph::load(root),
        Err(SourceLoadError::Parse(SourceParseError::Parse { .. }))
    ));
}

#[test]
fn source_graph_rejects_an_unknown_builtin_role() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[builtin(number)] _");

    let SourceLoadError::Parse(SourceParseError::BuiltinDirective { error, .. }) =
        SourceGraph::load(root).unwrap_err()
    else {
        panic!("expected an invalid Builtin directive")
    };
    let zydeco_surface::textual::BuiltinDirectiveError::Invalid { source, .. } = *error else {
        panic!("expected an invalid Builtin role")
    };
    assert!(matches!(source.as_ref(), zydeco_surface::metadata::BuiltinMetaError::UnknownRole(_)));
}

#[test]
fn source_graph_rejects_a_roleless_intrinsic_splice() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[intrinsic] _");

    let SourceLoadError::Parse(SourceParseError::IntrinsicDirective { error, .. }) =
        SourceGraph::load(root).unwrap_err()
    else {
        panic!("expected an invalid intrinsic directive")
    };
    assert!(matches!(
        &*error,
        zydeco_surface::textual::IntrinsicDirectiveError::Invalid {
            source: zydeco_surface::metadata::IntrinsicMetaError::RoleArity { found: 0 },
            ..
        }
    ));
}

#[test]
fn program_assembly_consumes_import_directives_and_preserves_a_source_boundary() {
    let fixture = SourceFixture::new();
    fixture.write("library.zy", "1");
    let root = fixture.write("main.zy", r#"@[import("library.zy")] _"#);

    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    assert!(matches!(
        program.arena.terms[&program.unit.root],
        zydeco_surface::textual::syntax::Term::SourceBoundary(_)
    ));
    assert!(program.arena.terms.iter().all(|(_, term)| {
        !matches!(
            term,
            zydeco_surface::textual::syntax::Term::Meta(zydeco_syntax::MetaT(meta, _))
                if meta.is("import")
        )
    }));
}

#[test]
fn textual_program_ascribes_an_implementation_with_its_signature() {
    use zydeco_surface::textual::syntax::{Ann, SignatureBoundary, Term};

    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "@[intrinsic(unit)] _");
    let root = fixture.write("library.zy", "()");

    let program = SourceGraph::load(root).unwrap().parse().unwrap();
    let Term::Ann(Ann { tm: _, ty }) = program.arena.terms[&program.unit.root] else {
        panic!("expected a root ascription")
    };

    assert!(matches!(program.arena.terms[&ty], Term::SignatureBoundary(SignatureBoundary(_))));
}

#[test]
fn builtin_operation_roles_remain_specializable_through_name_resolution() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[builtin(int64_add)] _");
    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    let resolved = resolve_program(program).unwrap();
    let zydeco_surface::scoped::syntax::Term::Meta(zydeco_syntax::MetaT(meta, payload)) =
        &resolved.arena.terms[&resolved.root]
    else {
        panic!("expected a Builtin metadata term")
    };

    assert_eq!(
        meta.specialize::<zydeco_surface::metadata::BuiltinMeta>().unwrap().map(|meta| meta.role),
        Some(zydeco_syntax::BuiltinRole::Value(zydeco_syntax::BuiltinValueRole::Integer(
            zydeco_syntax::IntegerType::Int64,
            zydeco_syntax::IntegerOperation::Add,
        ),))
    );
    assert!(matches!(resolved.arena.terms[payload], zydeco_surface::scoped::syntax::Term::Hole(_)));
}

#[test]
fn program_assembly_freshens_each_import_occurrence() {
    use zydeco_surface::textual::syntax::{Abs, CoPattern, Paren, Pattern, SourceBoundary, Term};

    let fixture = SourceFixture::new();
    fixture.write("library.zy", "fn value => value");
    let root =
        fixture.write("main.zy", r#"(@[import("library.zy")] _, @[import("library.zy")] _)"#);

    let program = SourceGraph::load(root).unwrap().parse().unwrap();
    let Term::Paren(Paren(imports)) = &program.arena.terms[&program.unit.root] else {
        panic!("expected a pair of imports")
    };
    let definitions = imports
        .iter()
        .map(|import| {
            let Term::SourceBoundary(SourceBoundary(function)) = &program.arena.terms[import]
            else {
                panic!("expected a source boundary")
            };
            let Term::Abs(Abs(copattern, _)) = &program.arena.terms[function] else {
                panic!("expected an imported function")
            };
            let CoPattern::Pat(pattern) = program.arena.copats[copattern] else {
                panic!("expected a parameter pattern")
            };
            let Pattern::Var(definition) = program.arena.pats[&pattern] else {
                panic!("expected a variable parameter")
            };
            definition
        })
        .collect::<Vec<_>>();

    assert_eq!(definitions.len(), 2);
    assert_ne!(definitions[0], definitions[1]);
    assert_eq!(program.arena.defs[&definitions[0]], program.arena.defs[&definitions[1]]);
}

#[test]
fn program_assembly_retains_importer_and_provider_spans() {
    use zydeco_surface::textual::syntax::{SourceBoundary, Term};

    let fixture = SourceFixture::new();
    fixture.write("library.zy", "fn value => value");
    let root = fixture.write("main.zy", r#"@[import("library.zy")] _"#);

    let program = SourceGraph::load(root).unwrap().parse().unwrap();
    let boundary = program.unit.root;
    let Term::SourceBoundary(SourceBoundary(provider)) = program.arena.terms[&boundary] else {
        panic!("expected a source boundary")
    };

    assert_eq!(
        program.spans[&boundary.into()].get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("main.zy"))
    );
    assert_eq!(
        program.spans[&provider.into()].get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("library.zy"))
    );
}

#[test]
fn program_assembly_expands_nested_imports_recursively() {
    use zydeco_surface::textual::syntax::{SourceBoundary, Term};

    let fixture = SourceFixture::new();
    fixture.write("leaf.zy", "1");
    fixture.write("library.zy", r#"@[import("leaf.zy")] _"#);
    let root = fixture.write("main.zy", r#"@[import("library.zy")] _"#);

    let program = SourceGraph::load(root).unwrap().parse().unwrap();
    let Term::SourceBoundary(SourceBoundary(library)) = program.arena.terms[&program.unit.root]
    else {
        panic!("expected the library boundary")
    };
    let Term::SourceBoundary(SourceBoundary(leaf)) = program.arena.terms[&library] else {
        panic!("expected the nested leaf boundary")
    };

    assert!(matches!(program.arena.terms[&leaf], Term::Lit(_)));
    assert_eq!(
        program.spans[&leaf.into()].get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("leaf.zy"))
    );
}

#[test]
fn imported_free_names_do_not_capture_importer_bindings() {
    let fixture = SourceFixture::new();
    fixture.write("library.zy", "value");
    let root = fixture.write("main.zy", r#"let value = _ in @[import("library.zy")] _"#);
    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    let Err(error) = resolve_program(program) else {
        panic!("expected name resolution to reject caller capture")
    };
    let zydeco_surface::scoped::ResolveError::UnboundVar(name) = error.as_ref() else {
        panic!("expected an unbound imported name, got {error}")
    };

    assert_eq!(name.inner.0, "value");
    assert_eq!(
        name.info.get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("library.zy"))
    );
}

#[test]
fn imported_mobile_bindings_do_not_move_into_an_importer_block() {
    let fixture = SourceFixture::new();
    fixture.write("library.zy", "param value that value");
    let root = fixture.write("main.zy", r#"begin @[import("library.zy")] _ end"#);
    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    let Err(error) = resolve_program(program) else {
        panic!("expected name resolution to reject cross-file mobility")
    };
    let zydeco_surface::scoped::ResolveError::UnenclosedThat(span) = error.as_ref() else {
        panic!("expected an unenclosed imported binding, got {error}")
    };

    assert_eq!(
        span.get_path().and_then(|path| path.file_name()),
        Some(std::ffi::OsStr::new("library.zy"))
    );
}

#[test]
fn a_self_contained_imported_term_resolves_normally() {
    let fixture = SourceFixture::new();
    fixture.write("library.zy", "fn value => value");
    let root = fixture.write("main.zy", r#"@[import("library.zy")] _"#);
    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    let resolved = resolve_program(program).unwrap();

    assert!(matches!(
        resolved.arena.terms[&resolved.root],
        zydeco_surface::scoped::syntax::Term::SourceBoundary(_)
    ));
    assert!(resolved.arena.coctxs_term_local[&resolved.root].0.is_empty());
}

#[test]
fn importing_once_and_binding_once_shares_one_lexical_identity() {
    use zydeco_surface::scoped::syntax::{Let, Pattern, Term};

    let fixture = SourceFixture::new();
    fixture.write("library.zy", "fn value => value");
    let root = fixture
        .write("main.zy", r#"let library = @[import("library.zy")] _ in (library, library)"#);
    let program = SourceGraph::load(root).unwrap().parse().unwrap();

    let resolved = resolve_program(program).unwrap();
    let Term::Let(Let { binder, .. }) = resolved.arena.terms[&resolved.root] else {
        panic!("expected an explicit sharing binding")
    };
    let Pattern::Var(library) = resolved.arena.pats[&binder] else {
        panic!("expected a variable binder")
    };

    assert_eq!(resolved.arena.users.forth(&library).len(), 2);
}

#[test]
fn the_source_pipeline_reaches_statics_without_a_declaration_entry() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "_");
    let scoped =
        SourceGraph::load(root).unwrap().parse().unwrap().desugar().unwrap().resolve().unwrap();

    let _root_term = &scoped.arena.terms[&scoped.root];
    let Err(reports) = scoped.check() else {
        panic!("an unclassified root hole must not count as a checked source term")
    };
    assert!(!reports.spans.is_empty());
}

#[test]
fn a_literal_splice_parses_to_a_string_literal() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "--| Line one\n--| Line two\n@[literal] _");
    let program = SourceGraph::load(root).unwrap().parse().unwrap();
    let zydeco_surface::textual::syntax::Term::Lit(
        zydeco_surface::textual::syntax::Literal::String(text),
    ) = &program.arena.terms[&program.unit.root]
    else {
        panic!("expected `@[literal]` to parse to a string literal")
    };
    assert_eq!(text.as_str(), "Line one\nLine two");
}

#[test]
fn a_literal_splice_checks_as_a_string_value() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "--| Message\n@[literal] _");
    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Value(_, _)));
}

#[test]
fn a_literal_splice_without_an_attached_text_block_is_rejected() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[literal] _");
    let error = SourceGraph::load(root).unwrap_err();
    let SourceLoadError::Parse(SourceParseError::LiteralDirective { error, .. }) = error else {
        panic!("expected an invalid literal splice")
    };
    assert!(matches!(&*error, zydeco_surface::textual::LiteralDirectiveError::MissingText { .. }));
}

#[test]
fn a_literal_splice_on_a_non_hole_term_is_rejected() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "--| Text\n@[literal] 1");
    let error = SourceGraph::load(root).unwrap_err();
    let SourceLoadError::Parse(SourceParseError::LiteralDirective { error, .. }) = error else {
        panic!("expected an invalid literal splice")
    };
    assert!(matches!(
        &*error,
        zydeco_surface::textual::LiteralDirectiveError::PayloadNotHole { .. }
    ));
}

#[test]
fn a_matching_companion_signature_checks_the_implementation() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "@[intrinsic(unit)] _");
    let root = fixture.write("library.zy", "()");

    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Value(_, _)));
}

#[test]
fn a_companion_signature_can_import_its_type_dependencies() {
    let fixture = SourceFixture::new();
    fixture.write("unit_type.zy", "@[intrinsic(unit)] _");
    fixture.write("library.zyi", r#"@[import("unit_type.zy")] _"#);
    let root = fixture.write("library.zy", "()");

    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Value(_, _)));
}

#[test]
fn a_mismatched_companion_signature_rejects_the_implementation() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "@[intrinsic(i64)] _");
    let root = fixture.write("library.zy", "()");

    let analysis = CompilerSession::default().analyze(root).unwrap();

    assert!(analysis.outcome().root().is_none());
    assert!(analysis.outcome().reports().is_some_and(|reports| !reports.spans.is_empty()));
}

#[test]
fn a_signature_root_must_itself_be_a_type() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "()");
    let root = fixture.write("library.zy", "()");

    let analysis = CompilerSession::default().analyze(root).unwrap();

    assert!(analysis.outcome().root().is_none());
    assert!(analysis.outcome().reports().is_some_and(|reports| !reports.spans.is_empty()));
}

#[test]
fn an_imported_implementation_is_checked_against_its_companion_signature() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "@[intrinsic(unit)] _");
    fixture.write("library.zy", "()");
    let root = fixture.write("main.zy", r#"@[import("library.zy")] _"#);

    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Value(_, _)));
}

#[test]
fn an_explicit_signature_import_is_a_type_term() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "@[intrinsic(unit)] _");
    let root = fixture.write("main.zy", r#"@[import("library.zyi")] _"#);

    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Type(_, _)));
}

#[test]
fn an_explicit_signature_import_still_rejects_a_non_type_root() {
    let fixture = SourceFixture::new();
    fixture.write("library.zyi", "()");
    let root = fixture.write("main.zy", r#"@[import("library.zyi")] _"#);

    let analysis = CompilerSession::default().analyze(root).unwrap();

    assert!(analysis.outcome().root().is_none());
    assert!(analysis.outcome().reports().is_some_and(|reports| !reports.spans.is_empty()));
}

#[test]
fn the_declaration_free_unbound_fixture_fails_during_resolution() {
    let program =
        SourceGraph::load(repository_source("tests/fail/unbound.zy")).unwrap().parse().unwrap();
    let Err(error) = resolve_program(program) else {
        panic!("the unbound fixture unexpectedly resolved")
    };
    let zydeco_surface::scoped::ResolveError::UnboundVar(name) = error.as_ref() else {
        panic!("expected an unbound variable, got {error}")
    };

    assert_eq!(name.inner.0, "x");
}

#[test]
fn the_declaration_free_annotation_fixture_fails_during_type_checking() {
    let scoped = SourceGraph::load(repository_source("tests/fail/annotation.zy"))
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap();

    assert!(scoped.check().is_err());
}

#[test]
fn explicit_intrinsic_splices_produce_canonical_cbpv_terms() {
    use zydeco_statics::syntax::{Fillable, Kind, TermAnnId, Type};

    [
        ("@[intrinsic(vtype)] _", "vtype"),
        ("@[intrinsic(ctype)] _", "ctype"),
        ("@[intrinsic(thk)] _", "thk"),
        ("@[intrinsic(ret)] _", "ret"),
        ("@[intrinsic(unit)] _", "unit"),
    ]
    .into_iter()
    .for_each(|(source, expected)| {
        let fixture = SourceFixture::new();
        let root = fixture.write("main.zy", source);
        let checked = SourceGraph::load(root)
            .unwrap()
            .parse()
            .unwrap()
            .desugar()
            .unwrap()
            .resolve()
            .unwrap()
            .check()
            .unwrap();

        match (checked.root, expected) {
            | (TermAnnId::Kind(kind), "vtype") => {
                assert!(matches!(checked.statics.kinds_pre[&kind], Fillable::Done(Kind::VType(_))));
            }
            | (TermAnnId::Kind(kind), "ctype") => {
                assert!(matches!(checked.statics.kinds_pre[&kind], Fillable::Done(Kind::CType(_))));
            }
            | (TermAnnId::Type(ty, _), "thk") => {
                assert!(matches!(checked.statics.types_pre[&ty], Fillable::Done(Type::Thk(_))));
            }
            | (TermAnnId::Type(ty, _), "ret") => {
                assert!(matches!(checked.statics.types_pre[&ty], Fillable::Done(Type::Ret(_))));
            }
            | (TermAnnId::Type(ty, _), "unit") => {
                assert!(matches!(checked.statics.types_pre[&ty], Fillable::Done(Type::Unit(_))));
            }
            | (found, expected) => panic!("unexpected intrinsic result {found:?} for {expected}"),
        }
    });
}

#[test]
fn intrinsic_spellings_are_ordinary_bindable_names_in_root_sources() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "begin def VType = @[intrinsic(unit)] _ that VType end");

    SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();
}

#[test]
fn a_zero_dependency_source_program_checks_and_runs_as_one_term() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "ret ()");
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Compu(_, _)));

    let SourceDynamics { program: arena, .. } = checked.dynamics().unwrap();
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], arena).run();

    assert!(matches!(
        result,
        zydeco_dynamics::ProgKont::Ret(zydeco_dynamics::syntax::SemValue::Triv(
            zydeco_syntax::Triv
        ))
    ));
}

#[test]
fn a_zero_dependency_source_program_lowers_directly_to_stack_ir() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "ret ()");
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();

    let SourceStack { stackir, scoped, .. } = checked.stackir().unwrap();
    let stackir = stackir.as_program();
    assert!(stackir.arena.inner.compus.get(&stackir.root).is_some());
    zydeco_stackir::sps::check::check(stackir, &scoped);
}

#[test]
fn a_fixed_primitive_intrinsic_classifies_literals_without_a_package_scope() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        "begin let Int64 = @[intrinsic(i64)] _ that def value : Int64 = 1 that ret value end",
    );
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Compu(_, _)));
}

#[test]
fn an_integer_literal_defaults_to_int64_without_a_package_scope() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "ret 1");
    SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();
}

#[test]
fn repeated_primitive_intrinsics_have_one_applicative_identity() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        concat!(
            "begin let IntA = @[intrinsic(i64)] _ that ",
            "let IntB = @[intrinsic(i64)] _ that ",
            "def a : IntA = 1 that def b : IntB = a that ret b end",
        ),
    );
    SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();
}

#[test]
fn one_package_signature_rejects_duplicate_builtin_type_roles() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        r#"
param (
  (IntA, IntB, value) :
  exists
    @[builtin(reader)] (IntA : @[intrinsic(vtype)] _)
    @[builtin(reader)] (IntB : @[intrinsic(vtype)] _)
  . IntA
) in
  ret value
"#,
    );
    let scoped =
        SourceGraph::load(root).unwrap().parse().unwrap().desugar().unwrap().resolve().unwrap();

    assert!(scoped.check().is_err());
}

#[test]
fn one_package_signature_rejects_duplicate_builtin_operation_roles() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        r#"
param (
  (OS, = first, = second) :
  exists
    @[builtin(os)] (OS : @[intrinsic(ctype)] _)
  .
    ((@[builtin(int64_add)] (first :: @[intrinsic(unit)] _)) *
     (@[builtin(int64_add)] (second :: @[intrinsic(unit)] _)))
) in
  ret ()
"#,
    );
    let scoped =
        SourceGraph::load(root).unwrap().parse().unwrap().desugar().unwrap().resolve().unwrap();

    assert!(scoped.check().is_err());
}

#[test]
fn builtin_host_type_roles_require_abstract_entries_of_the_right_kind() {
    let cases = [
        concat!(
            "exists @[builtin(reader)] (Reader as (@[intrinsic(unit)] _) : ",
            "@[intrinsic(vtype)] _) . (@[intrinsic(unit)] _)"
        ),
        concat!(
            "exists @[builtin(reader)] (Reader : @[intrinsic(ctype)] _) . ",
            "(@[intrinsic(unit)] _)"
        ),
        concat!("exists @[builtin(os)] (OS : @[intrinsic(vtype)] _) . ", "(@[intrinsic(unit)] _)"),
    ];

    cases.into_iter().for_each(|source| {
        let fixture = SourceFixture::new();
        let root = fixture.write("main.zy", source);
        let scoped =
            SourceGraph::load(root).unwrap().parse().unwrap().desugar().unwrap().resolve().unwrap();

        assert!(scoped.check().is_err(), "`{source}` unexpectedly checked");
    });
}

#[test]
fn a_builtin_operation_role_attaches_to_its_named_classifier() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[builtin(int64_add)] (add :: @[intrinsic(unit)] _)");
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();
    let zydeco_statics::syntax::TermAnnId::Type(entry, _) = checked.root else {
        panic!("a named classifier must check as a type")
    };

    assert_eq!(
        checked.statics.builtin_roles.value(entry),
        Some(zydeco_syntax::BuiltinValueRole::Integer(
            zydeco_syntax::IntegerType::Int64,
            zydeco_syntax::IntegerOperation::Add,
        ))
    );
}

#[test]
fn a_builtin_operation_role_rejects_an_unnamed_classifier() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", "@[builtin(int64_add)] @[intrinsic(unit)] _");
    let scoped =
        SourceGraph::load(root).unwrap().parse().unwrap().desugar().unwrap().resolve().unwrap();

    assert!(scoped.check().is_err());
}

#[test]
fn the_interpreter_constructs_and_applies_a_typed_builtin_package() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", builtin_add_exit_source());
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();

    let SourceDynamics { program: arena, .. } = checked.dynamics_with_builtin().unwrap();
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], arena).run();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(3)));
}

#[test]
fn continuation_io_uses_its_foundational_builtin_classifier() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        r#"
begin
  let Int64 = @[intrinsic(i64)] _ that
  param (
    (/OS; /stdio; /process) :
    exists
      @[builtin(os)] (OS : @[intrinsic(ctype)] _)
    .
      (stdio :: @[builtin(write_int)]
        (write_int :: (@[intrinsic(thk)] _)
          (Int64 -> (@[intrinsic(thk)] _) OS -> OS))) *
      (process :: @[builtin(exit)]
        (exit :: (@[intrinsic(thk)] _) (Int64 -> OS)))
  ) in
    ! (stdio/write_int) 7 { ! (process/exit) 0 }
end
"#,
    );
    let dynamics = TestPipeline::check(root).unwrap().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();

    assert_eq!(output, b"7");
    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
}

#[test]
fn exact_builtin_classifiers_follow_bound_intrinsic_aliases() {
    let fixture = SourceFixture::new();
    let root = fixture.write(
        "main.zy",
        r#"
begin
  let Thk = @[intrinsic(thk)] _ that
  let Ret = @[intrinsic(ret)] _ that
  let Int64 = @[intrinsic(i64)] _ that
  param (
    (/OS; /int64; /process) :
    exists
      @[builtin(os)] (OS : @[intrinsic(ctype)] _)
    .
      (int64 :: @[builtin(int64_add)] (add :: Thk (Int64 -> Int64 -> Ret Int64))) *
      (process :: @[builtin(exit)] (exit :: Thk (Int64 -> OS)))
  ) in
    do sum <- ! (int64/add) 1 2;
    ! (process/exit) sum
end
"#,
    );

    let checked = TestPipeline::check(root).unwrap();

    assert!(matches!(checked.root, zydeco_statics::syntax::TermAnnId::Compu(_, _)));
}

#[test]
fn stack_ir_constructs_and_applies_the_same_typed_builtin_package() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", builtin_add_exit_source());
    let checked = SourceGraph::load(root)
        .unwrap()
        .parse()
        .unwrap()
        .desugar()
        .unwrap()
        .resolve()
        .unwrap()
        .check()
        .unwrap();

    let SourceStack { stackir, scoped, .. } = checked.stackir_with_builtin().unwrap();

    let stackir = stackir.as_program();
    assert!(stackir.arena.inner.compus.get(&stackir.root).is_some());
    zydeco_stackir::sps::check::check(stackir, &scoped);
}

#[test]
fn builtin_applied_stack_ir_reaches_analyzed_assembly() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", builtin_add_exit_source());
    let SourceAssembly { sps_low, assembly, .. } =
        TestPipeline::zasm(root, crate::TestOutput::quiet()).unwrap();

    assert!(sps_low.arena().inner.compus.get(&sps_low.root()).is_some());
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn declaration_free_source_reaches_native_assembly_emission() {
    let fixture = SourceFixture::new();
    let root = fixture.write("main.zy", builtin_add_exit_source());
    let checked = TestPipeline::check(&root).unwrap();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert_eq!(native.name, "main");
    assert!(!native.assembly.is_empty());
}

#[test]
fn canonical_builtin_signature_imports_into_interpreter_and_native_compilation() {
    let root = repository_source("tests/builtin/full.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn authored_intrinsic_splices_are_each_introduced_once_in_the_builtin_tree() {
    let builtin_root = repository_source("std/builtin.zy").canonicalize().unwrap();
    let builtin_modules = repository_source("std/builtin").canonicalize().unwrap();
    let (builtin_sources, unexpected): (Vec<_>, Vec<_>) = RepositorySourceFiles::all()
        .into_iter()
        .filter(|path| std::fs::read_to_string(path).unwrap().contains("@[intrinsic("))
        .partition(|path| path == &builtin_root || path.starts_with(&builtin_modules));
    let source = builtin_sources
        .iter()
        .map(|path| std::fs::read_to_string(path).unwrap())
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        unexpected.is_empty(),
        "intrinsic splices outside the canonical Builtin package tree: {unexpected:?}"
    );
    [
        "vtype", "ctype", "thk", "ret", "unit", "i8", "i16", "i32", "i64", "u8", "u16", "u32",
        "u64", "f32", "f64", "char", "string", "bytes",
    ]
    .into_iter()
    .for_each(|role| {
        let spelling = format!("@[intrinsic({role})] _");
        assert_eq!(
            source.match_indices(&spelling).count(),
            1,
            "expected exactly one `{role}` intrinsic splice"
        );
    });
}

#[test]
fn canonical_builtin_signature_keeps_only_system_capabilities_abstract() {
    use zydeco_statics::syntax::{ExistsMode, Fillable, Kind, ManifestKind, TermAnnId, Type};
    use zydeco_syntax::{BuiltinRole, BuiltinTypeRole};

    #[derive(Clone, Copy)]
    enum ExpectedField {
        VType,
        CType,
        Abstract(BuiltinTypeRole),
    }

    let checked = TestPipeline::check(repository_source("tests/builtin/full.zy")).unwrap();
    let TermAnnId::Compu(_, root_type) = checked.root else {
        panic!("expected the canonical Builtin consumer to be a computation")
    };
    let Fillable::Done(Type::PackPi(signature)) = checked.statics.types_pre[&root_type].to_owned()
    else {
        panic!("expected the Builtin consumer to have a package-dependent arrow")
    };
    let fields = [
        ExpectedField::VType,
        ExpectedField::CType,
        ExpectedField::Abstract(BuiltinTypeRole::Reader),
        ExpectedField::Abstract(BuiltinTypeRole::Writer),
        ExpectedField::Abstract(BuiltinTypeRole::OS),
    ];
    let (tail, abstract_witnesses) = fields.into_iter().fold(
        (signature.domain, Vec::new()),
        |(field_type, abstract_witnesses), expected| match (
            expected,
            checked.statics.types_pre[&field_type].to_owned(),
        ) {
            | (
                ExpectedField::VType,
                Fillable::Done(Type::ManifestKind(ManifestKind { definition, body, .. })),
            ) => {
                assert!(matches!(
                    checked.statics.kinds_pre[&definition],
                    Fillable::Done(Kind::VType(_))
                ));
                (body, abstract_witnesses)
            }
            | (
                ExpectedField::CType,
                Fillable::Done(Type::ManifestKind(ManifestKind { definition, body, .. })),
            ) => {
                assert!(matches!(
                    checked.statics.kinds_pre[&definition],
                    Fillable::Done(Kind::CType(_))
                ));
                (body, abstract_witnesses)
            }
            | (ExpectedField::Abstract(role), Fillable::Done(Type::Exists(exists))) => {
                assert!(matches!(exists.mode, ExistsMode::Abstract));
                assert_eq!(
                    checked.statics.builtin_roles.witness(exists.binder.witness),
                    Some(BuiltinRole::Type(role))
                );
                (
                    exists.body,
                    abstract_witnesses.into_iter().chain([exists.binder.witness]).collect(),
                )
            }
            | _ => panic!("unexpected canonical Builtin field"),
        },
    );

    let abstract_roles = abstract_witnesses
        .into_iter()
        .map(|witness| checked.statics.builtin_roles.witness(witness))
        .collect::<Vec<_>>();
    let opened_roles = signature
        .witnesses
        .iter()
        .map(|witness| checked.statics.builtin_roles.witness(*witness))
        .collect::<Vec<_>>();
    assert_eq!(abstract_roles, opened_roles);
    assert_eq!(
        opened_roles,
        vec![
            Some(BuiltinRole::Type(BuiltinTypeRole::Reader)),
            Some(BuiltinRole::Type(BuiltinTypeRole::Writer)),
            Some(BuiltinRole::Type(BuiltinTypeRole::OS)),
        ]
    );
    assert!(matches!(checked.statics.types_pre[&tail], Fillable::Done(Type::Prod(_))));
}

#[test]
fn foundational_comparison_selects_a_computation_without_constructing_bool() {
    let root = repository_source("tests/builtin/comparison-branch.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn foundational_line_parser_selects_a_continuation_without_constructing_option() {
    let root = repository_source("tests/builtin/read-line-as-int-branch.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::Cursor::new("42\nnot-an-integer\n");
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn foundational_argument_fold_preserves_sequence_without_constructing_list() {
    let root = repository_source("tests/builtin/arg-fold.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let arguments = ["alpha".to_string(), "beta".to_string()];
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &arguments, dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn standard_library_package_composes_as_an_ordinary_imported_term() {
    RepositorySourceFiles::assert_pure_package("std/std.zy");

    let root = repository_source("tests/std/minimal.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn standard_library_reifies_foundational_comparisons_as_abstract_bool() {
    let root = repository_source("tests/std/comparisons.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn standard_library_reifies_foundational_splits_as_abstract_option() {
    let root = repository_source("tests/std/splits.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn standard_library_reifies_foundational_line_parsing_as_abstract_option() {
    let root = repository_source("tests/std/read-line-as-int.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::Cursor::new("42\nnot-an-integer\n");
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn standard_library_reifies_foundational_argument_fold_as_abstract_list() {
    let root = repository_source("tests/std/arg-list.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let arguments = ["alpha".to_string(), "beta".to_string()];
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &arguments, dynamics).run();
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm_from_checked(checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn legacy_alias_example_ports_to_uniform_term_composition() {
    let root = repository_source("tests/exec/alias.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn standard_prelude_exports_legacy_thunk_and_return_aliases() {
    let root = repository_source("tests/std/identity.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_tuple_example_ports_to_uniform_term_composition() {
    let root = repository_source("tests/builtin/tuple.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn exact_signed_arithmetic_agrees_through_interpretation_and_native_emission() {
    let root = repository_source("tests/builtin/arithmetic.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(native.assembly.contains("call zydeco_int64_div"));
    assert!(native.assembly.contains("call zydeco_int64_mod"));
}

#[test]
fn recursive_nominal_types_port_to_a_declaration_free_block() {
    let root = repository_source("tests/builtin/recursive-data.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn abstract_bool_package_exports_values_and_an_eliminator() {
    RepositorySourceFiles::assert_pure_package("std/data/bool.zy");

    let root = repository_source("tests/std/bool.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn abstract_option_package_exports_a_type_constructor_and_an_eliminator() {
    RepositorySourceFiles::assert_pure_package("std/data/option.zy");

    let root = repository_source("tests/std/option.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn abstract_list_package_exports_case_analysis_and_a_recursive_fold() {
    RepositorySourceFiles::assert_pure_package("std/data/list.zy");

    let root = repository_source("tests/std/list.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn named_manifest_package_example_ports_without_declarations() {
    let root = repository_source("tests/pack/named.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn interleaved_pack_pi_example_ports_without_declarations() {
    let root = repository_source("tests/pack/interleaved.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_match_example_ports_without_declarations() {
    let root = repository_source("tests/compile/match.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_comatch_example_ports_without_declarations() {
    let root = repository_source("tests/compile/comatch.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_continuation_clone_example_ports_without_declarations() {
    let root = repository_source("tests/compile/kont-clone.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_factorial_example_ports_through_foundational_comparison() {
    let root = repository_source("tests/compile/fact.zy");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn legacy_constant_example_ports_with_mobile_term_definitions() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/const.zy");
}

#[test]
fn legacy_higher_order_example_ports_with_type_and_computation_parameters() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/hof.zy");
}

#[test]
fn legacy_nested_bind_example_ports_without_declarations() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/nested.zy");
}

#[test]
fn legacy_recursive_sum_example_ports_through_foundational_comparison() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/sum.zy");
}

#[test]
fn legacy_uniform_composition_example_ports_as_one_term() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/uniform.zy");
}

#[test]
fn legacy_nested_existential_product_example_ports_as_one_term() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/triple.zy");
}

#[test]
fn remaining_small_compile_examples_port_as_root_terms() {
    [
        "tests/compile/add0.zy",
        "tests/compile/add.zy",
        "tests/compile/exit.zy",
        "tests/compile/sub.zy",
        "tests/compile/mul.zy",
        "tests/compile/env.zy",
        "tests/compile/nested-out.zy",
        "tests/compile/label.zy",
        "tests/compile/id.zy",
        "tests/compile/cmp.zy",
        "tests/compile/fn-cmp-ret.zy",
        "tests/compile/tuple.zy",
        "tests/compile/tuple-do.zy",
        "tests/compile/let-stack.zy",
    ]
    .into_iter()
    .for_each(assert_source_program_exits_zero_and_reaches_amd64);
}

#[test]
fn finite_io_compile_examples_port_as_root_terms() {
    [
        ("tests/compile/echo-none.zy", "ignored\n", ""),
        ("tests/compile/echo-none-cap.zy", "captured\n", ""),
        ("tests/compile/echo-none-twice.zy", "first\nsecond\n", ""),
        ("tests/compile/echo-once.zy", "echoed\n", "echoed\n"),
    ]
    .into_iter()
    .for_each(|(fixture, input, output)| assert_source_io_program(fixture, input, output));
}

#[test]
fn nonterminating_compile_examples_check_and_lower_as_root_terms() {
    ["tests/compile/echo.zy", "tests/compile/loop.zy", "tests/compile/looping.zy"]
        .into_iter()
        .for_each(assert_source_program_reaches_amd64);
}

#[test]
fn named_term_compile_example_ports_as_one_block() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/named.zy");
}

#[test]
fn mixed_named_data_and_codata_compile_example_ports_as_one_block() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/compile/named-mixed.zy");
}

#[test]
fn small_non_monadic_exec_examples_port_as_root_terms() {
    [
        "tests/exec/forall.zy",
        "tests/exec/fn-opt.zy",
        "tests/exec/partial-annotation.zy",
        "tests/exec/ret.zydeco",
        "tests/exec/exists.zy",
        "tests/exec/abort.zy",
        "tests/exec/explosion.zy",
        "tests/exec/num.zy",
        "tests/exec/even-odd-codata.zy",
        "tests/exec/even-odd-fix.zy",
        "tests/exec/even-odd-data.zy",
        "tests/exec/add.zy",
        "tests/exec/ifz.zy",
        "tests/exec/bigmac.zy",
        "tests/exec/comment.zy",
        "tests/exec/loopy.zy",
        "tests/exec/optiont.zy",
        "tests/exec/loop.zydeco",
    ]
    .into_iter()
    .for_each(assert_source_program_exits_zero_and_reaches_amd64);
}

#[test]
fn intrinsic_unit_exec_example_ports_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/unit.zy", "", "()\n");
}

#[test]
fn literal_text_exec_example_runs_through_the_standard_library() {
    assert_source_io_program_reaches_zasm(
        "tests/exec/literal.zy",
        "",
        "Hello literal\nsecond line\n",
    );
}

#[test]
fn choice_exec_example_composes_the_standard_library_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/choice.zy", "", "0\n1\no\n");
}

#[test]
fn choice_root_reaches_zasm_through_sps_low() {
    let SourceAssembly { assembly, .. } =
        TestPipeline::zasm(repository_source("tests/exec/choice.zy"), crate::TestOutput::quiet())
            .unwrap();

    assert!(assembly.arena.programs.get(&assembly.root).is_some());
}

#[test]
fn y_combinator_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/Y.zydeco", "", "");
}

#[test]
fn lazy_list_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm(
        "tests/exec/listm.zydeco",
        "",
        "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n",
    );
}

#[test]
fn variadic_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm(
        "tests/exec/variadic.zy",
        "",
        "hello\nworld\nhello\nworld\n",
    );
}

#[test]
fn abstract_list_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/list.zydeco", "", "4\n");
}

#[test]
fn object_oriented_codata_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/oo.zydeco", "", "42\n");
}

#[test]
fn defunctionalization_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/defunctionalization.zydeco", "", "18\n");
}

#[test]
fn deterministic_pushdown_automaton_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm(
        "tests/exec/deterministic-pushdown-automaton.zydeco",
        "",
        concat!(
            "The following parens are balanced? (())\n",
            "true\n",
            "The following parens are balanced? ()()\n",
            "true\n",
            "The following parens are balanced? )(()\n",
            "false\n",
            "The following parens are balanced? ()(\n",
            "false\n",
            "The following parens are balanced? ()\n",
            "true\n",
            "The following parens are balanced? (\n",
            "false\n",
            "The following parens are balanced? )\n",
            "false\n",
            "The following parens are balanced? \n",
            "true\n",
        ),
    );
}

#[test]
fn self_interpreter_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/interpreter.zydeco", "", "false\n");
}

#[test]
fn regular_expression_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/regex.zy", "", "\\^o^/\n");
}

#[test]
fn monadic_block_resolves_its_basis_from_ordinary_lexical_types() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/monadic-ret.zy");
}

#[test]
fn monadic_block_lifts_builtin_literals_with_trivial_value_structures() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/monadic-int.zy");
}

#[test]
fn nested_monadic_block_uses_the_nearest_lexical_basis() {
    TestPipeline::check(repository_source("tests/monadic/shadow.zy")).unwrap();
}

#[test]
fn monadic_pack_pi_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/pack/monadic.zy");
}

#[test]
fn algebra_construction_exec_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/alg.zy");
}

#[test]
fn algebra_translation_exec_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/algtrans.zy");
}

#[test]
fn generated_exception_transformer_exports_a_pure_package() {
    RepositorySourceFiles::assert_pure_package("tests/oopsla/exnt.zydeco");
}

#[test]
fn generated_continuation_exception_transformer_exports_a_pure_package() {
    RepositorySourceFiles::assert_pure_package("tests/oopsla/exnkt.zydeco");
}

#[test]
fn free_monad_exec_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/free.zy");
}

#[test]
fn monad_transformer_exec_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/exec/trans.zy");
}

#[test]
fn free_handler_exec_example_uses_the_lexical_library_basis() {
    assert_source_program_exits_zero_and_reaches_zasm("tests/exec/free'.zy");
}

#[test]
fn backtracking_exec_example_uses_the_lexical_library_basis() {
    assert_source_io_program_reaches_zasm("tests/exec/backtrack.zydeco", "", "1 + 4\n2 + 3\n");
}

#[test]
fn call_by_value_interpreter_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/cbv.zy", "", "\\^o^/\n");
}

#[test]
fn cbpv_interpreter_exec_example_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/exec/cbpv.zy", "", "\\^o^/\n\\^o^/\n");
}

#[test]
fn cbpv_parser_and_monadic_interpreter_compose_as_a_root_term() {
    assert_source_io_program_reaches_zasm(
        "tests/exec/cbpv-monadic.zy",
        "",
        "\\^o^/\n\\^o^/\n\\^o^/\n\\^o^/\n",
    );
}

#[test]
fn initial_oopsla_artifact_examples_compose_as_root_terms() {
    ["tests/oopsla/polynomial.zydeco", "tests/oopsla/cc.zydeco"]
        .into_iter()
        .for_each(assert_source_program_exits_zero_and_reaches_amd64);
}

#[test]
fn playground_is_a_configuration_free_root_program() {
    let root = repository_source("playground/main.zydeco");
    let checked = TestPipeline::check(&root).unwrap();
    let dynamics = checked.clone().dynamics_with_builtin().unwrap().program;
    let mut input = std::io::empty();
    let mut output = std::io::sink();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], dynamics).run();
    let native =
        TestPipeline::amd64_from_checked(root, checked, crate::TestOutput::quiet()).unwrap();

    assert!(matches!(result, zydeco_dynamics::ProgKont::ExitCode(42)));
    assert!(!native.assembly.is_empty());
}

#[test]
fn reusable_examples_are_configuration_free_root_programs() {
    assert_source_program_exits_zero_and_reaches_amd64("examples/abort.zydeco");
    assert_source_io_program_reaches_zasm(
        "examples/echo_sum.zydeco",
        "1\n2\nnot-an-integer\n",
        "1 = sum\n3 = sum\n",
    );
}

#[test]
fn oopsla_cbv_interpreter_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/oopsla/cbv.zydeco", "", "true\n");
}

#[test]
fn oopsla_monad_examples_compose_with_a_lexical_basis() {
    assert_source_io_program_reaches_zasm("tests/oopsla/monads.zydeco", "", "Hello, world!\n");
}

#[test]
fn oopsla_free_monad_composes_with_a_lexical_basis() {
    assert_source_io_program_reaches_zasm("tests/oopsla/free.zydeco", "", "\n");
}

#[test]
fn oopsla_defunctionalized_exception_counterexample_composes_as_a_root_term() {
    assert_source_io_program_reaches_zasm("tests/oopsla/exn.zydeco", "", "2 != 1\n");
}

#[test]
fn oopsla_relative_monad_algebras_compose_as_a_root_term() {
    assert_source_io_program_reaches_zasm("examples/algebra.zydeco", "", "");
}

#[test]
fn named_exec_examples_have_focused_root_term_counterparts() {
    [
        "tests/compile/named.zy",
        "tests/exec/named-tuple.zy",
        "tests/exec/named-nested.zy",
        "tests/exec/named-pattern.zy",
        "tests/exec/named-data.zy",
        "tests/exec/named-function.zy",
        "tests/exec/named-codata.zy",
        "tests/exec/named-pun.zy",
        "tests/compile/named-mixed.zy",
    ]
    .into_iter()
    .for_each(assert_source_program_exits_zero_and_reaches_amd64);
}

#[test]
fn manifest_package_example_ports_as_a_root_term() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/pack/manifest.zy");
}

#[test]
fn nested_block_bindings_shadow_enclosing_mobile_names() {
    assert_source_program_exits_zero_and_reaches_amd64("tests/pack/uniform.zy");
}

#[test]
fn checked_computation_roots_link_directly_to_dynamics() {
    let SourceDynamics { program: arena, .. } = checked_trivial_computation().dynamics().unwrap();
    let mut input = std::io::empty();
    let mut output = Vec::new();
    let result = zydeco_dynamics::Runtime::new(&mut input, &mut output, &[], arena).run();

    assert!(matches!(
        result,
        zydeco_dynamics::ProgKont::Ret(zydeco_dynamics::syntax::SemValue::Triv(
            zydeco_syntax::Triv
        ))
    ));
}

#[test]
fn checked_computation_roots_lower_directly_to_stack_ir() {
    let SourceStack { stackir, scoped, .. } = checked_trivial_computation().stackir().unwrap();

    let stackir = stackir.as_program();
    assert!(stackir.arena.inner.compus.get(&stackir.root).is_some());
    zydeco_stackir::sps::check::check(stackir, &scoped);
}
