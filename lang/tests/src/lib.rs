pub mod utils {
    use std::{
        collections::BTreeSet,
        path::{Path, PathBuf},
        process::Stdio,
    };
    use thiserror::Error;
    use walkdir::WalkDir;
    use zydeco_cli::{
        BuildOptions, CommandCompiler, CompileError, DiagnosticRenderer, NativeError,
        TargetArchitecture, TargetOs, WasmBackendKind,
    };
    use zydeco_session::{AnalysisError, DesugarError};
    use zydeco_statics::{TyckDiagnosticCode, syntax::TermAnnId};

    #[derive(Debug, Error)]
    pub enum CaseError {
        #[error(transparent)]
        Compile(#[from] CompileError),
        #[error(transparent)]
        Native(#[from] NativeError),
        #[error(transparent)]
        Io(#[from] std::io::Error),
        #[error("failed to start WebAssembly test host `{}`: {source}", executable.display())]
        WasmHostStart {
            executable: PathBuf,
            #[source]
            source: std::io::Error,
        },
    }

    impl CaseError {
        pub fn is_resolve_error(&self) -> bool {
            matches!(self, Self::Compile(CompileError::Analysis(AnalysisError::Resolve { .. })))
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum TestBackend {
        Interpreter,
        Amd64,
        WasmAm,
        WasmSps,
    }

    pub struct SourceProgram {
        path: PathBuf,
        arguments: Vec<String>,
        standard_input: Option<String>,
    }

    /// A source fixture whose root is checked without imposing the executable contract.
    pub struct SourceLibrary {
        path: PathBuf,
    }

    impl SourceLibrary {
        pub fn setup(relative: impl Into<PathBuf>) -> Self {
            Self { path: SourceProgram::resolve(relative.into()) }
        }

        pub fn check(self) {
            let compiler = CommandCompiler::default();
            let analysis = compiler.analyze(&self.path).unwrap_or_else(|error| {
                panic!("Error checking source {}: {error}", self.path.display())
            });
            let checked =
                compiler.checked_program(&analysis).expect("successful analysis must be checked");
            assert!(
                matches!(checked.root, TermAnnId::Value(_, _)),
                "Library source {} must export a value",
                self.path.display()
            );
        }
    }

    /// One executed program: the streams it produced and its exit status.
    struct TestRun {
        stdout: String,
        stderr: String,
        code: i32,
    }

    impl TestRun {
        fn from_output(output: std::process::Output) -> Self {
            Self {
                stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                code: output.status.code().unwrap_or(-1),
            }
        }
    }

    impl SourceProgram {
        pub fn setup(relative: impl Into<PathBuf>) -> Self {
            let path = Self::resolve(relative.into());
            Self { path, arguments: Vec::new(), standard_input: None }
        }

        fn resolve(relative: PathBuf) -> PathBuf {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../lib")
                .join(&relative)
                .canonicalize()
                .unwrap_or_else(|error| {
                    panic!("Error locating source {}: {error}", relative.display())
                })
        }

        pub fn with_args(mut self, arguments: impl IntoIterator<Item = impl Into<String>>) -> Self {
            self.arguments = arguments.into_iter().map(Into::into).collect();
            self
        }

        /// Feed the program this standard input instead of immediate EOF.
        pub fn with_stdin(mut self, input: impl Into<String>) -> Self {
            self.standard_input = Some(input.into());
            self
        }

        /// Run the program, requiring a zero exit status.
        pub fn test(self, backend: TestBackend) {
            self.assert_interaction(backend, None);
        }

        /// Run the program, asserting the exact output and exit status.
        pub fn test_io(self, backend: TestBackend, expected_output: &str, expected_code: i32) {
            self.assert_interaction(backend, Some((expected_output, expected_code)));
        }

        fn assert_interaction(self, backend: TestBackend, expected: Option<(&str, i32)>) {
            let fixture = self.path.display().to_string();
            let run = self.run(backend).unwrap_or_else(|error| {
                panic!("Error running source {fixture} with {backend:?}: {error}")
            });
            match expected {
                | None => assert_eq!(
                    run.code, 0,
                    "source {fixture} with {backend:?} exited with {}:\nstdout:\n{}\nstderr:\n{}",
                    run.code, run.stdout, run.stderr
                ),
                | Some((expected_output, expected_code)) => assert_eq!(
                    (&*run.stdout, run.code),
                    (expected_output, expected_code),
                    "source {fixture} with {backend:?} interacted unexpectedly:\nstderr:\n{}",
                    run.stderr
                ),
            }
        }

        fn run(&self, backend: TestBackend) -> Result<TestRun, CaseError> {
            match backend {
                | TestBackend::Interpreter => {
                    let stdin = self.standard_input.as_deref().unwrap_or("");
                    let interaction = CommandCompiler::default()
                        .test_io(&self.path, &self.arguments, stdin)
                        .map_err(CaseError::Compile)?;
                    Ok(TestRun {
                        stdout: interaction.output,
                        stderr: String::new(),
                        code: interaction.code,
                    })
                }
                | TestBackend::Amd64 => self.run_amd64(),
                | TestBackend::WasmAm => self.run_wasm(WasmBackendKind::AbstractMachine),
                | TestBackend::WasmSps => self.run_wasm(WasmBackendKind::SpsLow),
            }
        }

        /// Spawn a test child with EOF-or-declared stdin and captured streams;
        /// a test program never inherits the developer's terminal.
        fn execute(
            &self, mut command: std::process::Command,
        ) -> std::io::Result<std::process::Output> {
            use std::io::Write;
            command
                .stdin(if self.standard_input.is_some() { Stdio::piped() } else { Stdio::null() })
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());
            let mut child = command.spawn()?;
            if let Some(input) = &self.standard_input {
                let mut stdin = child.stdin.take().expect("stdin was configured piped");
                stdin.write_all(input.as_bytes())?;
            }
            child.wait_with_output()
        }

        fn run_amd64(&self) -> Result<TestRun, CaseError> {
            let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let build_directory = tempfile::tempdir()?;
            let operating_system =
                TargetOs::host().map_err(NativeError::UnsupportedHostOperatingSystem)?;
            let options = BuildOptions::new(
                build_directory.path().to_path_buf(),
                workspace.join("../../runtime"),
                TargetArchitecture::X86_64,
                operating_system,
            );
            let backend = CommandCompiler::default().lower(&self.path)?;
            let assembly = backend.emit_amd64(operating_system);
            let foreign_libraries = backend.foreign_libraries();
            let executable = options.link_amd64("test", &assembly, &foreign_libraries)?;
            let mut command = std::process::Command::new(executable.path());
            command.args(&self.arguments);
            let output = self.execute(command).map_err(CaseError::Io)?;
            Ok(TestRun::from_output(output))
        }

        fn run_wasm(&self, backend_kind: WasmBackendKind) -> Result<TestRun, CaseError> {
            let build_directory = tempfile::tempdir()?;
            let backend = CommandCompiler::default().lower(&self.path)?;
            let module = match backend_kind {
                | WasmBackendKind::AbstractMachine => backend.emit_wasm_am()?,
                | WasmBackendKind::SpsLow => backend.emit_wasm_sps()?,
            };
            let module_path = build_directory.path().join("test.wasm");
            std::fs::write(&module_path, module)?;

            let host = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("wasm-host.mjs");
            let node = PathBuf::from(std::env::var_os("NODE").unwrap_or_else(|| "node".into()));
            let mut command = std::process::Command::new(&node);
            command.arg(host).arg(&module_path).args(&self.arguments);
            let output = self
                .execute(command)
                .map_err(|source| CaseError::WasmHostStart { executable: node, source })?;
            Ok(TestRun::from_output(output))
        }
    }

    /// The standard-library basis a case source is wrapped with.
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub enum SourceCasePrelude {
        #[default]
        Core,
        Monadic,
    }

    /// A temporary root-source fixture with explicit core and host dependencies.
    pub struct SourceCase;

    impl SourceCase {
        pub fn check(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Core, source, |path| {
                CommandCompiler::default().analyze(path).map(|_| ()).map_err(CaseError::Compile)
            })
        }

        /// Check a source with the internal type lint enabled.
        ///
        /// The lint aborts on any internal inconsistency, so success here
        /// asserts that the finished arena satisfies the lint's invariants.
        pub fn check_linted(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Core, source, |path| {
                CommandCompiler::default()
                    .with_lint_types(true)
                    .analyze(path)
                    .map(|_| ())
                    .map_err(CaseError::Compile)
            })
        }

        /// The cloned finished arena of a checked source, with its root annotation.
        ///
        /// Mutation tests corrupt the clone and re-run the lint directly.
        pub fn checked_arena(
            source: &str,
        ) -> Result<
            (zydeco_statics::arena::StaticsArena, zydeco_statics::syntax::TermAnnId),
            CaseError,
        > {
            Self::with_source(SourceCasePrelude::Core, source, |path| {
                let compiler = CommandCompiler::default();
                let analysis = compiler.analyze(path).map_err(CaseError::Compile)?;
                let checked = compiler
                    .checked_program(&analysis)
                    .expect("successful analysis must be checked");
                Ok(((*checked.statics).clone(), checked.root))
            })
        }

        pub fn check_value(source: &str) -> Result<(), CaseError> {
            Self::check(&format!("let case_value = ({source}) in ret ()"))
        }

        pub fn check_monadic(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Monadic, source, |path| {
                CommandCompiler::default().analyze(path).map(|_| ()).map_err(CaseError::Compile)
            })
        }

        pub fn check_monadic_value(source: &str) -> Result<(), CaseError> {
            Self::check_monadic(&format!("let case_value = ({source}) in ret ()"))
        }

        pub fn check_with_import(source: &str, imported: &str) -> Result<(), CaseError> {
            let directory = tempfile::tempdir()?;
            std::fs::write(directory.path().join("imported.zy"), imported)?;
            let root = directory.path().join("case.zy");
            std::fs::write(&root, Self::wrap(SourceCasePrelude::Core, source))?;
            CommandCompiler::default().analyze(&root).map(|_| ()).map_err(CaseError::Compile)
        }

        pub fn run(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Core, source, |path| {
                CommandCompiler::default().test(path, &[]).map_err(CaseError::Compile)
            })
        }

        pub fn run_monadic(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Monadic, source, |path| {
                CommandCompiler::default().test(path, &[]).map_err(CaseError::Compile)
            })
        }

        /// Lower an inline source through the compiled-backend entry point,
        /// which keeps value functions second-class through SPS lowering.
        pub fn lower(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Core, source, |path| {
                CommandCompiler::default().lower(path).map(|_| ()).map_err(CaseError::Compile)
            })
        }

        /// Assert an accepted case, rendering its diagnostic on failure.
        pub fn assert_accepted(result: Result<(), CaseError>) {
            if let Err(CaseError::Compile(error)) = &result {
                DiagnosticRenderer::error(error);
            }
            result
                .unwrap_or_else(|error| panic!("expected the case to be accepted, found: {error}"));
        }

        /// Assert a rejected case carrying `expected` among its diagnostics.
        ///
        /// A rejection is only actionable when the expected code appears on a
        /// diagnostic that points into the source. Companion diagnostics may
        /// elaborate without a span of their own.
        pub fn assert_rejected(result: Result<(), CaseError>, expected: TyckDiagnosticCode) {
            let Err(CaseError::Compile(CompileError::Rejected(analysis))) = &result else {
                panic!("expected a rejection with `{expected}`, found: {result:?}")
            };
            let diagnostics = analysis.outcome().diagnostics().unwrap();
            let found = diagnostics
                .iter()
                .map(|diagnostic| format!("`{}`: {}", diagnostic.code, diagnostic.message))
                .collect::<Vec<_>>()
                .join(", ");
            assert!(
                diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.code == expected && diagnostic.primary.is_some()),
                "expected `{expected}` with a primary span, found: {found}"
            );
        }

        /// Assert a case rejected during desugaring, before checking runs.
        ///
        /// The predicate identifies the expected desugaring error variant;
        /// structural rejections from the surface phase have no diagnostic
        /// codes to match on.
        pub fn assert_desugar_error(
            result: Result<(), CaseError>, expected: impl FnOnce(&DesugarError) -> bool,
        ) {
            let Err(CaseError::Compile(CompileError::Analysis(AnalysisError::Desugar {
                error,
                ..
            }))) = &result
            else {
                panic!("expected a desugaring rejection, found: {result:?}")
            };
            assert!(expected(error.as_ref()), "unexpected desugaring rejection: {error:?}");
        }

        /// Assert a case that fails during resolution rather than checking.
        pub fn assert_resolve_error(result: Result<(), CaseError>) {
            match &result {
                | Err(error) if error.is_resolve_error() => {}
                | Ok(()) => panic!("expected a resolution error, but the program was accepted"),
                | Err(error) => panic!("expected a resolution error, found: {error:?}"),
            }
        }

        fn with_source<T>(
            prelude: SourceCasePrelude, source: &str,
            action: impl FnOnce(&std::path::Path) -> Result<T, CaseError>,
        ) -> Result<T, CaseError> {
            let directory = tempfile::tempdir()?;
            let path = directory.path().join("case.zy");
            std::fs::write(&path, Self::wrap(prelude, source))?;
            action(&path)
        }

        fn wrap(prelude: SourceCasePrelude, source: &str) -> String {
            let library = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
            let builtin = library.join("builtin.zy").canonicalize().unwrap();
            let monadic = match prelude {
                | SourceCasePrelude::Core => String::new(),
                | SourceCasePrelude::Monadic => {
                    let basis = library.join("control/monad.zy").canonicalize().unwrap();
                    format!(
                        concat!("let monadic_basis = @[import(\"{}\")] _ in\n",),
                        basis.display()
                    )
                }
            };
            let open_monadic = match prelude {
                | SourceCasePrelude::Core => String::new(),
                | SourceCasePrelude::Monadic => {
                    concat!("let (= Monad, = Algebra, ()) =\n", "  builtin |> monadic_basis in\n",)
                        .to_string()
                }
            };

            format!(
                r#"let Builtin = @[import("{builtin}")] _ in
{monadic}param (/core; /representations; /numeric; /system; builtin) : Builtin in
let (/VType; /CType; /Thk; /Ret; /Unit) = core in
let (/Int8) = representations/i8 in
let (/Int16) = representations/i16 in
let (/Int32) = representations/i32 in
let (/Int64) = representations/i64 in
let (/UInt8) = representations/u8 in
let (/UInt16) = representations/u16 in
let (/UInt32) = representations/u32 in
let (/UInt64) = representations/u64 in
let (/Float32) = representations/f32 in
let (/Float64) = representations/f64 in
let (/Char) = representations/char in
let (/String) = representations/string in
let (/Bytes) = representations/bytes in
let (#Int64 = NumericInt64, int64) = numeric/int64 in
let (/Reader; /Writer; /OS; /process) = system in
let Thunk = Thk in
let U = Thk in
let F = Ret in
{open_monadic}
let api = (#int64 = int64, #exit = process/exit) in
let exit = process/exit in
let Top : CType = codata end in
let triv : Thk Top = {{ comatch end }} in
{source}
"#,
                builtin = builtin.display(),
            )
        }
    }
    /// The pipeline stage a case fixture exercises.
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub enum CaseStage {
        /// Analyze the source; the default.
        #[default]
        Check,
        /// Analyze the source wrapped in a value binding.
        CheckValue,
        /// Execute the source on the reference interpreter.
        Run,
        /// Lower the source through the compiled-backend entry point.
        Lower,
    }

    /// The outcome a case fixture asserts.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum CaseExpectation {
        /// The directed stage accepts the source; the default.
        Accepted,
        /// Checking rejects the source with this diagnostic code.
        Rejected(TyckDiagnosticCode),
        /// Resolution fails before checking runs.
        ResolveError,
    }

    /// Directives parsed from the leading `--` comment lines of a case fixture.
    ///
    /// A fixture with no directives is checked against the core prelude and must
    /// be accepted. Malformed directives are errors, never silently skipped:
    /// a test whose stated expectation cannot be honored has no value.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct CaseDirective {
        pub stage: CaseStage,
        pub prelude: SourceCasePrelude,
        pub expectation: CaseExpectation,
    }

    /// A case fixture whose directives cannot be honored as written.
    #[derive(Debug, Error, PartialEq, Eq)]
    pub enum CaseDirectiveError {
        #[error("`{line}` is not a directive; the leading comment block must hold only directives")]
        NotADirective { line: String },
        #[error("duplicate directive `{key}`")]
        Duplicate { key: &'static str },
        #[error("unknown directive key `{key}`; expected `stage`, `prelude`, or `expect`")]
        UnknownKey { key: String },
        #[error("unknown `{key}` spelling `{value}`; expected one of {expected}")]
        UnknownValue { key: &'static str, value: String, expected: &'static str },
        #[error("malformed rejection `{text}`; expected `reject(<diagnostic code>)`")]
        MalformedRejection { text: String },
        #[error("unknown diagnostic code in `{text}`")]
        UnknownDiagnosticCode { text: String },
        #[error("stage `lower` supports only the core prelude")]
        LowerRequiresCore,
    }

    impl CaseDirective {
        /// Parse the leading directive block of a fixture source.
        ///
        /// Scanning skips blank lines, then consumes consecutive `--` lines.
        /// A `--|` documentation line or any source line ends the block, so
        /// directives must come first; every consumed line must be a directive.
        pub fn parse(source: &str) -> Result<Self, CaseDirectiveError> {
            let mut stage: Option<CaseStage> = None;
            let mut prelude: Option<SourceCasePrelude> = None;
            let mut expectation: Option<CaseExpectation> = None;

            for line in source.lines() {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                let Some(directive) = trimmed.strip_prefix("-- ") else { break };
                if directive.starts_with("|") {
                    break;
                }
                let (key, value) = directive
                    .split_once(':')
                    .ok_or_else(|| CaseDirectiveError::NotADirective { line: line.to_owned() })?;
                let key = key.trim();
                let value = value.trim();
                match key {
                    | "stage" => {
                        duplicate(stage.is_none(), "stage")?;
                        stage = Some(match value {
                            | "check" => CaseStage::Check,
                            | "check-value" => CaseStage::CheckValue,
                            | "run" => CaseStage::Run,
                            | "lower" => CaseStage::Lower,
                            | _ => {
                                return Err(CaseDirectiveError::UnknownValue {
                                    key: "stage",
                                    value: value.to_owned(),
                                    expected: "`check`, `check-value`, `run`, or `lower`",
                                });
                            }
                        });
                    }
                    | "prelude" => {
                        duplicate(prelude.is_none(), "prelude")?;
                        prelude = Some(match value {
                            | "core" => SourceCasePrelude::Core,
                            | "monadic" => SourceCasePrelude::Monadic,
                            | _ => {
                                return Err(CaseDirectiveError::UnknownValue {
                                    key: "prelude",
                                    value: value.to_owned(),
                                    expected: "`core` or `monadic`",
                                });
                            }
                        });
                    }
                    | "expect" => {
                        duplicate(expectation.is_none(), "expect")?;
                        expectation = Some(match value {
                            | "accepted" => CaseExpectation::Accepted,
                            | "resolve-error" => CaseExpectation::ResolveError,
                            | rejection if rejection.starts_with("reject(") => {
                                let code = rejection
                                    .strip_prefix("reject(")
                                    .and_then(|inner| inner.strip_suffix(')'))
                                    .ok_or_else(|| CaseDirectiveError::MalformedRejection {
                                        text: value.to_owned(),
                                    })?;
                                CaseExpectation::Rejected(code.parse().map_err(|_| {
                                    CaseDirectiveError::UnknownDiagnosticCode {
                                        text: value.to_owned(),
                                    }
                                })?)
                            }
                            | _ => {
                                return Err(CaseDirectiveError::UnknownValue {
                                    key: "expect",
                                    value: value.to_owned(),
                                    expected: "`accepted`, `resolve-error`, or `reject(<code>)`",
                                });
                            }
                        });
                    }
                    | _ => {
                        return Err(CaseDirectiveError::UnknownKey { key: key.to_owned() });
                    }
                }
            }

            let directive = Self {
                stage: stage.unwrap_or_default(),
                prelude: prelude.unwrap_or_default(),
                expectation: expectation.unwrap_or(CaseExpectation::Accepted),
            };
            if directive.stage == CaseStage::Lower
                && directive.prelude == SourceCasePrelude::Monadic
            {
                return Err(CaseDirectiveError::LowerRequiresCore);
            }
            Ok(directive)
        }

        /// Run the fixture source through the directed pipeline stage.
        pub fn run(self, source: &str) -> Result<(), CaseError> {
            match (self.stage, self.prelude) {
                | (CaseStage::Check, SourceCasePrelude::Core) => SourceCase::check(source),
                | (CaseStage::Check, SourceCasePrelude::Monadic) => {
                    SourceCase::check_monadic(source)
                }
                | (CaseStage::CheckValue, SourceCasePrelude::Core) => {
                    SourceCase::check_value(source)
                }
                | (CaseStage::CheckValue, SourceCasePrelude::Monadic) => {
                    SourceCase::check_monadic_value(source)
                }
                | (CaseStage::Run, SourceCasePrelude::Core) => SourceCase::run(source),
                | (CaseStage::Run, SourceCasePrelude::Monadic) => SourceCase::run_monadic(source),
                | (CaseStage::Lower, SourceCasePrelude::Core) => SourceCase::lower(source),
                | (CaseStage::Lower, SourceCasePrelude::Monadic) => {
                    unreachable!("directive parsing rejects the monadic prelude for `lower`")
                }
            }
        }

        /// Assert the directed outcome of running the fixture source.
        pub fn assert(self, result: Result<(), CaseError>) {
            match self.expectation {
                | CaseExpectation::Accepted => SourceCase::assert_accepted(result),
                | CaseExpectation::Rejected(code) => SourceCase::assert_rejected(result, code),
                | CaseExpectation::ResolveError => SourceCase::assert_resolve_error(result),
            }
        }
    }

    fn duplicate(fresh: bool, key: &'static str) -> Result<(), CaseDirectiveError> {
        fresh.then_some(()).ok_or(CaseDirectiveError::Duplicate { key })
    }

    /// Every `.zy` case fixture under `lang/tests/cases`, sorted by path.
    ///
    /// Discovery is dynamic: dropping a fixture file into the tree registers a
    /// test without any Rust change. Non-`.zy` files (such as a topic README)
    /// are ignored.
    pub fn case_fixtures() -> BTreeSet<PathBuf> {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("cases");
        let files = zy_files_below(&root);
        assert!(!files.is_empty(), "case fixture directory {} is empty", root.display());
        files
    }

    fn zy_files_below(root: &Path) -> BTreeSet<PathBuf> {
        WalkDir::new(root)
            .into_iter()
            .map(|entry| {
                entry.unwrap_or_else(|error| {
                    panic!("cannot walk case fixtures below {}: {error}", root.display())
                })
            })
            .filter(|entry| entry.path().extension().is_some_and(|extension| extension == "zy"))
            .map(|entry| entry.path().to_path_buf())
            .collect()
    }
}

#[macro_export]
macro_rules! check_source {
    ($name:ident, $source:expr) => {
        #[test]
        fn $name() {
            $crate::utils::SourceLibrary::setup($source).check();
        }
    };
}

#[macro_export]
macro_rules! runtime_source {
    ($name:ident, $source:expr) => {
        mod $name {
            $crate::__source_test!(interpreter, $source, $crate::utils::TestBackend::Interpreter);
            $crate::__source_test!(wasm_am, $source, $crate::utils::TestBackend::WasmAm);
            $crate::__source_test!(wasm_sps, $source, $crate::utils::TestBackend::WasmSps);
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __source_test {
    ($name:ident, $source:expr, $backend:expr) => {
        #[test]
        fn $name() {
            $crate::utils::SourceProgram::setup($source).test($backend);
        }
    };
}

#[macro_export]
macro_rules! e2e_sources {
    ({ $($name:ident => $source:expr),* $(,)? }) => {
        mod interpreter {
            $(
                $crate::__source_test!(
                    $name,
                    $source,
                    $crate::utils::TestBackend::Interpreter
                );
            )*
        }

        mod amd64 {
            $(
                $crate::__source_test!(
                    $name,
                    $source,
                    $crate::utils::TestBackend::Amd64
                );
            )*
        }

        mod wasm_am {
            $(
                $crate::__source_test!(
                    $name,
                    $source,
                    $crate::utils::TestBackend::WasmAm
                );
            )*
        }

        mod wasm_sps {
            $(
                $crate::__source_test!(
                    $name,
                    $source,
                    $crate::utils::TestBackend::WasmSps
                );
            )*
        }
    };
}

/// Register one whole program that reads standard input on every backend,
/// asserting the exact output and exit status; the status defaults to 0.
#[macro_export]
macro_rules! e2e_io_source {
    ($name:ident, $source:expr, $stdin:expr, $output:expr $(, $code:expr)?) => {
        mod $name {
            $crate::__source_io_test!(
                interpreter, $source, $stdin, $output $(, $code)?,
                $crate::utils::TestBackend::Interpreter
            );
            $crate::__source_io_test!(
                amd64, $source, $stdin, $output $(, $code)?,
                $crate::utils::TestBackend::Amd64
            );
            $crate::__source_io_test!(
                wasm_am, $source, $stdin, $output $(, $code)?,
                $crate::utils::TestBackend::WasmAm
            );
            $crate::__source_io_test!(
                wasm_sps, $source, $stdin, $output $(, $code)?,
                $crate::utils::TestBackend::WasmSps
            );
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __source_io_test {
    ($name:ident, $source:expr, $stdin:expr, $output:expr, $backend:expr) => {
        $crate::__source_io_test!($name, $source, $stdin, $output, 0, $backend);
    };
    ($name:ident, $source:expr, $stdin:expr, $output:expr, $code:expr, $backend:expr) => {
        #[test]
        fn $name() {
            $crate::utils::SourceProgram::setup($source)
                .with_stdin($stdin)
                .test_io($backend, $output, $code);
        }
    };
}

#[cfg(test)]
mod directive_tests {
    use crate::utils::{
        CaseDirective, CaseDirectiveError, CaseExpectation, CaseStage, SourceCasePrelude,
    };
    use zydeco_statics::TyckDiagnosticCode;

    fn parse(lines: &[&str]) -> Result<CaseDirective, CaseDirectiveError> {
        CaseDirective::parse(&lines.join("\n"))
    }

    #[test]
    fn sources_without_directives_default_to_core_check_acceptance() {
        let directive = parse(&["ret ()"]).unwrap();
        assert_eq!(directive.stage, CaseStage::Check);
        assert_eq!(directive.prelude, SourceCasePrelude::Core);
        assert_eq!(directive.expectation, CaseExpectation::Accepted);
    }

    #[test]
    fn parses_every_directive_and_stops_at_source() {
        let directive = parse(&[
            "-- stage: run",
            "-- prelude: monadic",
            "",
            "--| attached documentation",
            "do value <- ret 0;",
        ])
        .unwrap();
        assert_eq!(directive.stage, CaseStage::Run);
        assert_eq!(directive.prelude, SourceCasePrelude::Monadic);
        assert_eq!(directive.expectation, CaseExpectation::Accepted);
    }

    #[test]
    fn parses_rejections_with_their_diagnostic_code() {
        let directive = parse(&["-- expect: reject(tyck.coverage)", "match value end"]).unwrap();
        assert_eq!(directive.expectation, CaseExpectation::Rejected(TyckDiagnosticCode::Coverage));
    }

    #[test]
    fn rejects_malformed_leading_comment_lines() {
        assert_eq!(
            parse(&["-- a prose header", "-- stage: run"]),
            Err(CaseDirectiveError::NotADirective { line: "-- a prose header".into() })
        );
        assert_eq!(
            parse(&["-- stage: fly"]),
            Err(CaseDirectiveError::UnknownValue {
                key: "stage",
                value: "fly".into(),
                expected: "`check`, `check-value`, `run`, or `lower`",
            })
        );
        assert_eq!(
            parse(&["-- tempo: fast"]),
            Err(CaseDirectiveError::UnknownKey { key: "tempo".into() })
        );
        assert_eq!(
            parse(&["-- stage: run", "-- stage: check"]),
            Err(CaseDirectiveError::Duplicate { key: "stage" })
        );
    }

    #[test]
    fn rejects_unknown_or_malformed_rejection_codes() {
        assert_eq!(
            parse(&["-- expect: reject(tyck.nonexistent)"]),
            Err(CaseDirectiveError::UnknownDiagnosticCode {
                text: "reject(tyck.nonexistent)".into()
            })
        );
        assert_eq!(
            parse(&["-- expect: reject coverage"]),
            Err(CaseDirectiveError::UnknownValue {
                key: "expect",
                value: "reject coverage".into(),
                expected: "`accepted`, `resolve-error`, or `reject(<code>)`",
            })
        );
    }

    #[test]
    fn rejects_the_monadic_prelude_for_lowering() {
        assert_eq!(
            parse(&["-- stage: lower", "-- prelude: monadic"]),
            Err(CaseDirectiveError::LowerRequiresCore)
        );
    }
}
