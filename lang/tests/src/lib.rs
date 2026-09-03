pub mod utils {
    use std::{path::PathBuf, process::Stdio};
    use thiserror::Error;
    use zydeco_cli::{
        BuildOptions, CommandCompiler, CompileError, NativeError, TargetArchitecture, TargetOs,
        WasmBackendKind,
    };
    use zydeco_session::AnalysisError;
    use zydeco_statics::syntax::TermAnnId;

    #[derive(Debug, Error)]
    pub enum CaseError {
        #[error(transparent)]
        Compile(#[from] CompileError),
        #[error(transparent)]
        Native(#[from] NativeError),
        #[error(transparent)]
        Io(#[from] std::io::Error),
        #[error("native source test exited with {0}")]
        NativeExit(std::process::ExitStatus),
        #[error("failed to start WebAssembly test host `{}`: {source}", executable.display())]
        WasmHostStart {
            executable: PathBuf,
            #[source]
            source: std::io::Error,
        },
        #[error(
            "{backend:?} WebAssembly source test exited with {status}\nstdout:\n{stdout}\nstderr:\n{stderr}"
        )]
        WasmExit {
            backend: WasmBackendKind,
            status: std::process::ExitStatus,
            stdout: String,
            stderr: String,
        },
    }

    impl CaseError {
        pub fn is_type_error(&self) -> bool {
            matches!(self, Self::Compile(CompileError::Rejected(_)))
        }

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

    impl SourceProgram {
        pub fn setup(relative: impl Into<PathBuf>) -> Self {
            let path = Self::resolve(relative.into());
            Self { path, arguments: Vec::new() }
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

        pub fn test(self, backend: TestBackend) {
            let result = match backend {
                | TestBackend::Interpreter => CommandCompiler::default()
                    .test(&self.path, &self.arguments)
                    .map_err(CaseError::Compile),
                | TestBackend::Amd64 => self.test_amd64(),
                | TestBackend::WasmAm => self.test_wasm(WasmBackendKind::AbstractMachine),
                | TestBackend::WasmSps => self.test_wasm(WasmBackendKind::SpsLow),
            };
            if let Err(error) = result {
                panic!("Error running source {} with {backend:?}: {error}", self.path.display());
            }
        }

        fn test_amd64(&self) -> Result<(), CaseError> {
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
            let status = executable.run(&self.arguments)?;
            if status.success() { Ok(()) } else { Err(CaseError::NativeExit(status)) }
        }

        fn test_wasm(&self, backend_kind: WasmBackendKind) -> Result<(), CaseError> {
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
            let output = std::process::Command::new(&node)
                .arg(host)
                .arg(module_path)
                .args(&self.arguments)
                .stdin(Stdio::null())
                .output()
                .map_err(|source| CaseError::WasmHostStart { executable: node, source })?;
            if output.status.success() {
                Ok(())
            } else {
                Err(CaseError::WasmExit {
                    backend: backend_kind,
                    status: output.status,
                    stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
                    stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                })
            }
        }
    }

    #[derive(Clone, Copy)]
    enum SourceCasePrelude {
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
            Self::check(&format!("ret ({source})"))
        }

        pub fn check_monadic(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Monadic, source, |path| {
                CommandCompiler::default().analyze(path).map(|_| ()).map_err(CaseError::Compile)
            })
        }

        pub fn check_monadic_value(source: &str) -> Result<(), CaseError> {
            Self::check_monadic(&format!("ret ({source})"))
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
