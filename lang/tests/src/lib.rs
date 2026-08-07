pub mod utils {
    use std::path::PathBuf;
    use thiserror::Error;
    use zydeco_cli::{
        BuildOptions, CommandCompiler, CompileError, NativeError, TargetArchitecture, TargetOs,
    };
    use zydeco_session::AnalysisError;
    use zydeco_stackir::CpsMode;
    use zydeco_statics::syntax::{Fillable, TermAnnId, Type};

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
            let analysis = CommandCompiler::default().analyze(&self.path).unwrap_or_else(|error| {
                panic!("Error checking source {}: {error}", self.path.display())
            });
            let checked = analysis.checked_program().expect("successful analysis must be checked");
            let TermAnnId::Value(_, root_type) = checked.root else {
                panic!("Library source {} must export a value", self.path.display());
            };
            assert!(
                matches!(checked.statics.types_pre[&root_type], Fillable::Done(Type::VPackPi(_))),
                "Library source {} must export a pure package function",
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
            let backend = CommandCompiler::default().lower(&self.path, CpsMode::Enabled)?;
            let assembly = backend.emit_amd64(operating_system);
            let executable = options.link_amd64("test", &assembly)?;
            let status = executable.run(&self.arguments)?;
            if status.success() { Ok(()) } else { Err(CaseError::NativeExit(status)) }
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

        pub fn check_monadic(source: &str) -> Result<(), CaseError> {
            Self::with_source(SourceCasePrelude::Monadic, source, |path| {
                CommandCompiler::default().analyze(path).map(|_| ()).map_err(CaseError::Compile)
            })
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
                    let basis = library.join("monad.zy").canonicalize().unwrap();
                    format!(
                        concat!("let monadic_basis = @[import(\"{}\")] _ in\n",),
                        basis.display()
                    )
                }
            };
            let open_monadic = match prelude {
                | SourceCasePrelude::Core => String::new(),
                | SourceCasePrelude::Monadic => concat!(
                    "let (= Monad, = Algebra, ()) =\n",
                    "  monadic_basis ",
                    concat!(
                        "(VType, CType, Thk, Ret, Unit, Int, Float, Char, String, ",
                        "Bytes, Reader, Writer, OS, api) in\n",
                    ),
                )
                .to_string(),
            };

            format!(
                r#"let Builtin = @[import("{builtin}")] _ in
{monadic}param (
  (VType, CType, Thk, Ret, Unit, Int, Float, Char, String, Bytes, Reader, Writer, OS, api) :
  Builtin
) in
let Thunk = Thk in
let U = Thk in
let F = Ret in
{open_monadic}
let exit = api/exit in
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
macro_rules! interp_source {
    ($name:ident, $source:expr) => {
        $crate::__source_test!($name, $source, $crate::utils::TestBackend::Interpreter);
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
    };
}
