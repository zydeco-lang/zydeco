pub mod utils {
    use std::path::PathBuf;
    use zydeco_driver::{BuildConf, BuildSystem, Driver, PackId, Verbosity};

    #[derive(Clone, Copy, Debug)]
    pub enum TestBackend {
        Interpreter,
        Amd64,
    }

    pub struct ProjectBinary {
        build_sys: BuildSystem,
        pack: PackId,
    }

    impl ProjectBinary {
        pub fn setup(proj_path: impl Into<PathBuf>, binary_name: impl Into<String>) -> Self {
            let proj_path = proj_path.into();
            let binary_name = binary_name.into();
            let Driver { build_sys } =
                Driver::setup(vec![proj_path.clone()]).unwrap_or_else(|err| {
                    panic!("Error loading project {}: {err}", proj_path.display())
                });
            let pack = build_sys.pick_marked(Some(binary_name.clone())).unwrap_or_else(|err| {
                panic!(
                    "Error selecting binary {binary_name:?} from project {}: {err}",
                    proj_path.display()
                )
            });
            Self { build_sys, pack }
        }

        pub fn test(mut self, backend: TestBackend) {
            let result = match backend {
                | TestBackend::Interpreter => self.build_sys.test_pack(self.pack, false),
                | TestBackend::Amd64 => {
                    self.configure_amd64();
                    self.build_sys.test_amd64_pack(self.pack, Verbosity::new(3))
                }
            };
            match result {
                | Ok(()) => {}
                | Err(err) => {
                    eprintln!("{err}");
                    panic!("Error running project with {backend:?}");
                }
            }
        }

        fn configure_amd64(&mut self) {
            let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let build_dir = tempfile::tempdir().unwrap().keep();
            println!("build_dir: {}", build_dir.display());
            let build_conf = BuildConf {
                build_dir,
                runtime_dir: dir.join("../../runtime"),
                target_arch: "x86_64".to_string(),
                target_os: std::env::consts::OS.to_string(),
                link_existing: false,
            };
            self.build_sys.build_confs.insert_new(self.pack, build_conf);
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __proj_bin_test {
    ($($proj:ident)/+, $name:ident, $binary:expr, $backend:expr) => {
        #[test]
        fn $name() {
            let local = ::std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../lib")
                $(.join(stringify!($proj)))+
                .join("proj.toml")
                .canonicalize()
                .unwrap();
            $crate::utils::ProjectBinary::setup(local, $binary).test($backend);
        }
    };
}

#[macro_export]
macro_rules! interp_proj_bin {
    ($($proj:ident)/+, $name:ident, $binary:expr) => {
        $crate::__proj_bin_test!(
            $($proj)/+,
            $name,
            $binary,
            $crate::utils::TestBackend::Interpreter
        );
    };
}

#[macro_export]
macro_rules! amd64_proj_bin {
    ($($proj:ident)/+, $name:ident, $binary:expr) => {
        $crate::__proj_bin_test!(
            $($proj)/+,
            $name,
            $binary,
            $crate::utils::TestBackend::Amd64
        );
    };
}

#[macro_export]
macro_rules! e2e_proj_bins {
    ($($proj:ident)/+, { $($name:ident => $binary:expr),* $(,)? }) => {
        $crate::__e2e_proj_bins!(
            [$($proj)/+],
            { $($name => $binary),* }
        );
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __e2e_proj_bins {
    ($proj:tt, { $($name:ident => $binary:expr),* $(,)? }) => {
        mod interpreter {
            $(
                $crate::__e2e_proj_bin!($proj, $name, $binary, Interpreter);
            )*
        }

        mod amd64 {
            $(
                $crate::__e2e_proj_bin!($proj, $name, $binary, Amd64);
            )*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __e2e_proj_bin {
    ([$($proj:ident)/+], $name:ident, $binary:expr, $backend:ident) => {
        $crate::__proj_bin_test!(
            $($proj)/+,
            $name,
            $binary,
            $crate::utils::TestBackend::$backend
        );
    };
}
