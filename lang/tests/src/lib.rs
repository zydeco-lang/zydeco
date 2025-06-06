pub mod utils {
    use std::path::PathBuf;
    use zydeco_driver::BuildSystem;

    pub fn wrapper_proj_bin(proj_path: impl Into<PathBuf>, bin_path: impl Into<PathBuf>) {
        let mut build_sys = BuildSystem::new();
        build_sys.add_local_package(proj_path).unwrap();
        let pack = build_sys.add_orphan_file(bin_path).unwrap();
        match build_sys.test_pack(pack, false) {
            | Ok(_) => {}
            | Err(err) => {
                eprintln!("{}", err);
                panic!("Error running project");
            }
        }
    }
}

#[macro_export]
macro_rules! lib_proj_bin {
    ($proj:ident, $name:ident, $binary:expr) => {
        #[test]
        fn $name() {
            let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../lib")
                .join(stringify!($proj));
            let local = dir.join("proj.toml").canonicalize().unwrap();
            let bin = {
                let bin = dir.join(format!("{}", $binary));
                let zy = dir.join(format!("{}.zy", $binary));
                let zydeco = dir.join(format!("{}.zydeco", $binary));
                if bin.exists() {
                    bin
                } else if zy.exists() {
                    zy
                } else if zydeco.exists() {
                    zydeco
                } else {
                    panic!("No zydeco file found for {}", $binary);
                }
            };

            ::zydeco_tests::utils::wrapper_proj_bin(local, bin);
        }
    };
}
