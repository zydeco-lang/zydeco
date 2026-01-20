pub mod utils {
    use std::path::PathBuf;
    use zydeco_driver::{BuildConf, BuildSystem, Driver};

    pub fn interp_proj_bin_aux(proj_path: impl Into<PathBuf>, bin_path: impl Into<PathBuf>) {
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

    pub fn amd64_proj_bin_aux(proj_path: impl Into<PathBuf>, bin_path: impl Into<PathBuf>) {
        let Driver { mut build_sys } = Driver::setup(vec![proj_path.into()]).unwrap();
        let name = bin_path.into().file_stem().unwrap().to_string_lossy().to_string();
        let pack = build_sys.pick_marked(Some(name.clone())).unwrap();
        let dir = ::std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let build_dir = tempfile::tempdir().unwrap().keep();
        println!("build_dir: {}", build_dir.display());
        let build_conf = BuildConf {
            build_dir,
            runtime_dir: dir.join("../../runtime"),
            target_arch: "x86_64".to_string(),
            target_os: std::env::consts::OS.to_string(),
            link_existing: false,
        };
        build_sys.build_confs.insert(pack, build_conf);
        match build_sys.test_amd64_pack(pack, true) {
            | Ok(_) => {}
            | Err(err) => {
                eprintln!("{}", err);
                panic!("Error running project");
            }
        }
    }
}

#[macro_export]
macro_rules! __proj_path {
    ($proj:ident, $name:ident, $binary:expr) => {{
        let dir = ::std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
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
        (local, bin)
    }};
}

#[macro_export]
macro_rules! interp_proj_bin {
    ($proj:ident, $name:ident, $binary:expr) => {
        #[test]
        fn $name() {
            let (local, bin) = ::zydeco_tests::__proj_path!($proj, $name, $binary);
            ::zydeco_tests::utils::interp_proj_bin_aux(local, bin);
        }
    };
}

#[macro_export]
macro_rules! amd64_proj_bin {
    ($proj:ident, $name:ident, $binary:expr) => {
        #[test]
        fn $name() {
            let (local, bin) = ::zydeco_tests::__proj_path!($proj, $name, $binary);
            ::zydeco_tests::utils::amd64_proj_bin_aux(local, bin);
        }
    };
}
