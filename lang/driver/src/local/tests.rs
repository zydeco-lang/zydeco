use std::path::PathBuf;

macro_rules! lib_proj_toml {
    ($name:ident) => {
        #[test]
        fn $name() {
            let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../lib")
                .join(stringify!($name))
                .join("proj.toml")
                .canonicalize()
                .unwrap();
            let proj = super::pack::LocalPackage::new(dir).unwrap();
            match proj.test() {
                | Ok(_) => {}
                | Err(err) => {
                    eprintln!("{}", err);
                    panic!("Error running project");
                }
            }
        }
    };
}

lib_proj_toml!(std);
lib_proj_toml!(exec);
lib_proj_toml!(icfp);
lib_proj_toml!(monadic);
