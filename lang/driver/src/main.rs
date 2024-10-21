use zydeco_driver::{BuildSystem, Package};

fn main() {
    env_logger::init();

    let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        // .join("../lib/icfp/proj.toml")
        // .join("../lib/exec/proj.toml")
        .join("../lib/exec/alg.toml")
        // .join("../lib/std/proj.toml")
        .canonicalize()
        .unwrap();
    let mut build_sys = BuildSystem::new();
    let pack_id = build_sys.add_local_package(dir).unwrap();
    let Package::Local(pack) = &build_sys.packages[&pack_id] else { unreachable!() };
    match pack.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}
