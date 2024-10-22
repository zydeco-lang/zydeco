use zydeco_driver::*;

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
    let pack = build_sys.add_local_package(dir).unwrap();
    match build_sys.run_pack(pack, false, false) {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}
