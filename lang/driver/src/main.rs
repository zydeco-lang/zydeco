fn main() {
    let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        // .join("../lib/icfp/proj.toml")
        // .join("../lib/exec/proj.toml")
        .join("../lib/exec/alg.toml")
        // .join("../lib/std/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = zydeco_driver::local::pack::LocalPackage::new(dir).unwrap();
    match proj.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}
