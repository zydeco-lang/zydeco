fn main() {
    let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/exec/proj.toml")
        // .join("../lib/std/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = zydeco_driver::package::pack::Package::new(dir).unwrap();
    match proj.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}
