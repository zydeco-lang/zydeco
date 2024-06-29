use std::path::PathBuf;

#[test]
fn std() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/std/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = super::pack::LocalPackage::new(dir).unwrap();
    match proj.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}

#[test]
fn exec() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/exec/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = super::pack::LocalPackage::new(dir).unwrap();
    match proj.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}

#[test]
fn icfp() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/icfp/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = super::pack::LocalPackage::new(dir).unwrap();
    match proj.run() {
        | Ok(_) => {}
        | Err(err) => {
            eprintln!("{}", err);
            panic!("Error running project");
        }
    }
}
