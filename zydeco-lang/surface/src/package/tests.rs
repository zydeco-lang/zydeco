use std::path::PathBuf;

#[test]
fn test_std() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/std/proj.toml")
        .canonicalize()
        .unwrap();
    let proj = super::pack::Package::new(dir).unwrap();
    proj.run().unwrap();
}
