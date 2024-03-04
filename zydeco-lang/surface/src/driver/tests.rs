use std::path::PathBuf;

#[test]
fn test_std() {
    // let dir = std::env::current_dir().unwrap();
    let dir = PathBuf::from("../lib/std/proj.toml");
    let proj = super::proj::Project::new(dir).unwrap();
    proj.run().unwrap();
}
