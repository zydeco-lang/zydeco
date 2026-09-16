use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
};

struct Fixture(tempfile::TempDir);

impl Fixture {
    fn new() -> Self {
        Self(tempfile::tempdir().unwrap())
    }
    fn write(&self, name: &str, source: &str) -> PathBuf {
        let path = self.0.path().join(name);
        std::fs::write(&path, source).unwrap();
        path
    }
    fn command(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(self.0.path())
            .args(args)
            .output()
            .unwrap()
    }
    fn success(&self, args: &[&str]) -> Output {
        let output = self.command(args);
        assert!(output.status.success(), "{args:?}: {}", String::from_utf8_lossy(&output.stderr));
        output
    }
    fn executable() -> String {
        let builtin = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../lib/std/builtin.zy")
            .canonicalize()
            .unwrap();
        format!("param (/process) : @(import({:?})) in ! process/exit 0", builtin.to_str().unwrap())
    }
}

#[test]
fn cli_selection_uses_lexical_absolute_and_parent_package_paths() {
    let f = Fixture::new();
    f.write("package.zy", "@[package(library, name(std))] (@[package(library, name(data))] (#answer = 42), @[package(library, name(client))] @(import(../data)))");
    for path in ["std/client", "/std/client", "./std/data/../client"] {
        f.success(&["check", "-p", path]);
    }
    let listing = f.success(&["show", "-p", "/std/client"]);
    assert!(String::from_utf8_lossy(&listing.stdout).contains("/std/client"));
    for path in ["/std/missing", "../std/client"] {
        let failed = f.command(&["check", "-p", path]);
        assert!(!failed.status.success());
        assert!(!String::from_utf8_lossy(&failed.stderr).contains("panicked"));
    }
    assert!(!f.command(&["show", "-p", "/std/missing"]).status.success());
}

#[test]
fn normalized_package_selection_builds_distinct_named_artifacts() {
    let f = Fixture::new();
    let executable = Fixture::executable();
    f.write("package.zy", &format!("(@[package(binary, name(tools/one))] ({executable}), @[package(binary, name(tools/two))] ({executable}))"));
    f.success(&["run", "-p", "/tools/one"]);
    f.success(&[
        "build",
        "-p",
        "/tools/one",
        "-p",
        "/tools/./two",
        "--target",
        "wasm-am",
        "-b",
        "build",
    ]);
    assert!(f.0.path().join("build/tools.one.am.wasm").exists());
    assert!(f.0.path().join("build/tools.two.am.wasm").exists());
}

#[test]
fn cli_conflicts_compare_resolved_contents_and_report_both_origins() {
    let f = Fixture::new();
    f.write("workspace.zy", "@[discover(include(\"a.zy\", \"b.zy\"))] ()");
    f.write("a.zy", "@[package(library, name(value))] 42");
    f.write("b.zy", "@[package(library, name(/./value))] 42");
    f.success(&["check", "-p", "/value"]);
    f.write("b.zy", "@[package(library, name(/value))] 43");
    let output = f.command(&["check", "-p", "/value"]);
    assert!(!output.status.success());
    let diagnostics = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostics.contains("a.zy") && diagnostics.contains("b.zy"), "{diagnostics}");
    assert!(diagnostics.contains("conflict"), "{diagnostics}");
}

#[test]
fn explicit_inspection_and_checking_select_packages_through_file_imports() {
    let f = Fixture::new();
    f.write("package.zy", "@(import(\"bridge.zy\"))");
    f.write("bridge.zy", "@(import(\"provided.zy\"))");
    f.write("provided.zy", "@[package(library, name(data))] 42");
    f.success(&["check", "-p", "/data"]);
    let listing = f.success(&["show", "-p", "/data"]);
    let text = String::from_utf8_lossy(&listing.stdout);
    assert!(text.contains("/data") && text.contains("provided.zy"), "{text}");
}
