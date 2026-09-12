use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
};

struct Fixture {
    directory: tempfile::TempDir,
}
impl Fixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }
    fn write(&self, path: &str, source: &str) -> PathBuf {
        let path = self.directory.path().join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, source).unwrap();
        path
    }
    fn executable(code: i32) -> String {
        let builtin = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../lib/std/builtin.zy")
            .canonicalize()
            .unwrap();
        format!(
            "param (/process) : @(import({:?})) in ! process/exit {code}",
            builtin.to_str().unwrap()
        )
    }
    fn command(&self, arguments: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(self.directory.path())
            .args(arguments)
            .output()
            .unwrap()
    }
    fn success(&self, arguments: &[&str]) -> Output {
        let output = self.command(arguments);
        assert!(
            output.status.success(),
            "{arguments:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }
}

#[test]
fn declared_discovery_selects_test_side_subjects_and_keeps_plain_tests_independent() {
    let fixture = Fixture::new();
    fixture.write("packages.zy", r#"@[discover(include("tests/**/*.zy"), exclude("tests/fixtures/**"), include("tests/fixtures/keep.zy"))]
        (#lib = @[package(library, name("lib"))] 1)"#);
    fixture.write(
        "tests/smoke.zy",
        &format!(
            r#"@[package(test(of("../packages.zy#lib")))]
        let lib = @(import("../packages.zy#lib")) in ({})"#,
            Fixture::executable(0)
        ),
    );
    fixture.write(
        "tests/fixtures/keep.zy",
        &format!(r#"@[package(test(of("../../packages.zy#lib")))] ({})"#, Fixture::executable(0)),
    );
    fixture.write("tests/fixtures/invalid.zy", "(");
    fixture.write("tests/plain.zy", &format!("@[package(test)] ({})", Fixture::executable(7)));
    let show = fixture.success(&["package", "show", "packages.zy", "tests/smoke.zy"]);
    let show = String::from_utf8_lossy(&show.stdout);
    assert!(show.contains("of ->") && show.contains("tests/plain.zy"));
    assert_eq!(
        show.matches("tests/smoke.zy").count(),
        1,
        "overlapping scopes display each package once"
    );
    assert!(!show.contains("invalid.zy") && !show.contains("unsupported"));
    let test = fixture.success(&["package", "test", "packages.zy#lib"]);
    let stdout = String::from_utf8_lossy(&test.stdout);
    assert!(stdout.contains("2 passed; 0 failed."));
    assert!(!stdout.contains("plain.zy"));
    let plain = fixture.command(&["package", "test", "tests/plain.zy"]);
    assert_eq!(plain.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&plain.stdout).contains("plain.zy (exit 7)"));

    fixture.write("tests/invalid.zy", "(");
    fixture.success(&["package", "check", "packages.zy#lib"]);
    for command in ["show", "test"] {
        let source = if command == "show" { "packages.zy" } else { "packages.zy#lib" };
        let output = fixture.command(&["package", command, source]);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("tests/invalid.zy"));
        assert!(output.stdout.is_empty(), "discovery must finish before output or execution");
    }
    fixture.write("tests/invalid.zy", r#"@[package(test(of("../packages.zy#lib")))] 1"#);
    let output = fixture.command(&["package", "test", "packages.zy#lib"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("classified as a value"));
    assert!(
        !String::from_utf8_lossy(&output.stdout).contains("PASS"),
        "all selected tests are preflighted"
    );
}

#[test]
fn concluding_files_register_existing_files_and_named_binaries_build_distinct_artifacts() {
    let fixture = Fixture::new();
    fixture.write("lib.zy", "1");
    fixture.write("first.zy", &Fixture::executable(3));
    fixture.write("second.zy", &Fixture::executable(9));
    fixture.write(
        "packages.zy",
        r#"let library = @[package(library, name("lib"), test("testing.zy#smoke"))] @(import("lib.zy")) in
        let one = { @[package(binary, name("first"))] @(import("first.zy")) } in
        let two = { @[package(binary, name("second"))] @(import("second.zy")) } in
        let broken = { @[package(binary, name("broken"))] @(import("missing.zy")) } in
        (library, one, two, broken)"#,
    );
    fixture.write("testing.zy", r#"{ @[package(test, name("smoke"))] @(import("smoke.zy")) }"#);
    fixture.write(
        "smoke.zy",
        &format!(r#"let lib = @(import("packages.zy#lib")) in ({})"#, Fixture::executable(0)),
    );
    let show = fixture.success(&["package", "show", "packages.zy", "testing.zy"]);
    let show = String::from_utf8(show.stdout).unwrap();
    assert!(show.contains("packages.zy#lib") && show.contains("testing.zy#smoke"));
    assert!(show.contains("binary ") && show.contains("code ->"));
    fixture.success(&["package", "check", "packages.zy#lib"]);
    fixture.success(&["check", "packages.zy#lib"]);
    let tests = fixture.success(&["package", "test", "packages.zy#lib"]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));
    assert_eq!(fixture.command(&["run", "packages.zy#first"]).status.code(), Some(3));
    assert_eq!(fixture.command(&["run", "packages.zy#second"]).status.code(), Some(9));
    for name in ["first", "second"] {
        fixture.success(&[
            "build",
            &format!("packages.zy#{name}"),
            "--target",
            "wasm-sps",
            "--build-dir",
            "build",
        ]);
        let wasm =
            std::fs::read(fixture.directory.path().join(format!("build/{name}.sps.wasm"))).unwrap();
        wasmparser::Validator::new().validate_all(&wasm).unwrap();
    }
    let wrong = fixture.command(&["run", "packages.zy#lib"]);
    assert!(!wrong.status.success());
    assert!(String::from_utf8_lossy(&wrong.stderr).contains("has role library; expected binary"));
    let invalid = fixture.command(&["run", "missing.zy#../bad"]);
    assert!(!invalid.status.success());
    assert!(String::from_utf8_lossy(&invalid.stderr).contains("invalid package name"));
}

#[test]
fn whole_files_need_no_names_and_library_tests_import_the_library_normally() {
    let fixture = Fixture::new();
    fixture.write("plain.zy", "42");
    let plain = fixture.success(&["package", "show", "plain.zy"]);
    assert!(String::from_utf8_lossy(&plain.stdout).contains("library "));
    fixture.success(&["package", "check", "plain.zy"]);
    fixture.write("lib.zy", r#"@[package(library, test("smoke.zy"))] 42"#);
    fixture.write(
        "smoke.zy",
        &format!(
            r#"@[package(test)] let lib = @(import("lib.zy")) in ({})"#,
            Fixture::executable(0)
        ),
    );
    fixture.success(&["package", "check", "lib.zy"]);
    for source in ["lib.zy", "smoke.zy"] {
        let tests = fixture.success(&["package", "test", source]);
        assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));
    }
    fixture.write("main.zy", &format!("@[package(binary)] ({})", Fixture::executable(0)));
    fixture.success(&["package", "check", "main.zy"]);
    fixture.success(&["run", "main.zy"]);
    fixture.success(&["build", "main.zy", "--target", "wasm-sps", "--build-dir", "build"]);
    let wasm = std::fs::read(fixture.directory.path().join("build/main.sps.wasm")).unwrap();
    wasmparser::Validator::new().validate_all(&wasm).unwrap();
    for source in ["plain.zy#", "main.zy#"] {
        let output = fixture.command(&["package", "check", source]);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("invalid package name"));
        assert!(output.stdout.is_empty());
    }
}

#[test]
fn nested_package_annotations_preserve_normal_execution_and_scope() {
    let fixture = Fixture::new();
    let source = Fixture::executable(7)
        .replace("! process/exit", r#"@[package(test, name("nested"))] ! process/exit"#);
    fixture.write("main.zy", &format!("@[package(binary)] {source}"));
    fixture.success(&["package", "check", "main.zy"]);
    assert_eq!(fixture.command(&["run", "main.zy"]).status.code(), Some(7));
    let separate = fixture.command(&["package", "check", "main.zy#nested"]);
    assert!(
        !separate.status.success(),
        "an independently selected term must supply its own imports"
    );
    assert!(String::from_utf8_lossy(&separate.stderr).contains("Unbound variable"));
    assert!(separate.stdout.is_empty());
}

#[test]
fn inline_same_file_tests_are_not_cyclic_and_unrelated_packages_are_not_loaded() {
    let fixture = Fixture::new();
    fixture.write(
        "packages.zy",
        &format!(
            r#"(
        #lib = @[package(library, test("packages.zy#smoke"), name("lib"))] 1,
        #smoke = @[package(test, name("smoke"))] let lib = @(import("packages.zy#lib")) in ({}),
        #unrelated = @[package(test, name("unrelated"))] ({}),
        #broken = @[package(binary, name("broken"))] missing
    )"#,
            Fixture::executable(0),
            Fixture::executable(8)
        ),
    );
    fixture.success(&["package", "check", "packages.zy#lib"]);
    let output = fixture.success(&["package", "test", "packages.zy#lib"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("packages.zy#smoke") && stdout.contains("1 passed; 0 failed."));
    assert!(!stdout.contains("unrelated") && !stdout.contains("broken"));
}

#[test]
fn missing_and_unsupported_relationships_are_visible_but_fail_test_planning() {
    let fixture = Fixture::new();
    for kind in ["test", "tset"] {
        fixture.write("lib.zy", &format!(r#"@[package(library, {kind}("missing.zy"))] 42"#));
        let show = fixture.success(&["package", "show", "lib.zy"]);
        let show = String::from_utf8_lossy(&show.stdout);
        assert!(show.contains(&format!("{kind} ->")));
        if kind == "tset" {
            assert!(show.contains("[unsupported]"));
        }
        fixture.success(&["package", "check", "lib.zy"]);
        let test = fixture.command(&["package", "test", "lib.zy"]);
        assert!(!test.status.success() && test.stdout.is_empty());
        let stderr = String::from_utf8_lossy(&test.stderr);
        assert!(if kind == "test" {
            stderr.contains("cannot read source")
        } else {
            stderr.contains("unsupported relationship kind") && stderr.contains("tset")
        });
    }
}

#[test]
fn preflight_checks_every_role_and_executable_before_running_any_test() {
    let fixture = Fixture::new();
    fixture.write(
        "lib.zy",
        r#"@[package(library, test("tests.zy#a_valid"), test("tests.zy#z_invalid"))] 42"#,
    );
    for (role, term, expected) in [
        ("test", "42", "classified as a value"),
        ("binary", "42", "has role binary; expected test"),
        ("test", "missing", "Unbound variable"),
    ] {
        fixture.write(
            "tests.zy",
            &format!(
                r#"(
            #a_valid = @[package(test, name("a_valid"))] ({}),
            #z_invalid = @[package({role}, name("z_invalid"))] {term}
        )"#,
                Fixture::executable(0)
            ),
        );
        let output = fixture.command(&["package", "test", "lib.zy"]);
        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains(expected),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(!String::from_utf8_lossy(&output.stdout).contains("PASS"));
    }
}

#[test]
fn nonzero_tests_are_reported_and_the_selected_suite_continues() {
    let fixture = Fixture::new();
    fixture
        .write("lib.zy", r#"@[package(library, test("tests.zy#fail"), test("tests.zy#pass"))] ()"#);
    fixture.write(
        "tests.zy",
        &format!(
            r#"(
        #fail = @[package(test, name("fail"))] ({}),
        #pass = @[package(test, name("pass"))] ({})
    )"#,
            Fixture::executable(7),
            Fixture::executable(0)
        ),
    );
    let output = fixture.command(&["package", "test", "lib.zy"]);
    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("tests.zy#fail (exit 7)"));
    assert!(stdout.contains("tests.zy#pass"));
    assert!(stdout.contains("1 passed; 1 failed."));
}

#[test]
fn obsolete_forms_duplicate_names_and_implicit_captures_are_rejected() {
    let fixture = Fixture::new();
    for (source, expected) in [
        (r#"@[package(library("lib", "lib.zy"))] ()"#, "first argument"),
        (r#"@[package(library("lib"))] ()"#, "first argument"),
        (
            r#"(#lib = @[package(library, name("lib"))] 1, #lib = @[package(test, name("lib"))] ())"#,
            "duplicate package name",
        ),
    ] {
        fixture.write("packages.zy", source);
        let output = fixture.command(&["package", "show", "packages.zy"]);
        assert!(!output.status.success(), "{source}");
        assert!(String::from_utf8_lossy(&output.stderr).contains(expected));
        assert!(output.stdout.is_empty());
    }
    fixture.write(
        "packages.zy",
        "let outside = 42 in (#lib = @[package(library, name(\"lib\"))] outside)",
    );
    fixture.success(&["check", "packages.zy"]);
    let output = fixture.command(&["package", "check", "packages.zy#lib"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Unbound variable"));
    fixture.write("old-import.zy", r#"@(import(package("packages.zy", "lib")))"#);
    assert!(!fixture.command(&["check", "old-import.zy"]).status.success());
    assert!(!fixture.command(&["run", "packages.zy", "--binary", "lib"]).status.success());
}

#[test]
fn checked_in_file_packages_check_run_and_test_without_selectors() {
    let fixture = Fixture::new();
    let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../docs/examples/packages");
    let library = directory.join("library.zy").to_str().unwrap().to_owned();
    let binary = directory.join("main.zy").to_str().unwrap().to_owned();
    let show = fixture.success(&["package", "show", &library, &binary]);
    let show = String::from_utf8(show.stdout).unwrap();
    assert!(show.contains("library ") && show.contains("binary "));
    assert!(!show.contains('#'), "whole-file addresses never acquire a selector");
    fixture.success(&["package", "check", &library]);
    fixture.success(&["package", "check", &binary]);
    let run = fixture.success(&["run", &binary]);
    assert_eq!(String::from_utf8(run.stdout).unwrap(), "hello from a package\n");
    let tests = fixture.success(&["package", "test", &library]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("tests/smoke.zy"));
}
