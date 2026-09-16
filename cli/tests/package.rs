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
    fn with_discovery(self, files: &[&str]) -> Self {
        let files = files.iter().map(|file| format!("{file:?}")).collect::<Vec<_>>().join(", ");
        self.write("workspace.zy", &format!("@[discover(include({files}))] ()"));
        self
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
        Self::command_in(self.directory.path(), arguments)
    }
    fn command_in(directory: &Path, arguments: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory)
            .args(arguments)
            .output()
            .unwrap()
    }
    fn success(&self, arguments: &[&str]) -> Output {
        Self::success_in(self.directory.path(), arguments)
    }
    fn success_in(directory: &Path, arguments: &[&str]) -> Output {
        let output = Self::command_in(directory, arguments);
        assert!(
            output.status.success(),
            "{arguments:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }
}

#[test]
fn standard_library_runs_its_standalone_suite_without_fixture_dependent_programs() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let output = Fixture::success_in(&root, &["test", "std"]);
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("29 passed; 0 failed."), "{stdout}");
    assert!(stdout.contains("represented-call/main.zy"), "nested tests belong to the suite");
    for harness_only in ["arg-list.zy", "filesystem.zy", "read-line-as-int.zy"] {
        assert!(
            !stdout.contains(harness_only),
            "{harness_only} needs explicit test input or setup"
        );
    }
}

#[test]
fn declared_discovery_selects_test_side_subjects_and_keeps_plain_tests_independent() {
    let fixture = Fixture::new();
    fixture.write("workspace.zy", r#"@[discover(include("tests/**/*.zy"), exclude("tests/fixtures/**"), include("tests/fixtures/keep.zy"))]
        (#lib = @[package(library, name(lib))] 1)"#);
    fixture.write(
        "tests/smoke.zy",
        &format!(
            r#"@[package(test(of(lib)))]
        let lib = @(import(lib)) in ({})"#,
            Fixture::executable(0)
        ),
    );
    fixture.write(
        "tests/fixtures/keep.zy",
        &format!(r#"@[package(test(of(lib)))] ({})"#, Fixture::executable(0)),
    );
    fixture.write("tests/fixtures/invalid.zy", "(");
    fixture.write("tests/plain.zy", &format!("@[package(test)] ({})", Fixture::executable(7)));
    let show = fixture.success(&["show"]);
    let show = String::from_utf8_lossy(&show.stdout);
    assert!(show.contains("of ->") && show.contains("tests/plain.zy"));
    assert_eq!(
        show.matches("tests/smoke.zy").count(),
        1,
        "each matching package is displayed once"
    );
    assert!(!show.contains("invalid.zy") && !show.contains("unsupported"));
    let test = fixture.success(&["test", "lib"]);
    let stdout = String::from_utf8_lossy(&test.stdout);
    assert!(stdout.contains("2 passed; 0 failed."));
    assert!(!stdout.contains("plain.zy"));
    let plain = fixture.command(&["test", "tests/plain.zy"]);
    assert_eq!(plain.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&plain.stdout).contains("plain.zy (exit 7)"));

    fixture.write("tests/invalid.zy", "(");
    for arguments in [&["show"][..], &["test", "lib"], &["check", "workspace.zy"]] {
        let output = fixture.command(arguments);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("tests/invalid.zy"));
        assert!(output.stdout.is_empty(), "discovery must finish before output or execution");
    }
    fixture.write("tests/invalid.zy", r#"@[package(test(of(lib)))] 1"#);
    let output = fixture.command(&["test", "lib"]);
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
        "workspace.zy",
        r#"@[discover(include("testing.zy"))]
        let library = @[package(library, name(lib), test(smoke))] @(import("lib.zy")) in
        let one = { @[package(binary, name(tools/first))] @(import("first.zy")) } in
        let two = { @[package(binary, name(tools-first))] @(import("second.zy")) } in
        let broken = { @[package(binary, name(broken))] @(import("missing.zy")) } in
        (library, one, two, broken)"#,
    );
    fixture.write("testing.zy", r#"{ @[package(test, name(smoke))] @(import("smoke.zy")) }"#);
    fixture
        .write("smoke.zy", &format!(r#"let lib = @(import(lib)) in ({})"#, Fixture::executable(0)));
    let show = fixture.success(&["show"]);
    let show = String::from_utf8(show.stdout).unwrap();
    assert!(show.contains("library lib (") && show.contains("test smoke ("));
    assert!(show.contains("binary ") && show.contains("code ->"));
    fixture.success(&["check", "lib"]);
    let tests = fixture.success(&["test", "lib"]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));
    assert_eq!(fixture.command(&["run", "tools/first"]).status.code(), Some(3));
    assert_eq!(fixture.command(&["run", "tools-first"]).status.code(), Some(9));
    for arguments in [
        vec!["build", "-p", "tools/first", "-p", "broken", "-t", "wasm-sps"],
        vec!["build", "-p", "tools/first", "-p", "lib", "-t", "wasm-sps"],
        vec!["run", "-p", "tools/first", "-p", "tools-first"],
        vec!["build", "-p", "tools/first", "-p", "tools-first", "--execute"],
    ] {
        let output = fixture.command(&arguments);
        assert_eq!(output.status.code(), Some(1));
        let stderr = String::from_utf8_lossy(&output.stderr);
        let diagnostic = if arguments.contains(&"broken") {
            "missing.zy"
        } else if arguments.contains(&"lib") {
            "source libraries have no independent compilation boundary"
        } else {
            "require exactly one selected package"
        };
        assert!(stderr.contains(diagnostic), "{stderr}");
        assert!(output.stdout.is_empty());
        assert!(!fixture.directory.path().join("build").exists());
    }
    let built = fixture.success(&[
        "build",
        "-p",
        "tools/first",
        "-p",
        "tools-first",
        "-p",
        "tools/first",
        "-t",
        "wasm-sps",
    ]);
    assert_eq!(String::from_utf8_lossy(&built.stdout).lines().count(), 2);
    for name in ["tools/first", "tools-first"] {
        let wasm = std::fs::read(
            fixture.directory.path().join(format!("build/{}.sps.wasm", name.replace('/', "."))),
        )
        .unwrap();
        wasmparser::Validator::new().validate_all(&wasm).unwrap();
    }
    let wrong = fixture.command(&["run", "lib"]);
    assert!(!wrong.status.success());
    assert!(String::from_utf8_lossy(&wrong.stderr).contains("has role library; expected binary"));
    let invalid = fixture.command(&["run", "missing.zy#../bad"]);
    assert!(!invalid.status.success());
    assert!(String::from_utf8_lossy(&invalid.stderr).contains("invalid package name"));
}

#[test]
fn selecting_multiple_packages_runs_each_shared_companion_once() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"@[discover(include("tests.zy"))]
        (@[package(library, name(example/one))] 1,
         @[package(library, name(example/two))] 2)"#,
    );
    fixture.write(
        "tests.zy",
        &format!(
            "{{ @[package(test(of(example/one, example/two)), name(example/smoke))] {} }}",
            Fixture::executable(0)
        ),
    );
    for packages in [
        vec!["-p", "example/one"],
        vec!["-p", "example/two"],
        vec!["-p", "example/smoke"],
        vec![
            "-p",
            "example/one",
            "--pkg",
            "example/two",
            "--package",
            "example/smoke",
            "-p",
            "example/one",
        ],
    ] {
        let arguments = [vec!["test"], packages].concat();
        let output = fixture.success(&arguments);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout.matches("PASS [interpreter] example/smoke").count(), 1, "{stdout}");
        assert!(stdout.contains("1 passed; 0 failed."), "{stdout}");
    }
}

#[test]
fn whole_files_need_no_names_and_library_tests_import_the_library_normally() {
    let fixture = Fixture::new().with_discovery(&["plain.zy", "lib.zy"]);
    fixture.write("plain.zy", "42");
    let plain = fixture.success(&["show"]);
    assert!(String::from_utf8_lossy(&plain.stdout).contains("library "));
    fixture.success(&["check", "plain.zy"]);
    fixture.write("lib.zy", r#"@[package(library, test("smoke.zy"))] 42"#);
    fixture.write(
        "smoke.zy",
        &format!(
            r#"@[package(test)] let lib = @(import("lib.zy")) in ({})"#,
            Fixture::executable(0)
        ),
    );
    fixture.success(&["check", "lib.zy"]);
    for source in ["lib.zy", "smoke.zy"] {
        let tests = fixture.success(&["test", source]);
        assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));
    }
    fixture.write("main.zy", &format!("@[package(binary)] ({})", Fixture::executable(0)));
    fixture.success(&["check", "main.zy"]);
    fixture.success(&["run", "main.zy"]);
    fixture.success(&["build", "main.zy", "--target", "wasm-sps", "--build-dir", "build"]);
    let wasm = std::fs::read(fixture.directory.path().join("build/main.sps.wasm")).unwrap();
    wasmparser::Validator::new().validate_all(&wasm).unwrap();
    for source in ["plain.zy#", "main.zy#"] {
        let output = fixture.command(&["check", source]);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("invalid package name"));
        assert!(output.stdout.is_empty());
    }
}

#[test]
fn nested_package_annotations_preserve_normal_execution_and_scope() {
    let fixture = Fixture::new().with_discovery(&["main.zy"]);
    let source = Fixture::executable(7)
        .replace("! process/exit", r#"@[package(test, name(nested))] ! process/exit"#);
    fixture.write("main.zy", &format!("@[package(binary)] {source}"));
    fixture.success(&["check", "main.zy"]);
    assert_eq!(fixture.command(&["run", "main.zy"]).status.code(), Some(7));
    let separate = fixture.command(&["check", "nested"]);
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
        "workspace.zy",
        &format!(
            r#"(
        #lib = @[package(library, test(smoke), name(lib))] 1,
        #smoke = @[package(test, name(smoke))] let lib = @(import(lib)) in ({}),
        #unrelated = @[package(test, name(unrelated))] ({}),
        #broken = @[package(binary, name(broken))] missing
    )"#,
            Fixture::executable(0),
            Fixture::executable(8)
        ),
    );
    fixture.success(&["check", "lib"]);
    let output = fixture.success(&["test", "lib"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("smoke (") && stdout.contains("1 passed; 0 failed."));
    assert!(!stdout.contains("unrelated") && !stdout.contains("broken"));
}

#[test]
fn missing_test_relationships_are_visible_but_fail_test_planning() {
    let fixture = Fixture::new().with_discovery(&["lib.zy"]);
    fixture.write("lib.zy", r#"@[package(library, test("missing.zy"))] 42"#);
    let show = fixture.success(&["show"]);
    let show = String::from_utf8_lossy(&show.stdout);
    assert!(show.contains("test ->"));
    fixture.success(&["check", "lib.zy"]);
    let test = fixture.command(&["test", "lib.zy"]);
    assert!(!test.status.success() && test.stdout.is_empty());
    assert!(String::from_utf8_lossy(&test.stderr).contains("cannot read source"));
}

#[test]
fn preflight_checks_every_role_and_executable_before_running_any_test() {
    let fixture = Fixture::new().with_discovery(&["lib.zy", "tests.zy"]);
    fixture.write("lib.zy", r#"@[package(library, test(a_valid), test(z_invalid))] 42"#);
    for (role, term, expected) in [
        ("test", "42", "classified as a value"),
        ("binary", "42", "has role binary; expected test"),
        ("test", "missing", "Unbound variable"),
    ] {
        fixture.write(
            "tests.zy",
            &format!(
                r#"(
            #a_valid = @[package(test, name(a_valid))] ({}),
            #z_invalid = @[package({role}, name(z_invalid))] {term}
        )"#,
                Fixture::executable(0)
            ),
        );
        let output = fixture.command(&["test", "lib.zy"]);
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
    let fixture = Fixture::new().with_discovery(&["lib.zy", "tests.zy"]);
    fixture.write("lib.zy", r#"@[package(library, test(fail), test(pass))] ()"#);
    fixture.write(
        "tests.zy",
        &format!(
            r#"(
        #fail = @[package(test, name(fail))] ({}),
        #pass = @[package(test, name(pass))] ({})
    )"#,
            Fixture::executable(7),
            Fixture::executable(0)
        ),
    );
    let output = fixture.command(&["test", "lib.zy"]);
    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("FAIL [interpreter] fail (") && stdout.contains("(exit 7)"));
    assert!(stdout.contains("PASS [interpreter] pass ("));
    assert!(stdout.contains("1 passed; 1 failed."));
}

#[test]
fn obsolete_forms_duplicate_names_and_implicit_captures_are_rejected() {
    let fixture = Fixture::new();
    for (source, expected) in [
        (r#"@[package(library("lib", "lib.zy"))] ()"#, "compiled library requires"),
        (r#"@[package(library("lib"))] ()"#, "compiled library requires"),
        (
            r#"(#lib = @[package(library, name(lib))] 1, #lib = @[package(test, name(lib))] ())"#,
            "duplicate package name",
        ),
    ] {
        fixture.write("workspace.zy", source);
        let output = fixture.command(&["show"]);
        assert!(!output.status.success(), "{source}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(expected), "{source}: expected {expected:?}, received {stderr}");
        assert!(output.stdout.is_empty());
    }
    fixture.write(
        "workspace.zy",
        "let outside = 42 in (#lib = @[package(library, name(lib))] outside)",
    );
    fixture.success(&["check", "workspace.zy"]);
    let output = fixture.command(&["check", "lib"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Unbound variable"));
    fixture.write("old-import.zy", r#"@(import(package("workspace.zy", "lib")))"#);
    assert!(!fixture.command(&["check", "old-import.zy"]).status.success());
    assert!(!fixture.command(&["run", "workspace.zy", "--binary", "lib"]).status.success());
}

#[test]
fn checked_in_file_packages_check_run_and_test_without_selectors() {
    let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../docs/examples/packages");
    let library = directory.join("library.zy").to_str().unwrap().to_owned();
    let binary = directory.join("main.zy").to_str().unwrap().to_owned();
    let show = Fixture::success_in(&directory, &["show"]);
    let show = String::from_utf8(show.stdout).unwrap();
    assert!(show.contains("library ") && show.contains("binary "));
    assert!(!show.contains('#'), "whole-file addresses never acquire a selector");
    Fixture::success_in(&directory, &["check", &library]);
    Fixture::success_in(&directory, &["check", &binary]);
    let run = Fixture::success_in(&directory, &["run", &binary]);
    assert_eq!(String::from_utf8(run.stdout).unwrap(), "hello from a package\n");
    let tests = Fixture::success_in(&directory, &["test", &library]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("tests/smoke.zy"));
}

#[test]
fn either_conventional_root_is_detected_and_both_are_combined_without_precedence() {
    for root in ["package.zy", "workspace.zy"] {
        let fixture = Fixture::new();
        fixture.write(root, "@[package(library, name(example))] 42");
        let show = fixture.success(&["show"]);
        assert!(String::from_utf8_lossy(&show.stdout).contains("library example ("));
        fixture.success(&["check", "example"]);
    }

    let fixture = Fixture::new();
    fixture.write("package.zy", "@[package(library, name(example))] 42");
    fixture.write("workspace.zy", r#"@[discover(include("package.zy", "tests/*.zy"))] ()"#);
    fixture.write(
        "tests/smoke.zy",
        &format!(r#"@[package(test(of(example)))] ({})"#, Fixture::executable(0)),
    );
    let show = fixture.success(&["show"]);
    assert_eq!(String::from_utf8_lossy(&show.stdout).matches("library example (").count(), 1);
    let tests = fixture.success(&["test", "example"]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));

    fixture.write("workspace.zy", "@[package(library, name(example))] 7");
    let duplicate = fixture.command(&["check", "example"]);
    assert!(!duplicate.status.success() && duplicate.stdout.is_empty());
    let error = String::from_utf8_lossy(&duplicate.stderr);
    assert!(error.contains("duplicate package name") && error.contains("example"), "{error}");
    assert!(error.contains("package.zy") && error.contains("workspace.zy"), "{error}");
}

#[test]
fn workspace_replaces_the_old_automatic_filename_without_reserving_other_file_names() {
    let fixture = Fixture::new();
    fixture.write("packages.zy", "@[package(library, name(legacy))] 1");
    assert!(fixture.success(&["show"]).stdout.is_empty());
    let missing = fixture.command(&["check", "legacy"]);
    assert!(!missing.status.success() && missing.stdout.is_empty());
    assert!(String::from_utf8_lossy(&missing.stderr).contains("unknown package `legacy`"));
    fixture.success(&["check", "packages.zy"]);

    fixture.write("workspace.zy", "@[package(library, name(current))] 42");
    let show = fixture.success(&["show"]);
    let show = String::from_utf8_lossy(&show.stdout);
    assert!(show.contains("library current (") && !show.contains("legacy"));
    fixture.success(&["check", "current"]);

    fixture.write("packages.zy", "(");
    fixture.success(&["check", "current"]);
    let explicit = fixture.command(&["check", "packages.zy"]);
    assert!(!explicit.status.success() && explicit.stdout.is_empty());
    let error = String::from_utf8_lossy(&explicit.stderr);
    assert!(error.contains("packages.zy") && error.contains("Unrecognized EOF"), "{error}");

    let help = fixture.success(&["--help"]);
    let help = String::from_utf8_lossy(&help.stdout);
    assert!(help.contains("workspace.zy") && !help.contains("packages.zy"));
}

#[test]
fn catalog_detection_stays_in_the_working_directory_and_paths_work_without_one() {
    let fixture = Fixture::new();
    fixture.write("package.zy", "@[package(library, name(parent))] 1");
    fixture.write("child/nested/workspace.zy", "(");
    let source = fixture.write("child/plain.zy", "42");
    fixture.write("consumer.zy", "@(import(parent))");
    let child = fixture.directory.path().join("child");

    let show = Fixture::success_in(&child, &["show"]);
    assert!(show.stdout.is_empty(), "neither parent nor nested catalogs are searched");
    Fixture::success_in(&child, &["check", source.to_str().unwrap()]);
    for source in ["parent", "../consumer.zy"] {
        let output = Fixture::command_in(&child, &["check", source]);
        assert!(!output.status.success() && output.stdout.is_empty());
        assert!(String::from_utf8_lossy(&output.stderr).contains("unknown package `parent`"));
    }

    fixture.write("child/package.zy", "@[package(library, name(child))] 2");
    Fixture::success_in(&child, &["check", "child"]);
    let parent = fixture.success(&["show"]);
    assert!(!String::from_utf8_lossy(&parent.stdout).contains("library child"));
}

#[test]
fn only_selected_catalog_roots_expand_discovery() {
    let fixture = Fixture::new();
    fixture.write(
        "package.zy",
        r#"@[discover(include("nested/workspace.zy"))]
        @[package(library, name(example))] 42"#,
    );
    fixture.write("nested/workspace.zy", r#"@[discover(include("broken.zy"))] ()"#);
    fixture.write("nested/broken.zy", "(");
    fixture.success(&["check", "example"]);
    let show = fixture.success(&["show"]);
    assert!(!String::from_utf8_lossy(&show.stdout).contains("broken.zy"));
    let explicit = Fixture::command_in(&fixture.directory.path().join("nested"), &["show"]);
    assert!(!explicit.status.success() && explicit.stdout.is_empty());
    assert!(String::from_utf8_lossy(&explicit.stderr).contains("broken.zy"));

    let standalone = Fixture::new();
    standalone.write("library.zy", r#"@[discover(include("broken.zy"))] 42"#);
    standalone.write("broken.zy", "(");
    standalone.success(&["check", "library.zy"]);
    let tests = standalone.success(&["test", "library.zy"]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("0 passed; 0 failed."));
}

#[test]
fn check_validates_declared_executable_roles_for_both_names_and_paths() {
    let fixture = Fixture::new().with_discovery(&["entry.zy"]);
    for role in ["binary", "test"] {
        fixture.write(
            "entry.zy",
            &format!("@[package({role}, name(example))] ({})", Fixture::executable(7)),
        );
        for source in ["example", "entry.zy"] {
            let checked = fixture.success(&["check", source]);
            assert!(checked.stdout.is_empty(), "checking never executes the entry");
        }
        fixture.write("entry.zy", &format!("@[package({role}, name(example))] 42"));
        for source in ["example", "entry.zy"] {
            let invalid = fixture.command(&["check", source]);
            assert!(!invalid.status.success() && invalid.stdout.is_empty());
            assert!(String::from_utf8_lossy(&invalid.stderr).contains("classified as a value"));
        }
    }
    fixture.write("entry.zy", "@[package(library, name(example))] 42");
    fixture.success(&["check", "example"]);
    fixture.success(&["check", "entry.zy"]);
}

#[test]
fn invalid_catalogs_block_source_commands_but_not_independent_tools() {
    let fixture = Fixture::new();
    fixture.write("package.zy", "(");
    fixture.write("plain.zy", "42\n");
    for arguments in [
        &["show"][..],
        &["check", "plain.zy"],
        &["test", "plain.zy"],
        &["run", "plain.zy"],
        &["build", "plain.zy", "--target", "wasm-sps"],
        &["repl"],
    ] {
        let output = fixture.command(arguments);
        assert!(!output.status.success() && output.stdout.is_empty(), "{arguments:?}");
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains("package.zy") && error.contains("Unrecognized EOF"), "{error}");
    }
    assert!(!fixture.directory.path().join("build").exists(), "no build artifacts on failure");
    fixture.success(&["--help"]);
    fixture.success(&["passes"]);
    fixture.success(&["fmt", "--check", "plain.zy"]);
}

#[test]
fn obsolete_catalog_flags_and_package_command_are_removed() {
    let fixture = Fixture::new();
    for arguments in [&["--packages", "workspace.zy", "show"][..], &["package", "show"]] {
        let output = fixture.command(arguments);
        assert_eq!(output.status.code(), Some(2));
        assert!(output.stdout.is_empty());
        assert!(String::from_utf8_lossy(&output.stderr).contains(arguments[0]));
    }
}

#[test]
fn package_flags_select_existing_names_and_do_not_change_discovery() {
    let fixture = Fixture::new().with_discovery(&["vendor/library.zy", "extra.zy"]);
    fixture.write("package.zy", "@[package(library, name(local))] @(import(shared))");
    fixture.write("vendor/library.zy", "@[package(library, name(shared))] 42");
    fixture.write("extra.zy", "@[package(library, name(extra))] @(import(shared))");
    fixture.write("unselected.zy", "(");
    for flag in ["-p", "--pkg", "--package"] {
        fixture.success(&["check", flag, "local"]);
    }
    fixture.success(&["check", "-p", "local", "--pkg", "extra", "--package", "local"]);
    let show = fixture.success(&["show", "-p", "local", "--pkg", "extra", "--package", "local"]);
    let shown = String::from_utf8_lossy(&show.stdout);
    for name in ["local", "extra"] {
        assert_eq!(shown.matches(&format!("library {name} (")).count(), 1, "{shown}");
    }
    assert!(!shown.contains("library shared ("), "dependencies are available but not selected");
    let help = fixture.success(&["check", "--help"]);
    let help = String::from_utf8_lossy(&help.stdout);
    assert!(help.contains("-p") && help.contains("--pkg") && help.contains("--package"));
    assert!(help.contains("NAME") && !help.contains("Add a package file"));
}

#[test]
fn package_selectors_reject_paths_unknown_names_and_mixed_source_selection() {
    let fixture = Fixture::new();
    fixture.write("library.zy", "@[package(library, name(example))] 42");
    fixture.success(&["check", "library.zy"]);
    let unselected = fixture.command(&["check", "-p", "example"]);
    assert!(!unselected.status.success() && unselected.stdout.is_empty());
    assert!(String::from_utf8_lossy(&unselected.stderr).contains("unknown package `example`"));

    fixture.write("package.zy", "@[package(library, name(example))] 7");
    fixture.success(&["check", "-p", "example"]);
    fixture.write("missing", "(");
    for command in ["check", "show", "test", "build", "run"] {
        for path in ["library.zy", "./library.zy", "../shared/package.zy"] {
            let output = fixture.command(&[command, "-p", path]);
            assert_eq!(output.status.code(), Some(2));
            assert!(output.stdout.is_empty());
            assert!(String::from_utf8_lossy(&output.stderr).contains("invalid package name"));
        }
        let output = fixture.command(&[command, "-p", "example", "-p", "missing"]);
        assert!(!output.status.success() && output.stdout.is_empty());
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains("unknown package `missing`"), "{error}");
    }
    for command in ["check", "test", "build", "run"] {
        for source in ["example", "library.zy"] {
            let output = fixture.command(&[command, source, "-p", "example"]);
            assert_eq!(output.status.code(), Some(2));
            assert!(output.stdout.is_empty());
            assert!(String::from_utf8_lossy(&output.stderr).contains("cannot be used with"));
        }
    }
    assert!(!fixture.directory.path().join("build").exists());
}

#[test]
fn name_and_file_relationships_select_the_same_test_only_once() {
    let fixture = Fixture::new().with_discovery(&["lib.zy", "tests/*.zy"]);
    fixture.write(
        "lib.zy",
        r#"@[package(library, name(lib), test(smoke), test("tests/smoke.zy"))] 42"#,
    );
    fixture.write(
        "tests/smoke.zy",
        &format!(
            r#"@[package(test(of(lib, "../lib.zy")), name(smoke))] ({})"#,
            Fixture::executable(0)
        ),
    );
    for source in ["lib", "lib.zy", "./lib.zy", "smoke", "tests/smoke.zy"] {
        let output = fixture.success(&["test", source]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout.matches("PASS [interpreter] ").count(), 1, "{stdout}");
        assert!(stdout.contains("tests/smoke.zy"), "{stdout}");
        assert!(stdout.contains("1 passed; 0 failed."), "{stdout}");
    }
}

#[test]
fn registration_wrappers_and_implementation_files_have_distinct_test_subjects() {
    let fixture = Fixture::new().with_discovery(&["library.zy", "wrapper.zy", "tests/*.zy"]);
    fixture.write("library.zy", "@[package(library, name(core))] 42");
    fixture.write("wrapper.zy", "@[package(library, name(wrapped))] @(import(core))");
    for (subject, name) in [("core", "core-test"), ("wrapped", "wrapper-test")] {
        fixture.write(
            &format!("tests/{name}.zy"),
            &format!("@[package(test(of({subject})), name({name}))] ({})", Fixture::executable(0)),
        );
    }
    for (source, selected, excluded) in [
        ("core", "core-test", "wrapper-test"),
        ("library.zy", "core-test", "wrapper-test"),
        ("wrapped", "wrapper-test", "core-test"),
        ("wrapper.zy", "wrapper-test", "core-test"),
    ] {
        let output = fixture.success(&["test", source]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains(&format!("tests/{selected}.zy")), "{stdout}");
        assert!(!stdout.contains(excluded), "{stdout}");
        assert!(stdout.contains("1 passed; 0 failed."), "{stdout}");
    }
}

#[test]
fn suites_do_not_recursively_activate_dependency_or_selected_test_relationships() {
    let fixture = Fixture::new().with_discovery(&["lib.zy", "dependency.zy", "tests/*.zy"]);
    fixture.write("lib.zy", "@[package(library, name(app))] @(import(dependency))");
    fixture
        .write("dependency.zy", "@[package(library, name(dependency), test(dependency-test))] 42");
    for (name, role, relations, code) in [
        ("smoke", "test(of(app))", ", test(extra)", 0),
        ("dependency-test", "test", "", 7),
        ("extra", "test", "", 9),
    ] {
        fixture.write(
            &format!("tests/{name}.zy"),
            &format!("@[package({role}, name({name}){relations})] ({})", Fixture::executable(code)),
        );
    }
    let output = fixture.success(&["test", "app"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("tests/smoke.zy"), "{stdout}");
    assert!(stdout.contains("1 passed; 0 failed."), "{stdout}");
    assert!(!stdout.contains("dependency-test") && !stdout.contains("extra"), "{stdout}");

    for (source, selected, summary) in [
        ("dependency", "dependency-test", "0 passed; 1 failed."),
        ("smoke", "extra", "1 passed; 1 failed."),
    ] {
        let output = fixture.command(&["test", source]);
        assert_eq!(output.status.code(), Some(1));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("FAIL [interpreter] "), "{stdout}");
        assert!(stdout.contains(&format!("tests/{selected}.zy")), "{stdout}");
        assert!(stdout.contains(summary), "{stdout}");
    }
}

#[test]
fn code_cycles_reject_before_execution_or_replacing_existing_build_artifacts() {
    let fixture = Fixture::new().with_discovery(&["lib.zy", "smoke.zy", "main.zy"]);
    fixture.write("lib.zy", "@[package(library, name(lib), test(smoke))] 42");
    fixture.write(
        "smoke.zy",
        &format!(
            "@[package(test, name(smoke))] let lib = @(import(lib)) in ({})",
            Fixture::executable(0)
        ),
    );
    fixture.write(
        "main.zy",
        &format!(
            "@[package(binary, name(main))] let lib = @(import(lib)) in ({})",
            Fixture::executable(0)
        ),
    );
    fixture.success(&["test", "lib"]);
    let build = ["build", "main", "-t", "wasm-sps", "--build-dir", "build"];
    fixture.success(&build);
    let artifact = fixture.directory.path().join("build/main.sps.wasm");
    let original = std::fs::read(&artifact).unwrap();
    wasmparser::Validator::new().validate_all(&original).unwrap();

    fixture.write("lib.zy", "@[package(library, name(lib), test(smoke))] @(import(smoke))");
    for arguments in [&["check", "lib"][..], &["test", "lib"], &["run", "main"], &build] {
        let output = fixture.command(arguments);
        assert!(!output.status.success() && output.stdout.is_empty(), "{arguments:?}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("cyclic source dependencies"), "{stderr}");
        assert!(stderr.contains("lib.zy") && stderr.contains("smoke.zy"), "{stderr}");
        assert_eq!(std::fs::read(&artifact).unwrap(), original, "{arguments:?}");
    }
}

#[test]
fn quoted_imports_remain_relative_to_their_defining_file_through_named_registrations() {
    let fixture = Fixture::new().with_discovery(&["vendor/catalog/entries.zy"]);
    fixture.write("package.zy", "@[package(library, name(app))] @(import(vendor/core))");
    fixture.write(
        "vendor/catalog/entries.zy",
        r#"@[package(library, name(vendor/core))] @(import("../src/impl.zy"))"#,
    );
    fixture.write("vendor/src/impl.zy", r#"@(import("value.zy"))"#);
    fixture.write("vendor/src/value.zy", "42");
    for decoy in ["value.zy", "vendor/catalog/value.zy", "src/impl.zy"] {
        fixture.write(decoy, "(");
    }
    let arguments = ["check", "-p", "app"];
    fixture.success(&arguments);

    fixture.write("vendor/src/value.zy", r#"@(import("missing.zy"))"#);
    let output = fixture.command(&arguments);
    assert!(!output.status.success() && output.stdout.is_empty());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("source file not found") && stderr.contains("vendor/src/missing.zy"),
        "{stderr}"
    );
}

#[test]
fn declaration_discovery_order_preserves_cross_file_resolution_and_suite_order() {
    let fixture = Fixture::new();
    fixture.write("first.zy", "@[package(library, name(consumer))] @(import(provider))");
    fixture.write("second.zy", "@[package(library, name(provider))] 42");
    for name in ["z-last", "a-first"] {
        fixture.write(
            &format!("tests/{name}.zy"),
            &format!("@[package(test(of(provider)), name({name}))] ({})", Fixture::executable(0)),
        );
    }
    let patterns =
        [r#""first.zy", "second.zy", "tests/*.zy""#, r#""tests/*.zy", "second.zy", "first.zy""#];
    for operation in [&["show"][..], &["check", "consumer"], &["test", "provider"]] {
        let outputs = patterns.map(|patterns| {
            fixture.write("workspace.zy", &format!("@[discover(include({patterns}))] ()"));
            fixture.success(operation)
        });
        assert_eq!(outputs[0].stdout, outputs[1].stdout, "{operation:?}");
    }
    let output = fixture.success(&["test", "-p", "provider"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let names = stdout
        .lines()
        .filter_map(|line| line.strip_prefix("PASS [interpreter] "))
        .map(|path| Path::new(path).file_stem().unwrap().to_str().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(names, ["a-first", "z-last"]);
}

#[cfg(unix)]
#[test]
fn discovery_ignores_symlinked_files_and_directories_until_explicitly_selected() {
    let fixture = Fixture::new().with_discovery(&["tests/**/*.zy"]);
    fixture.write("tests/valid.zy", "@[package(library, name(valid))] 42");
    let outside = Fixture::new();
    let broken = outside.write("broken.zy", "(");
    std::os::unix::fs::symlink(&broken, fixture.directory.path().join("tests/linked.zy")).unwrap();
    std::os::unix::fs::symlink(
        outside.directory.path(),
        fixture.directory.path().join("tests/linked-directory"),
    )
    .unwrap();
    fixture.success(&["check", "valid"]);
    let shown = fixture.success(&["show"]);
    let stdout = String::from_utf8_lossy(&shown.stdout);
    assert!(stdout.contains("library valid ("), "{stdout}");
    assert!(!stdout.contains("linked") && !stdout.contains("broken"), "{stdout}");

    let output = fixture.command(&["check", "tests/linked.zy"]);
    assert!(!output.status.success() && output.stdout.is_empty());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("broken.zy") && stderr.contains("Unrecognized EOF"), "{stderr}");
}
