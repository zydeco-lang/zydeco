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
        self.write("packages.zy", &format!("@[discover(include({files}))] ()"));
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
    assert!(stdout.contains("23 passed; 0 failed."), "{stdout}");
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
    fixture.write("packages.zy", r#"@[discover(include("tests/**/*.zy"), exclude("tests/fixtures/**"), include("tests/fixtures/keep.zy"))]
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
    for arguments in [&["show"][..], &["test", "lib"], &["check", "packages.zy"]] {
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
        "packages.zy",
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
    for name in ["tools/first", "tools-first"] {
        fixture.success(&["build", name, "--target", "wasm-sps", "--build-dir", "build"]);
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
        "packages.zy",
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
fn missing_and_unsupported_relationships_are_visible_but_fail_test_planning() {
    let fixture = Fixture::new().with_discovery(&["lib.zy"]);
    for kind in ["test", "tset"] {
        fixture.write("lib.zy", &format!(r#"@[package(library, {kind}("missing.zy"))] 42"#));
        let show = fixture.success(&["show"]);
        let show = String::from_utf8_lossy(&show.stdout);
        assert!(show.contains(&format!("{kind} ->")));
        if kind == "tset" {
            assert!(show.contains("[unsupported]"));
        }
        fixture.success(&["check", "lib.zy"]);
        let test = fixture.command(&["test", "lib.zy"]);
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
        (r#"@[package(library("lib", "lib.zy"))] ()"#, "first argument"),
        (r#"@[package(library("lib"))] ()"#, "first argument"),
        (
            r#"(#lib = @[package(library, name(lib))] 1, #lib = @[package(test, name(lib))] ())"#,
            "duplicate package name",
        ),
    ] {
        fixture.write("packages.zy", source);
        let output = fixture.command(&["show"]);
        assert!(!output.status.success(), "{source}");
        assert!(String::from_utf8_lossy(&output.stderr).contains(expected));
        assert!(output.stdout.is_empty());
    }
    fixture.write(
        "packages.zy",
        "let outside = 42 in (#lib = @[package(library, name(lib))] outside)",
    );
    fixture.success(&["check", "packages.zy"]);
    let output = fixture.command(&["check", "lib"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Unbound variable"));
    fixture.write("old-import.zy", r#"@(import(package("packages.zy", "lib")))"#);
    assert!(!fixture.command(&["check", "old-import.zy"]).status.success());
    assert!(!fixture.command(&["run", "packages.zy", "--binary", "lib"]).status.success());
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
    for root in ["package.zy", "packages.zy"] {
        let fixture = Fixture::new();
        fixture.write(root, "@[package(library, name(example))] 42");
        let show = fixture.success(&["show"]);
        assert!(String::from_utf8_lossy(&show.stdout).contains("library example ("));
        fixture.success(&["check", "example"]);
    }

    let fixture = Fixture::new();
    fixture.write("package.zy", "@[package(library, name(example))] 42");
    fixture.write("packages.zy", r#"@[discover(include("package.zy", "tests/*.zy"))] ()"#);
    fixture.write(
        "tests/smoke.zy",
        &format!(r#"@[package(test(of(example)))] ({})"#, Fixture::executable(0)),
    );
    let show = fixture.success(&["show"]);
    assert_eq!(String::from_utf8_lossy(&show.stdout).matches("library example (").count(), 1);
    let tests = fixture.success(&["test", "example"]);
    assert!(String::from_utf8_lossy(&tests.stdout).contains("1 passed; 0 failed."));

    fixture.write("packages.zy", "@[package(library, name(example))] 7");
    let duplicate = fixture.command(&["check", "example"]);
    assert!(!duplicate.status.success() && duplicate.stdout.is_empty());
    let error = String::from_utf8_lossy(&duplicate.stderr);
    assert!(error.contains("duplicate package name") && error.contains("example"), "{error}");
    assert!(error.contains("package.zy") && error.contains("packages.zy"), "{error}");
}

#[test]
fn catalog_detection_stays_in_the_working_directory_and_paths_work_without_one() {
    let fixture = Fixture::new();
    fixture.write("package.zy", "@[package(library, name(parent))] 1");
    fixture.write("child/nested/packages.zy", "(");
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
        r#"@[discover(include("nested/packages.zy"))]
        @[package(library, name(example))] 42"#,
    );
    fixture.write("nested/packages.zy", r#"@[discover(include("broken.zy"))] ()"#);
    fixture.write("nested/broken.zy", "(");
    fixture.success(&["check", "example"]);
    let show = fixture.success(&["show"]);
    assert!(!String::from_utf8_lossy(&show.stdout).contains("broken.zy"));
    let explicit = fixture.command(&["-p", "nested/packages.zy", "check", "example"]);
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
        &["doc", "show", "plain.zy"],
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
    fixture.success(&["__doc-example-worker"]);
}

#[test]
fn obsolete_catalog_flags_and_package_command_are_removed() {
    let fixture = Fixture::new();
    for arguments in [&["--packages", "packages.zy", "show"][..], &["package", "show"]] {
        let output = fixture.command(arguments);
        assert_eq!(output.status.code(), Some(2));
        assert!(output.stdout.is_empty());
        assert!(String::from_utf8_lossy(&output.stderr).contains(arguments[0]));
    }
}

#[test]
fn explicit_package_flags_are_repeatable_aliases_and_extend_automatic_discovery() {
    let fixture = Fixture::new();
    fixture.write("package.zy", "@[package(library, name(local))] @(import(shared))");
    fixture.write("vendor/entries.zy", r#"@[discover(include("library.zy"))] ()"#);
    fixture.write("vendor/library.zy", "@[package(library, name(shared))] 42");
    fixture.write("extra.zy", "@[package(library, name(extra))] @(import(shared))");
    for flag in ["-p", "--pkg", "--package"] {
        fixture.success(&[flag, "vendor/entries.zy", "check", "local"]);
        let misplaced = fixture.command(&["check", "local", flag, "vendor/entries.zy"]);
        assert_eq!(misplaced.status.code(), Some(2));
        assert!(misplaced.stdout.is_empty());
        assert!(String::from_utf8_lossy(&misplaced.stderr).contains(flag));
    }
    let show = fixture.success(&[
        "-p",
        "vendor/entries.zy",
        "--pkg",
        "package.zy",
        "--package",
        "extra.zy",
        "show",
    ]);
    let shown = String::from_utf8_lossy(&show.stdout);
    for name in ["local", "shared", "extra"] {
        assert_eq!(shown.matches(&format!("library {name} (")).count(), 1, "{shown}");
    }
    let misplaced = fixture.command(&["-p", "vendor/entries.zy", "show", "--pkg", "extra.zy"]);
    assert_eq!(misplaced.status.code(), Some(2));
    assert!(misplaced.stdout.is_empty(), "mixed placement must not silently discard earlier files");
    assert!(String::from_utf8_lossy(&misplaced.stderr).contains("--pkg"));
    let help = fixture.success(&["--help"]);
    let help = String::from_utf8_lossy(&help.stdout);
    assert!(help.contains("-p") && help.contains("--pkg") && help.contains("--package"));
}

#[test]
fn explicit_package_files_work_without_conventional_roots_and_reject_invalid_inputs() {
    let fixture = Fixture::new();
    fixture.write("library.zy", "@[package(library, name(example))] 42");
    fixture.success(&["--package", "library.zy", "check", "example"]);
    let unselected = fixture.command(&["check", "example"]);
    assert!(!unselected.status.success() && unselected.stdout.is_empty());
    assert!(String::from_utf8_lossy(&unselected.stderr).contains("unknown package `example`"));

    fixture.write("package.zy", "@[package(library, name(example))] 7");
    fixture.write("broken.zy", "(");
    for (file, expected) in [
        ("library.zy", "duplicate package name"),
        ("missing.zy", "cannot read source"),
        ("broken.zy", "Unrecognized EOF"),
    ] {
        let output = fixture.command(&["-p", file, "show"]);
        assert!(!output.status.success() && output.stdout.is_empty());
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains(expected) && error.contains(file), "{error}");
    }
}
