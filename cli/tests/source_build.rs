use std::{path::PathBuf, process::Command};

#[test]
fn typed_holes_are_inspectable_but_rejected_before_execution_or_lowering() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("hole.zy");
    for hole in ["_", "0"] {
        std::fs::write(&source, format!(
            "param (/Int64; /process) : @(import(\"{}\")) in let x : Int64 = {hole} in ! process/exit x",
            workspace.join("lib/std/builtin.zy").display()
        )).unwrap();
        let checked =
            Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(&source).output().unwrap();
        assert!(checked.status.success(), "{}", String::from_utf8_lossy(&checked.stderr));
        for arguments in [
            vec!["run"],
            vec!["run", "--dry"],
            vec!["build", "--target", "zir"],
            vec!["build", "--target", "wasm-am"],
            vec!["build", "--target", "wasm-sps"],
        ] {
            // Successful builds only need the IR target; failures must stop before emission.
            if hole == "0" && arguments.last().is_some_and(|arg| arg.starts_with("wasm")) {
                continue;
            }
            let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
                .args(&arguments)
                .arg(&source)
                .output()
                .unwrap();
            let error = String::from_utf8_lossy(&output.stderr);
            assert_eq!(output.status.success(), hole == "0", "{arguments:?}: {error}");
            if hole == "_" {
                assert!(
                    error.contains("unfilled value or computation hole"),
                    "{arguments:?}: {error}"
                );
                assert!(!error.contains("panicked"));
            }
        }
    }
    std::fs::write(&source, format!(
        "param (/Int64; /process) : @(import(\"{}\")) in let val unused (x : Int64) : Int64 = _ in ! process/exit 0",
        workspace.join("lib/std/builtin.zy").display()
    )).unwrap();
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("run").arg(&source).output().unwrap();
    assert!(
        output.status.success(),
        "an eliminated static function must not make the executable incomplete: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn partial_patterns_report_runtime_failure_across_backends() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("partial.zy");
    let build = directory.path().join("build");
    let builtin = workspace.join("lib/std/builtin.zy");
    for (body, expected) in [
        ("@[partial] let 0 = 0 in ! process/exit 0", 0),
        ("@[partial] let 0 = 1 in ! process/exit 42", 1),
        ("do r <- (@[partial] fn (0 : Int64) => ret 0) 1; ! process/exit 42", 1),
        (
            "let B = data | +T : Unit | +F : Unit end in @[partial] let +T() = (+F() : B) in ! process/exit 42",
            1,
        ),
    ] {
        std::fs::write(
            &source,
            format!(
                "param (/process; /Int64; /Unit) : @(import(\"{}\")) in {body}\n",
                builtin.display()
            ),
        )
        .unwrap();
        for target in ["exe", "wasm-am", "wasm-sps"] {
            let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
            command.arg("build").arg(&source).args(["--target", target, "--build-dir"]).arg(&build);
            if target == "exe" {
                command
                    .args(["--target-arch", "x86-64", "--runtime-dir"])
                    .arg(workspace.join("runtime"))
                    .arg("--execute");
            }
            let mut output = command.output().unwrap();
            if target != "exe" {
                assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
                let extension = if target == "wasm-am" { "am" } else { "sps" };
                output = Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
                    .arg(workspace.join("lang/tests/wasm-host.mjs"))
                    .arg(build.join(format!("partial.{extension}.wasm")))
                    .output()
                    .unwrap();
            }
            let diagnostic = String::from_utf8_lossy(&output.stderr);
            assert_eq!(output.status.code(), Some(expected), "{target}: {body}\n{diagnostic}");
            assert!(output.stdout.is_empty(), "a failed pattern must not continue executing");
            if expected != 0 {
                assert!(diagnostic.contains("pattern match failed"), "{target}: {diagnostic}");
                assert!(!diagnostic.contains("panicked"), "{target}: {diagnostic}");
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn rebuilding_a_native_program_uses_the_current_assembly() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("rebuild.zy");
    let build = directory.path().join("build");
    let builtin = workspace.join("lib/std/builtin.zy");

    for expected in [17, 23, 23] {
        std::fs::write(
            &source,
            format!(
                "param (/process; /Int64) : @(import(\"{}\")) in ! process/exit {expected}\n",
                builtin.display()
            ),
        )
        .unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(&source)
            .args(["--target-arch", "x86-64", "--build-dir"])
            .arg(&build)
            .arg("--runtime-dir")
            .arg(workspace.join("runtime"))
            .arg("--execute")
            .output()
            .unwrap();
        assert_eq!(
            output.status.code(),
            Some(expected),
            "native rebuild failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn a_root_term_builds_without_project_configuration() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/builtin/exit.zy");
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("build")
        .arg(root)
        .args(["--target", "zir"])
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "source build failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("[root]"));
}

#[test]
fn the_wasm_targets_write_distinct_valid_core_modules() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/core/fact.zy");
    let build = tempfile::tempdir().unwrap();
    let build_target = |target: &str, filename: &str| {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(&root)
            .args(["--target", target, "--build-dir"])
            .arg(build.path())
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{target} build failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        let module_path = build.path().join(filename);
        let module = std::fs::read(&module_path).unwrap();
        wasmparser::validate(&module).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&output.stdout).trim(),
            module_path.display().to_string()
        );
        module
    };

    let am = build_target("wasm-am", "fact.am.wasm");
    let sps = build_target("wasm-sps", "fact.sps.wasm");
    let function_bodies = |module: &[u8]| {
        wasmparser::Parser::new(0)
            .parse_all(module)
            .filter(|payload| matches!(payload, Ok(wasmparser::Payload::CodeSectionEntry(_))))
            .count()
    };
    assert!(function_bodies(&sps) < function_bodies(&am));
}

#[test]
fn a_rejected_root_returns_a_failure_status_with_source_diagnostics() {
    let root =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/core/fail-annotation.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(!output.status.success());
    let diagnostic = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostic.contains("tyck.missing-annotation"));
    assert!(diagnostic.contains("Cannot infer the data type of constructor `+True`"));
    assert!(diagnostic.contains("add a type ascription"));
    assert!(!diagnostic.contains("when tycking"));
    assert!(!diagnostic.contains("Error location:"));
    assert!(diagnostic.lines().count() <= 12, "diagnostic was unexpectedly noisy: {diagnostic}");
}

#[test]
fn an_unattached_text_block_emits_a_non_fatal_warning() {
    let root =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/core/warn-unattached-text.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(output.status.success());
    let warning = String::from_utf8_lossy(&output.stderr);
    assert!(warning.contains("text block is not attached to an annotation"));
    assert!(warning.contains("this text block contributes no text"));
}

#[test]
fn check_renders_debug_observations_from_the_materialized_program() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("debug.zy");
    std::fs::write(&root, r#"@[debug("answer")] ret 1"#).unwrap();

    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(
        output.status.success(),
        "source check failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let observation = String::from_utf8_lossy(&output.stdout);
    assert!(observation.contains(r#"[debug printing] "answer""#), "{observation}");
    assert!(observation.contains("ret 1"), "{observation}");
    assert!(observation.contains("Ret Int64"), "{observation}");
}
