use std::{path::PathBuf, process::Command};
use zydeco_assembly::syntax::{AssemblyProgram, Instruction, Program};
use zydeco_cli::{CommandCompiler, RepresentationStrategy};

struct Fixture;

impl Fixture {
    fn source() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../lib/tests/core/representation-policies.zy")
    }

    fn allocations(program: &AssemblyProgram) -> usize {
        program
            .arena()
            .programs
            .iter()
            .filter(|(_, node)| {
                matches!(node, Program::Instruction(Instruction::PackProduct(_), _))
            })
            .count()
    }
}

#[test]
fn reselecting_a_policy_recomputes_the_cached_layout() {
    let backend = CommandCompiler::default()
        .with_representation(RepresentationStrategy::Shared)
        .lower(&Fixture::source())
        .unwrap();
    assert_eq!(backend.representation(), RepresentationStrategy::Shared);
    let shared = Fixture::allocations(backend.assembly());
    let backend = backend.with_representation(RepresentationStrategy::Boxed);
    let boxed = Fixture::allocations(backend.assembly());
    assert_eq!(boxed, shared + 1, "the tail-recursive closure should lose exactly one cell");
    let backend = backend.with_representation(RepresentationStrategy::Shared);
    assert_eq!(Fixture::allocations(backend.assembly()), shared);
    let independent = CommandCompiler::default().lower(&Fixture::source()).unwrap();
    assert_eq!(independent.representation(), RepresentationStrategy::Local);
    assert_eq!(Fixture::allocations(independent.assembly()), boxed);
}

#[test]
fn cli_accepts_each_policy_for_an_assembly_target() {
    for &strategy in RepresentationStrategy::ALL {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(Fixture::source())
            .args(["--target", "zasm", "--representation", strategy.name()])
            .output()
            .unwrap();
        assert!(output.status.success(), "{strategy}: {}", String::from_utf8_lossy(&output.stderr));
        assert!(!output.stdout.is_empty());
        assert_eq!(strategy.name().parse::<RepresentationStrategy>().unwrap(), strategy);
    }
}

#[test]
fn unsupported_targets_and_unknown_policies_fail_before_artifact_creation() {
    let directory = tempfile::tempdir().unwrap();
    let build = directory.path().join("uncreated");
    for (target, policy, diagnostic) in [
        ("zir", "shared", "this target does not use assembly representation analysis"),
        ("wasm-sps", "local", "this target does not use assembly representation analysis"),
        ("exe", "unknown", "invalid value 'unknown'"),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(Fixture::source())
            .args(["--target", target, "--representation", policy, "--build-dir"])
            .arg(&build)
            .output()
            .unwrap();
        assert!(!output.status.success());
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains(diagnostic), "{target}: {error}");
        assert!(!error.contains("panicked"));
        assert!(!build.exists(), "rejected configuration must not write artifacts");
    }
    assert!("unknown".parse::<RepresentationStrategy>().is_err());
}
