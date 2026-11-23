use std::{path::PathBuf, process::Command};
use zydeco_cli::{CommandCompiler, RepresentationStrategy, TargetArchitecture, TargetOs};
use zydeco_stackir::{SpsLowProgram, low::unbox::LocalUnboxing};

struct Fixture;

impl Fixture {
    fn source() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../lib/tests/core/representation-policies.zy")
    }

    fn unboxed_items(program: &SpsLowProgram, strategy: RepresentationStrategy) -> usize {
        let plan = LocalUnboxing::with_policy(program, &strategy);
        plan.values.len() + plan.unboxed_vars.len()
    }
}

#[test]
fn reselecting_a_policy_recomputes_the_cached_layout() {
    let backend = CommandCompiler::default()
        .with_representation(RepresentationStrategy::Shared)
        .lower(&Fixture::source())
        .unwrap();
    assert_eq!(backend.representation(), RepresentationStrategy::Shared);
    let shared = Fixture::unboxed_items(&backend.sps_low, backend.representation());
    let backend = backend.with_representation(RepresentationStrategy::Boxed);
    let boxed = Fixture::unboxed_items(&backend.sps_low, backend.representation());
    assert!(shared > boxed && boxed == 0, "the fixture must distinguish the selected layouts");
    let independent_boxed = CommandCompiler::default()
        .with_representation(RepresentationStrategy::Boxed)
        .lower(&Fixture::source())
        .unwrap();
    assert_eq!(
        boxed,
        Fixture::unboxed_items(&independent_boxed.sps_low, independent_boxed.representation())
    );
    let backend = backend.with_representation(RepresentationStrategy::Shared);
    assert_eq!(Fixture::unboxed_items(&backend.sps_low, backend.representation()), shared);
    let independent = CommandCompiler::default().lower(&Fixture::source()).unwrap();
    assert_eq!(independent.representation(), RepresentationStrategy::Local);
    let backend = backend.with_representation(RepresentationStrategy::Local);
    assert_eq!(
        Fixture::unboxed_items(&backend.sps_low, backend.representation()),
        Fixture::unboxed_items(&independent.sps_low, independent.representation())
    );
    let arch = TargetArchitecture::host().unwrap();
    let os = TargetOs::host().unwrap();
    assert!(!backend.emit_native(arch, os).unwrap().object.is_empty());
}

#[test]
fn native_emission_is_deterministic_across_repeated_calls_on_one_program() {
    let backend = CommandCompiler::default().lower(&Fixture::source()).unwrap();
    for arch in [TargetArchitecture::X86_64, TargetArchitecture::Aarch64] {
        for os in [TargetOs::Linux, TargetOs::Macos] {
            let first = backend.emit_native(arch, os).unwrap().object;
            let second = backend.emit_native(arch, os).unwrap().object;
            assert!(!first.is_empty());
            assert_eq!(first, second, "object output must be deterministic for {arch}/{os:?}");
        }
    }
}

#[test]
fn cli_accepts_each_policy_for_a_lowering_target() {
    let directory = tempfile::tempdir().unwrap();
    for &strategy in RepresentationStrategy::ALL {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(Fixture::source())
            .args(["--target", "wasm-sps", "--representation", strategy.name(), "--build-dir"])
            .arg(directory.path().join(strategy.name()))
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
        ("zir", "shared", "this target does not select physical representations"),
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
