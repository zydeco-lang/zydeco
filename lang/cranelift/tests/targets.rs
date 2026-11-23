use std::path::PathBuf;
use std::sync::Arc;
use zydeco_cranelift::{
    CExportEntry, Emitter, NativeArch, NativeEntry, NativeOs, NativeTarget, emit_guard_object,
};
use zydeco_foundation::pass::CompilerPass;
use zydeco_session::CompilerSession;
use zydeco_session::source::{PackageId, Project};
use zydeco_stackir::low::representation::RepresentationStrategy;
use zydeco_stackir::{RootLowerer, SpsLowPipeline, SpsLowProgram, SpsScheme};
use zydeco_surface::metadata::{LibraryRole, PackageRole};
use zydeco_syntax::ForeignSymbolName;

const ALL_TARGETS: [NativeTarget; 4] = [
    NativeTarget::new(NativeArch::X86_64, NativeOs::Linux),
    NativeTarget::new(NativeArch::X86_64, NativeOs::Macos),
    NativeTarget::new(NativeArch::Aarch64, NativeOs::Linux),
    NativeTarget::new(NativeArch::Aarch64, NativeOs::Macos),
];

fn builtin_path() -> String {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/std/builtin.zy")
        .canonicalize()
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned()
}

fn lower_process(source: &str) -> SpsLowProgram {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("main.zy");
    std::fs::write(&file, source).unwrap();
    let session = CompilerSession::default();
    let project = Arc::new(Project::default());
    let analysis = session.analyze_package(&PackageId { path: file, name: None }, project).unwrap();
    let exec = session.executable_program(&analysis).unwrap();
    let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
        .run_with_builtin(&exec.core, Some(&exec.builtin));
    let low = SpsLowPipeline.run_infallible(frag);
    low.validate().unwrap();
    low
}

fn lower_c_export(source: &str) -> (SpsLowProgram, CExportEntry) {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("lib.zy");
    std::fs::write(&file, source).unwrap();
    let session = CompilerSession::default();
    let project = Arc::new(Project::default());
    let id = PackageId { path: file, name: None };
    let pkg = session.package_in(&id, project.clone()).unwrap();
    let PackageRole::Library(LibraryRole::Compiled(contract)) = pkg.role else {
        panic!("expected compiled library package");
    };
    let analysis = session.analyze_package(&id, project).unwrap();
    let lib = session.library_program(&analysis, &contract).unwrap();
    let export = &lib.library.exports[0];
    let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
        .run_with_builtin(&export.core, export.builtin.as_ref());
    let low = SpsLowPipeline.run_infallible(frag);
    low.validate().unwrap();
    let entry = CExportEntry {
        symbol: export.symbol.clone(),
        signature: export.signature.clone(),
        guard: ForeignSymbolName::parse("zydeco_test_guard").unwrap(),
    };
    (low, entry)
}

fn lower_unit(source: &str) -> SpsLowProgram {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("unit.zy");
    std::fs::write(&file, source).unwrap();
    let session = CompilerSession::default();
    let project = Arc::new(Project::default());
    let analysis = session.analyze_package(&PackageId { path: file, name: None }, project).unwrap();
    let unit = session.unit_program(&analysis).unwrap();
    let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
        .run_with_builtin(&unit.unit.initializer.core, unit.unit.initializer.builtin.as_ref());
    let low = SpsLowPipeline.run_infallible(frag);
    low.validate().unwrap();
    low
}

fn assert_object_magic(target: NativeTarget, bytes: &[u8]) {
    assert!(bytes.len() > 32, "emitted object for {:?} unexpectedly small", target);
    match target.os {
        | NativeOs::Linux => assert_eq!(&bytes[..4], b"\x7fELF", "expected ELF magic"),
        | NativeOs::Macos => {
            assert_eq!(&bytes[..4], &0xFEED_FACF_u32.to_le_bytes(), "expected Mach-O 64 magic");
        }
    }
}

#[test]
fn all_four_targets_and_strategies_emit_deterministic_elf_and_macho_objects() {
    let builtin = builtin_path();
    let proc_src = format!(
        r#"
param (/OS; /Unit; /Int; /String; /numeric; /process; /system) : @(import({builtin:?})) in
! system/stdio/write_line "cross-target" {{
  do s <- ! numeric/int/add 20 22;
  ! process/exit s
}}
"#
    );
    let c_src = format!(
        r#"
@[package(library(c, export(root, symbol("c_add"))), name(example/c))]
param val (/Thk; /Ret; /Int; /Int64; /numeric) : @(import({builtin:?})) in
({{ fn (a : Int) (b : Int64) =>
  do _ <- ! numeric/int64/add b (1 : Int64);
  ! numeric/int/add a a
}} : Thk (Int -> Int64 -> Ret Int))
"#
    );
    let unit_src = r#"
@[package(library(zydeco), name(example/unit))]
let Thk = @(intrinsic(thk)) in
let Ret = @(intrinsic(ret)) in
let I = @(intrinsic(int)) in
(#make = ({ fn (x : I) => ret { fn (y : I) => ret (x, y) } } : Thk (I -> Ret (Thk (I -> Ret (I * I))))))
"#;

    let proc_low = lower_process(&proc_src);
    let (c_low, c_entry) = lower_c_export(&c_src);
    let unit_low = lower_unit(unit_src);
    let unit_sym = ForeignSymbolName::parse("zydeco_unit_example_unit_init").unwrap();

    for target in ALL_TARGETS {
        let guard1 = emit_guard_object(target, &c_entry.guard).unwrap();
        let guard2 = emit_guard_object(target, &c_entry.guard).unwrap();
        assert_object_magic(target, &guard1);
        assert_eq!(guard1, guard2, "guard object emission not deterministic for {target:?}");

        for &strategy in RepresentationStrategy::ALL {
            let entries: [(&SpsLowProgram, NativeEntry); 3] = [
                (&proc_low, NativeEntry::Process),
                (&c_low, NativeEntry::C(c_entry.clone())),
                (&unit_low, NativeEntry::Unit(unit_sym.clone())),
            ];
            for (program, entry) in entries {
                let first = Emitter::new(program, target)
                    .with_entry(entry.clone())
                    .with_representation(strategy)
                    .run()
                    .unwrap();
                let second = Emitter::new(program, target)
                    .with_entry(entry)
                    .with_representation(strategy)
                    .run()
                    .unwrap();
                assert_object_magic(target, &first.object);
                assert_eq!(
                    first.object, second.object,
                    "emission not deterministic for {target:?} / {strategy:?}"
                );
            }
        }
    }
}
