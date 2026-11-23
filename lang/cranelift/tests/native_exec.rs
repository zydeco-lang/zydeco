use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::{Arc, OnceLock};
use tempfile::TempDir;
use zydeco_cranelift::{CExportEntry, Emitter, NativeEntry, NativeTarget, emit_guard_object};
use zydeco_foundation::pass::CompilerPass;
use zydeco_session::CompilerSession;
use zydeco_session::source::{PackageId, Project};
use zydeco_stackir::low::representation::RepresentationStrategy;
use zydeco_stackir::{RootLowerer, SpsLowPipeline, SpsLowProgram, SpsScheme};
use zydeco_surface::metadata::{LibraryRole, PackageRole};
use zydeco_syntax::ForeignSymbolName;

struct RuntimeArchives {
    _process_dir: TempDir,
    process_lib: PathBuf,
    main_shim: PathBuf,
    _library_dir: TempDir,
    library_lib: PathBuf,
}

static RUNTIMES: OnceLock<RuntimeArchives> = OnceLock::new();

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap()
}

fn builtin_path() -> String {
    repo_root().join("lib/std/builtin.zy").to_str().unwrap().to_owned()
}

fn build_runtime_archive(with_process_entry: bool) -> (TempDir, PathBuf) {
    let dir = tempfile::tempdir().unwrap();
    let manifest = repo_root().join("runtime/Cargo.toml");
    let mut cmd = Command::new("cargo");
    cmd.args(["build", "--manifest-path"])
        .arg(&manifest)
        .args(["--lib", "--target-dir"])
        .arg(dir.path());
    if !with_process_entry {
        cmd.arg("--no-default-features");
    }
    let output = cmd.output().unwrap();
    assert!(
        output.status.success(),
        "building runtime archive failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let archive = dir.path().join("debug/libzydeco_runtime.a");
    assert!(archive.is_file(), "missing archive at {}", archive.display());
    (dir, archive)
}

fn runtimes() -> &'static RuntimeArchives {
    RUNTIMES.get_or_init(|| {
        let (_process_dir, process_lib) = build_runtime_archive(true);
        let main_c = _process_dir.path().join("main_shim.c");
        let main_shim = _process_dir.path().join("main_shim.o");
        std::fs::write(
            &main_c,
            "extern void zydeco_clif_run_process(void);\nint main(void) { zydeco_clif_run_process(); return 0; }\n",
        )
        .unwrap();
        let status = Command::new("cc").args(["-c"]).arg(&main_c).arg("-o").arg(&main_shim).status().unwrap();
        assert!(status.success(), "failed to compile main_shim.c");
        let (_library_dir, library_lib) = build_runtime_archive(false);
        RuntimeArchives { _process_dir, process_lib, main_shim, _library_dir, library_lib }
    })
}

fn compile_process_program(source: &str, strategy: RepresentationStrategy) -> Vec<u8> {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("main.zy");
    std::fs::write(&file, source).unwrap();
    let session = CompilerSession::default();
    let analysis = session
        .analyze_package(&PackageId { path: file, name: None }, Arc::new(Project::default()))
        .unwrap();
    let exec = session.executable_program(&analysis).unwrap();
    let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
        .run_with_builtin(&exec.core, Some(&exec.builtin));
    let low = SpsLowPipeline.run_infallible(frag);
    low.validate().unwrap();
    let target = NativeTarget::host().unwrap();
    Emitter::new(&low, target)
        .with_entry(NativeEntry::Process)
        .with_representation(strategy)
        .run()
        .unwrap()
        .object
}

fn compile_unit_program(source: &str, symbol: &str) -> Vec<u8> {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("unit.zy");
    std::fs::write(&file, source).unwrap();
    let session = CompilerSession::default();
    let analysis = session
        .analyze_package(&PackageId { path: file, name: None }, Arc::new(Project::default()))
        .unwrap();
    let unit = session.unit_program(&analysis).unwrap();
    let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
        .run_with_builtin(&unit.unit.initializer.core, unit.unit.initializer.builtin.as_ref());
    let low = SpsLowPipeline.run_infallible(frag);
    low.validate().unwrap();
    let target = NativeTarget::host().unwrap();
    let sym = ForeignSymbolName::parse(symbol).unwrap();
    Emitter::new(&low, target).with_entry(NativeEntry::Unit(sym)).run().unwrap().object
}

fn compile_library_exports(source: &str, guard_symbol: &str) -> Vec<(String, Vec<u8>)> {
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
    let target = NativeTarget::host().unwrap();
    let guard = ForeignSymbolName::parse(guard_symbol).unwrap();

    let mut objects = Vec::with_capacity(1 + lib.library.exports.len());
    objects.push(("guard".to_owned(), emit_guard_object(target, &guard).unwrap()));
    for export in &lib.library.exports {
        let frag = RootLowerer { scheme: SpsScheme::BranchJoin }
            .run_with_builtin(&export.core, export.builtin.as_ref());
        let low: SpsLowProgram = SpsLowPipeline.run_infallible(frag);
        low.validate().unwrap();
        let entry = NativeEntry::C(CExportEntry {
            symbol: export.symbol.clone(),
            signature: export.signature.clone(),
            guard: guard.clone(),
        });
        let bytes = Emitter::new(&low, target).with_entry(entry).run().unwrap().object;
        objects.push((export.symbol.as_str().to_owned(), bytes));
    }
    objects
}

fn link_and_run_process(objects: &[&[u8]], args: &[&str]) -> Output {
    let rt = runtimes();
    let dir = tempfile::tempdir().unwrap();
    let mut cmd = Command::new("cc");
    for (idx, bytes) in objects.iter().enumerate() {
        let obj = dir.path().join(format!("mod_{idx}.o"));
        std::fs::write(&obj, bytes).unwrap();
        cmd.arg(obj);
    }
    let exe = dir.path().join("prog");
    cmd.arg(&rt.main_shim).arg(&rt.process_lib);
    add_platform_link_libs(&mut cmd);
    cmd.arg("-o").arg(&exe);
    let link = cmd.output().unwrap();
    assert!(
        link.status.success(),
        "link failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&link.stdout),
        String::from_utf8_lossy(&link.stderr)
    );
    Command::new(&exe).args(args).output().unwrap()
}

fn add_platform_link_libs(cmd: &mut Command) {
    #[cfg(target_os = "linux")]
    {
        cmd.args(["-lpthread", "-ldl", "-lm"]);
    }
    #[cfg(target_os = "macos")]
    {
        let _ = cmd;
    }
}

fn assert_run_ok(output: &Output) {
    assert!(
        output.status.success(),
        "program exited with {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn local_letlabel_contification_loop_sums_100k() {
    let builtin = builtin_path();
    let source = format!(
        r#"
param (/OS; /Unit; /Int; /Int64; /numeric; /process; /system) : @(import({builtin:?})) in
let fix loop (i : Int) (acc : Int64) : OS =
  ! numeric/int/gt OS i 100000
    {{
      do s <- ! numeric/int64/to_string acc;
      ! system/stdio/write_line s {{ ! process/exit 0 }}
    }}
    {{
      do i64 <- ! numeric/int64/from_int i;
      do next_acc <- ! numeric/int64/add acc i64;
      do next_i <- ! numeric/int/add i 1;
      ! loop next_i next_acc
    }}
in
do zero64 <- ! numeric/int64/from_int 0;
! loop 1 zero64
"#
    );
    let obj = compile_process_program(&source, RepresentationStrategy::default());
    let out = link_and_run_process(&[&obj], &[]);
    assert_run_ok(&out);
    assert_eq!(String::from_utf8(out.stdout).unwrap(), "5000050000\n");
}

#[test]
fn cheney_gc_preserves_products_boxed_scalars_closures_and_strings() {
    let builtin = builtin_path();
    let source = format!(
        r#"
param (/OS; /Unit; /Int; /Int64; /Float64; /String; /Thk; /Ret; /numeric; /text; /process; /system) : @(import({builtin:?})) in
do keep_i64 <- ! numeric/int64/from_int 1234567;
let keep_f64 = (3.25 : Float64) in
let make_fn = {{ fn (base : Int64) =>
  ret {{ fn (delta : Int64) => ! numeric/int64/add base delta }}
}} in
do closure <- ! make_fn keep_i64;
do dyn_str <- ! text/string/append "hello-" "gc";
let flat4 = (keep_i64, (keep_f64, (closure, dyn_str))) in
let (first_i64, suffix_rest) = flat4 in
let fix churn (n : Int) (acc : (Int * Int)) : Ret (Int * Int) =
  ! numeric/int/eq (Ret (Int * Int)) n 0
    {{ ret acc }}
    {{
      do garbage_str <- ! text/string/append dyn_str "!";
      let (a, b) = acc in
      do a1 <- ! numeric/int/add a 1;
      do next <- ! numeric/int/sub n 1;
      ! churn next (b, a1)
    }}
in
do (x, y) <- ! churn 6000 (10, 20);
let (live_f64, (live_closure, live_str)) = suffix_rest in
do res64 <- ! live_closure (1 : Int64);
do fsum <- ! numeric/float64/add live_f64 0.5;
do s0 <- ! numeric/int64/to_string first_i64;
do s1 <- ! numeric/int64/to_string res64;
do s2 <- ! text/string/append live_str ":";
do s3 <- ! text/string/append s2 s1;
do fstr <- ! numeric/float64/to_string fsum;
do s4 <- ! text/string/append s3 ":";
do s5 <- ! text/string/append s4 fstr;
do s6 <- ! text/string/append s5 ":";
do s7 <- ! text/string/append s6 s0;
! system/stdio/write_line s7 {{
  do xy <- ! numeric/int/add x y;
  ! numeric/int/eq OS xy 6030 {{ ! process/exit 0 }} {{ ! process/exit 9 }}
}}
"#
    );
    for &strategy in RepresentationStrategy::ALL {
        let obj = compile_process_program(&source, strategy);
        let out = link_and_run_process(&[&obj], &[]);
        assert_run_ok(&out);
        assert_eq!(String::from_utf8(out.stdout).unwrap(), "hello-gc:1234568:3.75:1234567\n");
    }
}

#[test]
fn nullary_and_payload_constructors_and_cocase_dispatch() {
    let builtin = builtin_path();
    let source = format!(
        r#"
param (/OS; /Unit; /Int; /Ret; /Thk; /VType; /CType; /numeric; /process) : @(import({builtin:?})) in
begin
  def List (A : VType) : VType =
    data
    | +Nil : Unit
    | +Cons : A * List A
    end
  that
  def Acc : CType =
    codata
    | .step : Int -> Acc
    | .done : Ret Int
    end
  that
  let fix build_acc (total : Int) : Acc =
    comatch
    | .step n =>
      do next <- ! numeric/int/add total n;
      ! build_acc next
    | .done => ret total
    end
  in
  let fix sum_list (xs : List Int) (acc : Thk Acc) : Ret Int =
    match xs
    | +Nil () => ! acc .done
    | +Cons (head, tail) =>
      ! sum_list tail {{ ! acc .step head }}
    end
  in
  let list = (+Cons (10, (+Cons (20, (+Cons (12, +Nil ()))))) : List Int) in
  do total <- ! sum_list list {{ ! build_acc 0 }};
  ! numeric/int/eq OS total 42 {{ ! process/exit 0 }} {{ ! process/exit 7 }}
end
"#
    );
    let obj = compile_process_program(&source, RepresentationStrategy::default());
    let out = link_and_run_process(&[&obj], &[]);
    assert_run_ok(&out);
}

#[test]
fn fixed_width_ints_edge_div_rem_nan_and_memory_kernel() {
    let builtin = builtin_path();
    let source = format!(
        r#"
param (/OS; /Unit; /String; /Int; /Int8; /Int16; /Int32; /Int64; /UInt8; /UInt16; /UInt32; /UInt64; /UInt; /Float32; /Float64; /numeric; /system; /process) : @(import({builtin:?})) in
let fail = {{ fn (_ : Int) => ! process/exit 91 }} in
do wrap8 <- ! numeric/int8/add (127 : Int8) (1 : Int8);
! numeric/int8/eq OS wrap8 (-128 : Int8) {{
  do min64_div <- ! numeric/int64/div (-9223372036854775808 : Int64) (-1 : Int64);
  do min64_rem <- ! numeric/int64/mod (-9223372036854775808 : Int64) (-1 : Int64);
  ! numeric/int64/eq OS min64_div (-9223372036854775808 : Int64) {{
    ! numeric/int64/eq OS min64_rem (0 : Int64) {{
      do nan <- ! numeric/float32/div (0.0 : Float32) (0.0 : Float32);
      ! numeric/float32/lt OS nan (1.0 : Float32) {{ ! process/exit 92 }} {{
        ! numeric/float32/eq OS nan nan {{ ! process/exit 93 }} {{
          ! system/memory/allocate OS 16 8 fail {{ fn base =>
            ! numeric/int64/store_le OS base (100 : Int64) {{
              let fix kernel (n : Int) : OS =
                ! numeric/int/eq OS n 0
                  {{
                    ! numeric/int64/load_le OS base {{ fn loaded =>
                      ! system/memory/free OS base 16 8 fail {{
                        ! numeric/int64/eq OS loaded (150 : Int64) {{ ! process/exit 0 }} {{ ! process/exit 94 }}
                      }}
                    }}
                  }}
                  {{
                    ! numeric/int64/load_le OS base {{ fn cur =>
                      do sum <- ! numeric/int64/add cur (5 : Int64);
                      ! numeric/int64/store_le OS base sum {{
                        do next <- ! numeric/int/sub n 1;
                        ! kernel next
                      }}
                    }}
                  }}
              in
              ! kernel 10
            }}
          }}
        }}
      }}
    }} {{ ! process/exit 95 }}
  }} {{ ! process/exit 96 }}
}} {{ ! process/exit 97 }}
"#
    );
    let obj = compile_process_program(&source, RepresentationStrategy::default());
    let out = link_and_run_process(&[&obj], &[]);
    assert_run_ok(&out);
}

const C_HARNESS_SRC: &str = r#"
#include <assert.h>
#include <stdint.h>
#include <string.h>
extern int8_t zy_id_i8(int8_t);
extern int16_t zy_id_i16(int16_t);
extern int32_t zy_id_i32(int32_t);
extern int64_t zy_id_i64(int64_t);
extern int64_t zy_id_int(int64_t);
extern uint8_t zy_id_u8(uint8_t);
extern uint16_t zy_id_u16(uint16_t);
extern uint32_t zy_id_u32(uint32_t);
extern uint64_t zy_id_u64(uint64_t);
extern uint64_t zy_id_uint(uint64_t);
extern uint64_t zy_wide(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
extern int64_t zy_reenter(void);

int64_t c_reenter_callback(void) { return zy_id_int(7); }

int main(int argc, char **argv) {
    if (argc > 1 && strcmp(argv[1], "--reenter") == 0) {
        return (int)zy_reenter();
    }
    assert(zy_id_i8(-42) == -42);
    assert(zy_id_i16(-12345) == -12345);
    assert(zy_id_i32(-1000000000) == -1000000000);
    assert(zy_id_i64(INT64_MIN) == INT64_MIN);
    assert(zy_id_int(-123456789012345LL) == -123456789012345LL);
    assert(zy_id_u8(250) == 250);
    assert(zy_id_u16(65000) == 65000);
    assert(zy_id_u32(4000000000U) == 4000000000U);
    assert(zy_id_u64(UINT64_MAX) == UINT64_MAX);
    assert(zy_id_uint(4611686018427387903ULL) == 4611686018427387903ULL);
    for (int i = 0; i < 4; i++) {
        uint64_t res = zy_wide(UINT64_MAX, 1, 10, 20, 30, 40);
        assert(res == 100);
    }
    return 0;
}
"#;

#[test]
fn c_export_library_round_trips_ints_reuses_instance_and_rejects_reentrancy() {
    let builtin = builtin_path();
    let lib_src = format!(
        r#"
@[package(library(c,
  export(field(Int8), symbol("zy_id_i8")),
  export(field(Int16), symbol("zy_id_i16")),
  export(field(Int32), symbol("zy_id_i32")),
  export(field(Int64), symbol("zy_id_i64")),
  export(field(Int), symbol("zy_id_int")),
  export(field(UInt8), symbol("zy_id_u8")),
  export(field(UInt16), symbol("zy_id_u16")),
  export(field(UInt32), symbol("zy_id_u32")),
  export(field(UInt64), symbol("zy_id_u64")),
  export(field(UInt), symbol("zy_id_uint")),
  export(field(wide), symbol("zy_wide")),
  export(field(reenter), symbol("zy_reenter"))
), name(example/cexp))]
param val (/Thk; /Ret; /Unit; /Int; /Int8; /Int16; /Int32; /Int64; /UInt; /UInt8; /UInt16; /UInt32; /UInt64; /numeric) : @(import({builtin:?})) in
(
  #Int8 = ({{ fn x => ret x }} : Thk (Int8 -> Ret Int8)),
  #Int16 = ({{ fn x => ret x }} : Thk (Int16 -> Ret Int16)),
  #Int32 = ({{ fn x => ret x }} : Thk (Int32 -> Ret Int32)),
  #Int64 = ({{ fn x => ret x }} : Thk (Int64 -> Ret Int64)),
  #Int = ({{ fn x => ret x }} : Thk (Int -> Ret Int)),
  #UInt8 = ({{ fn x => ret x }} : Thk (UInt8 -> Ret UInt8)),
  #UInt16 = ({{ fn x => ret x }} : Thk (UInt16 -> Ret UInt16)),
  #UInt32 = ({{ fn x => ret x }} : Thk (UInt32 -> Ret UInt32)),
  #UInt64 = ({{ fn x => ret x }} : Thk (UInt64 -> Ret UInt64)),
  #UInt = ({{ fn x => ret x }} : Thk (UInt -> Ret UInt)),
  #wide = ({{ fn a b c d e f =>
    let fix churn (n : Int) : Ret Unit =
      ! numeric/int/eq (Ret Unit) n 0 {{ ret () }}
        {{ do next <- ! numeric/int/sub n 1; ! churn next }}
    in
    do () <- ! churn 2000;
    do ab <- ! numeric/uint64/add a b;
    do abc <- ! numeric/uint64/add ab c;
    do abcd <- ! numeric/uint64/add abc d;
    do abcde <- ! numeric/uint64/add abcd e;
    ! numeric/uint64/add abcde f
  }} : Thk (UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> Ret UInt64)),
  #reenter = ({{
    let cb = (@(ffi(c, library("harness"), symbol("c_reenter_callback"))) : Thk (Ret Int)) in
    ! cb
  }} : Thk (Ret Int))
)
"#
    );

    let objects = compile_library_exports(&lib_src, "zydeco_test_cexp_guard");
    let dir = tempfile::tempdir().unwrap();
    let harness_c = dir.path().join("harness.c");
    std::fs::write(&harness_c, C_HARNESS_SRC).unwrap();

    let rt = runtimes();
    let exe = dir.path().join("c_harness");
    let mut cmd = Command::new("cc");
    cmd.arg(&harness_c);
    for (name, bytes) in &objects {
        let obj = dir.path().join(format!("{name}.o"));
        std::fs::write(&obj, bytes).unwrap();
        cmd.arg(obj);
    }
    cmd.arg(&rt.library_lib);
    add_platform_link_libs(&mut cmd);
    cmd.arg("-o").arg(&exe);
    let link = cmd.output().unwrap();
    assert!(
        link.status.success(),
        "C harness link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    assert_run_ok(&Command::new(&exe).output().unwrap());

    let reenter = Command::new(&exe).arg("--reenter").output().unwrap();
    assert!(!reenter.status.success(), "reentrant entry should have aborted");
    let stderr = String::from_utf8_lossy(&reenter.stderr);
    assert!(stderr.contains("reentrant entry"), "unexpected stderr: {stderr}");
}

#[test]
fn native_unit_object_export_and_closure_call() {
    let builtin = builtin_path();
    let unit_src = format!(
        r#"
@[package(library(zydeco), name(example/producer))]
param val (/Thk; /Ret; /Unit; /Int; /Int64; /numeric) : @(import({builtin:?})) in
(
  #make_adder = ({{ fn (delta : Int64) =>
    ret {{ fn (value : Int64) =>
      let fix churn (n : Int) : Ret Unit =
        ! numeric/int/eq (Ret Unit) n 0 {{ ret () }}
          {{ do next <- ! numeric/int/sub n 1; ! churn next }}
      in
      do () <- ! churn 2000;
      ! numeric/int64/add delta value
    }}
  }} : Thk (Int64 -> Ret (Thk (Int64 -> Ret Int64)))),
  #offset = (40 : Int64)
)
"#
    );
    let client_src = format!(
        r#"
param (/OS; /Unit; /Int64; /Thk; /Ret; /numeric; /process; /system) : @(import({builtin:?})) in
let init = (@(ffi(zydeco, library("example.producer"), symbol("zydeco_unit_example_producer_init")))
    : Thk (Ret (
        (#make_adder :: Thk (Int64 -> Ret (Thk (Int64 -> Ret Int64))))
      * (#offset :: Int64)
    )))
in
do api <- ! init;
do adder <- ! api/make_adder api/offset;
do res <- ! adder (2 : Int64);
do s <- ! numeric/int64/to_string res;
! system/stdio/write_line s {{ ! process/exit 0 }}
"#
    );

    let unit_obj = compile_unit_program(&unit_src, "zydeco_unit_example_producer_init");
    let client_obj = compile_process_program(&client_src, RepresentationStrategy::default());
    let out = link_and_run_process(&[&unit_obj, &client_obj], &[]);
    assert_run_ok(&out);
    assert_eq!(String::from_utf8(out.stdout).unwrap(), "42\n");
}
