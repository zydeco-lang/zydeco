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
    fn write(&self, name: &str, text: &str) -> PathBuf {
        let path = self.directory.path().join(name);
        std::fs::write(&path, text).unwrap();
        path
    }
    fn command(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(self.directory.path())
            .args(args)
            .output()
            .unwrap()
    }
    fn checked(&self, text: &str, expected: Result<(), &str>) {
        self.write("library.zy", text);
        let output = self.command(&["check", "./library.zy"]);
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.success(), expected.is_ok(), "{diagnostic}");
        if let Err(message) = expected {
            assert!(diagnostic.contains(message), "{diagnostic}");
        }
        assert!(!diagnostic.contains("panicked"), "{diagnostic}");
        assert!(!self.directory.path().join("build").exists());
    }
    fn source(role: &str, body: &str) -> String {
        format!(
            "@[package({role}, name(example/math))]\nlet Thk = @(intrinsic(thk)) in let Ret = @(intrinsic(ret)) in let I = @(intrinsic(i64)) in let Unit = @(intrinsic(unit)) in {body}"
        )
    }
}

#[test]
fn compiled_role_requires_an_explicit_complete_contract() {
    let fixture = Fixture::new();
    for (role, error) in [
        ("library(c)", "at least one explicit export"),
        ("library(rust, export(root, symbol(\"f\")))", "requires the C ABI"),
        ("library(c, export(root, symbol(\"zydeco_private\")))", "reserved zydeco_"),
        (
            "library(c, export(root, symbol(\"f\")), export(field(x), symbol(\"g\")))",
            "cannot be combined",
        ),
        (
            "library(c, export(field(x), symbol(\"f\")), export(field(x), symbol(\"g\")))",
            "duplicate compiled-library",
        ),
        (
            "library(c, export(field(x), symbol(\"f\")), export(field(y), symbol(\"f\")))",
            "duplicate compiled-library",
        ),
    ] {
        fixture.checked(&Fixture::source(role, "()"), Err(error));
    }
    fixture.checked(
        "@[package(library(c, export(root, symbol(\"f\"))))] ()",
        Err("explicit package name"),
    );
    fixture.checked(&Fixture::source("library", "param val (x : I) in x"), Ok(()));
}

#[test]
fn exports_are_checked_after_field_selection_and_static_specialization() {
    let fixture = Fixture::new();
    let role = "library(c, export(field(api/id), symbol(\"identity\")))";
    fixture.checked(
        &Fixture::source(
            role,
            "(#api = (#id = ({ fn (x : I) => ret x } : Thk (I -> Ret I))), #unused = (_ : I))",
        ),
        Ok(()),
    );
    fixture.checked(&Fixture::source(role, "let api = (#api = (#id = ({ fn (x : I) => ret x } : Thk (I -> Ret I))), #unused = (_ : I)) in api"), Ok(()));
    fixture.checked(&Fixture::source(role, "let Function = val pi (_ : I) . I in (#api = (#id = ({ fn (x : I) => ret x } : Thk (I -> Ret I))), #private = ({ fn (f : Function) => ret (f 0) } : Thk (Function -> Ret I)))"), Ok(()));
    fixture.checked(&Fixture::source(role, "let val identity (x : I) : I = x in (#api = (#id = ({ fn (x : I) => ret (identity x) } : Thk (I -> Ret I))))"), Ok(()));
    fixture.checked(
        &Fixture::source(
            role,
            "(#api = (#id = ({ fn (x : I) => ret (_ : I) } : Thk (I -> Ret I))))",
        ),
        Err("unfilled value or computation hole"),
    );
    fixture.checked(&Fixture::source(role, "(#api = (#other = 0))"), Err("id"));
    fixture.checked(&Fixture::source(role, "(#api = (#id = 0, #id = 1))"), Err("id"));
    fixture.checked(
        &Fixture::source(role, "(#api = (#id = param val (x : I) in x))"),
        Err("requires a thunk"),
    );
    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "({ fn (x : I) => ret x } : Thk (I -> Ret I))",
        ),
        Ok(()),
    );
    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "({ ret () } : Thk (Ret Unit))",
        ),
        Ok(()),
    );
    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "({ fn a b c d e f g => ret a } : Thk (I -> I -> I -> I -> I -> I -> I -> Ret I))",
        ),
        Err("at most 6"),
    );
}

#[test]
fn named_entries_are_closed_and_c_exports_reject_incoming_windows() {
    let fixture = Fixture::new();
    fixture.write("workspace.zy", "let captured = 1 in @[package(library(c, export(root, symbol(\"f\"))), name(captured))] captured");
    let output = fixture.command(&["check", "-p", "captured"]);
    assert!(!output.status.success());
    let diagnostic = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostic.contains("captured") && !diagnostic.contains("panicked"), "{diagnostic}");
    std::fs::remove_file(fixture.directory.path().join("workspace.zy")).unwrap();
    let builtin =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib/std/builtin.zy").canonicalize().unwrap();
    fixture.checked(&format!("@[package(library(c, export(root, symbol(\"f\"))), name(window))] param val (/Thk; /Ret; /Access; /Addr; /Int64) : @(import({:?})) in ({{ fn (window : Access * Addr * Int64) => ret 0 }} : Thk ((Access * Addr * Int64) -> Ret Int64))", builtin.to_str().unwrap()), Err("incoming memory grants"));
}

#[test]
#[ignore = "requires NASM, AMD64 native tools, and builds the matching Rust runtime"]
fn shared_library_c_harness_round_trips_wide_arguments_and_has_only_declared_exports() {
    let fixture = Fixture::new();
    let integers = ["Int8", "Int16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64"];
    let mut contracts = integers
        .iter()
        .map(|integer| format!("export(field({integer}), symbol(\"id_{integer}\"))"))
        .collect::<Vec<_>>();
    contracts.extend([
        "export(field(zero), symbol(\"zero\"))".into(),
        "export(field(six), symbol(\"six\"))".into(),
    ]);
    let mut fields = integers
        .iter()
        .map(|integer| {
            format!("#{integer} = ({{ fn x => ret x }} : Thk ({integer} -> Ret {integer}))")
        })
        .collect::<Vec<_>>();
    fields.extend(["#zero = ({ ret () } : Thk (Ret Unit))".into(), "#six = ({ fn a b c d e f => ret f } : Thk (Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Ret Int64))".into()]);
    fixture.write("library.zy", &format!("@[package(library(c, {}), name(example/math))]\nparam val (/Thk; /Ret; /Unit; {}) : @(import({:?})) in ({})", contracts.join(", "), integers.iter().map(|integer| format!("/{integer}")).collect::<Vec<_>>().join("; "), Fixture::builtin(), fields.join(", ")));
    let runtime = Path::new(env!("CARGO_MANIFEST_DIR")).join("../runtime").canonicalize().unwrap();
    let output = fixture.command(&[
        "build",
        "./library.zy",
        "--target",
        "sharedlib",
        "--target-arch",
        "x86-64",
        "--runtime-dir",
        runtime.to_str().unwrap(),
    ]);
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let manifest_path = fixture.directory.path().join("build/example.math.sharedlib.library.json");
    let manifest: zydeco_cli::library::LibraryManifest =
        serde_json::from_slice(&std::fs::read(&manifest_path).unwrap()).unwrap();
    let manifest_path = manifest_path.canonicalize().unwrap();
    let base = manifest_path.parent().unwrap();
    let header = base.join(&manifest.header.path);
    let artifact = base.join(&manifest.artifact.path);
    let symbols = Command::new("nm")
        .args(if cfg!(target_os = "macos") { vec!["-gU"] } else { vec!["-D", "--defined-only"] })
        .arg(&artifact)
        .output()
        .unwrap();
    Fixture::success(&symbols);
    let visible =
        String::from_utf8_lossy(&symbols.stdout)
            .lines()
            .filter_map(|line| line.split_whitespace().last())
            .map(|name| {
                if cfg!(target_os = "macos") {
                    name.strip_prefix('_').unwrap_or(name)
                } else {
                    name
                }
            })
            .map(str::to_owned)
            .collect::<std::collections::BTreeSet<_>>();
    let expected = manifest.exports.iter().map(|export| export.symbol.to_string()).collect();
    assert_eq!(visible, expected);
    let checks = integers.iter().map(|integer| {
        let c = integer.to_ascii_uppercase();
        let minimum = if integer.starts_with('U') { "0".into() } else { format!("{c}_MIN") };
        format!("assert(id_{integer}({minimum}) == {minimum}); assert(id_{integer}({c}_MAX) == {c}_MAX);")
    }).collect::<String>();
    let source = fixture.write("harness.c", &format!("#include {:?}\n#include <assert.h>\nint main(void) {{ for (int i = 0; i < 100; ++i) {{ {checks} assert(six(1,2,3,4,5,INT64_MIN) == INT64_MIN); zero(); }} return 0; }}\n", header.to_str().unwrap()));
    let exe = fixture.directory.path().join("harness");
    let mut cc = Command::new("cc");
    if cfg!(target_os = "macos") {
        cc.args(["-arch", "x86_64"]);
    } else {
        cc.arg("-m64");
    }
    let output = cc
        .arg("-O2")
        .arg(source)
        .arg(&artifact)
        .arg(format!("-Wl,-rpath,{}", artifact.parent().unwrap().display()))
        .arg("-o")
        .arg(&exe)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert!(Command::new(exe).status().unwrap().success());
    // A source consumer checks against the generated binding alone.
    std::fs::remove_file(fixture.directory.path().join("library.zy")).unwrap();
    let output = fixture.command(&["check", base.join(manifest.bindings.path).to_str().unwrap()]);
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

impl Fixture {
    fn runtime() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../runtime").canonicalize().unwrap()
    }

    fn builtin() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib/std/builtin.zy").canonicalize().unwrap()
    }

    fn build(
        &self, source: &Path, kind: &str, directory: &str, dependencies: &[PathBuf],
    ) -> Output {
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        command
            .current_dir(self.directory.path())
            .arg("build")
            .arg(source)
            .args(["--target", kind, "--target-arch", "x86-64", "--build-dir", directory])
            .arg("--runtime-dir")
            .arg(Self::runtime());
        for dependency in dependencies {
            command.arg("--link-library").arg(dependency);
        }
        command.output().unwrap()
    }

    fn manifest(&self, directory: &str, package: &str, kind: &str) -> PathBuf {
        self.directory.path().join(directory).join(format!("{package}.{kind}.library.json"))
    }

    fn library(path: &Path) -> (PathBuf, zydeco_cli::library::LibraryManifest) {
        let path = path.canonicalize().unwrap();
        let manifest = serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
        (path.parent().unwrap().to_owned(), manifest)
    }

    fn success(output: &Output) {
        assert!(
            output.status.success(),
            "stdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn run(executable: &Path) -> Output {
        use std::{
            process::Stdio,
            time::{Duration, Instant},
        };
        let mut child =
            Command::new(executable).stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().unwrap();
        let deadline = Instant::now() + Duration::from_secs(30);
        while child.try_wait().unwrap().is_none() {
            if Instant::now() >= deadline {
                child.kill().unwrap();
                let output = child.wait_with_output().unwrap();
                panic!(
                    "native harness did not terminate: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        child.wait_with_output().unwrap()
    }

    fn harness(&self, manifest: &Path, body: &str) -> PathBuf {
        use zydeco_cli::library::{LibraryDigest, LibraryPlatform, LinkedLibraries};
        let (base, interface) = Self::library(manifest);
        let source = self.write(
            "harness.c",
            &format!(
                "#include {:?}\n#include <assert.h>\n{body}\n",
                base.join(&interface.header.path)
            ),
        );
        let executable = self.directory.path().join("harness");
        let mut command = Command::new("cc");
        if cfg!(target_os = "macos") {
            command.args(["-arch", "x86_64"]);
        } else {
            command.arg("-m64");
        }
        command.arg(source);
        let libraries = LinkedLibraries::load(&[manifest.to_owned()]).unwrap();
        libraries
            .validate_imports(
                &[],
                interface.platform,
                Some(&LibraryDigest::runtime(&Self::runtime()).unwrap()),
                false,
            )
            .unwrap();
        libraries
            .configure_linker(&mut command, &[], interface.platform, self.directory.path())
            .unwrap();
        if let Some(support) = &interface.support {
            command.arg(base.join(&support.path));
        }
        if interface.platform == LibraryPlatform::Linux {
            command.args(["-ldl", "-lpthread", "-lm"]);
        }
        command.arg("-o").arg(&executable);
        Self::success(&command.output().unwrap());
        executable
    }
}

#[test]
fn library_factory_accepts_only_a_builtin_value_domain() {
    let fixture = Fixture::new();
    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "let factory = param val (() : Unit) in ({ ret () } : Thk (Ret Unit)) in factory",
        ),
        Ok(()),
    );
    let builtin = Fixture::builtin();
    fixture.checked(
        &format!(
            r#"@[package(library(c, export(root, symbol("f"))), name(example/composed))]
let factory = param val (/Thk; /Ret; /Int64; /numeric) : @(import({builtin:?})) in
({{ fn x => ! numeric/int64/add x 1 }} : Thk (Int64 -> Ret Int64)) in factory"#
        ),
        Ok(()),
    );

    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "param val (() : Unit) in ({ ret () } : Thk (Ret Unit))",
        ),
        Ok(()),
    );
    fixture.checked(
        &Fixture::source(
            "library(c, export(root, symbol(\"f\")))",
            "param val (n : I) in ({ ret n } : Thk (Ret I))",
        ),
        Err("Builtin"),
    );
}

#[test]
#[ignore = "requires NASM, AMD64 native tools, and builds matching Rust runtimes"]
fn nested_units_preserve_gc_roots_with_shared_and_raw_runtime_support() {
    let fixture = Fixture::new();
    let inner = fixture.write(
        "inner.zy",
        &format!(
            r#"
@[package(library(c, export(root, symbol("inner_identity"))), name(example/inner))]
param val (/Thk; /Ret; /Int64; /numeric) : @(import({:?})) in
({{ fn x =>
    let fix loop (n : Int64) : Ret Int64 =
        ! numeric/int64/eq (Ret Int64) n 0 {{ ret x }}
            {{ do next <- ! numeric/int64/sub n 1; ! loop next }}
    in ! loop 100000
}} : Thk (Int64 -> Ret Int64))
"#,
            Fixture::builtin()
        ),
    );
    for (index, (inner_kind, outer_kind)) in
        [("sharedlib", "sharedlib"), ("staticlib", "sharedlib"), ("object", "staticlib")]
            .into_iter()
            .enumerate()
    {
        let inner_dir = format!("inner-{index}");
        Fixture::success(&fixture.build(&inner, inner_kind, &inner_dir, &[]));
        let inner_manifest = fixture.manifest(&inner_dir, "example.inner", inner_kind);
        let (base, interface) = Fixture::library(&inner_manifest);
        let outer = fixture.write(
            "outer.zy",
            &format!(
                r#"
@[package(library(c, export(root, symbol("outer_identity"))), name(example/outer))]
param val (/Thk; /Ret; /Int64; /numeric) : @(import({:?})) in
let other = @(import({:?})) in
({{ fn x => do y <- ! other x;
    let fix loop (n : Int64) : Ret Int64 =
        ! numeric/int64/eq (Ret Int64) n 0
            {{ do delta <- ! numeric/int64/sub y x; ! numeric/int64/add x delta }}
            {{ do next <- ! numeric/int64/sub n 1; ! loop next }}
    in ! loop 100000
}} : Thk (Int64 -> Ret Int64))
"#,
                Fixture::builtin(),
                base.join(&interface.bindings.path)
            ),
        );
        let outer_dir = format!("outer-{index}");
        Fixture::success(&fixture.build(&outer, outer_kind, &outer_dir, &[inner_manifest]));
        let outer_manifest = fixture.manifest(&outer_dir, "example.outer", outer_kind);
        let executable = fixture.harness(&outer_manifest, "int main(void) { assert(outer_identity(INT64_MIN) == INT64_MIN); assert(outer_identity(INT64_MAX) == INT64_MAX); return 0; }");
        Fixture::success(&Fixture::run(&executable));

        // The generated interface and manifest suffice after the producer source is gone.
        std::fs::remove_file(&outer).unwrap();
        let (base, interface) = Fixture::library(&outer_manifest);
        let consumer = fixture.write(
            "consumer.zy",
            &format!(
                r#"
param (/Thk; /Ret; /Int64; /OS; /numeric; /process) : @(import({:?})) in
let identity = @(import({:?})) in
do result <- ! identity -9223372036854775808;
! numeric/int64/eq OS result -9223372036854775808 {{ ! process/exit 0 }} {{ ! process/exit 1 }}
"#,
                Fixture::builtin(),
                base.join(&interface.bindings.path)
            ),
        );
        let client_dir = format!("client-{index}");
        Fixture::success(&fixture.build(&consumer, "exe", &client_dir, &[outer_manifest]));
        Fixture::success(&Fixture::run(
            &fixture.directory.path().join(client_dir).join("consumer.exe"),
        ));
    }
}

#[test]
#[ignore = "requires NASM, AMD64 native tools, and builds matching Rust runtimes"]
fn compiled_entry_rejects_reentry_and_concurrency_before_source_execution() {
    let fixture = Fixture::new();
    let source = fixture.write(
        "library.zy",
        &Fixture::source(
            "library(c, export(root, symbol(\"entry\")))",
            r#"
let again = (@(ffi(c, library("example.math"), symbol("entry"))) : Thk (I -> Ret I)) in
({ fn x => ! again x } : Thk (I -> Ret I))"#,
        ),
    );
    Fixture::success(&fixture.build(&source, "sharedlib", "reentry", &[]));
    let manifest = fixture.manifest("reentry", "example.math", "sharedlib");
    let executable = fixture.harness(&manifest, "int main(void) { return (int)entry(0); }");
    let output = Fixture::run(&executable);
    assert_eq!(output.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&output.stderr).contains("concurrent or reentrant entry"));

    fixture.write(
        "library.zy",
        &Fixture::source(
            "library(c, export(root, symbol(\"entry\")))",
            "({ fn x => let fix loop (n : I) : Ret I = ! loop n in ! loop x } : Thk (I -> Ret I))",
        ),
    );
    Fixture::success(&fixture.build(&source, "sharedlib", "concurrent", &[]));
    let manifest = fixture.manifest("concurrent", "example.math", "sharedlib");
    let executable = fixture.harness(&manifest, r#"
#include <pthread.h>
#include <stdatomic.h>
static atomic_int ready = 0;
static void *worker(void *ignored) {
    (void)ignored;
    atomic_fetch_add(&ready, 1);
    while (atomic_load(&ready) < 2) {}
    entry(0);
    return 0;
}
int main(void) { pthread_t a, b; assert(!pthread_create(&a, 0, worker, 0)); assert(!pthread_create(&b, 0, worker, 0)); pthread_join(a, 0); pthread_join(b, 0); return 0; }
"#);
    let output = Fixture::run(&executable);
    assert_eq!(output.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&output.stderr).contains("concurrent or reentrant entry"));
}

#[test]
#[ignore = "requires NASM, AMD64 native tools, and builds matching Rust runtimes"]
fn failed_late_build_preserves_the_published_library() {
    let fixture = Fixture::new();
    let source = fixture.write(
        "library.zy",
        &Fixture::source(
            "library(c, export(root, symbol(\"identity\")))",
            "({ fn x => ret x } : Thk (I -> Ret I))",
        ),
    );
    Fixture::success(&fixture.build(&source, "sharedlib", "build", &[]));
    let manifest = fixture.manifest("build", "example.math", "sharedlib");
    let original = manifest.canonicalize().unwrap();
    let executable = fixture.harness(
        &manifest,
        "int main(void) { assert(identity(INT64_MIN) == INT64_MIN); return 0; }",
    );
    let runtime = fixture.directory.path().join("broken-runtime");
    std::fs::create_dir(&runtime).unwrap();
    for entry in std::fs::read_dir(Fixture::runtime()).unwrap() {
        let entry = entry.unwrap();
        if entry.path().is_file() {
            std::fs::copy(entry.path(), runtime.join(entry.file_name())).unwrap();
        }
    }
    std::fs::write(
        runtime.join("stub.rs"),
        "compile_error!(\"deliberate runtime compilation failure\");",
    )
    .unwrap();
    let output = fixture.command(&[
        "build",
        source.to_str().unwrap(),
        "--target",
        "sharedlib",
        "--target-arch",
        "x86-64",
        "--runtime-dir",
        runtime.to_str().unwrap(),
    ]);
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("deliberate runtime compilation failure")
    );
    assert_eq!(manifest.canonicalize().unwrap(), original);
    Fixture::success(&Fixture::run(&executable));
    assert!(
        !std::fs::read_dir(fixture.directory.path().join("build")).unwrap().any(|entry| entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(".zydeco-library-"))
    );
}

#[test]
fn arithmetic_example_checks_its_compiled_boundary() {
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib/ffi/arithmetic.zy");
    Fixture::success(&Fixture::new().command(&["check", source.to_str().unwrap()]));
}

#[cfg(unix)]
#[test]
fn interpreter_uses_the_supplied_exact_library_path() {
    use std::collections::BTreeMap;
    use zydeco_cli::CommandCompiler;
    use zydeco_syntax::ForeignLibraryName;

    let fixture = Fixture::new();
    let library = fixture.directory.path().join("different-artifact-name");
    let c = fixture
        .write("host.c", "#include <stdint.h>\nint64_t exact_identity(int64_t x) { return x; }\n");
    let mut cc = Command::new("cc");
    if cfg!(target_os = "macos") {
        cc.arg("-dynamiclib");
    } else {
        cc.args(["-shared", "-fPIC"]);
    }
    Fixture::success(&cc.arg(c).arg("-o").arg(&library).output().unwrap());
    let source = fixture.write("consumer.zy", &format!(r#"
param (/Thk; /Ret; /Int64; /OS; /numeric; /process) : @(import({:?})) in
let foreign = (@(ffi(c, library("zydeco_exact_path_fixture"), symbol("exact_identity"))) : Thk (Int64 -> Ret Int64)) in
do result <- ! foreign -9223372036854775808;
! numeric/int64/eq OS result -9223372036854775808 {{ ! process/exit 0 }} {{ ! process/exit 1 }}
"#, Fixture::builtin()));
    let compiler = CommandCompiler::default();
    let executable = compiler.executable(&source).unwrap();
    let paths = BTreeMap::from([(
        ForeignLibraryName::parse("zydeco_exact_path_fixture").unwrap(),
        library.clone(),
    )]);
    let result =
        CommandCompiler::test_io_with_libraries(executable.clone(), &[], "", paths.clone())
            .unwrap();
    assert_eq!(result.code, 0);
    assert!(result.output.is_empty() && result.stderr.is_empty());
    std::fs::remove_file(&library).unwrap();
    let error = CommandCompiler::test_io_with_libraries(executable, &[], "", paths).unwrap_err();
    assert!(error.to_string().contains("cannot load foreign library"), "{error}");
}
