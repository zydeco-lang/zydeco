use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
};
use zydeco_cli::unit::UnitManifest;

struct Fixture {
    directory: tempfile::TempDir,
}

impl Fixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }

    fn write(&self, name: &str, source: &str) -> PathBuf {
        let path = self.directory.path().join(name);
        std::fs::write(&path, source).unwrap();
        path
    }

    fn command(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(self.directory.path())
            .args(args)
            .output()
            .unwrap()
    }

    fn success(output: &Output) {
        assert!(
            output.status.success(),
            "stdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn reject(output: &Output, diagnostic: &str) {
        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(diagnostic), "{stderr}");
        assert!(!stderr.contains("panicked"), "{stderr}");
    }

    fn source(body: &str) -> String {
        format!(
            "@[package(library(zydeco), name(example/unit))]\n\
            let VType = @(intrinsic(vtype)) in let Thk = @(intrinsic(thk)) in\n\
            let Ret = @(intrinsic(ret)) in let I = @(intrinsic(int)) in\n\
            let Unit = @(intrinsic(unit)) in {body}"
        )
    }

    fn builtin() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib/std/builtin.zy").canonicalize().unwrap()
    }

    fn runtime() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../runtime").canonicalize().unwrap()
    }

    fn build(
        &self, file: &Path, target: &str, directory: &str, dependencies: &[PathBuf],
    ) -> Output {
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        command
            .current_dir(self.directory.path())
            .args([
                "build",
                file.to_str().unwrap(),
                "--target",
                target,
                "--target-arch",
                "x86-64",
                "--build-dir",
                directory,
                "--runtime-dir",
            ])
            .arg(Self::runtime());
        for dependency in dependencies {
            command.arg("--link-library").arg(dependency);
        }
        command.output().unwrap()
    }

    fn manifest(&self, directory: &str, name: &str) -> (PathBuf, PathBuf, UnitManifest) {
        let path = self
            .directory
            .path()
            .join(directory)
            .join(format!("{name}.unit.json"))
            .canonicalize()
            .unwrap();
        let manifest = serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
        (path.parent().unwrap().to_owned(), path, manifest)
    }
}

#[test]
fn unit_boundary_checks_complete_closed_interfaces_and_readiness() {
    let fixture = Fixture::new();
    for body in [
        "()",
        "(#number = (7 : I), #nested = ((\"native\", 'x'), (1.25 : @(intrinsic(f64)))), #id = ({ fn (x : I) => ret x } : Thk (I -> Ret I)))",
        "let make = val (x : I) => ({ ret { fn (y : I) => ret (x, y) } } : Thk (Ret (Thk (I -> Ret (I * I))))) in make 7",
    ] {
        fixture.write("unit.zy", &Fixture::source(body));
        Fixture::success(&fixture.command(&["check", "./unit.zy"]));
    }
    for (body, error) in [
        ("ret (1 : I)", "closed value"),
        ("(+Some (1 : I) : data | +Some : I | +None : Unit end)", "native unit interface"),
        (
            "({ fn (A : VType) (x : A) => ret x } : Thk (forall (A : VType) . A -> Ret A))",
            "native unit interface",
        ),
        ("(_ : I)", "hole"),
        ("param val (x : I) in ({ ret x } : Thk (Ret I))", "Builtin"),
        (
            "(@(ffi(zydeco, library(\"example.other\"), symbol(\"init_other\"))) : Thk (I -> Ret I))",
            "Thk (Ret Exports)",
        ),
    ] {
        fixture.write("unit.zy", &Fixture::source(body));
        Fixture::reject(&fixture.command(&["check", "./unit.zy"]), error);
    }
    fixture.write("unit.zy", "@[package(library(zydeco))] ()");
    Fixture::success(&fixture.command(&["check", "./unit.zy"]));
    fixture.write(
        "unit.zy",
        "@[package(library(zydeco, export(root, symbol(\"unit\"))), name(example/unit))] ()",
    );
    Fixture::reject(&fixture.command(&["check", "./unit.zy"]), "requires library(c");
    assert!(!fixture.directory.path().join("build").exists());
}

#[test]
fn native_artifact_restrictions_reject_before_publication() {
    let fixture = Fixture::new();
    let native = fixture.write("native.zy", &Fixture::source("()"));
    for target in ["staticlib", "sharedlib"] {
        Fixture::reject(&fixture.build(&native, target, "invalid", &[]), "--target object");
    }
    let native_calls_c = fixture.write(
        "native.zy",
        &Fixture::source("(@(ffi(c, library(\"provider\"), symbol(\"provide\"))) : Thk (Ret I))"),
    );
    Fixture::success(&fixture.command(&["check", native_calls_c.to_str().unwrap()]));
    Fixture::reject(
        &fixture.build(&native_calls_c, "object", "invalid", &[]),
        "cannot be combined inside a library",
    );
    let c_calls_native = fixture.write(
        "c.zy",
        &Fixture::source(
            "(@(ffi(zydeco, library(\"provider\"), symbol(\"zydeco_unit_provider_init\"))) : Thk (Ret I))",
        )
        .replace("library(zydeco)", "library(c, export(root, symbol(\"provide\")))"),
    );
    Fixture::success(&fixture.command(&["check", c_calls_native.to_str().unwrap()]));
    Fixture::reject(
        &fixture.build(&c_calls_native, "object", "invalid", &[]),
        "cannot be combined inside a library",
    );
    assert!(!fixture.directory.path().join("invalid").exists());
}

#[test]
fn native_initializer_requires_a_native_artifact_before_execution() {
    let fixture = Fixture::new();
    let source = fixture.write(
        "client.zy",
        &format!(
            r#"
param (/Thk; /Ret; /Int; /OS; /process) : @(import({:?})) in
let init = (@(ffi(zydeco, library("missing.unit"), symbol("zydeco_unit_missing_init")))
    : Thk (Ret Int)) in
do result <- ! init;
! process/exit 0
"#,
            Fixture::builtin()
        ),
    );
    Fixture::success(&fixture.command(&["check", source.to_str().unwrap()]));
    Fixture::reject(&fixture.command(&["run", source.to_str().unwrap()]), "AMD64 exe");
    Fixture::reject(&fixture.build(&source, "exe", "missing", &[]), "matching --link-library");
    assert!(!fixture.directory.path().join("missing/client.exe").exists());
}

#[test]
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn source_free_units_share_captures_and_continuations_across_collection() {
    for (ty, group, delta, argument, expected) in [
        ("Float64", "float64", "-1.5", "0.5", "-1.0"),
        ("Int64", "int64", "9223372036854775807", "1", "-9223372036854775808"),
        ("UInt64", "uint64", "18446744073709551615", "1", "0"),
    ] {
        let fixture = Fixture::new();
        let one = if ty == "Float64" { "1.0" } else { "1" };
        let producer = fixture.write(
            "producer.zy",
            &format!(
                r#"
@[package(library(zydeco), name(example/producer))]
param val (/Thk; /Ret; /Unit; /Int; /{ty}; /numeric) : @(import({:?})) in
(
  #make_adder = ({{ fn (delta : {ty}) =>
    ret {{ fn (value : {ty}) =>
      let fix churn (n : Int) : Ret Unit =
        ! numeric/int/eq (Ret Unit) n 0 {{ ret () }}
          {{ do next <- ! numeric/int/sub n 1; ! churn next }}
      in
      do () <- ! churn 100000;
      do sum <- ! numeric/{group}/add delta value;
      do product <- ! numeric/{group}/mul sum ({one} : {ty});
      do () <- ! churn 100000;
      ret product
    }}
  }} : Thk ({ty} -> Ret (Thk ({ty} -> Ret {ty})))),
  #pair = ((({delta} : {ty}), ({argument} : {ty})), "native unit")
)
"#,
                Fixture::builtin()
            ),
        );
        Fixture::success(&fixture.build(&producer, "object", "producer", &[]));
        let (producer_base, producer_manifest, producer_interface) =
            fixture.manifest("producer", "example.producer");
        let producer_binding = producer_base.join(&producer_interface.bindings.path);
        Fixture::success(&fixture.command(&["check", producer_binding.to_str().unwrap()]));
        std::fs::remove_file(producer).unwrap();

        let middle = fixture.write(
            "middle.zy",
            &format!(
                r#"
@[package(library(zydeco), name(example/middle))]
param val (/Thk; /Ret; /{ty}) : @(import({:?})) in
let init = @(import({:?})) in
({{ do api <- ! init;
    let ((delta, _), _) = api/pair in
    ! api/make_adder delta
}} : Thk (Ret (Thk ({ty} -> Ret {ty}))))
"#,
                Fixture::builtin(),
                producer_binding
            ),
        );
        Fixture::success(&fixture.build(&middle, "object", "middle", &[producer_manifest]));
        let (middle_base, middle_manifest, middle_interface) =
            fixture.manifest("middle", "example.middle");
        let middle_binding = middle_base.join(&middle_interface.bindings.path);
        std::fs::remove_file(middle).unwrap();

        let client = fixture.write(
            "client.zy",
            &format!(
                r#"
param (/Unit; /Ret; /Int; /{ty}; /OS; /numeric; /process) : @(import({:?})) in
let init = @(import({:?})) in
let shifted = {{
    do make <- ! init;
    do add <- ! make;
    fn (_ : Int) =>
    do value <- ! add {argument};
    ! numeric/{group}/eq OS value {expected} {{ ! process/exit 0 }} {{ ! process/exit 3 }}
}} in
do make <- ! init;
do add <- ! make;
let fix churn (n : Int) : Ret Unit =
    ! numeric/int/eq (Ret Unit) n 0 {{ ret () }}
        {{ do next <- ! numeric/int/sub n 1; ! churn next }}
in
do () <- ! churn 100000;
do first <- ! add {argument};
do () <- ! churn 100000;
do second <- ! add {argument};
! numeric/{group}/eq OS first {expected}
    {{ ! numeric/{group}/eq OS second {expected} {{ ! shifted 0 }} {{ ! process/exit 2 }} }}
    {{ ! process/exit 1 }}
"#,
                Fixture::builtin(),
                middle_binding
            ),
        );
        Fixture::success(&fixture.build(
            &client,
            "exe",
            "client",
            std::slice::from_ref(&middle_manifest),
        ));
        let output =
            Command::new(fixture.directory.path().join("client/client.exe")).output().unwrap();
        Fixture::success(&output);

        // A differently typed import must reject before another executable can be published.
        let wrong = fixture.write(
            "wrong.zy",
            &format!(
                r#"
param (/Thk; /Unit; /Ret; /process) : @(import({:?})) in
do value <- ! (@(ffi(zydeco, library("example.middle"), symbol({:?})))
    : Thk (Ret Unit));
! process/exit 0
"#,
                Fixture::builtin(),
                middle_interface.symbol.as_str()
            ),
        );
        Fixture::reject(
            &fixture.build(&wrong, "exe", "wrong", &[middle_manifest]),
            "different exported type",
        );
        assert!(!fixture.directory.path().join("wrong/wrong.exe").exists());
    }
}
