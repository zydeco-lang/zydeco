use std::{
    path::PathBuf,
    process::{Command, Output},
};

struct RuntimeFixture {
    workspace: PathBuf,
    directory: tempfile::TempDir,
}

impl RuntimeFixture {
    fn new() -> Self {
        Self {
            workspace: PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".."),
            directory: tempfile::tempdir().unwrap(),
        }
    }

    fn run(&self, body: &str, backend: &str) -> Output {
        let source = self.directory.path().join("runtime.zy");
        let build = self.directory.path().join("build");
        std::fs::write(
            &source,
            format!(
                "param (/OS; /Int64; /numeric; /process; /system) : @(import(\"{}\")) in {body}\n",
                self.workspace.join("lib/std/builtin.zy").display(),
            ),
        )
        .unwrap();
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        if backend == "interpreter" {
            return command.arg("run").arg(&source).output().unwrap();
        }
        command.arg("build").arg(&source).args(["--target", backend, "--build-dir"]).arg(&build);
        if backend == "exe" {
            command
                .args(["--target-arch", "x86-64", "--runtime-dir"])
                .arg(self.workspace.join("runtime"))
                .arg("--execute");
            return command.output().unwrap();
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let extension = if backend == "wasm-am" { "am" } else { "sps" };
        Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
            .arg(self.workspace.join("lang/tests/wasm-host.mjs"))
            .arg(build.join(format!("runtime.{extension}.wasm")))
            .output()
            .unwrap()
    }

    fn assert_failure(output: Output, expected: &str, context: &str) {
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{context}: failure must stop execution");
        assert!(output.status.code().is_some(), "{context}: must not terminate by signal");
        assert!(diagnostic.contains(expected), "{context}: {diagnostic}");
        assert!(!diagnostic.contains("panicked"), "{context}: {diagnostic}");
        assert!(output.stdout.is_empty(), "{context}: must not produce stdout");
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn integer_zero_divisors_are_runtime_errors_at_every_width() {
    let fixture = RuntimeFixture::new();
    for integer in ["int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64"] {
        for (operation, message) in
            [("div", "integer division by zero"), ("mod", "integer remainder by zero")]
        {
            let body =
                format!("do result <- ! numeric/{integer}/{operation} 7 0; ! process/exit 0");
            for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
                RuntimeFixture::assert_failure(
                    fixture.run(&body, backend),
                    message,
                    &format!("{integer}/{operation}: {backend}"),
                );
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn integer_division_and_remainder_preserve_wrapping_at_every_width() {
    let fixture = RuntimeFixture::new();
    let cases = [
        ("int8", "-128", "-1", "-128", "0"),
        ("int16", "-32768", "-1", "-32768", "0"),
        ("int32", "-2147483648", "-1", "-2147483648", "0"),
        ("int64", "-9223372036854775808", "-1", "-9223372036854775808", "0"),
        ("uint8", "7", "2", "3", "1"),
        ("uint16", "7", "2", "3", "1"),
        ("uint32", "7", "2", "3", "1"),
        ("uint64", "7", "2", "3", "1"),
    ];
    let body = cases.into_iter().fold("! process/exit 0".to_owned(), |tail, (integer, first, second, quotient, remainder)| {
        format!("do q <- ! numeric/{integer}/div {first} {second}; do r <- ! numeric/{integer}/mod {first} {second}; ! numeric/{integer}/eq OS q {quotient} {{ ! numeric/{integer}/eq OS r {remainder} {{ {tail} }} {{ ! process/exit 42 }} }} {{ ! process/exit 42 }}")
    });
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture.run(&body, backend);
        assert!(output.status.success(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stdout.is_empty());
    }
}
