use zydeco_tests::e2e_sources;
use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

e2e_sources!({
    bool => "tests/std/bool.zy",
    option => "tests/std/option.zy",
    utf8 => "tests/std/utf8.zy",
    bytes => "tests/std/bytes.zy",
    collections => "tests/std/collections.zy",
    float => "tests/std/float.zy",
    numeric_capabilities => "tests/std/numeric-capabilities.zy",
});

struct FilesystemCase;

impl FilesystemCase {
    fn run(backend: ExecutionTarget) {
        let directory = tempfile::tempdir().expect("filesystem fixture directory");
        let path = directory.path().join("roundtrip.bin");
        std::fs::write(path.with_extension("bin.invalid"), [0xff, 0xfe])
            .expect("invalid UTF-8 fixture");
        SourceProgram::setup("tests/std/filesystem.zy")
            .with_args([path.to_string_lossy().into_owned()])
            .test(backend);
    }
}

#[test]
fn filesystem_interpreter() {
    FilesystemCase::run(ExecutionTarget::Interpreter);
}

#[test]
fn filesystem_amd64() {
    FilesystemCase::run(ExecutionTarget::Exe);
}

#[test]
fn filesystem_wasm_am() {
    FilesystemCase::run(ExecutionTarget::WasmAm);
}

#[test]
fn filesystem_wasm_sps() {
    FilesystemCase::run(ExecutionTarget::WasmSps);
}

struct ArgumentListCase;

impl ArgumentListCase {
    fn run(backend: ExecutionTarget) {
        SourceProgram::setup("tests/std/arg-list.zy").with_args(["alpha"]).test(backend);
    }
}

#[test]
fn argument_list_interpreter() {
    ArgumentListCase::run(ExecutionTarget::Interpreter);
}

#[test]
fn argument_list_wasm_am() {
    ArgumentListCase::run(ExecutionTarget::WasmAm);
}

#[test]
fn argument_list_wasm_sps() {
    ArgumentListCase::run(ExecutionTarget::WasmSps);
}
