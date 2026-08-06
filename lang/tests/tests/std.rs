use zydeco_tests::e2e_sources;
use zydeco_tests::utils::{SourceProgram, TestBackend};

e2e_sources!({
    utf8 => "tests/std/utf8.zy",
    collections => "tests/std/collections.zy",
});

struct FilesystemCase;

impl FilesystemCase {
    fn run(backend: TestBackend) {
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
    FilesystemCase::run(TestBackend::Interpreter);
}

#[test]
fn filesystem_amd64() {
    FilesystemCase::run(TestBackend::Amd64);
}
