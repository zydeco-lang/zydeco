use super::*;
use crate::host::SharedBytes;
use std::{path::PathBuf, process::Command};
use zydeco_syntax::{ForeignParameter, ForeignTarget};

struct ForeignFixture {
    runtime: ForeignRuntime,
    _directory: tempfile::TempDir,
}

impl ForeignFixture {
    fn new() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let library = directory.path().join(if cfg!(target_os = "macos") {
            "libzyffi_boundary.dylib"
        } else {
            "libzyffi_boundary.so"
        });
        let source =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/ffi/boundary.c");
        let output = Command::new("cc")
            .args(if cfg!(target_os = "macos") {
                &["-dynamiclib", "-fPIC"][..]
            } else {
                &["-shared", "-fPIC"][..]
            })
            .arg(source)
            .arg("-o")
            .arg(&library)
            .output()
            .expect("C compiler is required for FFI tests");
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let name = ForeignLibraryName::parse("zyffi_boundary").unwrap();
        use std::os::unix::ffi::OsStrExt;
        let filename = CString::new(library.as_os_str().as_bytes()).unwrap();
        let library = DynamicLibrary::open_filename(&name, &filename).unwrap();
        let mut runtime = ForeignRuntime::new();
        runtime.libraries.insert(name, library);
        Self { runtime, _directory: directory }
    }

    fn import(symbol: &str, parameters: Vec<ForeignParameter>) -> ForeignImport {
        ForeignImport {
            target: ForeignTarget {
                abi: ForeignAbi::C,
                library: ForeignLibraryName::parse("zyffi_boundary").unwrap(),
                symbol: ForeignSymbolName::parse(symbol).unwrap(),
            },
            signature: ForeignSignature::new(parameters, ForeignResult::UInt64).unwrap(),
        }
    }

    fn integer(value: u64) -> ds::SemValue {
        ds::SemValue::Literal(Literal::Integer(IntegerLiteral::UInt64(value)))
    }

    fn bytes(value: &[u8]) -> ds::SemValue {
        ds::SemValue::Host(HostValue::Bytes(SharedBytes::from_buffer(Rc::from(value))))
    }

    fn call(
        &mut self, symbol: &str, parameters: Vec<ForeignParameter>, arguments: Vec<ds::SemValue>,
    ) -> u64 {
        let import = Self::import(symbol, parameters);
        let ds::Computation::Ret(Return(value)) = self.runtime.invoke(&import, arguments).unwrap()
        else {
            panic!("C call must return through Ret")
        };
        let ds::Value::Lit(Literal::Integer(IntegerLiteral::UInt64(result))) = *value else {
            panic!("C call must preserve the UInt64 result representation")
        };
        result
    }
}

#[test]
fn calls_c_with_zero_scalar_and_borrowed_arguments() {
    use ForeignParameter::{BorrowedBytes as B, UInt64 as U};
    let mut fixture = ForeignFixture::new();
    assert_eq!(fixture.call("zyffi_zero", vec![], vec![]), u64::MAX);
    for value in [0, 7, 1 << 63, u64::MAX] {
        assert_eq!(
            fixture.call("zyffi_echo", vec![U], vec![ForeignFixture::integer(value)]),
            value
        );
    }
    for (bytes, expected) in [(b"".as_slice(), 0), (b"hello".as_slice(), 1389), (&[0, 255], 514)] {
        assert_eq!(
            fixture.call("zyffi_bytes", vec![B], vec![ForeignFixture::bytes(bytes)]),
            expected
        );
    }
    assert_eq!(fixture.runtime.functions.len(), 3, "repeated calls reuse their interfaces");
}

#[test]
fn borrows_only_shared_byte_windows_after_the_parent_is_dropped() {
    let (window, nested, empty) = {
        let parent = SharedBytes::from_buffer(Rc::from(b"..hello!!".as_slice()));
        let window = parent.slice(2, 5).unwrap();
        let nested = window.slice(1, 3).unwrap();
        let empty = window.slice(5, 0).unwrap();
        (window, nested, empty)
    };
    let mut fixture = ForeignFixture::new();
    for (window, expected) in [(window, 1389), (nested, 872), (empty, 0)] {
        assert_eq!(
            fixture.call(
                "zyffi_bytes",
                vec![ForeignParameter::BorrowedBytes],
                vec![ds::SemValue::Host(HostValue::Bytes(window))],
            ),
            expected
        );
    }
}

#[test]
fn preserves_source_order_across_six_flattened_c_arguments() {
    use ForeignParameter::{BorrowedBytes as B, UInt64 as U};
    let mut fixture = ForeignFixture::new();
    assert_eq!(
        fixture.call(
            "zyffi_mixed",
            vec![U, B, B, U],
            vec![
                ForeignFixture::integer(1 << 63),
                ForeignFixture::bytes(b"hello"),
                ForeignFixture::bytes(b"world"),
                ForeignFixture::integer(7),
            ]
        ),
        9_223_378_066_988_860_778
    );
    assert_eq!(
        fixture.call(
            "zyffi_three_bytes",
            vec![B, B, B],
            vec![
                ForeignFixture::bytes(b"hello"),
                ForeignFixture::bytes(b""),
                ForeignFixture::bytes(b"world"),
            ]
        ),
        8409
    );
    assert_eq!(
        fixture.call("zyffi_six", vec![U; 6], (1..=6).map(ForeignFixture::integer).collect()),
        183
    );
}

#[test]
fn rejects_invalid_runtime_arguments_before_loading_or_calling() {
    use ForeignParameter::{BorrowedBytes as B, UInt64 as U};
    let mut runtime = ForeignRuntime::new();
    for (parameters, arguments) in [
        (vec![U], vec![]),
        (vec![], vec![ForeignFixture::integer(1)]),
        (vec![U], vec![ForeignFixture::bytes(b"hello")]),
        (vec![B], vec![ForeignFixture::integer(1)]),
        (vec![U], vec![ds::SemValue::Literal(Literal::Integer(IntegerLiteral::UInt32(1)))]),
    ] {
        let import = ForeignFixture::import("zyffi_echo", parameters);
        assert!(
            matches!(runtime.invoke(&import, arguments), Err(ForeignRuntimeError::InvalidArguments(symbol))
            if symbol == import.target.symbol)
        );
        assert!(runtime.libraries.is_empty());
        assert!(runtime.functions.is_empty());
    }
}

#[test]
fn missing_library_is_a_recoverable_loader_error() {
    let mut runtime = ForeignRuntime::new();
    let mut import = ForeignFixture::import("zyffi_zero", vec![]);
    import.target.library =
        ForeignLibraryName::parse("zydeco_ffi_missing_library_83710a6f").unwrap();
    for _ in 0..2 {
        assert!(
            matches!(runtime.invoke(&import, vec![]), Err(ForeignRuntimeError::OpenLibrary { library, .. })
            if library == import.target.library)
        );
        assert!(runtime.libraries.is_empty());
        assert!(runtime.functions.is_empty());
    }
}

#[test]
fn missing_symbol_does_not_poison_subsequent_calls() {
    let mut fixture = ForeignFixture::new();
    let import = ForeignFixture::import("zyffi_missing_symbol", vec![]);
    assert!(
        matches!(fixture.runtime.invoke(&import, vec![]), Err(ForeignRuntimeError::MissingSymbol { library, symbol, .. })
        if library == import.target.library && symbol == import.target.symbol)
    );
    assert!(fixture.runtime.functions.is_empty());
    assert_eq!(fixture.call("zyffi_zero", vec![], vec![]), u64::MAX);
}
