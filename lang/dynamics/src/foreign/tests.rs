use super::*;
use std::{path::PathBuf, process::Command};
use zydeco_machine::memory::{AccessHandle, AddressHandle, Permission};
use zydeco_syntax::{ForeignParameter, ForeignTarget};

struct ForeignFixture {
    runtime: ForeignRuntime,
    memory: BufferArena,
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
        Self { runtime, memory: BufferArena::default(), _directory: directory }
    }

    fn import(symbol: &str, parameters: Vec<ForeignParameter>) -> ForeignImport {
        ForeignImport {
            target: ForeignTarget {
                abi: ForeignAbi::C,
                library: ForeignLibraryName::parse("zyffi_boundary").unwrap(),
                symbol: ForeignSymbolName::parse(symbol).unwrap(),
            },
            signature: ForeignSignature::new(
                parameters,
                ForeignResult::Integer(IntegerType::UInt64),
            )
            .unwrap(),
        }
    }

    fn integer(value: u64) -> ds::SemValue {
        ds::SemValue::Literal(Literal::Integer(IntegerLiteral::UInt64(value)))
    }

    fn window(&mut self, value: &[u8]) -> ds::SemValue {
        let access = self.memory.import_memory(value).unwrap();
        let address = self.memory.base_address(access).unwrap();
        Self::borrowed(access, address, value.len() as i64)
    }

    fn borrowed(access: AccessHandle, address: AddressHandle, length: i64) -> ds::SemValue {
        ds::SemValue::VCons(vec![
            HostValue::Access(access).into(),
            HostValue::Address(address).into(),
            ds::SemValue::Literal(Literal::Integer(IntegerLiteral::Int64(length))),
        ])
    }

    fn call(
        &mut self, symbol: &str, parameters: Vec<ForeignParameter>, arguments: Vec<ds::SemValue>,
    ) -> u64 {
        let import = Self::import(symbol, parameters);
        let ds::Computation::Ret(Return(value)) =
            self.runtime.invoke(&import, arguments, &self.memory).unwrap()
        else {
            panic!("C call must return through Ret")
        };
        let ds::Value::Lit(Literal::Integer(IntegerLiteral::UInt64(result))) = *value else {
            panic!("C call must preserve the UInt64 result representation")
        };
        result
    }

    fn call_result(
        &mut self, symbol: &str, result: ForeignResult, argument: Option<IntegerLiteral>,
    ) -> Rc<ds::Value> {
        let parameters = argument
            .iter()
            .map(|value| ForeignParameter::Integer(value.integer_type().unwrap()))
            .collect();
        let mut import = Self::import(symbol, vec![]);
        import.signature = ForeignSignature::new(parameters, result).unwrap();
        let arguments = argument
            .into_iter()
            .map(|value| ds::SemValue::Literal(Literal::Integer(value)))
            .collect();
        let ds::Computation::Ret(Return(value)) =
            self.runtime.invoke(&import, arguments, &self.memory).unwrap()
        else {
            panic!("C call must return through Ret")
        };
        value
    }
}

#[test]
fn preserves_every_integer_width_and_signedness() {
    let mut fixture = ForeignFixture::new();
    for (integer, minimum, maximum) in [
        (IntegerType::Int8, i8::MIN as i128, i8::MAX as i128),
        (IntegerType::Int16, i16::MIN as i128, i16::MAX as i128),
        (IntegerType::Int32, i32::MIN as i128, i32::MAX as i128),
        (IntegerType::Int64, i64::MIN as i128, i64::MAX as i128),
        (IntegerType::UInt8, 0, u8::MAX as i128),
        (IntegerType::UInt16, 0, u16::MAX as i128),
        (IntegerType::UInt32, 0, u32::MAX as i128),
        (IntegerType::UInt64, 0, u64::MAX as i128),
    ] {
        for value in [minimum, maximum] {
            let literal = IntegerLiteral::from_value(value, integer);
            let returned = fixture.call_result(
                &format!("zyffi_{}", integer.source_name()),
                ForeignResult::Integer(integer),
                Some(literal),
            );
            assert!(
                matches!(*returned, ds::Value::Lit(Literal::Integer(actual)) if actual == literal)
            );
        }
    }
    for (integer, expected) in [
        (IntegerType::Int8, -128),
        (IntegerType::Int16, -32640),
        (IntegerType::Int32, -2147450752),
        (IntegerType::UInt8, 128),
        (IntegerType::UInt16, 32896),
        (IntegerType::UInt32, 2147516544),
    ] {
        let returned = fixture.call_result(
            &format!("zyffi_dirty_{}", integer.source_name()),
            ForeignResult::Integer(integer),
            None,
        );
        assert!(matches!(*returned, ds::Value::Lit(Literal::Integer(actual))
            if actual == IntegerLiteral::from_value(expected, integer)));
    }
}

#[test]
fn void_calls_execute_and_resume_with_unit() {
    let mut fixture = ForeignFixture::new();
    for value in [i64::MIN, i64::MAX] {
        let literal = IntegerLiteral::Int64(value);
        let returned = fixture.call_result("zyffi_save", ForeignResult::Unit, Some(literal));
        assert!(matches!(*returned, ds::Value::Triv(Triv)));
        let returned = fixture.call_result(
            "zyffi_saved_value",
            ForeignResult::Integer(IntegerType::Int64),
            None,
        );
        assert!(matches!(*returned, ds::Value::Lit(Literal::Integer(actual)) if actual == literal));
    }
}

#[test]
fn calls_c_with_zero_scalar_and_borrowed_arguments() {
    use ForeignParameter::BorrowedMemory as B;
    const U: ForeignParameter = ForeignParameter::Integer(IntegerType::UInt64);
    let mut fixture = ForeignFixture::new();
    assert_eq!(fixture.call("zyffi_zero", vec![], vec![]), u64::MAX);
    for value in [0, 7, 1 << 63, u64::MAX] {
        assert_eq!(
            fixture.call("zyffi_echo", vec![U], vec![ForeignFixture::integer(value)]),
            value
        );
    }
    for (bytes, expected) in [(b"".as_slice(), 0), (b"hello".as_slice(), 1389), (&[0, 255], 514)] {
        let window = fixture.window(bytes);
        assert_eq!(
            fixture.call(
                "zyffi_bytes",
                vec![B, U],
                vec![window, ForeignFixture::integer(bytes.len() as u64)]
            ),
            expected
        );
    }
    assert_eq!(fixture.runtime.functions.len(), 3, "repeated calls reuse their interfaces");
}

#[test]
fn borrows_shared_subranges_of_retained_immutable_storage() {
    let mut fixture = ForeignFixture::new();
    let access = fixture.memory.import_memory(b"..hello!!").unwrap();
    let base = fixture.memory.base_address(access).unwrap();
    for (offset, length, expected) in [(2, 5, 1389), (3, 3, 872), (7, 0, 0)] {
        let address = fixture.memory.offset_address(access, base, offset).unwrap();
        assert_eq!(
            fixture.call(
                "zyffi_bytes",
                vec![
                    ForeignParameter::BorrowedMemory,
                    ForeignParameter::Integer(IntegerType::UInt64)
                ],
                vec![
                    ForeignFixture::borrowed(access, address, length),
                    ForeignFixture::integer(length as u64)
                ]
            ),
            expected
        );
    }
}

#[test]
fn preserves_source_order_across_six_explicit_c_arguments() {
    use ForeignParameter::BorrowedMemory as B;
    const U: ForeignParameter = ForeignParameter::Integer(IntegerType::UInt64);
    let mut fixture = ForeignFixture::new();
    let hello = fixture.window(b"hello");
    let world = fixture.window(b"world");
    let empty = fixture.window(b"");
    assert_eq!(
        fixture.call(
            "zyffi_mixed",
            vec![U, B, U, B, U, U],
            vec![
                ForeignFixture::integer(1 << 63),
                hello.clone(),
                ForeignFixture::integer(5),
                world.clone(),
                ForeignFixture::integer(5),
                ForeignFixture::integer(7),
            ]
        ),
        9_223_378_066_988_860_778
    );
    assert_eq!(
        fixture.call(
            "zyffi_three_bytes",
            vec![B, U, B, U, B, U],
            vec![
                hello,
                ForeignFixture::integer(5),
                empty,
                ForeignFixture::integer(0),
                world,
                ForeignFixture::integer(5),
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
    use ForeignParameter::BorrowedMemory as B;
    const U: ForeignParameter = ForeignParameter::Integer(IntegerType::UInt64);
    let mut runtime = ForeignRuntime::new();
    let memory = BufferArena::default();
    for (parameters, arguments) in [
        (vec![U], vec![]),
        (vec![], vec![ForeignFixture::integer(1)]),
        (vec![U], vec![ds::SemValue::VCons(vec![])]),
        (vec![B], vec![ForeignFixture::integer(1)]),
        (vec![U], vec![ds::SemValue::Literal(Literal::Integer(IntegerLiteral::UInt32(1)))]),
    ] {
        let import = ForeignFixture::import("zyffi_echo", parameters);
        assert!(
            matches!(runtime.invoke(&import, arguments, &memory), Err(ForeignRuntimeError::InvalidArguments(symbol)) if symbol == import.target.symbol)
        );
        assert!(runtime.libraries.is_empty());
        assert!(runtime.functions.is_empty());
    }
}

#[test]
fn invalid_memory_windows_reject_before_loading_and_preserve_the_allocation() {
    let mut runtime = ForeignRuntime::new();
    let mut memory = BufferArena::default();
    let owner = memory.allocate_uninitialized(8, 8).unwrap();
    let access = memory.grant(owner, 0, 8, Permission::ReadWrite).unwrap();
    let write = memory.grant(owner, 0, 8, Permission::Write).unwrap();
    let address = memory.base_address(access).unwrap();
    let import = ForeignFixture::import("zyffi_bytes", vec![ForeignParameter::BorrowedMemory]);
    for (grant, length, expected) in [
        (access, 9, MemoryError::Bounds),
        (access, -1, MemoryError::Bounds),
        (access, 8, MemoryError::Uninitialized),
        (write, 8, MemoryError::Permission),
    ] {
        let argument = ForeignFixture::borrowed(grant, address, length);
        assert!(matches!(runtime.invoke(&import, vec![argument], &memory),
            Err(ForeignRuntimeError::Memory { fault, .. }) if fault == expected));
        assert!(runtime.libraries.is_empty());
        assert!(runtime.functions.is_empty());
        assert_eq!(memory.load_u8(access, address), Err(MemoryError::Uninitialized));
    }
    memory.close(owner).unwrap();
    let argument = ForeignFixture::borrowed(access, address, 0);
    assert!(matches!(
        runtime.invoke(&import, vec![argument], &memory),
        Err(ForeignRuntimeError::Memory { fault: MemoryError::Closed, .. })
    ));
    assert!(runtime.libraries.is_empty());
}

#[test]
fn missing_library_is_a_recoverable_loader_error() {
    let mut runtime = ForeignRuntime::new();
    let memory = BufferArena::default();
    let mut import = ForeignFixture::import("zyffi_zero", vec![]);
    import.target.library =
        ForeignLibraryName::parse("zydeco_ffi_missing_library_83710a6f").unwrap();
    for _ in 0..2 {
        assert!(
            matches!(runtime.invoke(&import, vec![], &memory), Err(ForeignRuntimeError::OpenLibrary { library, .. }) if library == import.target.library)
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
        matches!(fixture.runtime.invoke(&import, vec![], &fixture.memory), Err(ForeignRuntimeError::MissingSymbol { library, symbol, .. })
        if library == import.target.library && symbol == import.target.symbol)
    );
    assert!(fixture.runtime.functions.is_empty());
    assert_eq!(fixture.call("zyffi_zero", vec![], vec![]), u64::MAX);
}

#[test]
fn foreign_borrows_preserve_concrete_storage_alignment_and_contents() {
    let mut fixture = ForeignFixture::new();
    let mut octets = vec![0; 64];
    octets[0] = 7;
    octets[4..8].copy_from_slice(&16909060u32.to_le_bytes());
    let owner = fixture.memory.allocate(64, 64).unwrap();
    fixture.memory.write(owner, 0, &octets).unwrap();
    let access = fixture.memory.freeze_memory(owner).unwrap();
    let base = fixture.memory.base_address(access).unwrap();
    for (offset, length, expected) in [(0, 64, 1), (1, 63, 0), (0, 64, 1)] {
        let address = fixture.memory.offset_address(access, base, offset).unwrap();
        assert_eq!(
            fixture.call(
                "zyffi_record",
                vec![
                    ForeignParameter::BorrowedMemory,
                    ForeignParameter::Integer(IntegerType::UInt64)
                ],
                vec![
                    ForeignFixture::borrowed(access, address, length),
                    ForeignFixture::integer(length as u64)
                ]
            ),
            expected
        );
    }
}
