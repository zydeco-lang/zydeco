use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

struct EitherCase;

impl EitherCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
        format!(
            r#"
let data_package = @(import("{library}/data/package.zy")) in
let (/Either; /either) = data_package in
{body}
"#,
            library = library.display(),
        )
    }

    fn reject(body: &str, code: TyckDiagnosticCode) {
        let source = Self::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}

#[test]
fn either_shares_its_carrier_with_exception_and_the_data_signature() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = EitherCase::source(&format!(
        r#"
let (/Either = DirectEither) = @(import("{library}/data/either.zy")) in
let (/ExceptionT) = @(import("{library}/effect/exception.zy")) in
let _ = val (L : VType) (R : VType) (value : Either L R) =>
  (
    value : DirectEither L R
  ) in
let _ = val (E : VType) (A : VType) (value : Thk (ExceptionT E Ret A)) =>
  (
    value : Thk (Ret (Either E A))
  ) in
! exit 0
"#,
        library = library.display(),
    ));
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn either_rejects_result_exports_and_old_constructor_labels() {
    for body in [
        "let (/Result) = data_package in ! exit 0",
        "let _ = data_package/result in ! exit 0",
        "let _ = either/ok in ! exit 0",
        "let _ = either/err in ! exit 0",
        "let _ = either/map_err in ! exit 0",
        "let _ = either/is_ok in ! exit 0",
        "let _ = either/is_err in ! exit 0",
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::MissingNamedField);
    }
    for body in [
        "let _ : Either String Int = +Ok(1) in ! exit 0",
        "let _ : Either String Int = +Err(\"error\") in ! exit 0",
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::UnknownDataConstructor);
    }
}

#[test]
fn either_rejects_reversed_payloads_and_fold_handlers() {
    for body in [
        r#"
let _ : Thk (Ret (Either String Int)) = { ! either/right Int String "value" } in
! exit 0
"#,
        r#"
do _ <- ! either/fold String Int (Ret Int) (+Left("error") : Either String Int)
  { fn (value : Int) => ret value } { fn (_ : String) => ret 0 };
! exit 0
"#,
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::TypeMismatch);
    }
}

struct RepresentationCase;

impl RepresentationCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
        format!(
            r#"
let (/BytesPackage; /make = make_bytes) = @(import("{library}/text/bytes.zy")) in
let byte_package : BytesPackage Addr = make_bytes memory_builtin in
let (/Bytes; /bytes) = byte_package in
let (/Types; /make = make_types) = @(import("{library}/memory/types.zy")) in
let type_package : Types Addr = make_types Addr in
let (/Uninit; /Init; /Ptr; types) = type_package in
let (/StoragePackage; /storage = storage_package) = @(import("{library}/memory/storage.zy")) in
let (/Storage; /DynamicStorage; /SizeAlign) = storage_package in
let (/FieldOperations; /make = make_fields) = @(import("{library}/memory/field.zy")) in
let (= Field, = DynamicField, field_operations) = make_fields memory_builtin types in
let _ : FieldOperations Uninit Init Ptr Field DynamicField = field_operations in
{body}
"#,
            library = library.display(),
        )
    }
}

#[test]
fn colocated_contracts_preserve_representation_opacity() {
    for (abstract_type, representation) in [
        ("Bytes", "Addr * Int"),
        ("Ptr Int Init", "Addr"),
        ("Storage Int", "val pi (_ : Unit) . SizeAlign"),
        ("DynamicStorage Int", "SizeAlign"),
        ("Field Int Int", "val pi (_ : Unit) . Int"),
        ("DynamicField Int Int", "Int"),
    ] {
        let accepted = RepresentationCase::source(&format!(
            "let _ = val (value : {abstract_type}) => (value : {abstract_type}) in ! exit 0"
        ));
        SourceCase::assert_accepted(SourceCase::check_invariants(&accepted));
        let rejected = RepresentationCase::source(&format!(
            "let _ = val (value : {abstract_type}) => (value : {representation}) in ! exit 0"
        ));
        SourceCase::assert_rejected(SourceCase::check(&rejected), TyckDiagnosticCode::TypeMismatch);
        SourceCase::assert_rejected(SourceCase::lower(&rejected), TyckDiagnosticCode::TypeMismatch);
    }
}

#[test]
fn colocated_factories_keep_independent_openings_distinct() {
    SourceCase::assert_accepted(SourceCase::check_invariants(&RepresentationCase::source(
        "let _ : Thk (Ret Bytes) = bytes/empty in ! exit 0",
    )));
    SourceCase::assert_rejected(
        SourceCase::check(&RepresentationCase::source(
            "let (/Bytes = OtherBytes) = byte_package in \
             let _ = val (value : Bytes) => (value : OtherBytes) in ! exit 0",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn shared_dictionary_contracts_remain_manifest_and_constructible() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = |zero: &str| {
        format!(
            r#"
let (/Additive) = @(import("{library}/numeric/dictionary.zy")) in
let (/Additive = SameAdditive) = @(import("{library}/numeric/dictionary.zy")) in
let dictionary : Additive Int = (
  #zero = {zero}, #add = int/add, #sub = int/sub,
  #negate = {{ fn (value : Int) => ! int/sub 0 value }}
) in
let _ : SameAdditive Int = dictionary in
! exit 0
"#,
            library = library.display(),
        )
    };
    SourceCase::assert_accepted(SourceCase::check_invariants(&source("0")));
    SourceCase::assert_rejected(SourceCase::check(&source("()")), TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn list_identity_is_shared_across_data_std_and_nondet() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = format!(
        r#"
let (/List = DataList; /list) = @(import("{library}/data/package.zy")) in
let (/List = StdList; /list = std_list) = builtin |> (@(import("{library}/std.zy"))) in
let (/NonDet; nondet) = @(import("{library}/effect/nondet.zy")) in
let (N, n) = nondet/instance in
do nil <- ! list/nil Int;
do items <- ! list/cons Int 1 nil;
let same_data : DataList Int = items in
let same_std : StdList Int = same_data in
do observed <- ! nondet/observe_all Int {{ ! nondet/choose (N, n) Int same_std }};
let observed_data : DataList Int = observed in
let observed_std : StdList Int = observed_data in
do count <- ! std_list/length Int observed_std;
! int/eq OS count 1 {{ ! exit 0 }} {{ ! exit 1 }}
"#,
        library = library.display(),
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}

#[test]
fn retired_data_factories_and_bool_indexed_numeric_dictionaries_are_rejected() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    for body in [
        "let (/DataPackage) = @(import(\"{library}/data/package.zy\")) in ! exit 0",
        "let (/BoolModule) = @(import(\"{library}/data/package.zy\")) in ! exit 0",
        "let (/make) = @(import(\"{library}/data/package.zy\")) in ! exit 0",
        "let (/make) = @(import(\"{library}/effect/nondet.zy\")) in ! exit 0",
        "let (/make) = @(import(\"{library}/memory/storage.zy\")) in ! exit 0",
    ] {
        let source = body.replace("{library}", &library.display().to_string());
        SourceCase::assert_rejected(
            SourceCase::check(&source),
            TyckDiagnosticCode::MissingNamedField,
        );
    }
    for body in [
        "let (/PartialEquality) = @(import(\"{library}/numeric/dictionary.zy\")) in \
         let _ = val (d : PartialEquality Bool Int) => d in ! exit 0",
        "let (/PartialOrder) = @(import(\"{library}/numeric/dictionary.zy\")) in \
         let _ = val (d : PartialOrder Bool Int) => d in ! exit 0",
        "let (/Numeric) = @(import(\"{library}/numeric/dictionary.zy\")) in \
         let _ = val (d : Numeric Bool Int) => d in ! exit 0",
    ] {
        let source = body.replace("{library}", &library.display().to_string());
        SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::KindMismatch);
    }
}

#[test]
fn memory_capability_packages_have_no_os_dependency() {
    let lib =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib").canonicalize().unwrap();
    let session = zydeco_session::CompilerSession::default();
    for rel in
        ["std/memory/package.zy", "std/text/bytes.zy", "std/numeric/codecs.zy", "ffi/xxhash.zy"]
    {
        let path = lib.join(rel);
        let analysis = session.analyze(&path).unwrap();
        assert!(analysis.outcome().root().is_some(), "{rel}: {:?}", analysis.outcome());
        assert!(
            analysis.graph().source_inputs().any(|s| s.path.ends_with("builtin/memory.zy")),
            "{rel} should depend on std/builtin/memory.zy"
        );
        assert!(
            !analysis.graph().source_inputs().any(
                |s| s.path.ends_with("std/builtin.zy") || s.path.ends_with("builtin/system.zy")
            ),
            "{rel} must not depend on OS builtin packages"
        );
    }
}

#[test]
fn memory_and_system_capabilities_bridge_through_builtin_and_share_bytes() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = format!(
        r#"
let (/BytesPackage; /make = make_bytes) = @(import("{library}/text/bytes.zy")) in
let (/Bytes; /bytes; byte_package) : BytesPackage Addr = make_bytes memory_builtin in
let (/Path; /IoError; /fs; /process = sys_proc) =
  (builtin |> (@(import("{library}/system/package.zy")))) byte_package in
do chunk <- ! bytes/from_string "bridge";
let _ : Thk (Path -> Bytes -> Thk (Either IoError Unit -> OS) -> OS) = fs/write_bytes in
do len <- ! bytes/length chunk;
! int/eq OS len 6 {{ ! sys_proc/exit 0 }} {{ ! sys_proc/exit 1 }}
"#,
        library = library.display(),
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}
