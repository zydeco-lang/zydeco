//! Prelude sugar is tested without the test harness's own implicit primitive bindings.
use std::{path::PathBuf, sync::Arc};
use zydeco_session::{AnalysisError, CompilerSession, ProgramAnalysis};
use zydeco_statics::TyckDiagnosticCode;

const SUGAR: &str = "@[prelude(core, from(prelude))]";
const EXPLICIT: &str = "let (/VType; /CType; /Thk; /Ret; /OS; /Unit; /Int8; /Int16; /Int32; /Int64; /Int; /UInt8; /UInt16; /UInt32; /UInt64; /UInt; /Float32; /Float64; /Char; /String; /Bool; /Option; /Either) = prelude in";

struct Fixture {
    directory: tempfile::TempDir,
    session: CompilerSession,
}

impl Fixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap(), session: CompilerSession::default() }
    }

    fn source(source: &str) -> String {
        let std =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std").canonicalize().unwrap();
        source.replace("$std", std.to_str().unwrap())
    }

    fn write(&self, name: &str, source: &str) -> PathBuf {
        let path = self.directory.path().join(name);
        std::fs::write(&path, Self::source(source)).unwrap();
        path
    }

    fn analyze(&self, source: &str) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
        self.session.analyze(self.write("main.zy", source))
    }

    fn accepted(source: &str) {
        let analysis = Self::new().analyze(source).unwrap();
        assert!(analysis.outcome().root().is_some(), "{:?}", analysis.outcome());
    }

    fn rejected(source: &str, code: TyckDiagnosticCode) {
        let analysis = Self::new().analyze(source).unwrap();
        assert!(analysis.outcome().root().is_none(), "rejected programs publish no checked root");
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.code == code && diagnostic.primary.is_some()),
            "{diagnostics:?}"
        );
    }

    fn provider(body: &str) -> String {
        format!(
            r#"
param val (/process; builtin) : @(import("$std/builtin.zy")) in
let (/make = make_prelude) = @(import("$std/prelude.zy")) in
let prelude = make_prelude builtin in
{body}
"#
        )
    }
}

#[test]
fn packed_results_and_shadowing_match_the_explicit_opening() {
    for opening in [SUGAR, EXPLICIT] {
        Fixture::accepted(&Fixture::provider(&format!(
            "{opening}\nlet Callback = Thk (Int -> OS) in pack (= Callback as Callback) where () end"
        )));
        Fixture::accepted(&Fixture::provider(&format!(
            "{opening}\nlet Int = Unit in let value : Int = () in value"
        )));
        let renamed_opening =
            opening.replace("from(prelude)", "from(Int)").replace("= prelude in", "= Int in");
        Fixture::accepted(&Fixture::provider(&format!(
            "let Int = prelude in {renamed_opening} let value : Int = 0 in value"
        )));
        Fixture::accepted(&Fixture::provider(&format!(
            "{opening}\n{opening}\nlet value : Bool = +True () in value"
        )));
        Fixture::rejected(
            &Fixture::provider(&format!("{opening}\nThk (Int -> OS)")),
            TyckDiagnosticCode::SortMismatch,
        );
    }
}

#[test]
fn independent_openings_do_not_share_os_even_from_the_same_package() {
    let source = r#"
let (/make = make_prelude) = @(import("$std/prelude.zy")) in
param val raw : @(import("$std/builtin.zy")) in
let (/process = left_process; left) = raw in
let (/process = right_process; right) = raw in
let prelude = make_prelude left in
OPENING
let stop : Thk (Int -> OS) = right_process/exit in
()
"#;
    for opening in [SUGAR, EXPLICIT] {
        Fixture::rejected(&source.replace("OPENING", opening), TyckDiagnosticCode::TypeMismatch);
        Fixture::accepted(
            &source
                .replace("OPENING", opening)
                .replace("= right_process/exit", "= left_process/exit"),
        );
    }
}

#[test]
fn prelude_has_no_ambient_provider_or_operations() {
    for source in [
        format!("{SUGAR} ()"),
        Fixture::provider("let value : Int = 0 in value"),
        Fixture::provider(&format!("{SUGAR}\n! exit 0")),
        Fixture::provider(&format!("{SUGAR}\nstdio")),
        Fixture::provider("@[prelude(core, from(missing))] ()"),
    ] {
        let error = Fixture::new().analyze(&source).unwrap_err();
        assert!(matches!(error, AnalysisError::Resolve { .. }), "{error:?}");
    }
    Fixture::accepted("()");
    Fixture::accepted(&Fixture::provider(&format!(
        "{SUGAR}\nlet stop : Thk (Int -> OS) = process/exit in ()"
    )));
}

#[test]
fn imported_sources_do_not_inherit_the_opening() {
    let fixture = Fixture::new();
    fixture.write("child.zy", "let value : Int = 0 in value");
    let error = fixture
        .analyze(&Fixture::provider(&format!("{SUGAR}\n@(import(\"child.zy\"))")))
        .unwrap_err();
    assert!(matches!(error, AnalysisError::Resolve { .. }));
    Fixture::accepted("let Int = @(intrinsic(int)) in let value : Int = 0 in value");
}

#[test]
fn primitive_leaf_and_adapter_are_dependency_tracked() {
    let fixture = Fixture::new();
    let leaf = fixture.analyze("@(import(\"$std/primitive.zy\"))").unwrap();
    assert!(leaf.outcome().root().is_some());
    // The root and leaf are the entire closure: no Builtin, data, or adapter dependency.
    assert_eq!(leaf.graph().source_inputs().count(), 2);
    Fixture::accepted("@(import(\"$std/prelude.zy\"))");
}

zydeco_tests::runtime_source!(shared, "tests/prelude/shared.zy");

#[test]
fn malformed_views_fail_like_handwritten_projection() {
    for opening in [SUGAR, EXPLICIT] {
        Fixture::rejected(
            &format!("let prelude = () in {opening} ()"),
            TyckDiagnosticCode::MissingNamedField,
        );
        Fixture::rejected(
            &Fixture::provider(&format!("let prelude = (prelude, prelude) in {opening} ()")),
            TyckDiagnosticCode::DuplicateNamedField,
        );
    }
}

#[test]
fn a_provider_can_supply_a_direct_view_under_its_own_witness() {
    let source = r#"
let (/VType; /CType; /Thk; /Int; /Unit; primitive) = @(import("$std/primitive.zy")) in
let bool = @(import("$std/data/bool.zy")) in
let option = @(import("$std/data/option.zy")) in
let either = @(import("$std/data/either.zy")) in
let static = (primitive, bool, option, either) in
let Provider = exists (HostOS : CType) .
  (#prelude :: exists (= OS as HostOS) . @[typeof] static)
  * (#process :: (#exit :: Thk (Int -> HostOS)))
in
param val (/prelude; /process; provider) : Provider in
OPENING
let stop : Thk (Int -> OS) = process/exit in
()
"#;
    for opening in [SUGAR, EXPLICIT] {
        Fixture::accepted(&source.replace("OPENING", opening));
    }
}

#[test]
fn provider_refresh_cannot_reuse_stale_view_evidence() {
    let mut fixture = Fixture::new();
    let adapter = fixture.write(
        "adapter.zy",
        r#"
let (/make) = @(import("$std/prelude.zy")) in (#make = make)
"#,
    );
    let source =
        Fixture::provider(&format!("{SUGAR}\nlet stop : Thk (Int -> OS) = process/exit in ()"))
            .replace("$std/prelude.zy", "adapter.zy");
    let root = fixture.write("main.zy", &source);
    let before = fixture.session.analyze(&root).unwrap();
    assert!(before.outcome().root().is_some());
    fixture
        .session
        .set_overlay(
            &adapter,
            Fixture::source(
                r#"let make = val (_ : @(import("$std/builtin.zy"))) => () in (#make = make)"#,
            ),
        )
        .unwrap();
    let after = fixture.session.analyze(&root).unwrap();
    assert!(!Arc::ptr_eq(&before, &after));
    assert!(after.outcome().root().is_none());
    assert!(
        after
            .outcome()
            .diagnostics()
            .unwrap()
            .iter()
            .any(|diagnostic| diagnostic.code == TyckDiagnosticCode::MissingNamedField)
    );
    fixture.session.clear_overlay(&adapter).unwrap();
    assert!(fixture.session.analyze(&root).unwrap().outcome().root().is_some());
}

#[test]
fn raw_providers_cannot_leak_a_fresh_os_through_the_adapter() {
    for opening in [SUGAR, EXPLICIT] {
        let source = Fixture::provider(&format!(
            "{opening}\npack (= Callback as Thk (Int -> OS)) where () end"
        ));
        Fixture::accepted(&source);
        Fixture::rejected(
            &source.replace("(/process; builtin)", "builtin"),
            TyckDiagnosticCode::PackageWitnessesUnavailable,
        );
    }
}

#[test]
fn independently_selected_packages_cannot_borrow_outer_core_names() {
    use zydeco_session::source::PackageId;
    let fixture = Fixture::new();
    let source = Fixture::provider(&format!(
        "{SUGAR}\n@[package(library, name(/example/nested))] let value : Int = 0 in value"
    ));
    let root = fixture.write("packages.zy", &source);
    assert!(fixture.session.analyze(&root).unwrap().outcome().root().is_some());
    let error = fixture
        .session
        .analyze_package(
            &PackageId { path: root, name: Some("/example/nested".parse().unwrap()) },
            Arc::default(),
        )
        .unwrap_err();
    assert!(matches!(error, AnalysisError::Resolve { .. }));
}

#[test]
fn packaged_aliases_cannot_escape_a_local_provider_opening() {
    for opening in [SUGAR, EXPLICIT] {
        let source = Fixture::provider(&format!(
            "{opening}\npack (= Callback as Thk (Int -> OS)) where () end"
        ));
        let local = source.replace("param val (/process; builtin)", "param val raw").replace(
            "let (/make = make_prelude)",
            "let (/process; builtin) = raw in\nlet (/make = make_prelude)",
        );
        Fixture::rejected(&local, TyckDiagnosticCode::EscapingExistential);
    }
}

#[test]
fn the_standard_view_contains_types_not_host_operations() {
    for field in ["stdio", "process", "Addr", "Reader", "Writer"] {
        Fixture::rejected(
            &Fixture::provider(&format!("let (/{field}) = prelude in ()")),
            TyckDiagnosticCode::MissingNamedField,
        );
    }
    Fixture::rejected(
        "let (/OS) = @(import(\"$std/primitive.zy\")) in ()",
        TyckDiagnosticCode::MissingNamedField,
    );
}

#[test]
fn provider_independent_library_leaves_share_the_primitive_package() {
    for name in [
        "data/bool",
        "data/option",
        "data/either",
        "algebra/monoid",
        "effect/monad",
        "numeric/dictionary",
        "memory/types",
        "memory/shape",
        "memory/fault",
        "memory/layout-error",
    ] {
        let analysis = Fixture::new().analyze(&format!("@(import(\"$std/{name}.zy\"))")).unwrap();
        assert!(analysis.outcome().root().is_some(), "{name}: {:?}", analysis.outcome());
        // Only the test root, the requested library, and the shared primitive leaf are loaded.
        assert_eq!(analysis.graph().source_inputs().count(), 3, "{name}");
        assert!(
            analysis
                .graph()
                .source_inputs()
                .any(|source| source.path.ends_with("std/primitive.zy")),
            "{name} must obtain its canonical types from the primitive leaf"
        );
    }
}

#[test]
fn handwritten_and_sugared_programs_have_the_same_runtime_result() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    let fixture = Fixture::new();
    let source = include_str!("../../../lib/tests/prelude/shared.zy").replace("../../std", "$std");
    for (name, source) in
        [("sugar.zy", source.clone()), ("explicit.zy", source.replace(SUGAR, EXPLICIT))]
    {
        let path = fixture.write(name, &source);
        for backend in
            [ExecutionTarget::Interpreter, ExecutionTarget::WasmAm, ExecutionTarget::WasmSps]
        {
            SourceProgram::setup(path.clone()).test(backend);
        }
    }
}

#[test]
fn standard_view_has_only_manifest_core_fields_and_unit() {
    Fixture::accepted(&Fixture::provider(&format!(
        r#"{SUGAR}
let Exact = exists (VType as VType) (CType as CType)
  (= Thk as Thk) (= Ret as Ret) (= OS as OS) (= Unit as Unit)
  (= Int8 as Int8) (= Int16 as Int16) (= Int32 as Int32) (= Int64 as Int64) (= Int as Int)
  (= UInt8 as UInt8) (= UInt16 as UInt16) (= UInt32 as UInt32) (= UInt64 as UInt64) (= UInt as UInt)
  (= Float32 as Float32) (= Float64 as Float64) (= Char as Char) (= String as String)
  (= Bool as Bool) (= Option as Option) (= Either as Either) . Unit
in
let exact : Exact = prelude in ()
"#
    )));
}

#[test]
fn sugar_has_no_additional_residual_code_or_runtime_metadata() {
    let fixture = Fixture::new();
    let source = include_str!("../../../lib/tests/prelude/shared.zy").replace("../../std", "$std");
    let compiler = zydeco_cli::CommandCompiler::default().with_lint_types(true);
    let sugar = compiler.lower(&fixture.write("sugar.zy", &source)).unwrap();
    let explicit =
        compiler.lower(&fixture.write("explicit.zy", &source.replace(SUGAR, EXPLICIT))).unwrap();
    use zydeco_stackir::low::syntax::{
        BuiltinValueRole, Computation, Cons, ExternalFunction, IntegerLiteral, Literal, Stack,
        Value, ValuePattern,
    };
    // Compare the complete residual tree modulo allocator identities: bind zero, then exit.
    for program in [&sugar.sps_low, &explicit.sps_low] {
        let arena = &program.arena().inner;
        let Computation::LetValue(binding) = &arena.compus[&program.root()] else { panic!() };
        let ValuePattern::Var(definition) = arena.vpats[&binding.binder] else { panic!() };
        assert!(matches!(
            arena.values[&binding.bindee],
            Value::Literal(Literal::Integer(IntegerLiteral::Int(0)))
        ));
        let Computation::ExternCall(call) = &arena.compus[&binding.tail] else { panic!() };
        assert_eq!(call.function, ExternalFunction::Host(BuiltinValueRole::Exit));
        let Stack::Arg(Cons(value, residual)) = arena.stacks[&call.stack] else { panic!() };
        assert!(matches!(arena.values[&value], Value::Var(found) if found == definition));
        assert!(matches!(arena.stacks[&residual], Stack::Var(_)));
    }
}
