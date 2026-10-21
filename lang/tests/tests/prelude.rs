//! The default and its exceptions use the real source pipeline, not harness bindings.
use std::{path::PathBuf, sync::Arc};
use zydeco_session::{AnalysisError, CompilerSession, ProgramAnalysis};
use zydeco_statics::TyckDiagnosticCode;
use zydeco_surface::metadata::CorePrelude;

struct Fixture {
    directory: tempfile::TempDir,
    session: CompilerSession,
    next_source: std::cell::Cell<usize>,
}

impl Fixture {
    fn new() -> Self {
        Self {
            directory: tempfile::tempdir().unwrap(),
            session: CompilerSession::default(),
            next_source: Default::default(),
        }
    }

    fn standard() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std").canonicalize().unwrap()
    }

    fn source(source: &str) -> String {
        source.replace("$std", Self::standard().to_str().unwrap())
    }

    fn write(&self, name: &str, source: &str) -> PathBuf {
        let path = self.directory.path().join(name);
        std::fs::write(&path, Self::source(source)).unwrap();
        path
    }

    fn analyze(&self, source: &str) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
        let index = self.next_source.get();
        self.next_source.set(index + 1);
        self.session.analyze(self.write(&format!("main-{index}.zy"), source))
    }

    fn accepted(source: &str) {
        let analysis = Self::new().analyze(source).unwrap();
        assert!(analysis.outcome().root().is_some(), "{source}\n{:?}", analysis.outcome());
    }

    fn rejected(source: &str, code: TyckDiagnosticCode) {
        let analysis = Self::new().analyze(source).unwrap();
        assert!(analysis.outcome().root().is_none(), "rejection publishes no checked root");
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| { diagnostic.code == code && diagnostic.primary.is_some() }),
            "{source}\n{diagnostics:?}"
        );
    }

    fn unbound(source: &str) {
        let error = Self::new().analyze(source).unwrap_err();
        assert!(matches!(error, AnalysisError::Resolve { .. }), "{error:?}");
    }

    fn provider(body: &str) -> String {
        format!(
            r#"param val (/process; builtin) : @(import("$std/builtin.zy")) in
{body}"#
        )
    }

    fn view(body: &str) -> String {
        Self::provider(&format!(
            r#"
let closed = @(import("$std/prelude.zy")) in
let prelude = (closed, pack (= OS as OS) where () end) in
{body}"#
        ))
    }

    fn explicit(source: &str) -> String {
        let selections = CorePrelude::closed_names()
            .map(|name| format!("/{name}"))
            .collect::<Vec<_>>()
            .join("; ");
        format!(
            r#"@[prelude(none)]
let ({selections}) = @(import("$std/prelude.zy")) in
{}"#,
            source.replace("/OS = HostOS", "/OS").replace("HostOS", "OS")
        )
    }
}

#[test]
fn closed_names_are_implicit_in_values_types_and_signatures() {
    for source in [
        "VType",
        "CType",
        "Int",
        "Thk (Ret Unit)",
        "Option Int",
        "Either String Bool",
        "val pi (A : VType) . Thk (A -> Ret A)",
        "let value : Option Int = +Some (42) in value",
        "let value : Bool = +True () in value",
    ] {
        Fixture::accepted(source);
    }
    let mut fixture = Fixture::new();
    fixture.write("value.zyi", "Option Int");
    let value = fixture.write("value.zy", "+Some (42)");
    assert!(fixture.session.analyze(value).unwrap().outcome().root().is_some());
    fixture.write("value.zyi", "Option String");
    fixture.session.refresh_disk(fixture.directory.path().join("value.zyi")).unwrap();
    let rejected = fixture.session.analyze(fixture.directory.path().join("value.zy")).unwrap();
    assert!(rejected.outcome().root().is_none());
    assert!(
        rejected
            .outcome()
            .diagnostics()
            .unwrap()
            .iter()
            .any(|diagnostic| diagnostic.code == TyckDiagnosticCode::TypeMismatch)
    );
}

#[test]
fn explicit_openings_supply_the_same_os_witness_without_a_view() {
    Fixture::accepted(&Fixture::provider("let stop : Thk (Int -> OS) = process/exit in ()"));
    Fixture::accepted(
        r#"
param val (/OS = HostOS; /process) : @(import("$std/builtin.zy")) in
let same : Thk (Thk HostOS -> OS) = { fn next => ! next } in
let stop : Thk (Int -> OS) = process/exit in ()
"#,
    );
    // The role, not the spelling of the binder or the signature's import path, identifies OS.
    Fixture::accepted(
        r#"
let Provider = exists @[builtin(os)] (HostOS : CType) . (#finish :: Thk (Int -> HostOS)) in
param val (/finish; custom) : Provider in
let stop : Thk (Int -> OS) = finish in ()
"#,
    );
    Fixture::accepted(
        r#"
let Provider = @(import("$std/builtin.zy")) in
val ((/process; arbitrary_name) : Provider) =>
  let stop : Thk (Int -> OS) = process/exit in ()
"#,
    );
}

#[test]
fn independent_openings_require_an_explicit_provider_selection() {
    let source = r#"
param val raw : @(import("$std/builtin.zy")) in
let (/process = left_process; left) = raw in
let (/process = right_process; right) = raw in
let stop : Thk (Int -> OS) = left_process/exit in ()
"#;
    Fixture::rejected(source, TyckDiagnosticCode::AmbiguousBuiltinTypeRole);
    let selected = source.replace("/process = left_process", "/OS; /process = left_process");
    Fixture::accepted(&selected);
    Fixture::rejected(
        &selected.replace("= left_process/exit", "= right_process/exit"),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn mobile_bindings_observe_provider_openings_independently_of_source_order() {
    for first in [true, false] {
        let provider = r#"param val (/process; builtin) : @(import("$std/builtin.zy")) that"#;
        let callback = "let Callback = Thk (Int -> OS) that";
        let bindings = if first {
            format!("{provider}\n{callback}")
        } else {
            format!("{callback}\n{provider}")
        };
        Fixture::accepted(&format!(
            "begin {bindings}\nlet stop : Callback = process/exit in () end"
        ));
    }
    // A binding depending on the OS user must not become an implicit back-edge.
    Fixture::accepted(
        r#"begin
let Callback = Thk (Int -> OS) that
let use_callback : Thk (Callback -> Ret Unit) = { fn _ => ret () } that
param val (/process) : @(import("$std/builtin.zy")) that
let stop : Callback = process/exit in ()
end"#,
    );
    Fixture::rejected(
        r#"begin
let Callback = Thk (Int -> OS) that
param val (/process = left) : @(import("$std/builtin.zy")) that
param val (/process = right) : @(import("$std/builtin.zy")) that
let stop : Callback = left/exit in ()
end"#,
        TyckDiagnosticCode::AmbiguousBuiltinTypeRole,
    );
}

#[test]
fn the_default_neither_acquires_a_provider_nor_imports_operations() {
    for source in [
        "OS",
        &Fixture::provider("process2"),
        &Fixture::provider("! exit 0"),
        &Fixture::provider("stdio"),
        "Addr",
        "Reader",
        "Writer",
    ] {
        if source == "OS" {
            Fixture::rejected(source, TyckDiagnosticCode::MissingBuiltinTypeRole);
        } else {
            Fixture::unbound(source);
        }
    }
    Fixture::rejected(
        r#"param val raw : @(import("$std/builtin.zy")) in
let Callback = Thk (Int -> OS) in ()"#,
        TyckDiagnosticCode::MissingBuiltinTypeRole,
    );
}

#[test]
fn opt_out_is_lexical_preserves_authored_names_and_restores_at_siblings() {
    Fixture::unbound("@[prelude(none)] let value : Int = 0 in value");
    Fixture::unbound("let value = () in @[prelude(none)] Int");
    Fixture::unbound(&Fixture::provider("@[prelude(none)] OS"));
    Fixture::accepted("let Int = Unit in @[prelude(none)] let value : Int = () in value");
    Fixture::accepted("(@[prelude(none)] (), (let value : Int = 0 in value))");
    Fixture::unbound("begin @[prelude(none)] let value : Int = 0 that value end");
    Fixture::accepted(
        "@[prelude(none)] let Int = @(intrinsic(int)) in let value : Int = 0 in value",
    );
    let fixture = Fixture::new();
    let disabled = fixture.analyze("@[prelude(none)] ()").unwrap();
    assert_eq!(disabled.graph().source_inputs().count(), 1, "no unused default dependency");
}

#[test]
fn authored_lexical_and_mobile_names_shadow_default_names() {
    Fixture::accepted("let Int = Unit in let value : Int = () in value");
    Fixture::accepted("begin let value : Int = () that let Int = Unit that value end");
    Fixture::rejected(
        "begin let value : Int = 0 that let Int = Unit that value end",
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn imported_sources_get_their_own_defaults_but_not_the_callers_os() {
    let fixture = Fixture::new();
    fixture.write("child.zy", "let value : Int = 0 in value");
    assert!(
        fixture
            .analyze("@[prelude(none)] @(import(\"child.zy\"))")
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
    fixture.write("disabled.zy", "@[prelude(none)] let value : Int = 0 in value");
    assert!(matches!(
        fixture.analyze("@(import(\"disabled.zy\"))"),
        Err(AnalysisError::Resolve { .. })
    ));
    fixture.write("os.zy", "OS");
    let analysis = fixture.analyze(&Fixture::provider("@(import(\"os.zy\"))")).unwrap();
    assert!(analysis.outcome().root().is_none());
    assert!(
        analysis
            .outcome()
            .diagnostics()
            .unwrap()
            .iter()
            .any(|diagnostic| diagnostic.code == TyckDiagnosticCode::MissingBuiltinTypeRole)
    );
}

#[test]
fn selected_packages_have_defaults_and_inherit_a_file_opt_out() {
    use zydeco_session::source::PackageId;
    let fixture = Fixture::new();
    let root = fixture.write(
        "packages.zy",
        r#"
let unrelated = 1 in
@[package(library, name(/example/nested))] let value : Int = 0 in value
"#,
    );
    let package = PackageId { path: root, name: Some("/example/nested".parse().unwrap()) };
    assert!(
        fixture
            .session
            .analyze_package(&package, Arc::default())
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
    let disabled = fixture.write(
        "disabled.zy",
        r#"@[prelude(none)]
let unrelated = 1 in
@[package(library, name(/example/nested))] let value : Int = 0 in value
"#,
    );
    let package = PackageId { path: disabled, name: package.name };
    assert!(matches!(
        fixture.session.analyze_package(&package, Arc::default()),
        Err(AnalysisError::Resolve { .. })
    ));
}

#[test]
fn defaults_track_a_small_closed_dependency_graph() {
    let fixture = Fixture::new();
    let analysis = fixture.analyze("let value : Bool = +True () in value").unwrap();
    assert!(analysis.outcome().root().is_some());
    assert_eq!(analysis.graph().source_inputs().count(), 6);
    assert!(analysis.graph().sources[&analysis.graph().root].prelude.is_some());
    assert!(!analysis.graph().source_inputs().any(|source| source.path.ends_with("builtin.zy")));
    let leaf = fixture.analyze("@[prelude(none)] @(import(\"$std/primitive.zy\"))").unwrap();
    assert_eq!(leaf.graph().source_inputs().count(), 2);
    Fixture::accepted(
        "let (/Bool = ExplicitBool) = @(import(\"$std/data/bool.zy\")) in\nlet value : ExplicitBool = (+True () : Bool) in value",
    );
}

#[test]
fn a_bootstrap_cycle_is_reported_and_correcting_the_overlay_recovers() {
    use zydeco_session::source::{SourceDependencyKind, SourceLoadError};
    let mut fixture = Fixture::new();
    let root = fixture.write("main.zy", "let value : Int = 0 in value");
    assert!(fixture.session.analyze(&root).unwrap().outcome().root().is_some());
    fixture.session.set_overlay(Fixture::standard().join("prelude.zy"), "()".into()).unwrap();
    let error = fixture.session.analyze(&root).unwrap_err();
    let AnalysisError::Source { error } = error else { panic!("expected a source cycle") };
    assert!(error.iter().any(|error| matches!(error, SourceLoadError::Cycle(cycle)
        if cycle.steps.iter().any(|step| matches!(step.kind, SourceDependencyKind::Prelude)))));
    fixture.session.clear_overlay(Fixture::standard().join("prelude.zy")).unwrap();
    assert!(fixture.session.analyze(&root).unwrap().outcome().root().is_some());
}

#[test]
fn changing_the_implicit_dependency_invalidates_checked_results() {
    let mut fixture = Fixture::new();
    let root = fixture.write("main.zy", "let value : Int = 0 in value");
    let before = fixture.session.analyze(&root).unwrap();
    assert!(before.outcome().root().is_some());
    let default = Fixture::standard().join("prelude.zy");
    fixture.session.set_overlay(&default, "@[prelude(none)] ()".into()).unwrap();
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
    fixture.session.clear_overlay(&default).unwrap();
    assert!(fixture.session.analyze(&root).unwrap().outcome().root().is_some());
}

#[test]
fn replacement_views_are_ordinary_shared_projections() {
    let explicit = format!(
        "let ({}) = prelude in",
        CorePrelude::names().iter().map(|name| format!("/{name}")).collect::<Vec<_>>().join("; ")
    );
    for opening in ["@[prelude(from(prelude))]", explicit.as_str()] {
        Fixture::accepted(&Fixture::view(&format!(
            "{opening} let stop : Thk (Int -> OS) = process/exit in ()"
        )));
        Fixture::accepted(&Fixture::view(&format!(
            "{opening} {opening} let value : Bool = +True () in value"
        )));
        Fixture::rejected(
            &format!("let prelude = () in {opening} ()"),
            TyckDiagnosticCode::MissingNamedField,
        );
        Fixture::rejected(
            &Fixture::view(&format!("let prelude = (prelude, prelude) in {opening} ()")),
            TyckDiagnosticCode::DuplicateNamedField,
        );
    }
    Fixture::unbound("@[prelude(from(missing))] ()");
}

#[test]
fn a_replacement_can_change_a_default_type() {
    let fields = CorePrelude::names()
        .iter()
        .filter(|name| !matches!(**name, "VType" | "CType"))
        .map(|name| format!("(= {name} as {name})"))
        .collect::<Vec<_>>()
        .join(" ");
    let source = Fixture::provider(&format!(
        r#"
let replacement = (
  let Int = Unit in
  let types = pack {fields} where () end in
  ((VType, CType, types) : exists (VType as VType) (CType as CType) . @[typeof] types)
) in
OPENING let value : Int = () in value"#
    ));
    Fixture::accepted(&source.replace("OPENING", "@[prelude(from(replacement))]"));
    Fixture::rejected(&source.replace("OPENING", ""), TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn a_provider_can_supply_its_own_view_without_a_builtin_role() {
    let source = r#"
let static = @(import("$std/prelude.zy")) in
let Provider = exists (HostOS : CType) .
  (#prelude :: exists (= OS as HostOS) . @[typeof] static)
  * (#process :: (#exit :: Thk (Int -> HostOS))) in
param val (/prelude; /process; provider) : Provider in
OPENING let stop : Thk (Int -> OS) = process/exit in ()
"#;
    Fixture::accepted(&source.replace("OPENING", "@[prelude(from(prelude))]"));
    Fixture::rejected(&source.replace("OPENING", ""), TyckDiagnosticCode::MissingBuiltinTypeRole);
}

#[test]
fn static_only_openings_can_return_classifiers_without_escaping_witnesses() {
    Fixture::accepted("@[prelude(none)] let (/Int) = @(import(\"$std/primitive.zy\")) in Int");
    Fixture::accepted("@[prelude(none)] let (/VType) = @(import(\"$std/primitive.zy\")) in VType");
    Fixture::rejected("let value = 0 in Int", TyckDiagnosticCode::SortMismatch);
    Fixture::rejected(
        r#"param val raw : @(import("$std/builtin.zy")) in
let (/OS) = raw in OS"#,
        TyckDiagnosticCode::EscapingExistential,
    );
    Fixture::rejected(
        r#"param val raw : @(import("$std/builtin.zy")) in
let (/process; builtin) = raw in pack (= Callback as Thk (Int -> OS)) where () end"#,
        TyckDiagnosticCode::EscapingExistential,
    );
}

#[test]
fn the_closed_view_exports_no_runtime_provider_or_operations() {
    for field in ["OS", "stdio", "process", "Addr", "Reader", "Writer"] {
        Fixture::rejected(
            &format!("let (/{field}) = @(import(\"$std/prelude.zy\")) in ()"),
            TyckDiagnosticCode::MissingNamedField,
        );
    }
}

#[test]
fn provider_independent_libraries_need_no_manual_primitive_import() {
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
        assert!(
            !analysis.graph().source_inputs().any(|source| source.path.ends_with("builtin.zy"))
        );
        assert!(
            analysis
                .graph()
                .source_inputs()
                .any(|source| source.path.ends_with("std/primitive.zy"))
        );
    }
}

zydeco_tests::runtime_source!(shared, "tests/prelude/shared.zy");

#[test]
fn implicit_and_explicit_programs_have_the_same_runtime_result() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    let fixture = Fixture::new();
    let source = include_str!("../../../lib/tests/prelude/shared.zy").replace("../../std", "$std");
    for (name, source) in
        [("implicit.zy", source.clone()), ("explicit.zy", Fixture::explicit(&source))]
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
fn defaults_add_no_residual_code_or_runtime_metadata() {
    let fixture = Fixture::new();
    let source = include_str!("../../../lib/tests/prelude/shared.zy").replace("../../std", "$std");
    let compiler = zydeco_cli::CommandCompiler::default().with_lint_types(true);
    let implicit = compiler.lower(&fixture.write("implicit.zy", &source)).unwrap();
    let explicit =
        compiler.lower(&fixture.write("explicit.zy", &Fixture::explicit(&source))).unwrap();
    use zydeco_stackir::low::syntax::{
        BuiltinValueRole, Computation, Cons, ExternalFunction, IntegerLiteral, Literal, Stack,
        Value, ValuePattern,
    };
    for program in [&implicit.sps_low, &explicit.sps_low] {
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

#[test]
fn an_unused_default_disappears_before_interpreter_linking() {
    let fixture = Fixture::new();
    for source in
        ["ret 0", r#"param (/process) : @(import("$std/builtin.zy")) in ! process/exit 0"#]
    {
        let nodes = |source: &str| {
            let analysis = fixture.analyze(source).unwrap();
            let checked = fixture.session.checked_program(&analysis).unwrap();
            zydeco_statics::traverse::RuntimeGraph { statics: &checked.statics }
                .nodes(checked.root)
                .count()
        };
        assert_eq!(nodes(source), nodes(&format!("@[prelude(none)] {source}")), "{source}");
    }
}

#[test]
fn runtime_erasure_keeps_abstract_opening_evidence_and_ordinary_value_bindings() {
    use zydeco_statics::syntax::{Abs, TermAnnId, Value};
    let fixture = Fixture::new();
    let analysis = fixture
        .analyze("param val raw : (exists (A : VType) . Unit) in let (/A) = raw in ()")
        .unwrap();
    let checked = fixture.session.checked_program(&analysis).unwrap();
    let TermAnnId::Value(root, _) = checked.root else { panic!("expected a value function") };
    let Value::ValAbs(Abs(_, body)) = checked.statics.values[&root] else {
        panic!("expected a value abstraction")
    };
    assert!(
        matches!(checked.statics.values[&body], Value::Let(_)),
        "a fresh witness keeps the opening needed for static specialization"
    );

    let source = r#"param (/process) : @(import("$std/builtin.zy")) in
let ignored = (_ : Unit) in ! process/exit 0"#;
    let error =
        zydeco_cli::CommandCompiler::default().executable(&fixture.write("hole.zy", source));
    assert!(
        matches!(
            error,
            Err(zydeco_cli::CompileError::Executable(zydeco_session::ExecutableError::Hole(_)))
        ),
        "ordinary runtime holes remain reachable"
    );
}
