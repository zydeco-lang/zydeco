use super::*;
use crate::source::{AnalysisError, CompilerSession, SourceParseError};
use zydeco_surface::{scoped::ResolveError, textual::PackageDirectiveError};

pub(super) struct Fixture {
    pub(super) directory: tempfile::TempDir,
}
impl Fixture {
    pub(super) fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }
    pub(super) fn path(&self, path: &str) -> PathBuf {
        self.directory.path().join(path)
    }
    pub(super) fn write(&self, path: &str, source: &str) -> PathBuf {
        let path = self.path(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, source).unwrap();
        path
    }
    fn id(&self, path: &str) -> PackageId {
        PackageId { path: self.path(path), name: None }
    }
    fn named(&self, path: &str, name: &str) -> PackageId {
        PackageId { path: self.path(path), name: Some(name.parse().unwrap()) }
    }
    fn catalog(&self, session: &CompilerSession, files: &[&str]) -> PackageCatalog {
        session
            .package_catalog(&files.iter().map(|file| self.path(file)).collect::<Vec<_>>())
            .unwrap()
    }
}

#[test]
fn test_side_associations_work_locally_without_discovery_or_forward_edges() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"(
        #lib = @[package(library, name(lib))] 1,
        #other = @[package(library, name(other))] 2,
        #smoke = @[package(test(of(lib, other)), name(smoke))] @(import(lib)),
        #plain = @[package(test, name(plain))] absent
    )"#,
    );
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    for name in ["lib", "other"] {
        let plan = session.package_tests(&fixture.named("workspace.zy", name), &catalog).unwrap();
        assert_eq!(plan.tests.len(), 1);
        assert_eq!(plan.tests[0].id.name.as_ref().unwrap().to_string(), "smoke");
        assert!(
            session
                .analyze_package(&plan.tests[0].id, catalog.bindings.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
    }
    let plain = session.package_tests(&fixture.named("workspace.zy", "plain"), &catalog).unwrap();
    assert_eq!(plain.tests.len(), 1, "plain tests remain directly selectable");
    assert_eq!(plain.tests[0].id, plain.root.id);
}

#[test]
fn explicit_discovery_finds_reverse_tests_and_deduplicates_forward_edges() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"@[discover(include("tests/**/*.zy"), exclude("tests/fixtures/**"))]
        (#lib = @[package(library, test("tests/smoke.zy"), name(lib))] @(import("lib.zy")))"#,
    );
    fixture.write("lib.zy", "1");
    fixture.write("tests/smoke.zy", r#"@[package(test(of(lib)))] @(import(lib))"#);
    fixture.write("tests/unit/second.zy", r#"@[package(test(of(lib)))] 2"#);
    fixture.write("tests/plain.zy", "@[package(test)] absent");
    fixture.write("tests/unrelated.zy", r#"@[package(test(of("../missing.zy")))] absent"#);
    fixture.write("tests/fixtures/broken.zy", "(");
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let plan = session.package_tests(&fixture.named("workspace.zy", "lib"), &catalog).unwrap();
    assert_eq!(plan.tests.len(), 2);
    assert!(plan.tests.iter().all(|package| package.id.name.is_none()));
    for test in plan.tests {
        assert!(
            session
                .analyze_package(&test.id, catalog.bindings.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
    }
    assert_eq!(session.package_catalog(&[fixture.path("workspace.zy")]).unwrap().packages.len(), 5);
    assert!(
        session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.is_empty(),
        "an implementation does not inherit its registration's scope or identity"
    );
}

#[test]
fn discovery_is_not_expanded_by_checks_imports_or_matched_files() {
    let fixture = Fixture::new();
    let root = fixture.write("lib.zy", r#"@[discover(include("tests/*.zy"))] 1"#);
    fixture.write(
        "tests/smoke.zy",
        r#"@[discover(include("fixtures/*.zy"))]
        @[package(test(of("../lib.zy")))] @(import("../lib.zy"))"#,
    );
    fixture.write("tests/fixtures/broken.zy", "(");
    let main = fixture.write("main.zy", r#"@(import("lib.zy"))"#);
    let mut session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    assert_eq!(session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.len(), 1);
    fixture.write("tests/broken.zy", "(");
    assert_eq!(
        session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.len(),
        1,
        "prepared scope does not rescan"
    );
    assert!(session.analyze(&root).unwrap().outcome().root().is_some());
    assert!(session.analyze(main).unwrap().outcome().root().is_some());
    assert!(session.package(&fixture.id("lib.zy")).is_ok());
    let error = session.package_catalog(std::slice::from_ref(&root)).unwrap_err();
    assert!(
        matches!(error, SourceLoadError::Package(error) if matches!(*error, PackageError::DiscoveredSource { .. }))
    );
    session
        .set_overlay(
            &root,
            r#"@[discover(include("tests/*.zy"), exclude("tests/broken.zy"))] 1"#.into(),
        )
        .unwrap();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    assert_eq!(session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.len(), 1);
}

#[test]
fn discovery_membership_is_fresh_and_includes_overlay_only_sources() {
    let fixture = Fixture::new();
    fixture
        .write("lib.zy", r#"@[discover(include("tests/**/*.zy"), exclude("tests/ignored.zy"))] 1"#);
    let mut session = CompilerSession::default();
    let id = fixture.id("lib.zy");
    assert!(
        session
            .package_tests(&id, &fixture.catalog(&session, &["lib.zy"]))
            .unwrap()
            .tests
            .is_empty()
    );
    let test = fixture.write("tests/disk.zy", r#"@[package(test(of("../lib.zy")))] 1"#);
    assert_eq!(
        session.package_tests(&id, &fixture.catalog(&session, &["lib.zy"])).unwrap().tests.len(),
        1
    );
    std::fs::remove_file(test).unwrap();
    assert!(
        session
            .package_tests(&id, &fixture.catalog(&session, &["lib.zy"]))
            .unwrap()
            .tests
            .is_empty(),
        "cached text must not keep a removed match alive"
    );
    let overlay = fixture.path("tests/new/overlay.zy");
    session.set_overlay(&overlay, r#"@[package(test(of("../../lib.zy")))] 1"#.into()).unwrap();
    session.set_overlay(fixture.path("tests/ignored.zy"), "(".into()).unwrap();
    assert_eq!(
        session.package_tests(&id, &fixture.catalog(&session, &["lib.zy"])).unwrap().tests.len(),
        1
    );
    session.clear_overlay(overlay).unwrap();
    assert!(
        session
            .package_tests(&id, &fixture.catalog(&session, &["lib.zy"]))
            .unwrap()
            .tests
            .is_empty()
    );
}

#[test]
fn discovered_file_packages_need_no_role_and_plain_tests_need_no_subject() {
    let fixture = Fixture::new();
    let root = fixture.write("lib.zy", r#"@[discover(include("tests/*.zy"))] 1"#);
    fixture.write("tests/helper.zy", "1");
    fixture.write("tests/plain.zy", "@[package(test)] 2");
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    let packages = session.package_catalog(&[root]).unwrap().packages;
    assert_eq!(
        packages.iter().map(|package| package.role).collect::<Vec<_>>(),
        [PackageRole::Library, PackageRole::Library, PackageRole::Test]
    );
    assert!(session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.is_empty());
    assert_eq!(
        session.package_tests(&fixture.id("tests/plain.zy"), &catalog).unwrap().tests.len(),
        1
    );
}

#[test]
fn sibling_test_associations_share_canonical_ids_and_include_editor_overlays() {
    let fixture = Fixture::new();
    let root = fixture.write("lib/std/std.zy", r#"@[discover(include("../tests/*.zy", "../../lib/tests/*.zy"), exclude("../tests/ignored.zy"))] 1"#);
    fixture.write(
        "lib/tests/disk.zy",
        r#"@[package(test(of("../std/std.zy")))] @(import("../std/std.zy"))"#,
    );
    fixture.write("lib/tests/ignored.zy", "(");
    let mut session = CompilerSession::default();
    session
        .set_overlay(
            fixture.path("lib/tests/new.zy"),
            r#"@[package(test(of("../std/std.zy")))] 2"#.into(),
        )
        .unwrap();
    let catalog = fixture.catalog(&session, &["lib/std/std.zy"]);
    let plan = session.package_tests(&fixture.id("lib/std/std.zy"), &catalog).unwrap();
    assert_eq!(plan.tests.len(), 2, "overlapping parent prefixes identify one test each");
    for test in plan.tests {
        assert_eq!(test.id, session.package(&test.id).unwrap().id);
        assert!(
            session
                .analyze_package(&test.id, catalog.bindings.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
    }
    fixture.write("lib/tests/broken.zy", "(");
    assert!(
        session.analyze(&root).unwrap().outcome().root().is_some(),
        "ordinary imports and checking never expand discovery"
    );
    let error = session.package_catalog(std::slice::from_ref(&root)).unwrap_err();
    assert!(
        matches!(error, SourceLoadError::Package(error) if matches!(*error, PackageError::DiscoveredSource { .. }))
    );
}

#[test]
fn selected_roots_only_report_their_own_documentation_and_warnings() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"(
        #one = @[package(library, name(one))] (
            --| one documentation
            @[doc] 1,
            --| one warning
            2),
        #two = @[package(library, name(two))] (
            --| two documentation
            @[doc] 3,
            --| two warning
            4))"#,
    );
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    for name in ["one", "two"] {
        let analysis = session
            .analyze_package(&fixture.named("workspace.zy", name), catalog.bindings.clone())
            .unwrap();
        let graph = analysis.graph();
        let documentation = graph.documentation();
        let warnings = graph.warnings();
        assert_eq!(documentation.len(), 1);
        assert_eq!(warnings.len(), 1);
        assert_eq!(
            documentation[0].site.directive.comment.as_ref().unwrap().text.as_ref(),
            format!("{name} documentation")
        );
        assert_eq!(warnings[0].warning_source().trim(), format!("--| {name} warning"));
    }
}

#[test]
fn concluding_files_register_imports_without_loading_implementation_or_relationship_targets() {
    let fixture = Fixture::new();
    let path = fixture.write("workspace.zy", r#"(
        #library = @[package(library, test(smoke), documentation("docs.zy"), name(library))] @(import("math.zy")),
        #binary = @[package(binary, name(binary))] @(import("tool.zy"))
    )"#);
    let packages = CompilerSession::default().package_catalog(&[path]).unwrap().packages;
    assert!(
        Arc::ptr_eq(&packages[0].source, &packages[1].source),
        "inspection shares the containing file instead of cloning a template per registration"
    );
    assert_eq!(
        packages
            .iter()
            .map(|p| (p.id.name.as_ref().unwrap().to_string(), p.role))
            .collect::<Vec<_>>(),
        [("binary".into(), PackageRole::Binary), ("library".into(), PackageRole::Library)]
    );
    let library = &packages[1];
    assert_eq!(packages[0].imports.len(), 1, "each package reports only its own code");
    assert_eq!(library.imports.len(), 1);
    assert_eq!(library.relations.len(), 2);
    assert_eq!(library.relations[0].kind, PackageRelationKind::Test);
    assert!(
        matches!(&library.relations[1].kind, PackageRelationKind::Custom(name) if name.to_string() == "documentation")
    );
    assert!(library.source.source[library.origin.range().clone()].starts_with("package("));
}

#[test]
fn any_file_is_a_library_and_root_annotations_supply_roles_without_names() {
    let fixture = Fixture::new();
    for (source, role) in [
        ("42", PackageRole::Library),
        ("@[package(library)] 42", PackageRole::Library),
        ("(@[doc] (@[package(test)] 42) : @(intrinsic(i64)))", PackageRole::Test),
    ] {
        let path = fixture.write("single.zy", source);
        let session = CompilerSession::default();
        let catalog = fixture.catalog(&session, &["single.zy"]);
        let packages = session.package_catalog(std::slice::from_ref(&path)).unwrap().packages;
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0].id.name, None);
        assert_eq!(packages[0].role, role);
        let analysis =
            session.analyze_package(&fixture.id("single.zy"), catalog.bindings.clone()).unwrap();
        assert!(analysis.outcome().root().is_some());
        assert_eq!(analysis.graph().sources.len(), 1);
    }
}

#[test]
fn package_selection_ignores_surrounding_terms_and_unrelated_imports() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"let unused = @(import("missing.zy")) in
        (#one = @[package(library, name(one))] 1, #two = @[package(library, name(two))] "two", #broken = unknown)"#,
    );
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let one = session
        .analyze_package(&fixture.named("workspace.zy", "one"), catalog.bindings.clone())
        .unwrap();
    let two = session
        .analyze_package(&fixture.named("workspace.zy", "two"), catalog.bindings.clone())
        .unwrap();
    assert!(one.outcome().root().is_some() && two.outcome().root().is_some());
    assert_eq!(one.graph().imports.len(), 0);
    assert_eq!(two.graph().imports.len(), 0);
    assert_ne!(
        one.graph().sources[&one.graph().root].root,
        two.graph().sources[&two.graph().root].root
    );
    assert!(Arc::ptr_eq(
        &one.graph().sources[&one.graph().root].template,
        &two.graph().sources[&two.graph().root].template,
    ));
    assert!(session.checked_program(&one).is_some(), "rematerialization retains the selection");
    assert!(session.checked_program(&two).is_some());
    assert!(session.analyze(fixture.path("workspace.zy")).is_err());
}

#[test]
fn annotations_preserve_local_scope_but_separate_entries_require_self_contained_terms() {
    let fixture = Fixture::new();
    let source = "let outer = 1 in (#open = @[package(library, name(open))] outer)";
    let path = fixture.write("workspace.zy", source);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let local = session.analyze(&path).unwrap();
    assert!(local.outcome().root().is_some());
    assert_eq!(local.graph().imports.len(), 0, "annotations do not create source boundaries");
    let error = session
        .analyze_package(&fixture.named("workspace.zy", "open"), catalog.bindings.clone())
        .unwrap_err();
    assert!(
        matches!(&error, AnalysisError::Resolve { error, .. } if matches!(&**error, ResolveError::UnboundVar(name) if name.inner.0 == "outer"))
    );
    let site = error.diagnostic_site().unwrap();
    assert_eq!(site.path(), path.canonicalize().unwrap());
    assert_eq!(&source[site.range().clone()], "outer");
}

#[test]
fn repeated_named_imports_share_roots_and_file_snapshot_inputs() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        "(#one = @[package(library, name(one))] 1, #two = @[package(library, name(two))] 2)",
    );
    fixture.write("main.zy", r#"(@(import(one)), @(import(one)), @(import(two)))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let analysis =
        session.analyze_package(&fixture.id("main.zy"), catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    let graph = analysis.graph();
    let edges = &graph.sources[&graph.root].imports;
    assert_eq!(graph.imports[&edges[0]].imported, graph.imports[&edges[1]].imported);
    assert_ne!(graph.imports[&edges[0]].imported, graph.imports[&edges[2]].imported);
    assert_eq!(graph.sources.len(), 3);
    assert_eq!(analysis.sources().count(), 2);
}

#[test]
fn file_packages_retain_companions_and_registration_imports_share_the_file_root() {
    let fixture = Fixture::new();
    fixture.write("lib.zy", "@[package(library)] 1");
    fixture.write("lib.zyi", "@(intrinsic(i64))");
    fixture.write("workspace.zy", r#"(#lib = @[package(library, name(lib))] @(import("lib.zy")))"#);
    fixture.write("main.zy", r#"(@(import(lib)), @(import("lib.zy")))"#);
    let mut session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let analysis =
        session.analyze_package(&fixture.id("main.zy"), catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    let graph = analysis.graph();
    let edges = &graph.sources[&graph.root].imports;
    let registration = graph.imports[&edges[0]].imported;
    let direct = graph.imports[&edges[1]].imported;
    assert_eq!(graph.imports[&graph.sources[&registration].imports[0]].imported, direct);
    assert!(graph.sources[&direct].signature.is_some());
    session.set_overlay(fixture.path("lib.zyi"), "@(intrinsic(string))".into()).unwrap();
    assert!(
        session
            .analyze_package(&fixture.id("main.zy"), catalog.bindings.clone())
            .unwrap()
            .outcome()
            .root()
            .is_none()
    );
}

#[test]
fn local_annotated_terms_remain_local_and_only_explicit_imports_create_edges() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"(#lib = @[doc] (@[package(library, name(lib))] 1),
        #again = @(import(lib)))"#,
    );
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let analysis =
        session.analyze_package(&fixture.id("workspace.zy"), catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    let graph = analysis.graph();
    let edges = &graph.sources[&graph.root].imports;
    assert_eq!(edges.len(), 1);
    assert_ne!(graph.imports[&edges[0]].imported, graph.root);
    assert_eq!(graph.sources.len(), 2);
    assert_eq!(analysis.sources().count(), 1);
}

#[test]
fn explicit_parameters_and_nested_packages_are_ordinary_code_components() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"@[package(library)]
        (#inner = @[package(library, name(inner))] val (x : @(intrinsic(i64))) => x)"#,
    );
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let package = session.package(&fixture.id("workspace.zy")).unwrap();
    assert!(package.imports.is_empty());
    assert!(package.relations.is_empty());
    let analysis = session.analyze_package(&package.id, catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    assert_eq!(analysis.graph().sources.len(), 1);
    assert!(
        session
            .analyze_package(&fixture.named("workspace.zy", "inner"), catalog.bindings.clone())
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
}

#[test]
fn errors_in_multiple_selected_packages_keep_original_file_offsets() {
    let fixture = Fixture::new();
    let source =
        "(#one = @[package(library, name(one))] 1, #bad = @[package(library, name(bad))] absent)";
    let path = fixture.write("workspace.zy", source);
    let error = CompilerSession::default().analyze(&path).unwrap_err();
    let site = error.diagnostic_site().unwrap();
    assert_eq!(site.path(), path.canonicalize().unwrap());
    assert_eq!(&source[site.range().clone()], "absent");
}

#[test]
fn nested_package_does_not_inherit_the_whole_files_companion() {
    let fixture = Fixture::new();
    fixture.write("lib.zy", "(#lib = @[package(library, name(lib))] 1)");
    fixture.write("lib.zyi", "@(intrinsic(string))");
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    let analysis =
        session.analyze_package(&fixture.named("lib.zy", "lib"), catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    assert!(analysis.graph().sources[&analysis.graph().root].signature.is_none());
    assert!(session.analyze(fixture.path("lib.zy")).unwrap().outcome().root().is_none());
}

#[test]
fn test_associations_and_returning_code_dependencies_are_not_code_cycles() {
    let fixture = Fixture::new();
    fixture.write("lib.zy", r#"@[package(library, test("tests.zy"))] 1"#);
    fixture.write("tests.zy", r#"@[package(test)] @(import("lib.zy"))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    let id = fixture.id("lib.zy");
    assert_eq!(
        session.analyze_package(&id, catalog.bindings.clone()).unwrap().graph().sources.len(),
        1
    );
    let plan = session.package_tests(&id, &catalog).unwrap();
    assert_eq!(plan.tests.len(), 1);
    let test = session.analyze_package(&plan.tests[0].id, catalog.bindings.clone()).unwrap();
    assert!(test.outcome().root().is_some());
    assert_eq!(test.graph().sources.len(), 2);
}

#[test]
fn concluding_files_can_register_unannotated_implementations_and_be_split() {
    let fixture = Fixture::new();
    fixture.write(
        "workspace.zy",
        r#"(#lib = @[package(library, test(smoke), name(lib))] @(import("lib.zy")),
        #missing = @[package(binary, name(missing))] @(import("missing.zy")))"#,
    );
    fixture
        .write("testing.zy", r#"(#smoke = @[package(test, name(smoke))] @(import("tests.zy")))"#);
    fixture.write("lib.zy", "1");
    fixture.write("tests.zy", r#"@(import(lib))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy", "testing.zy"]);
    let id = fixture.named("workspace.zy", "lib");
    assert!(
        session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap().tests.is_empty(),
        "registration does not mutate the imported file"
    );
    let library = session.analyze_package(&id, catalog.bindings.clone()).unwrap();
    assert_eq!(library.graph().sources.len(), 2, "no test or unrelated entry is loaded");
    let plan = session.package_tests(&id, &catalog).unwrap();
    assert_eq!(plan.tests.len(), 1);
    let analysis = session.analyze_package(&plan.tests[0].id, catalog.bindings.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    assert_eq!(analysis.sources().count(), 4);
}

#[test]
fn same_file_test_associations_work_but_real_code_cycles_are_rejected() {
    let fixture = Fixture::new();
    let path = fixture.write(
        "workspace.zy",
        r#"(#lib = @[package(library, test(smoke), name(lib))] 1,
        #smoke = @[package(test, name(smoke))] @(import(lib)))"#,
    );
    let mut session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy"]);
    let plan = session.package_tests(&fixture.named("workspace.zy", "lib"), &catalog).unwrap();
    assert!(
        session
            .analyze_package(&plan.tests[0].id, catalog.bindings.clone())
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
    session
        .set_overlay(
            path,
            r#"(#lib = @[package(library, name(lib))] @(import(smoke)),
        #smoke = @[package(test, name(smoke))] @(import(lib)))"#
                .into(),
        )
        .unwrap();
    let error = session
        .analyze_package(&fixture.named("workspace.zy", "lib"), catalog.bindings.clone())
        .unwrap_err();
    assert!(
        matches!(error, AnalysisError::Source { error } if matches!(&*error, SourceLoadError::Cycle(cycle) if cycle.steps.len() == 2))
    );
}

#[test]
fn missing_tests_and_custom_kinds_do_not_break_code_use_but_fail_test_planning() {
    let fixture = Fixture::new();
    for kind in ["test", "benchmark"] {
        let path =
            fixture.write("lib.zy", &format!(r#"@[package(library, {kind}("absent.zy"))] 1"#));
        let session = CompilerSession::default();
        let catalog = fixture.catalog(&session, &["lib.zy"]);
        let id = fixture.id("lib.zy");
        assert!(
            session
                .analyze_package(&id, catalog.bindings.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
        let error = session.package_tests(&id, &catalog).unwrap_err();
        let SourceLoadError::Package(inner) = &error else { panic!("package error") };
        match kind {
            | "test" => assert!(
                matches!(&**inner, PackageError::Relation { error, .. } if matches!(**error, SourceLoadError::Read { .. }))
            ),
            | _ => assert!(matches!(**inner, PackageError::UnsupportedRelation { .. })),
        }
        assert_eq!(error.diagnostic_site().unwrap().path(), path.canonicalize().unwrap());
    }
}

#[test]
fn test_planning_is_direct_and_deduplicates_canonical_targets() {
    let fixture = Fixture::new();
    fixture.write(
        "lib.zy",
        r#"@[package(library, test("tests.zy"), test("./tests.zy"))] @(import("dependency.zy"))"#,
    );
    fixture.write("dependency.zy", r#"@[package(library, test("absent.zy"))] 1"#);
    fixture.write("tests.zy", r#"@[package(test, test("absent.zy"))] @(import("lib.zy"))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["lib.zy"]);
    let plan = session.package_tests(&fixture.id("lib.zy"), &catalog).unwrap();
    assert_eq!(plan.tests.len(), 1);
    assert!(
        session
            .analyze_package(&plan.tests[0].id, catalog.bindings.clone())
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
}

#[test]
fn test_targets_require_test_role_while_ordinary_imports_accept_any_role() {
    let fixture = Fixture::new();
    fixture.write("lib.zy", r#"@[package(library, test("tool.zy"))] 1"#);
    for source in ["()", "@[package(binary)] ()"] {
        fixture.write("tool.zy", source);
        let session = CompilerSession::default();
        let catalog = fixture.catalog(&session, &["lib.zy"]);
        assert!(matches!(session.package_tests(&fixture.id("lib.zy"), &catalog),
            Err(SourceLoadError::Package(error)) if matches!(*error, PackageError::WrongRole { expected: PackageRole::Test, .. })));
    }
    fixture.write("tool.zy", "@[package(test)] 1");
    let path = fixture.write("main.zy", r#"@(import("tool.zy"))"#);
    assert!(CompilerSession::default().analyze(path).unwrap().outcome().root().is_some());
}

#[test]
fn selection_and_planning_track_overlays_and_removed_registrations() {
    let fixture = Fixture::new();
    fixture.write("workspace.zy", r#"(#lib = @[package(library, test(old), name(lib))] 1)"#);
    let mut session = CompilerSession::default();
    session
        .set_overlay(
            fixture.path("tests.zy"),
            "(#old = @[package(test, name(old))] (), #new = @[package(test, name(new))] ())".into(),
        )
        .unwrap();
    let catalog = fixture.catalog(&session, &["workspace.zy", "tests.zy"]);
    let id = fixture.named("workspace.zy", "lib");
    let first = session.analyze_package(&id, catalog.bindings.clone()).unwrap();
    assert!(Arc::ptr_eq(&first, &session.analyze_package(&id, catalog.bindings.clone()).unwrap()));
    assert_eq!(
        session.package_tests(&id, &catalog).unwrap().tests[0]
            .id
            .name
            .as_ref()
            .unwrap()
            .to_string(),
        "old"
    );
    session
        .set_overlay(&id.path, r#"(#lib = @[package(library, test(new), name(lib))] 2)"#.into())
        .unwrap();
    let second = session.analyze_package(&id, catalog.bindings.clone()).unwrap();
    assert!(!Arc::ptr_eq(&first, &second));
    assert_eq!(
        session.package_tests(&id, &catalog).unwrap().tests[0]
            .id
            .name
            .as_ref()
            .unwrap()
            .to_string(),
        "new"
    );
    let canonical_id = session.package(&id).unwrap().id;
    for source in [
        r#"(#renamed = @[package(library, name(lib))] 2)"#,
        r#"let different = @[package(library, name(lib))] 2 in different"#,
        r#"(@[package(library, name(lib))] 2, #lib = 99)"#,
    ] {
        session.set_overlay(&id.path, source.into()).unwrap();
        assert!(
            session
                .analyze_package(&id, catalog.bindings.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
        assert_eq!(session.package(&id).unwrap().id, canonical_id);
    }
    session
        .set_overlay(&id.path, "(#renamed = @[package(library, name(renamed))] 2)".into())
        .unwrap();
    assert!(
        matches!(session.analyze_package(&id, catalog.bindings.clone()), Err(AnalysisError::Source { error }) if matches!(&*error, SourceLoadError::Package(error) if matches!(**error, PackageError::Missing { .. })))
    );
}

#[test]
fn duplicate_names_old_annotations_and_missing_names_have_specific_errors() {
    let fixture = Fixture::new();
    let duplicate = fixture.write(
        "duplicates.zy",
        "(#main = @[package(library, name(main))] 1, #main = @[package(test, name(main))] 2)",
    );
    assert!(matches!(CompilerSession::default().package_catalog(&[duplicate]),
        Err(SourceLoadError::Parse(SourceParseError::PackageDirective { error, .. })) if matches!(*error, PackageDirectiveError::DuplicateName { .. })));
    let old = fixture.write("old.zy", r#"@[package(library("main"))] ()"#);
    assert!(matches!(CompilerSession::default().package_catalog(&[old]),
        Err(SourceLoadError::Parse(SourceParseError::PackageDirective { error, .. })) if matches!(*error, PackageDirectiveError::Annotation { .. })));
    fixture.write("plain.zy", "()");
    assert!(matches!(CompilerSession::default().package(&fixture.named("plain.zy", "missing")),
        Err(SourceLoadError::Package(error)) if matches!(*error, PackageError::Missing { .. })));
}

#[test]
fn invalid_source_paths_and_syntax_are_reported_without_panicking() {
    let fixture = Fixture::new();
    let session = CompilerSession::default();
    let root = fixture.directory.path().ancestors().last().unwrap().to_path_buf();
    for path in [fixture.path("missing.zy"), fixture.directory.path().to_path_buf(), root] {
        assert!(matches!(session.package_catalog(&[path]), Err(SourceLoadError::Read { .. })));
    }
    fixture.write("broken.zy", "@[package(library)] let x = in x");
    assert!(matches!(
        session.package_catalog(&[fixture.path("broken.zy")]),
        Err(SourceLoadError::Parse(SourceParseError::Parse { .. }))
    ));
}

#[cfg(unix)]
#[test]
fn symlinked_sources_share_canonical_identity() {
    let fixture = Fixture::new();
    let source = fixture.write("workspace.zy", "(#lib = @[package(library, name(lib))] 1)");
    std::os::unix::fs::symlink(source, fixture.path("alias.zy")).unwrap();
    fixture.write("main.zy", r#"(@(import(lib)), @(import(lib)))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["workspace.zy", "alias.zy"]);
    let analysis =
        session.analyze_package(&fixture.id("main.zy"), catalog.bindings.clone()).unwrap();
    let graph = analysis.graph();
    let imports = &graph.sources[&graph.root].imports;
    assert_eq!(graph.imports[&imports[0]].imported, graph.imports[&imports[1]].imported);
}

#[test]
fn missing_named_import_reports_the_consumer_site() {
    let fixture = Fixture::new();
    fixture.write("workspace.zy", "(#one = @[package(library, name(one))] 1)");
    let source = r#"@(import(missing))"#;
    let path = fixture.write("main.zy", source);
    let error = CompilerSession::default().analyze(&path).unwrap_err();
    assert!(matches!(&error, AnalysisError::Source { error } if matches!(&**error,
        SourceLoadError::PackageImport { error, .. } if matches!(&**error, SourceLoadError::Package(error) if matches!(&**error, PackageError::Unknown { .. })))));
    let site = error.diagnostic_site().unwrap();
    assert_eq!(site.path(), path.canonicalize().unwrap());
    assert_eq!(&source[site.range().clone()], source);
}

#[test]
fn qualified_names_are_catalog_local_and_root_names_share_whole_file_identity() {
    let fixture = Fixture::new();
    fixture.write("catalog.zy", r#"@[discover(include("library.zy", "tests/*.zy"))] ()"#);
    fixture.write("library.zy", "@[package(library, name(std/data))] 1");
    fixture.write("library.zyi", "@(intrinsic(i64))");
    fixture.write(
        "tests/smoke.zy",
        "@[package(test(of(std/data)), name(std/data/smoke))] @(import(std/data))",
    );
    fixture.write("main.zy", r#"(@(import(std/data)), @(import("library.zy")))"#);
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["catalog.zy"]);
    let id =
        catalog.bindings.resolve(&"std/data".parse().unwrap(), fixture.directory.path()).unwrap();
    assert_eq!(id, session.package(&fixture.id("library.zy")).unwrap().id);
    assert_eq!(id.name, None, "a root name is not an inner-term selection");
    let analysis =
        session.analyze_package(&fixture.id("main.zy"), catalog.bindings.clone()).unwrap();
    let graph = analysis.graph();
    let imports = &graph.sources[&graph.root].imports;
    assert_eq!(graph.imports[&imports[0]].imported, graph.imports[&imports[1]].imported);
    assert!(graph.sources[&graph.imports[&imports[0]].imported].signature.is_some());
    let plan = session.package_tests(&id, &catalog).unwrap();
    assert_eq!(plan.tests.len(), 1);
    assert_eq!(plan.tests[0].name.as_ref().unwrap().to_string(), "std/data/smoke");
    assert!(session.analyze(fixture.path("main.zy")).is_err(), "no implicit catalog search");
}

#[test]
fn catalog_scopes_are_part_of_cached_analysis_and_rematerialization() {
    let fixture = Fixture::new();
    fixture.write("first.zy", "@[package(library, name(dep))] 1");
    fixture.write("second.zy", r#"@[package(library, name(dep))] "different type""#);
    fixture.write("main.zy", "(@(import(dep)) : @(intrinsic(i64)))");
    let session = CompilerSession::default();
    let first = fixture.catalog(&session, &["first.zy"]);
    let second = fixture.catalog(&session, &["second.zy"]);
    let root = fixture.id("main.zy");
    let accepted = session.analyze_package(&root, first.bindings.clone()).unwrap();
    let rejected = session.analyze_package(&root, second.bindings).unwrap();
    assert!(accepted.outcome().root().is_some());
    assert!(rejected.outcome().root().is_none());
    assert!(!Arc::ptr_eq(&accepted, &rejected));
    assert!(Arc::ptr_eq(&accepted, &session.analyze_package(&root, first.bindings).unwrap()));
    assert!(
        session.checked_program(&accepted).is_some(),
        "later analysis cannot change the retained catalog"
    );
    assert!(session.checked_program(&rejected).is_none());
    assert!(session.documentation_reference(accepted).is_ok());
}

#[test]
fn catalogs_deduplicate_files_and_reject_conflicting_names_with_both_locations() {
    let fixture = Fixture::new();
    let first = fixture.write("first.zy", "@[package(library, name(api))] 1");
    let second = fixture.write("second.zy", "(#unrelated = @[package(binary, name(api))] absent)");
    let session = CompilerSession::default();
    assert_eq!(fixture.catalog(&session, &["first.zy", "./first.zy"]).packages.len(), 1);
    let error = session.package_catalog(&[first.clone(), second.clone()]).unwrap_err();
    assert!(matches!(&error, SourceLoadError::Package(error) if matches!(&**error,
        PackageError::DuplicateName { name, first: origin, site } if
            name.to_string() == "api" && origin.path() == first.canonicalize().unwrap()
                && site.path() == second.canonicalize().unwrap())));
    assert!(
        fixture
            .catalog(&session, &["second.zy"])
            .bindings
            .resolve(&"api".parse().unwrap(), fixture.directory.path())
            .is_ok(),
        "different scopes can reuse a name"
    );
}

#[test]
fn unknown_names_never_fall_back_to_files_and_relationship_errors_keep_authored_spans() {
    let fixture = Fixture::new();
    fixture.write("missing", "1");
    fixture.write("library.zy", "@[package(library, name(api))] 1");
    let session = CompilerSession::default();
    let catalog = fixture.catalog(&session, &["library.zy"]);
    assert!(
        matches!(catalog.bindings.resolve(&"missing".parse().unwrap(), fixture.directory.path()),
        Err(SourceLoadError::Package(error)) if matches!(*error, PackageError::Unknown { .. }))
    );
    fixture.write("main.zy", r#"@(import("./missing"))"#);
    assert!(session.analyze(fixture.path("main.zy")).unwrap().outcome().root().is_some());
    for source in ["@[package(library, test(missing))] 1", "@[package(test(of(api, missing)))] 1"] {
        let path = fixture.write("relation.zy", source);
        let session = CompilerSession::default();
        let catalog = fixture.catalog(&session, &["library.zy", "relation.zy"]);
        let root = if source.contains("test(of") { "library.zy" } else { "relation.zy" };
        let error = session.package_tests(&fixture.id(root), &catalog).unwrap_err();
        assert!(matches!(&error, SourceLoadError::Package(error) if matches!(&**error,
            PackageError::Relation { error, .. } if matches!(&**error, SourceLoadError::Package(error)
                if matches!(**error, PackageError::Unknown { .. })))));
        let site = error.diagnostic_site().unwrap();
        assert_eq!(site.path(), path.canonicalize().unwrap());
        assert!(source[site.range().clone()].contains("missing"));
    }
}
