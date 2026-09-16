use super::*;
use crate::source::{CompilerSession, SourceGraph, SourceLoadErrors};

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
    fn project(&self, sources: &[&str]) -> Arc<Project> {
        Arc::new(Project::new(sources.iter().map(|source| self.path(source)).collect()))
    }
    fn load(
        &self, root: &str, project: Arc<Project>,
    ) -> Result<Arc<SourceGraph>, Arc<SourceLoadErrors>> {
        CompilerSession::default()
            .load_package(&PackageId { path: self.path(root), name: None }, project)
    }
}

#[test]
fn package_paths_resolve_lexically_and_siblings_keep_their_context() {
    let f = Fixture::new();
    f.write(
        "packages.zy",
        r#"@[package(library, name(std))] (
        @[package(library, name(data))] 1,
        @[package(test(of(../data)), name(text))] @(import(../data))
    )"#,
    );
    f.write("main.zy", "(@(import(/std/data)), @(import(std/./data)), @(import(std/data/../data)), @(import(std/text)))");
    let p = f.project(&["packages.zy"]);
    let g = f.load("main.zy", p.clone()).unwrap();
    let root = PackageContext::at_root(p.root);
    let data = g.select_package(&"std/data".parse().unwrap(), &root).unwrap();
    let text = g.select_package(&"std/text".parse().unwrap(), &root).unwrap();
    assert_eq!(text.relations.len(), 1);
    assert_eq!(text.relations[0].target, ResolvedPackageReference::Package(data.namespace.clone()));
    let targets =
        g.sources[&g.root].imports.iter().map(|id| g.imports[id].imported).collect::<Vec<_>>();
    assert_eq!(targets[..3], vec![data.source; 3]);
    assert_eq!(g.sources[&text.source].imports.len(), 1);
    assert_eq!(g.imports[&g.sources[&text.source].imports[0]].imported, data.source);
    assert!(g.select_package(&"std/data/text".parse().unwrap(), &root).is_err());
}

#[test]
fn prefixes_need_no_definition_and_parent_steps_above_root_are_diagnosed() {
    let f = Fixture::new();
    f.write("packages.zy", "@[package(library, name(std/data))] 42");
    f.write("main.zy", "@(import(/std/data))");
    let p = f.project(&["packages.zy"]);
    let g = f.load("main.zy", p.clone()).unwrap();
    let root = PackageContext::at_root(p.root);
    assert!(g.select_package(&"std".parse().unwrap(), &root).is_err());
    assert!(g.select_package(&"std/data".parse().unwrap(), &root).is_ok());
    f.write("main.zy", "@(import(../std/data))");
    let errors = f.load("main.zy", p).unwrap_err();
    assert!(errors.to_string().contains("above the namespace root"));
    assert!(errors.diagnostic_site().unwrap().path().ends_with("main.zy"));
}

#[test]
fn unnamed_packages_establish_opaque_contexts_and_named_children() {
    let f = Fixture::new();
    f.write("main.zy", "(@[package(library)] (@[package(library, name(data))] 42, @(import(data))), @[package(library)] ())");
    let g = f.load("main.zy", f.project(&[])).unwrap();
    let data = g.packages.iter().find(|p| p.namespace.to_string().ends_with("/data")).unwrap();
    assert!(matches!(data.namespace.components[0], NamespaceComponent::Opaque(_)));
    let parent = data.namespace.parent().unwrap();
    assert_eq!(data.context.resolve(&"..".parse().unwrap()).unwrap(), parent);
    assert_eq!(g.sources[&g.root].imports.len(), 1);
    assert_eq!(g.imports[&g.sources[&g.root].imports[0]].imported, data.source);
    let anonymous = g
        .packages
        .iter()
        .filter(|p| p.namespace.components.len() == 1)
        .map(|p| &p.namespace)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(anonymous.len(), 2);
}

#[test]
fn registration_substitutes_the_root_of_absolute_and_relative_paths() {
    let f = Fixture::new();
    let sources = f.write(
        "vendor.zy",
        "(@[package(library, name(data))] 7, @[package(library, name(client))] @(import(/data)))",
    );
    f.write("main.zy", "@(import(/vendor/b/client))");
    let project = Project::new(Vec::new())
        .with_registration("vendor/b".parse().unwrap(), Project::new(vec![sources]));
    let g = f.load("main.zy", Arc::new(project.clone())).unwrap();
    let root = PackageContext::at_root(project.root);
    let client = g.select_package(&"vendor/b/client".parse().unwrap(), &root).unwrap();
    let data = g.select_package(&"vendor/b/data".parse().unwrap(), &root).unwrap();
    assert_eq!(client.context.resolve(&"/data".parse().unwrap()).unwrap(), data.namespace);
    assert_eq!(g.imports[&g.sources[&client.source].imports[0]].imported, data.source);
    assert!(g.select_package(&"data".parse().unwrap(), &root).is_err());
}

#[test]
fn copies_merge_after_resolving_dependencies_and_preserve_each_route() {
    let f = Fixture::new();
    f.write("copy.zy", "let value = @(import(data)) in value");
    f.write(
        "main.zy",
        r#"(
        @[package(library, name(a))] (@[package(library, name(data))] 42, @(import("copy.zy"))),
        @[package(library, name(b))] (@[package(library, name(data))] 42, @(import("copy.zy")))
    )"#,
    );
    let session = CompilerSession::default();
    let analysis = session.analyze(f.path("main.zy")).unwrap();
    assert!(analysis.outcome().root().is_some(), "{:?}", analysis.outcome());
    let graph = analysis.graph();
    let instances =
        graph.instances.iter().filter(|i| i.template.path.ends_with("copy.zy")).collect::<Vec<_>>();
    assert!(instances.len() >= 2);
    assert!(instances.windows(2).all(|pair| pair[0].source == pair[1].source));
    assert!(instances.iter().any(|i| i.context.package.to_string() == "/a"));
    assert!(instances.iter().any(|i| i.context.package.to_string() == "/b"));
    let declarations = analysis.scoped().defs.iter().filter(|(_, name)| name.0 == "value").count();
    assert_eq!(declarations, 1, "merged copies receive semantics once");
}

#[test]
fn different_resolved_dependencies_keep_copies_and_semantic_identities_distinct() {
    let f = Fixture::new();
    f.write("copy.zy", "let value = @(import(data)) in value");
    f.write(
        "main.zy",
        r#"(
        @[package(library, name(a))] (@[package(library, name(data))] 41, @(import("copy.zy"))),
        @[package(library, name(b))] (@[package(library, name(data))] 42, @(import("copy.zy")))
    )"#,
    );
    let session = CompilerSession::default();
    let analysis = session.analyze(f.path("main.zy")).unwrap();
    assert!(analysis.outcome().root().is_some());
    let graph = analysis.graph();
    let sources = graph
        .instances
        .iter()
        .filter(|i| i.template.path.ends_with("copy.zy"))
        .map(|i| i.source)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(sources.len(), 2);
    assert_eq!(analysis.scoped().defs.iter().filter(|(_, name)| name.0 == "value").count(), 2);
}

#[test]
fn equal_definitions_share_a_binding_and_conflicts_retain_both_origins() {
    let f = Fixture::new();
    f.write("first.zy", "@[package(library, name(value))] 42");
    f.write("second.zy", "@[package(library, name(/./value))]  42 -- layout changes\n");
    f.write("main.zy", "@(import(value))");
    let p = f.project(&["first.zy", "second.zy"]);
    let g = f.load("main.zy", p.clone()).unwrap();
    assert_eq!(g.packages[0].source, g.packages[1].source);
    assert_eq!(g.source_inputs().count(), 3, "both physical origins remain query inputs");
    f.write("second.zy", "@[package(library, name(value))] 43");
    let error = f.load("main.zy", p).unwrap_err();
    assert!(error.iter().any(|error| matches!(error, SourceLoadError::Package(error) if matches!(error.as_ref(), PackageError::Conflict { site, first, .. } if site.path().ends_with("second.zy") && first.path().ends_with("first.zy")))));
}

#[test]
fn signatures_metadata_and_literals_participate_in_structural_equality() {
    let f = Fixture::new();
    f.write("a.zy", "42");
    f.write("b.zy", "42");
    f.write("a.zyi", "@(intrinsic(int))");
    f.write("main.zy", "(@(import(\"a.zy\")), @(import(\"b.zy\")))");
    let g = f.load("main.zy", f.project(&[])).unwrap();
    let imports = &g.sources[&g.root].imports;
    assert_ne!(g.imports[&imports[0]].imported, g.imports[&imports[1]].imported);
    f.write("b.zyi", "@(intrinsic(int))");
    let g = f.load("main.zy", f.project(&[])).unwrap();
    let imports = &g.sources[&g.root].imports;
    assert_eq!(g.imports[&imports[0]].imported, g.imports[&imports[1]].imported);
    f.write("b.zy", "--| Different documentation.\n@[doc] 42");
    let g = f.load("main.zy", f.project(&[])).unwrap();
    let imports = &g.sources[&g.root].imports;
    assert_ne!(g.imports[&imports[0]].imported, g.imports[&imports[1]].imported);
}

#[test]
fn imports_preserve_lexical_context_and_report_missing_targets_and_cycles() {
    let f = Fixture::new();
    f.write("inside.zy", "@[package(library, name(inner))] 1");
    f.write("main.zy", "(@(import(\"inside.zy\")), @[package(library, name(sibling))] 2)");
    let p = f.project(&[]);
    let g = f.load("main.zy", p.clone()).unwrap();
    let root = PackageContext::at_root(p.root);
    assert!(g.select_package(&"sibling".parse().unwrap(), &root).is_ok());
    assert!(g.select_package(&"inner/sibling".parse().unwrap(), &root).is_err());
    for source in ["@(import(missing))", "@(import(\"missing.zy\"))", "@(import(\"main.zy\"))"] {
        f.write("main.zy", source);
        let errors = f.load("main.zy", p.clone()).unwrap_err();
        assert!(errors.diagnostic_site().unwrap().path().ends_with("main.zy"));
        if source.contains("main.zy") {
            assert!(errors.iter().any(|error| matches!(error, SourceLoadError::Cycle(_))));
        }
    }
}

#[test]
fn selected_packages_keep_file_spans_and_do_not_inherit_file_companions() {
    let f = Fixture::new();
    let path = f.write("packages.zy", "let outer = 42 in (@[package(library, name(good))] 1, @[package(library, name(bad))] outer)");
    f.write("packages.zyi", "@(intrinsic(int))");
    let session = CompilerSession::default();
    let project = f.project(&["packages.zy"]);
    let good = session
        .analyze_package(
            &PackageId { path: path.clone(), name: Some("good".parse().unwrap()) },
            project.clone(),
        )
        .unwrap();
    assert!(good.outcome().root().is_some());
    assert!(good.graph().sources[&good.graph().root].signature.is_none());
    let error = session
        .analyze_package(&PackageId { path, name: Some("bad".parse().unwrap()) }, project)
        .unwrap_err();
    assert!(
        error.diagnostics().iter().any(|d| d.site.as_ref().is_some_and(|s| s.range().start > 60))
    );
}

#[test]
fn overlays_and_project_contexts_participate_in_analysis_and_rematerialization() {
    let f = Fixture::new();
    let a = f.write("a.zy", "@[package(library, name(data))] 42");
    let b = f.write("b.zy", "@[package(library, name(data))] absent");
    let root = f.write("main.zy", "@(import(data))");
    let mut session = CompilerSession::default();
    let id = PackageId { path: root, name: None };
    let project = Arc::new(Project::new(vec![a.clone()]));
    let analysis = session.analyze_package(&id, project.clone()).unwrap();
    assert!(session.checked_program(&analysis).is_some());
    assert!(session.analyze_package(&id, Arc::new(Project::new(vec![b]))).is_err());
    assert!(session.checked_program(&analysis).is_some());
    session.set_overlay(&a, "@[package(library, name(data))] 43".into()).unwrap();
    let updated = session.analyze_package(&id, project).unwrap();
    assert!(!Arc::ptr_eq(&analysis, &updated));
    assert!(analysis.source(&a).unwrap().contains("42"));
    assert!(updated.source(&a).unwrap().contains("43"));
}

#[test]
fn instance_queries_and_documentation_links_distinguish_copies_of_one_file() {
    let f = Fixture::new();
    f.write(
        "copy.zy",
        "let value = @(import(data)) in\n--| [value](zydeco:name:value)\n@[doc] value",
    );
    f.write(
        "main.zy",
        r#"(
        @[package(library, name(a))] (@[package(library, name(data))] 41, @(import("copy.zy"))),
        @[package(library, name(b))] (@[package(library, name(data))] 42, @(import("copy.zy")))
    )"#,
    );
    let analysis = CompilerSession::default().analyze(f.path("main.zy")).unwrap();
    let documents = analysis.documentation().entries();
    assert_eq!(documents.len(), 2);
    let targets = documents
        .iter()
        .map(|document| match document.links[0].target.as_ref().unwrap() {
            | crate::source::DocumentationLinkTarget::Definition(id) => *id,
            | _ => panic!("definition link"),
        })
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(targets.len(), 2);
    for (index, instance) in analysis
        .graph()
        .instances
        .iter()
        .enumerate()
        .filter(|(_, i)| i.template.path.ends_with("copy.zy"))
    {
        let definition =
            *instance.template.arena.defs.iter().find(|(_, name)| name.0 == "value").unwrap().0;
        let subjects = analysis.entities_in_instance(PackageInstanceId(index), definition.into());
        assert_eq!(subjects.len(), 1);
        let zydeco_surface::scoped::syntax::EntityId::Def(definition) = subjects[0] else {
            panic!()
        };
        assert!(targets.contains(&definition));
    }
}

#[test]
fn documentation_workers_replay_an_opaque_package_context_with_pinned_sources() {
    let f = Fixture::new();
    let root = f.write("main.zy", "@[package(library)] (\n@[package(library, name(data))] 42,\n--| ```zydeco check\n--| @(import(data))\n--| ```\n@[doc] ())");
    let session = CompilerSession::default();
    let analysis = session.analyze(&root).unwrap();
    let document = &analysis.documentation().entries()[0];
    let example = crate::source::DocumentationExample::from_documentation(
        document,
        analysis.source(&root).unwrap(),
    )
    .remove(0);
    let request = example.request_in_instance(&analysis, analysis.graph().root_instance).unwrap();
    f.write("main.zy", "absent");
    let request: crate::source::DocumentationExampleRequest =
        serde_json::from_str(&serde_json::to_string(&request).unwrap()).unwrap();
    let result = request.check();
    assert!(result.status.is_passed(), "{:?}", result.diagnostics);
}

#[test]
fn package_availability_follows_transitive_file_routes_independently_of_source_order() {
    let f = Fixture::new();
    f.write("definitions.zy", "@[package(library, name(data))] 42");
    f.write("bridge.zy", "@(import(\"definitions.zy\"))");
    for main in [
        "(@(import(data)), @(import(\"bridge.zy\")))",
        "(@(import(\"bridge.zy\")), @(import(data)))",
        "@[package(library)] (@(import(data)), @(import(\"bridge.zy\")))",
    ] {
        f.write("main.zy", main);
        assert!(
            CompilerSession::default()
                .analyze(f.path("main.zy"))
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
    }
    let project = f.project(&["bridge.zy"]);
    let id = PackageId { path: f.path("bridge.zy"), name: Some("/data".parse().unwrap()) };
    assert!(
        CompilerSession::default()
            .analyze_package(&id, project)
            .unwrap()
            .outcome()
            .root()
            .is_some()
    );
}

#[test]
fn shared_imported_computations_execute_at_each_dynamic_occurrence() {
    let f = Fixture::new();
    let builtin = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/std/builtin.zy")
        .canonicalize()
        .unwrap();
    f.write("effect.zy", &format!("param (/Thk; /OS; /stdio) : @(import({:?})) in fn (next : Thk OS) => ! stdio/write_line \"tick\" next", builtin.to_string_lossy()));
    let root = f.write("main.zy", &format!("param (/process; builtin) : @(import({:?})) in (@(import(\"effect.zy\"))) builtin {{ (@(import(\"effect.zy\"))) builtin {{ ! process/exit 0 }} }}", builtin.to_string_lossy()));
    let session = CompilerSession::default();
    let analysis = session.analyze(&root).unwrap();
    let executable = session.executable_program(&analysis).unwrap();
    let dynamic = zydeco_dynamics::BuiltinRootLinker {
        scoped: executable.scoped,
        statics: executable.statics,
        root: executable.root,
        signature: executable.signature,
    }
    .run()
    .unwrap();
    let mut input = std::io::Cursor::new(Vec::<u8>::new());
    let mut output = Vec::new();
    let outcome =
        zydeco_dynamics::Runtime::new(&mut input, &mut output, &mut Vec::new(), &[], dynamic).run();
    assert!(matches!(outcome, zydeco_dynamics::ProgKont::ExitCode(0)));
    assert_eq!(String::from_utf8(output).unwrap(), "tick\ntick\n");
    let instances = analysis
        .graph()
        .instances
        .iter()
        .filter(|instance| instance.template.path.ends_with("effect.zy"))
        .collect::<Vec<_>>();
    assert_eq!(instances.len(), 2);
    assert_eq!(instances[0].source, instances[1].source);
}

#[test]
fn package_selection_and_inspection_follow_nested_project_registrations() {
    let f = Fixture::new();
    let source = f.write(
        "vendor.zy",
        "(@[package(library, name(data))] 42, @[package(library, name(client))] @(import(/data)))",
    );
    let project = Arc::new(
        Project::new(Vec::new()).with_registration(
            "vendor".parse().unwrap(),
            Project::new(Vec::new())
                .with_registration("nested".parse().unwrap(), Project::new(vec![source])),
        ),
    );
    let session = CompilerSession::default();
    let id = session
        .selection(&SourceReference::Package("/vendor/nested/client".parse().unwrap()), &project)
        .unwrap();
    let analysis = session.analyze_package(&id, project.clone()).unwrap();
    assert!(analysis.outcome().root().is_some());
    let root = PackageContext::at_root(project.root);
    assert!(analysis.graph().select_package(&"/data".parse().unwrap(), &root).is_err());
    for (_, package) in session.declarations(&project).unwrap() {
        assert!(
            session
                .analyze_package(&package.id, project.clone())
                .unwrap()
                .outcome()
                .root()
                .is_some()
        );
    }
}

#[test]
fn merged_documentation_preserves_locations_and_examples_in_each_original_file() {
    let f = Fixture::new();
    f.write("a.zy", "--| ```zydeco check\n--| @(import(data))\n--| ```\n@[doc] ()");
    f.write("b.zy", "\n\n--| ```zydeco check\n--| @(import(data))\n--| ```\n@[doc] ()");
    let root = f.write("main.zy", "(@[package(library, name(left))] (@[package(library, name(data))] 42, @(import(\"a.zy\"))), @[package(library, name(right))] (@[package(library, name(data))] 42, @(import(\"b.zy\"))))");
    let session = CompilerSession::default();
    let analysis = session.analyze(&root).unwrap();
    assert_eq!(analysis.documentation().entries().len(), 1);
    let mut origins = std::collections::HashSet::new();
    for (index, instance) in
        analysis.graph().instances.iter().enumerate().filter(|(_, instance)| {
            instance.template.path.ends_with("a.zy") || instance.template.path.ends_with("b.zy")
        })
    {
        let id = PackageInstanceId(index);
        let documents = analysis.documentation().in_instance(analysis.graph(), id);
        assert_eq!(documents.len(), 1);
        let document = &documents[0];
        assert_eq!(document.path, instance.template.path);
        origins.insert(document.path.clone());
        let examples = crate::source::DocumentationExample::from_documentation(
            document,
            &instance.template.source,
        );
        assert_eq!(examples.len(), 1);
        let checked = examples[0].request_in_instance(&analysis, id).unwrap().check();
        assert!(checked.status.is_passed(), "{:?}", checked.diagnostics);
    }
    assert_eq!(origins.len(), 2);
}

#[test]
fn package_relationships_compare_resolved_targets_and_retain_each_origin() {
    let f = Fixture::new();
    f.write("a.zy", "@[package(test(of(../subject)), name(check))] 42");
    f.write("b.zy", "@[package(test(of(/subject)), name(/check))] 42");
    let project = f.project(&["a.zy", "b.zy"]);
    let id = PackageId { path: f.path("a.zy"), name: Some("check".parse().unwrap()) };
    let graph = CompilerSession::default().load_package(&id, project.clone()).unwrap();
    let declarations = graph
        .packages
        .iter()
        .filter(|package| package.namespace.to_string() == "/check")
        .collect::<Vec<_>>();
    assert_eq!(declarations.len(), 2);
    assert_eq!(declarations[0].source, declarations[1].source);
    assert_eq!(declarations[0].relations[0].target, declarations[1].relations[0].target);
    assert_ne!(declarations[0].origin, declarations[1].origin);
    f.write("b.zy", "@[package(test(of(/other)), name(/check))] 42");
    let error = CompilerSession::default().load_package(&id, project).unwrap_err();
    assert!(error.to_string().contains("conflict"));
    assert!(error.to_string().contains("a.zy") && error.to_string().contains("b.zy"));
}
