use super::*;
use crate::textual::StrictParser;

struct Fixture {
    parser: Parser,
    unit: SourceUnit,
}

impl Fixture {
    fn parse(source: &str) -> Self {
        let mut parser = Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        Self { parser, unit }
    }
    fn view(&self) -> SourceView<'_> {
        SourceView { unit: &self.unit, arena: &self.parser.arena, spans: &self.parser.spans }
    }
}

#[test]
fn composition_shares_reachability_sweep_and_semantic_decoding() {
    let fixture = Fixture::parse("(@[doc] _, @(import(1)), @[literal(extra)] _, @[unknown] _)");
    let view = fixture.view();
    let mut work = scan::ScanWork::default();
    let ((docs, imports), (docs_again, literals)) = SourceScan::scan(
        view,
        (
            (DocumentationAnalyzer::default(), ImportAnalyzer::default()),
            (DocumentationAnalyzer::default(), LiteralAnalyzer::default()),
        ),
        &mut work,
    );
    assert_eq!(work.reachability, 1);
    assert_eq!(work.terms, view.arena.terms.iter().count());
    assert_eq!(work.semantic, 3, "the second documentation observer shares the decoded event");
    assert_eq!(docs, docs_again);
    assert_eq!(docs, view.unit.documentation(view.arena, view.spans));
    assert_eq!(imports.facts, view.unit.imports(view.arena, view.spans).unwrap());
    assert_eq!(literals.diagnostics.len(), 1);
    assert_eq!(
        literals.diagnostics,
        view.unit.literals(view.arena, view.spans).unwrap_err().into_iter().collect::<Vec<_>>()
    );
    let mut work = scan::ScanWork::default();
    SourceScan::scan(view, ImportAnalyzer::default(), &mut work);
    assert_eq!(work.reachability, 0);
    assert_eq!(work.semantic, 1);
}

#[test]
fn retained_allocations_keep_each_analyzers_existing_domain() {
    let mut fixture =
        Fixture::parse("(@[doc] _, @(import), @[package] _, @[discover] _, @[literal] _)");
    // A parser can retain allocations that are absent from its returned tree.
    fixture.unit.root = fixture
        .parser
        .arena
        .terms
        .iter()
        .find_map(|(id, term)| matches!(term, Term::Hole(_)).then_some(*id))
        .unwrap();
    let inventory = SourceInventory::scan(fixture.view());
    assert!(inventory.documentation.is_empty());
    assert!(inventory.packages.diagnostics.is_empty());
    assert!(inventory.discovery.diagnostics.is_empty());
    assert_eq!(inventory.imports.diagnostics.len(), 1);
    assert_eq!(inventory.literals.diagnostics.len(), 1);
}

#[test]
fn independent_sites_report_all_errors_and_retain_valid_facts() {
    let fixture = Fixture::parse(
        "(@(import), @(import(0)), @(import(1)), @[literal(extra)] _, @[builtin] _, exists @[unsupported] (a : _) . _)",
    );
    let inventory = SourceInventory::scan(fixture.view());
    assert_eq!(inventory.imports.diagnostics.len(), 2);
    assert_eq!(inventory.imports.facts.len(), 1);
    assert_eq!(inventory.literals.diagnostics.len(), 1);
    assert_eq!(inventory.builtins.diagnostics.len(), 2);
    assert!(inventory.imports.into_result().is_err());
    let valid = Fixture::parse(
        "(@(import(1)), @(import(2)), @[builtin(int64_add)] _, exists @[builtin(os)] (a : _) . _)",
    );
    let inventory = SourceInventory::scan(valid.view());
    assert_eq!(inventory.imports.into_result().unwrap().len(), 2);
    assert_eq!(inventory.builtins.into_result().unwrap().len(), 2);
}

#[test]
fn aggregate_checks_continue_past_invalid_package_and_discovery_sites() {
    let fixture = Fixture::parse(
        "(@[package(library, name(p))] _, @[package(library, name(p))] _, @[package(library, name(p))] _, @[package(wrong)] _, @[discover(include(1))] _, @[discover(include(2))] _)",
    );
    let inventory = SourceInventory::scan(fixture.view());
    assert_eq!(inventory.packages.diagnostics.len(), 3);
    assert_eq!(inventory.packages.facts.len(), 1);
    assert_eq!(inventory.discovery.diagnostics.len(), 3, "two argument errors and one duplicate");
    let valid = Fixture::parse(
        "@[discover(include(\"*.zy\"))] (@[package(library, name(p))] _, @[package(library, name(q))] _)",
    );
    let inventory = SourceInventory::scan(valid.view());
    assert_eq!(inventory.packages.into_result().unwrap().len(), 2);
    assert_eq!(inventory.discovery.into_result().unwrap().len(), 1);
}
