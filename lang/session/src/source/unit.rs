//! Per-unit Salsa incremental resolution, checking, interface early-cutoff, and assembly.

use super::query::{AnalysisError, SourceInput, SourceQueryDb, parse_source, source_graph};
use crate::source::{
    LibraryProgram, SourceGraph, SourceId, SourceKind, SourceLoadError, SourceTemplate, UnitProgram,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;
#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use zydeco_foundation::arena::KeySpaceId;
use zydeco_foundation::span::{BytePos, FileSource, SourceMap};
use zydeco_statics::LibraryCheckError;
use zydeco_statics::separate::{
    AssembledUnitInput, CheckedUnitOutput, ImportedUnitDep, UnitCheckInput, UnitInterface,
};
use zydeco_surface::diagnostic::Diagnostics;
use zydeco_surface::scoped::{
    ResolveErrors, ResolveFolder, UnitResolveConfig, arena::ScopedArena, syntax as su,
};
use zydeco_surface::textual::syntax::{self as t, SpanArena};

#[cfg(test)]
#[derive(Debug, Default)]
pub(crate) struct QueryCounters {
    pub parse: AtomicUsize,
    pub resolve: AtomicUsize,
    pub check_unit: AtomicUsize,
    pub assemble: AtomicUsize,
}

#[cfg(test)]
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct CounterSnapshot {
    pub parse: usize,
    pub resolve: usize,
    pub check_unit: usize,
    pub assemble: usize,
}

#[cfg(test)]
impl QueryCounters {
    pub(crate) fn snapshot(&self) -> CounterSnapshot {
        CounterSnapshot {
            parse: self.parse.load(Ordering::Relaxed),
            resolve: self.resolve.load(Ordering::Relaxed),
            check_unit: self.check_unit.load(Ordering::Relaxed),
            assemble: self.assemble.load(Ordering::Relaxed),
        }
    }

    pub(crate) fn reset(&self) {
        self.parse.store(0, Ordering::Relaxed);
        self.resolve.store(0, Ordering::Relaxed);
        self.check_unit.store(0, Ordering::Relaxed);
        self.assemble.store(0, Ordering::Relaxed);
    }
}

/// Identifies which syntactic root within a parsed file defines a compilation unit.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum UnitSelection {
    FileRoot,
    PackageSite(u32),
}

/// Span-free, structural identity of one compilation unit within its source DAG.
#[salsa::interned(debug)]
pub(crate) struct UnitSpec<'db> {
    #[returns(copy)]
    pub input: SourceInput,
    #[returns(copy)]
    pub selection: UnitSelection,
    #[returns(copy)]
    pub is_signature: bool,
    #[returns(clone)]
    pub imports: Vec<UnitSpec<'db>>,
    #[returns(copy)]
    pub prelude: Option<UnitSpec<'db>>,
    #[returns(copy)]
    pub signature: Option<UnitSpec<'db>>,
}

/// Parsed template gated by structural `SyntaxDigest` so whitespace and `--` comments backdate.
#[derive(Clone, Debug)]
pub(crate) struct SemanticSource {
    pub template: Arc<SourceTemplate>,
}

impl PartialEq for SemanticSource {
    fn eq(&self, other: &Self) -> bool {
        self.template.syntax_digest == other.template.syntax_digest
    }
}

impl Eq for SemanticSource {}

/// Outcome of `semantic_source` where parse failures never backdate.
#[derive(Clone, Debug)]
pub(crate) enum SemanticSourceResult {
    Ok(SemanticSource),
    Err,
}

impl SemanticSourceResult {
    pub(crate) fn as_ok(&self) -> Option<&SemanticSource> {
        match self {
            | Self::Ok(source) => Some(source),
            | Self::Err => None,
        }
    }
}

impl PartialEq for SemanticSourceResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            | (Self::Ok(left), Self::Ok(right)) => left == right,
            | _ => false,
        }
    }
}

impl Eq for SemanticSourceResult {}

#[derive(Clone, Debug)]
pub(crate) struct ResolvedUnitMemo {
    pub arena: Arc<ScopedArena>,
    pub root: su::TermId,
    /// `(scoped_source_boundary_term, code_site_ordinal)` for each `@(import(...))`.
    pub boundary_imports: Vec<(su::TermId, usize)>,
    pub prelude_boundary: Option<su::TermId>,
    pub has_monadic: bool,
    pub textual_key_space: Option<KeySpaceId>,
}

#[derive(Clone, Debug)]
pub(crate) struct ResolveUnitFailure {
    pub errors: Box<ResolveErrors>,
    pub textual_key_space: Option<KeySpaceId>,
}

#[derive(Clone, Debug)]
pub(crate) enum ResolveUnitError {
    Unparsed,
    Failed(ResolveUnitFailure),
}

#[derive(Clone, Debug)]
pub(crate) struct AssembledProgram {
    pub graph: Arc<SourceGraph>,
    pub spans: Arc<SpanArena>,
    pub root_term: su::TermId,
    pub output: zydeco_statics::query::TyckOutput,
}

#[salsa::tracked(returns(clone), unsafe(non_salsa_values))]
pub(crate) fn semantic_source(db: &dyn SourceQueryDb, input: SourceInput) -> SemanticSourceResult {
    match parse_source(db, input) {
        | Ok(template) => SemanticSourceResult::Ok(SemanticSource { template }),
        | Err(_) => SemanticSourceResult::Err,
    }
}

pub(crate) fn unit_root_term(template: &SourceTemplate, selection: UnitSelection) -> t::TermId {
    match selection {
        | UnitSelection::FileRoot => template.unit.root,
        | UnitSelection::PackageSite(index) => template.package_sites[index as usize].term,
    }
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub(crate) fn resolve_unit<'db>(
    db: &'db dyn SourceQueryDb, spec: UnitSpec<'db>,
) -> Result<Arc<ResolvedUnitMemo>, Arc<ResolveUnitError>> {
    #[cfg(test)]
    db.query_counters().resolve.fetch_add(1, Ordering::Relaxed);

    let semantic_res = semantic_source(db, spec.input(db));
    let Some(semantic) = semantic_res.as_ok() else {
        return Err(Arc::new(ResolveUnitError::Unparsed));
    };
    let template = &semantic.template;
    let root = unit_root_term(template, spec.selection(db));
    let site_order: FxHashMap<t::TermId, usize> = template
        .code_sites(root)
        .into_iter()
        .enumerate()
        .map(|(ord, site)| (site.term, ord))
        .collect();

    let resolved = ResolveFolder::new(&template.spans, &template.arena)
        .run_unit(
            root,
            UnitResolveConfig {
                literals: &template.literals,
                has_prelude: spec.prelude(db).is_some(),
                is_signature: spec.is_signature(db),
            },
        )
        .map_err(|errors| {
            Arc::new(ResolveUnitError::Failed(ResolveUnitFailure {
                errors,
                textual_key_space: template.spans.key_space(),
            }))
        })?;

    let boundary_imports = resolved
        .imports
        .into_iter()
        .map(|(t_term, boundary)| {
            let ord = site_order[&t_term];
            (boundary, ord)
        })
        .collect();

    Ok(Arc::new(ResolvedUnitMemo {
        arena: Arc::new(resolved.arena.into_inner()),
        root: resolved.root,
        boundary_imports,
        prelude_boundary: resolved.prelude_boundary,
        has_monadic: resolved.has_monadic,
        textual_key_space: template.spans.key_space(),
    }))
}

pub(crate) fn unit_spec_from_graph<'db>(
    db: &'db dyn SourceQueryDb, graph: &SourceGraph, source: SourceId,
) -> Result<UnitSpec<'db>, SourceLoadError> {
    let mut memo = FxHashMap::default();
    build_unit_spec(db, graph, source, &mut memo)
}

fn build_unit_spec<'db>(
    db: &'db dyn SourceQueryDb, graph: &SourceGraph, source: SourceId,
    memo: &mut FxHashMap<SourceId, UnitSpec<'db>>,
) -> Result<UnitSpec<'db>, SourceLoadError> {
    if let Some(&spec) = memo.get(&source) {
        return Ok(spec);
    }
    let file = &graph.sources[&source];
    let input = db.source_input(file.template.path.clone())?;
    let selection = if file.root == file.template.unit.root {
        UnitSelection::FileRoot
    } else {
        let index = file
            .template
            .package_sites
            .iter()
            .position(|site| site.term == file.root)
            .expect("package root matches a parsed package site");
        UnitSelection::PackageSite(index as u32)
    };
    let is_signature = file.kind() == SourceKind::Signature;

    let import_map: FxHashMap<t::TermId, SourceId> = file
        .imports
        .iter()
        .map(|imp_id| {
            let imp = &graph.imports[imp_id];
            (imp.term, imp.imported)
        })
        .collect();
    let imports = file
        .template
        .code_sites(file.root)
        .into_iter()
        .map(|site| build_unit_spec(db, graph, import_map[&site.term], memo))
        .collect::<Result<Vec<_>, _>>()?;
    let prelude = file.prelude.map(|id| build_unit_spec(db, graph, id, memo)).transpose()?;
    let signature = file.signature.map(|id| build_unit_spec(db, graph, id, memo)).transpose()?;

    let spec = UnitSpec::new(db, input, selection, is_signature, imports, prelude, signature);
    memo.insert(source, spec);
    Ok(spec)
}

pub(crate) fn collect_unique_units<'db>(
    db: &'db dyn SourceQueryDb, root: UnitSpec<'db>,
) -> (Vec<UnitSpec<'db>>, FxHashMap<UnitSpec<'db>, usize>) {
    fn visit<'db>(
        db: &'db dyn SourceQueryDb, unit: UnitSpec<'db>, seen: &mut FxHashSet<UnitSpec<'db>>,
        out: &mut Vec<UnitSpec<'db>>, index: &mut FxHashMap<UnitSpec<'db>, usize>,
    ) {
        if !seen.insert(unit) {
            return;
        }
        for imp in unit.imports(db) {
            visit(db, imp, seen, out, index);
        }
        if let Some(pre) = unit.prelude(db) {
            visit(db, pre, seen, out, index);
        }
        if let Some(sig) = unit.signature(db) {
            visit(db, sig, seen, out, index);
        }
        index.insert(unit, out.len());
        out.push(unit);
    }

    let mut seen = FxHashSet::default();
    let mut out = Vec::new();
    let mut index = FxHashMap::default();
    visit(db, root, &mut seen, &mut out, &mut index);
    (out, index)
}

pub(crate) fn collect_unique_deps<'db>(
    db: &'db dyn SourceQueryDb, root: UnitSpec<'db>,
) -> (Vec<UnitSpec<'db>>, FxHashMap<UnitSpec<'db>, usize>) {
    let (mut all, mut index) = collect_unique_units(db, root);
    let popped = all.pop();
    debug_assert_eq!(popped, Some(root));
    index.remove(&root);
    (all, index)
}

pub(crate) fn compute_dep_to_global<'db>(
    db: &'db dyn SourceQueryDb, unit: UnitSpec<'db>, global_index: &FxHashMap<UnitSpec<'db>, usize>,
) -> Vec<usize> {
    let (deps, _) = collect_unique_deps(db, unit);
    deps.into_iter().map(|dep| global_index[&dep]).collect()
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub(crate) fn check_unit_internal<'db>(
    db: &'db dyn SourceQueryDb, spec: UnitSpec<'db>,
) -> Result<CheckedUnitOutput, ()> {
    #[cfg(test)]
    db.query_counters().check_unit.fetch_add(1, Ordering::Relaxed);

    let Ok(resolved) = resolve_unit(db, spec) else {
        return Err(());
    };
    let semantic_res = semantic_source(db, spec.input(db));
    let Some(semantic) = semantic_res.as_ok() else {
        return Err(());
    };
    let spans = Arc::new((*semantic.template.spans).clone());

    let (dep_specs, dep_index) = collect_unique_deps(db, spec);
    let dep_interfaces: Vec<UnitInterface> =
        dep_specs.iter().map(|&dep| unit_interface(db, dep)).collect();
    let dep_bodies: Vec<Option<CheckedUnitOutput>> = if resolved.has_monadic {
        dep_specs.iter().map(|&dep| check_unit_internal(db, dep).ok()).collect()
    } else {
        vec![None; dep_specs.len()]
    };
    let dep_mappings: Vec<Vec<usize>> =
        dep_specs.iter().map(|&dep| compute_dep_to_global(db, dep, &dep_index)).collect();

    let imported_deps: Vec<ImportedUnitDep<'_>> = dep_specs
        .iter()
        .enumerate()
        .map(|(idx, _)| ImportedUnitDep {
            interface: &dep_interfaces[idx],
            body: dep_bodies[idx].as_ref().map(|out| out.body.as_ref()),
            dep_to_global: &dep_mappings[idx],
            synthetic_def_space: db.unit_def_spaces().intern(idx),
        })
        .collect();

    let spec_imports = spec.imports(db);
    let mut boundary_deps: Vec<(su::TermId, usize)> = resolved
        .boundary_imports
        .iter()
        .map(|&(term, ord)| (term, dep_index[&spec_imports[ord]]))
        .collect();
    if let (Some(pre_boundary), Some(pre_spec)) = (resolved.prelude_boundary, spec.prelude(db)) {
        boundary_deps.push((pre_boundary, dep_index[&pre_spec]));
    }
    let signature_dep = spec.signature(db).map(|sig| dep_index[&sig]);

    Ok(zydeco_statics::separate::check_unit(
        db,
        UnitCheckInput {
            spans: &spans,
            scoped: &resolved.arena,
            root: resolved.root,
            signature_dep,
            has_monadic: resolved.has_monadic,
            boundary_deps: &boundary_deps,
            deps: &imported_deps,
        },
    ))
}

#[salsa::tracked(returns(clone), unsafe(non_salsa_values))]
pub(crate) fn unit_interface<'db>(
    db: &'db dyn SourceQueryDb, spec: UnitSpec<'db>,
) -> UnitInterface {
    match check_unit_internal(db, spec) {
        | Ok(out) => out.interface,
        | Err(()) => UnitInterface::unresolved(),
    }
}

fn assign_file_layout(
    db: &dyn SourceQueryDb, units: &[UnitSpec<'_>],
) -> (FxHashMap<SourceInput, (BytePos, Arc<SourceTemplate>)>, SourceMap) {
    let mut base = 1u32;
    let mut seen: FxHashMap<SourceInput, (BytePos, Arc<SourceTemplate>)> = FxHashMap::default();
    let mut sources = Vec::new();
    for &unit in units {
        let input = unit.input(db);
        if seen.contains_key(&input) {
            continue;
        }
        let template = parse_source(db, input).expect("source graph validated inputs");
        let len = u32::try_from(template.source.len()).expect("program address space exceeds u32");
        let assigned = BytePos(base);
        base += len;
        sources.push(FileSource {
            path: Some(Arc::new(template.path.clone())),
            source: template.source.as_str().into(),
        });
        seen.insert(input, (assigned, template));
    }
    (seen, SourceMap::from_sources(sources))
}

fn collect_resolve_failures<'db>(
    db: &'db dyn SourceQueryDb, units: &[UnitSpec<'db>],
    file_layout: &FxHashMap<SourceInput, (BytePos, Arc<SourceTemplate>)>, source_map: SourceMap,
    graph: Arc<SourceGraph>,
) -> Result<Vec<Arc<ResolvedUnitMemo>>, AnalysisError> {
    let mut resolved = Vec::with_capacity(units.len());
    let mut errors = Vec::new();
    for &unit in units {
        match resolve_unit(db, unit) {
            | Ok(memo) => resolved.push(memo),
            | Err(err) => {
                let ResolveUnitError::Failed(failure) = err.as_ref() else {
                    unreachable!("source_graph validated every input parses");
                };
                let (base, latest_template) = &file_layout[&unit.input(db)];
                if latest_template.spans.key_space() != failure.textual_key_space {
                    let fresh = ResolveFolder::new(&latest_template.spans, &latest_template.arena)
                        .run_unit(
                            unit_root_term(latest_template, unit.selection(db)),
                            UnitResolveConfig {
                                literals: &latest_template.literals,
                                has_prelude: unit.prelude(db).is_some(),
                                is_signature: unit.is_signature(db),
                            },
                        )
                        .err()
                        .expect("failing unit still fails on unchanged syntax digest");
                    errors.extend(fresh.into_iter().map(|err| err.rebase(*base)));
                } else {
                    errors.extend(failure.errors.iter().cloned().map(|err| err.rebase(*base)));
                }
            }
        }
    }
    if errors.is_empty() {
        return Ok(resolved);
    }
    let mut merged_spans = SpanArena::default();
    for (base, template) in file_layout.values() {
        merged_spans.merge_rebased_as(&template.spans, *base, None);
    }
    merged_spans.attach_map(Arc::new(source_map));
    let diagnostics = Diagnostics::with_errors(errors).expect("resolve failure recorded errors");
    Err(AnalysisError::Resolve {
        error: Box::new(diagnostics),
        graph,
        spans: Arc::new(merged_spans),
    })
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub(crate) fn assemble_program(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>,
) -> Result<AssembledProgram, AnalysisError> {
    #[cfg(test)]
    db.query_counters().assemble.fetch_add(1, Ordering::Relaxed);

    let graph = source_graph(db, root, package, project)
        .map_err(|error| AnalysisError::Source { error })?;
    let root_spec = unit_spec_from_graph(db, &graph, graph.root)
        .map_err(|error| AnalysisError::Source { error: Arc::new(error.into()) })?;
    let (units, unit_index) = collect_unique_units(db, root_spec);
    let (file_layout, source_map) = assign_file_layout(db, &units);
    let resolved =
        collect_resolve_failures(db, &units, &file_layout, source_map.clone(), graph.clone())?;

    let mut merged_spans = SpanArena::default();
    let mut merged_scoped = ScopedArena::default();
    let mut unit_roots = FxHashMap::default();
    for (unit, memo) in units.iter().zip(&resolved) {
        let (base, latest_template) = &file_layout[&unit.input(db)];
        merged_spans.merge_rebased_as(&latest_template.spans, *base, memo.textual_key_space);
        if latest_template.spans.key_space() != memo.textual_key_space {
            merged_spans.merge_rebased_as(&latest_template.spans, *base, None);
        }
        merged_scoped.merge_unit(&memo.arena);
        unit_roots.insert(*unit, memo.root);
    }
    merged_spans.attach_map(Arc::new(source_map));

    for (unit, memo) in units.iter().zip(&resolved) {
        let unit_imports = unit.imports(db);
        for &(boundary, ord) in &memo.boundary_imports {
            let dep_root = unit_roots[&unit_imports[ord]];
            merged_scoped.patch_term(boundary, su::SourceBoundary(dep_root).into());
        }
        if let (Some(pre_boundary), Some(pre_spec)) = (memo.prelude_boundary, unit.prelude(db)) {
            let pre_root = unit_roots[&pre_spec];
            merged_scoped.patch_term(pre_boundary, su::SourceBoundary(pre_root).into());
        }
    }

    let checked_units: Vec<CheckedUnitOutput> = units
        .iter()
        .map(|&unit| check_unit_internal(db, unit).expect("resolved unit produced check output"))
        .collect();
    let dep_mappings: Vec<Vec<usize>> =
        units.iter().map(|&unit| compute_dep_to_global(db, unit, &unit_index)).collect();
    let inputs: Vec<AssembledUnitInput<'_>> = checked_units
        .iter()
        .enumerate()
        .map(|(idx, out)| AssembledUnitInput {
            interface: &out.interface,
            body: &out.body,
            dep_to_global: &dep_mappings[idx],
        })
        .collect();

    let spans = Arc::new(merged_spans);
    let root_term = unit_roots[&root_spec];
    let output = zydeco_statics::separate::assemble_program(
        db,
        spans.clone(),
        Arc::new(merged_scoped),
        root_term,
        &inputs,
    );
    Ok(AssembledProgram { graph, spans, root_term, output })
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub(crate) fn prepare_library_program(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>, contract: zydeco_surface::metadata::LibraryContract,
) -> Result<LibraryProgram, LibraryCheckError> {
    let assembled =
        assemble_program(db, root, package, project).map_err(|_| LibraryCheckError::Root)?;
    let (statics, root_ann) = match assembled.output.outcome {
        | zydeco_statics::SourceCheckOutcome::Checked(checked) => (checked.statics, checked.root),
        | zydeco_statics::SourceCheckOutcome::Rejected(rejected) => {
            return Err(LibraryCheckError::Checking(rejected.diagnostics));
        }
    };
    let library = zydeco_statics::separate::prepare_assembled_library(
        db,
        assembled.spans.clone(),
        assembled.output.scoped.clone(),
        &statics,
        assembled.root_term,
        root_ann,
        &contract,
    )?;
    Ok(LibraryProgram { spans: assembled.spans, scoped: assembled.output.scoped, library })
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub(crate) fn prepare_unit_program(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>,
) -> Result<UnitProgram, LibraryCheckError> {
    let assembled =
        assemble_program(db, root, package, project).map_err(|_| LibraryCheckError::Root)?;
    let (statics, root_ann) = match assembled.output.outcome {
        | zydeco_statics::SourceCheckOutcome::Checked(checked) => (checked.statics, checked.root),
        | zydeco_statics::SourceCheckOutcome::Rejected(rejected) => {
            return Err(LibraryCheckError::Checking(rejected.diagnostics));
        }
    };
    let unit = zydeco_statics::separate::prepare_assembled_unit(
        db,
        assembled.spans.clone(),
        assembled.output.scoped.clone(),
        &statics,
        assembled.root_term,
        root_ann,
    )?;
    Ok(UnitProgram { spans: assembled.spans, scoped: assembled.output.scoped, unit })
}
