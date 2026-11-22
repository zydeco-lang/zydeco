use super::{
    AnalysisError, CompilerSession, QuerySourceProvider, SourceInput, SourceQueryDb, source_text,
};
use crate::source::{
    SourceGraph, SourceId, SourceKind, SourceLoadError, SourceTemplate,
    loader::{SourceGraphLoader, SourceProvider},
    unit::{UnitSpec, check_unit_internal, unit_interface, unit_spec_from_graph},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_foundation::{
    arena::ArenaAccess,
    span::{BytePos, FileMap},
};
use zydeco_statics::{
    arena::StaticsArena,
    check::{AnnotationCompatibility, CompletionTyping},
    separate::{
        CheckedUnitOutput, ImportedUnitDep, UnitBody, UnitCheckInput, UnitCompletionInput,
        UnitInterface, check_unit, check_unit_completion,
    },
    syntax::{AnnId, DefId},
};
use zydeco_surface::{
    scoped::{
        ResolveFolder, ResolvedUnitOut, UnitCompletionResolution, UnitResolveConfig,
        VisibleDefinition, arena::ScopedArena, syntax as su,
    },
    textual::{
        CompletionCursor, CompletionCursorError, LexicalRole, ParsedHole, RecoveringParser,
        TokenKind,
        syntax::{self as t, Parser, SpanArena},
    },
};

/// Visible names and optional classifiers owned by one current-source completion query.
#[derive(Debug)]
pub struct CompletionAnalysis {
    pub source: String,
    pub replacement: Range<usize>,
    pub candidates: Vec<VisibleDefinition>,
    /// Contextual OS fallback; it has no fabricated definition identity.
    pub implicit_os: bool,
    pub semantics: Option<CompletionSemantics>,
}

/// The arenas owning completion annotations, never an older strict analysis's IDs.
#[derive(Debug)]
pub struct CompletionSemantics {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub typing: CompletionTyping,
}

impl CompletionSemantics {
    pub fn annotation(&self, definition: DefId) -> Option<AnnId> {
        self.statics.annotations_var.get(&definition).copied()
    }

    pub fn compatibility(&self, definition: DefId) -> AnnotationCompatibility {
        self.typing.compatibility(definition)
    }
}

#[derive(Clone, Debug, Error)]
pub enum CompletionError {
    #[error(transparent)]
    Cursor(#[from] CompletionCursorError),
    #[error(transparent)]
    Analysis(#[from] AnalysisError),
}

impl CompilerSession {
    /// Complete ordinary names at a byte cursor without changing strict source inputs or analyses.
    /// An unavailable or non-reference syntactic site yields no result.
    pub fn complete(
        &self, root: impl AsRef<Path>, offset: usize,
    ) -> Result<Option<Arc<CompletionAnalysis>>, CompletionError> {
        self.complete_in(root, offset, Arc::default())
    }

    /// Complete in the project's selected package context, using the same resolution inputs as analysis.
    pub fn complete_in(
        &self, root: impl AsRef<Path>, offset: usize, project: Arc<crate::source::Project>,
    ) -> Result<Option<Arc<CompletionAnalysis>>, CompletionError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error.into()) })?;
        complete_source(self, root, offset, project)
    }
}

/// Only the edited root uses recovered syntax. Dependencies use the ordinary
/// provider, including overlays, companion signatures, and hygienic import edges.
struct CompletionSourceProvider<'db> {
    root: Arc<SourceTemplate>,
    ordinary: QuerySourceProvider<'db>,
}

impl SourceProvider for CompletionSourceProvider<'_> {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError> {
        if path == self.root.path { Ok(Arc::clone(&self.root)) } else { self.ordinary.load(path) }
    }

    fn load_optional(
        &mut self, path: &Path,
    ) -> Result<Option<Arc<SourceTemplate>>, SourceLoadError> {
        if path == self.root.path {
            Ok(Some(Arc::clone(&self.root)))
        } else {
            self.ordinary.load_optional(path)
        }
    }
}

// A completion owns transient arenas. Retain only the most recent result while
// leaving strict parsing and analysis query identities untouched.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
fn complete_source(
    db: &dyn SourceQueryDb, root: SourceInput, offset: usize, project: Arc<crate::source::Project>,
) -> Result<Option<Arc<CompletionAnalysis>>, CompletionError> {
    let Some(source) = source_text(db, root) else {
        return Ok(None);
    };
    let cursor = CompletionCursor::at(&source, offset)?;
    let prefix = match cursor.token_role() {
        | None
        | Some(
            LexicalRole::UpperIdentifier | LexicalRole::LowerIdentifier | LexicalRole::Keyword,
        ) => cursor.prefix().to_owned(),
        | Some(LexicalRole::Hole) => String::new(),
        | Some(_) => return Ok(None),
    };
    let mut parser = Parser::new();
    let parsed = RecoveringParser::with_completion(cursor).source(&mut parser);
    let Some(unit) = parsed.syntax else {
        return Ok(None);
    };
    let Some(site) = parsed.completion else {
        return Ok(None);
    };
    // Recovery can pop a projection or other restricted-name position into a
    // term hole. Require a hole to have been legal at the original cursor too.
    if !site.expected.contains(&TokenKind::Hole) {
        return Ok(None);
    }
    let Some(ParsedHole::Term(target)) = site.hole.map(|hole| hole.entity) else {
        return Ok(None);
    };
    let path = root.path(db);
    let file =
        FileMap::new(root.file_id(db), source.as_str(), Some(Arc::new(path.clone())), BytePos(0));
    let template = SourceTemplate::with_syntax(path.clone(), source.clone(), file, parser, unit)
        .map_err(|error| AnalysisError::Source {
            error: Arc::new(SourceLoadError::Parse(error).into()),
        })?;
    let provider =
        CompletionSourceProvider { root: Arc::new(template), ordinary: QuerySourceProvider { db } };
    let graph = SourceGraphLoader::with_provider(provider)
        .load_root(&path, None, project)
        .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
    let root_file = &graph.sources[&graph.root];
    let Some(target) = root_file.arena.terms.get(&target).map(|_| target) else {
        return Ok(None);
    };
    let root_spans = Arc::new((*root_file.spans).clone());
    let UnitCompletionResolution { site: resolved, unit, .. } =
        ResolveFolder::new(&root_file.spans, &root_file.arena).run_unit_completion(
            root_file.root,
            target,
            UnitResolveConfig {
                literals: &root_file.literals,
                has_prelude: root_file.prelude.is_some(),
                is_signature: root_file.kind() == SourceKind::Signature,
            },
        );
    let Some(resolved) = resolved else {
        return Ok(None);
    };
    let implicit_os = resolved.scope.implicit_os && "OS".starts_with(&prefix);
    let mut candidates = resolved
        .scope
        .definitions
        .into_iter()
        .filter(|definition| definition.name.starts_with(&prefix))
        .collect::<Vec<_>>();
    let semantics =
        unit.ok().filter(|_| implicit_os || !candidates.is_empty()).map(|resolved_unit| {
            let candidate_defs: Vec<_> =
                candidates.iter().map(|candidate| candidate.definition).collect();
            run_completion_check(
                db,
                &graph,
                &root_spans,
                resolved.target,
                resolved_unit,
                &candidate_defs,
            )
        });
    candidates.retain(|candidate| {
        !semantics.as_ref().is_some_and(|semantics| {
            semantics.compatibility(candidate.definition) == AnnotationCompatibility::Mismatch
        })
    });
    let implicit_os = implicit_os
        && !semantics.as_ref().is_some_and(|semantics| {
            semantics.typing.builtin_os_compatibility() == AnnotationCompatibility::Mismatch
        });
    candidates.sort_by(|left, right| {
        let compatibility = |candidate: &VisibleDefinition| {
            semantics
                .as_ref()
                .map(|semantics| semantics.compatibility(candidate.definition))
                .unwrap_or_default()
        };
        (left.name.as_str() != prefix, compatibility(left), left.distance, &left.name).cmp(&(
            right.name.as_str() != prefix,
            compatibility(right),
            right.distance,
            &right.name,
        ))
    });
    Ok(Some(Arc::new(CompletionAnalysis {
        source,
        replacement: site.replacement,
        candidates,
        implicit_os,
        semantics,
    })))
}

type CompletionUnitKey = (PathBuf, Option<usize>);

enum DepProduct<'db> {
    Cached { spec: UnitSpec<'db>, interface: UnitInterface, monadic_body: Option<Arc<UnitBody>> },
    Transient(CheckedUnitOutput),
    Failed(UnitInterface),
}

impl DepProduct<'_> {
    fn interface(&self) -> &UnitInterface {
        match self {
            | Self::Cached { interface, .. } => interface,
            | Self::Transient(out) => &out.interface,
            | Self::Failed(interface) => interface,
        }
    }

    fn body(&self) -> Option<&UnitBody> {
        match self {
            | Self::Cached { monadic_body, .. } => monadic_body.as_deref(),
            | Self::Transient(out) => Some(out.body.as_ref()),
            | Self::Failed(_) => None,
        }
    }
}

fn completion_unit_key(graph: &SourceGraph, source: SourceId) -> CompletionUnitKey {
    let file = &graph.sources[&source];
    let site = (file.root != file.template.unit.root).then(|| {
        file.template
            .package_sites
            .iter()
            .position(|site| site.term == file.root)
            .expect("package root matches a parsed package site")
    });
    (file.template.path.clone(), site)
}

fn collect_postorder_deps(graph: &SourceGraph, root: SourceId) -> Vec<SourceId> {
    fn visit(
        graph: &SourceGraph, source: SourceId, seen: &mut FxHashSet<CompletionUnitKey>,
        out: &mut Vec<SourceId>,
    ) {
        if !seen.insert(completion_unit_key(graph, source)) {
            return;
        }
        let file = &graph.sources[&source];
        for imp_id in &file.imports {
            visit(graph, graph.imports[imp_id].imported, seen, out);
        }
        if let Some(pre) = file.prelude {
            visit(graph, pre, seen, out);
        }
        if let Some(sig) = file.signature {
            visit(graph, sig, seen, out);
        }
        out.push(source);
    }

    let mut seen = FxHashSet::default();
    let mut out = Vec::new();
    visit(graph, root, &mut seen, &mut out);
    let popped = out.pop();
    debug_assert_eq!(popped, Some(root));
    out
}

fn boundary_deps_for_unit(
    graph: &SourceGraph, source: SourceId, resolved: &ResolvedUnitOut,
    index: &FxHashMap<CompletionUnitKey, usize>,
) -> (Vec<(su::TermId, usize)>, Option<usize>) {
    let file = &graph.sources[&source];
    let import_map: FxHashMap<t::TermId, SourceId> = file
        .imports
        .iter()
        .map(|imp_id| {
            let imp = &graph.imports[imp_id];
            (imp.term, imp.imported)
        })
        .collect();
    let mut boundary_deps: Vec<(su::TermId, usize)> = resolved
        .imports
        .iter()
        .map(|&(t_term, boundary)| {
            let dep = import_map[&t_term];
            (boundary, index[&completion_unit_key(graph, dep)])
        })
        .collect();
    if let (Some(pre_boundary), Some(pre_source)) = (resolved.prelude_boundary, file.prelude) {
        boundary_deps.push((pre_boundary, index[&completion_unit_key(graph, pre_source)]));
    }
    let signature_dep = file.signature.map(|sig| index[&completion_unit_key(graph, sig)]);
    (boundary_deps, signature_dep)
}

fn transient_check_dep<'db>(
    db: &'db dyn SourceQueryDb, graph: &SourceGraph, dep_sources: &[SourceId],
    dep_mappings: &[Vec<usize>], products: &[DepProduct<'db>], idx: usize,
) -> DepProduct<'db> {
    let dep_source = dep_sources[idx];
    let dep_file = &graph.sources[&dep_source];
    let Ok(dep_resolved) = ResolveFolder::new(&dep_file.spans, &dep_file.arena).run_unit(
        dep_file.root,
        UnitResolveConfig {
            literals: &dep_file.literals,
            has_prelude: dep_file.prelude.is_some(),
            is_signature: dep_file.kind() == SourceKind::Signature,
        },
    ) else {
        return DepProduct::Failed(UnitInterface::unresolved());
    };
    let own_deps = &dep_mappings[idx];
    let own_index: FxHashMap<usize, usize> =
        own_deps.iter().enumerate().map(|(local_i, &global_i)| (global_i, local_i)).collect();
    let local_key_index: FxHashMap<CompletionUnitKey, usize> = own_deps
        .iter()
        .enumerate()
        .map(|(local_i, &global_i)| (completion_unit_key(graph, dep_sources[global_i]), local_i))
        .collect();
    let local_mappings: Vec<Vec<usize>> = own_deps
        .iter()
        .map(|&global_i| dep_mappings[global_i].iter().map(|g| own_index[g]).collect())
        .collect();
    let extra_bodies: Vec<Option<Arc<UnitBody>>> = if dep_resolved.has_monadic {
        own_deps
            .iter()
            .map(|&global_i| match &products[global_i] {
                | DepProduct::Cached { spec, monadic_body: None, .. } => {
                    check_unit_internal(db, *spec).ok().map(|out| out.body)
                }
                | _ => None,
            })
            .collect()
    } else {
        vec![None; own_deps.len()]
    };
    let imported: Vec<ImportedUnitDep<'_>> = own_deps
        .iter()
        .enumerate()
        .map(|(local_i, &global_i)| {
            let prod = &products[global_i];
            ImportedUnitDep {
                interface: prod.interface(),
                body: dep_resolved
                    .has_monadic
                    .then(|| prod.body().or(extra_bodies[local_i].as_deref()))
                    .flatten(),
                dep_to_global: &local_mappings[local_i],
                synthetic_def_space: db.unit_def_spaces().intern(local_i),
            }
        })
        .collect();
    let (boundary_deps, signature_dep) =
        boundary_deps_for_unit(graph, dep_source, &dep_resolved, &local_key_index);
    let dep_spans = Arc::new((*dep_file.spans).clone());
    let dep_scoped = Arc::new(dep_resolved.arena.into_inner());
    let checked = check_unit(
        db,
        UnitCheckInput {
            spans: &dep_spans,
            scoped: &dep_scoped,
            root: dep_resolved.root,
            signature_dep,
            has_monadic: dep_resolved.has_monadic,
            boundary_deps: &boundary_deps,
            deps: &imported,
        },
    );
    DepProduct::Transient(checked)
}

fn run_completion_check<'db>(
    db: &'db dyn SourceQueryDb, graph: &SourceGraph, root_spans: &Arc<SpanArena>,
    target: su::TermId, resolved_unit: ResolvedUnitOut, candidates: &[su::DefId],
) -> CompletionSemantics {
    let root_path = &graph.sources[&graph.root].template.path;
    let dep_sources = collect_postorder_deps(graph, graph.root);
    let global_index: FxHashMap<CompletionUnitKey, usize> = dep_sources
        .iter()
        .enumerate()
        .map(|(idx, &src)| (completion_unit_key(graph, src), idx))
        .collect();
    let dep_mappings: Vec<Vec<usize>> = dep_sources
        .iter()
        .map(|&src| {
            collect_postorder_deps(graph, src)
                .into_iter()
                .map(|child| global_index[&completion_unit_key(graph, child)])
                .collect()
        })
        .collect();

    let mut touches_root = vec![false; dep_sources.len()];
    let mut products: Vec<DepProduct<'db>> = Vec::with_capacity(dep_sources.len());
    for (idx, &dep_source) in dep_sources.iter().enumerate() {
        let dep_file = &graph.sources[&dep_source];
        let direct_children_touch_root = dep_mappings[idx].iter().any(|&c| touches_root[c]);
        let is_transient = dep_file.template.path == *root_path || direct_children_touch_root;
        touches_root[idx] = is_transient;

        if !is_transient {
            let spec = unit_spec_from_graph(db, graph, dep_source)
                .expect("non-edited dependency has query spec");
            let interface = unit_interface(db, spec);
            let monadic_body = resolved_unit
                .has_monadic
                .then(|| check_unit_internal(db, spec).ok().map(|out| out.body))
                .flatten();
            products.push(DepProduct::Cached { spec, interface, monadic_body });
        } else {
            products.push(transient_check_dep(
                db,
                graph,
                &dep_sources,
                &dep_mappings,
                &products,
                idx,
            ));
        }
    }

    let imported_deps: Vec<ImportedUnitDep<'_>> = products
        .iter()
        .enumerate()
        .map(|(idx, prod)| ImportedUnitDep {
            interface: prod.interface(),
            body: resolved_unit.has_monadic.then(|| prod.body()).flatten(),
            dep_to_global: &dep_mappings[idx],
            synthetic_def_space: db.unit_def_spaces().intern(idx),
        })
        .collect();
    let (boundary_deps, signature_dep) =
        boundary_deps_for_unit(graph, graph.root, &resolved_unit, &global_index);
    let root_scoped = Arc::new(resolved_unit.arena.into_inner());
    let output = check_unit_completion(
        db,
        UnitCompletionInput {
            spans: root_spans,
            scoped: &root_scoped,
            root: resolved_unit.root,
            signature_dep,
            has_monadic: resolved_unit.has_monadic,
            boundary_deps: &boundary_deps,
            deps: &imported_deps,
            target,
            candidates,
        },
    );
    CompletionSemantics {
        scoped: output.root_scoped,
        statics: output.statics,
        typing: output.typing,
    }
}

#[cfg(test)]
mod tests;
