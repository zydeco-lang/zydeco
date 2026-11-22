//! Separate compilation checking, interface extraction, and whole-program assembly.

use crate::StaticsArena;
use crate::check::judgment::Tyck;
use crate::check::source::InferenceRegion;
use crate::check::{
    Action, CheckedExport, CheckedLibrary, CheckedSource, CheckedUnit, CompletionTyping,
    IntrinsicStatics, LibraryCheckError, PreloadedState, RejectedSource, ReportedError,
    SourceCheckOutcome, TyckDiagnostics, TyckError, Tycker,
};
use crate::query::{ScopedData, TyckDb, TyckOutput};
use crate::surface_syntax as su;
use crate::syntax::*;
use std::sync::Arc;
use zydeco_foundation::arena::KeySpaceId;
use zydeco_foundation::err::Report;
use zydeco_surface::metadata::LibraryContract;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_surface::textual::syntax::SpanArena;

mod digest;
mod extract;
mod rel;
mod transplant;

pub use digest::InterfaceDigest;
use rel::*;

/// Exported static facts recorded for one interface-visible definition.
#[derive(Clone, Debug, Default)]
pub(crate) struct RelDefFacts {
    pub annotation: Option<RelAnnId>,
    pub value_alias: Option<RelId<ValueId>>,
    pub package_alias: Option<Vec<RelStaticTermId>>,
    pub type_definition: Option<RelId<TypeId>>,
    pub inlinable: Option<RelId<ValueId>>,
    pub global: bool,
}

/// Exported abstract-type entry inside an accepted unit interface.
#[derive(Clone, Debug)]
pub(crate) struct RelAbstEntry {
    pub kind: RelId<KindId>,
    pub seal: Option<RelId<TypeId>>,
    pub hint: Option<RelDefId>,
    pub existential_skolem: bool,
    pub builtin_role: Option<BuiltinRole>,
}

/// Self-contained, span-free exported static semantics of an accepted compilation unit.
#[derive(Clone, Debug)]
pub struct AcceptedUnitInterface {
    pub(crate) root: RelTermAnnId,
    pub(crate) local_defs: Vec<VarName>,
    pub(crate) kinds: Vec<RelKind>,
    pub(crate) kpats: Vec<RelKindPattern>,
    pub(crate) tpats: Vec<(RelTypePattern, RelId<KindId>)>,
    pub(crate) types: Vec<(RelType, RelId<KindId>)>,
    pub(crate) vpats: Vec<(RelValuePattern, RelId<TypeId>)>,
    pub(crate) values: Vec<(RelValue, RelId<TypeId>)>,
    pub(crate) thunk_types: Vec<RelId<TypeId>>,
    pub(crate) absts: Vec<RelAbstEntry>,
    pub(crate) datas: Vec<RelData>,
    pub(crate) codatas: Vec<RelCoData>,
    pub(crate) value_builtin_roles: Vec<(u32, BuiltinValueRole)>,
    pub(crate) foreign_imports: Vec<(u32, CheckedImport)>,
    pub(crate) data_hints: Vec<(u32, RelId<DataId>)>,
    pub(crate) data_pat_hints: Vec<(u32, RelId<DataId>)>,
    pub(crate) package_pattern_opened_arity: Vec<(u32, usize)>,
    pub(crate) def_facts: Vec<(u32, RelDefFacts)>,
    pub(crate) global_terms: Vec<RelTermId>,
    pub(crate) digest: InterfaceDigest,
}

/// Fingerprinted unit interface enabling Salsa early-cutoff when bodies change without interface drift.
#[derive(Clone, Debug)]
pub enum UnitInterface {
    Accepted(Arc<AcceptedUnitInterface>),
    Rejected { digest: InterfaceDigest },
}

impl UnitInterface {
    /// Construct a sentinel interface for a unit whose static checking never ran due to an upstream error.
    pub fn unresolved() -> Self {
        Self::Rejected { digest: InterfaceDigest([0; 32]) }
    }

    pub fn digest(&self) -> InterfaceDigest {
        match self {
            | Self::Accepted(acc) => acc.digest,
            | Self::Rejected { digest } => *digest,
        }
    }

    pub fn is_accepted(&self) -> bool {
        matches!(self, Self::Accepted(_))
    }
}

impl PartialEq for UnitInterface {
    fn eq(&self, other: &Self) -> bool {
        self.digest() == other.digest()
    }
}

impl Eq for UnitInterface {}

/// Complete internal typed nodes, proof obligations, and diagnostics produced for one unit.
#[derive(Clone, Debug)]
pub struct UnitBody {
    pub(crate) root: Option<RelTermAnnId>,
    pub(crate) interface_def_map: Vec<su::DefId>,
    pub(crate) thunk_bodies: Vec<RelId<CompuId>>,
    pub(crate) kinds: Vec<RelFillable<RelKind>>,
    pub(crate) kinds_normalized: Vec<(RelId<KindId>, RelKind)>,
    pub(crate) kpats: Vec<RelKindPattern>,
    pub(crate) tpats: Vec<(RelTypePattern, RelId<KindId>)>,
    pub(crate) types: Vec<(RelFillable<RelType>, RelId<KindId>)>,
    pub(crate) types_normalized: Vec<(RelId<TypeId>, RelType)>,
    pub(crate) vpats: Vec<(RelValuePattern, RelId<TypeId>)>,
    pub(crate) values: Vec<(RelValue, RelId<TypeId>)>,
    pub(crate) compus: Vec<(RelComputation, RelId<TypeId>)>,
    pub(crate) absts: Vec<RelAbstEntry>,
    pub(crate) datas: Vec<RelData>,
    pub(crate) codatas: Vec<RelCoData>,
    pub(crate) fills: Vec<InferenceSite>,
    pub(crate) solus: Vec<(RelId<FillId>, RelAnnId)>,
    pub(crate) fill_scopes: Vec<(RelId<FillId>, Vec<RelId<AbstId>>)>,
    pub(crate) fill_hints: Vec<RelId<FillId>>,
    pub(crate) value_builtin_roles: Vec<(RelId<TypeId>, BuiltinValueRole)>,
    pub(crate) foreign_imports: Vec<(RelId<ValueId>, CheckedImport)>,
    pub(crate) data_hints: Vec<(RelId<ValueId>, RelId<DataId>)>,
    pub(crate) data_pat_hints: Vec<(RelId<VPatId>, RelId<DataId>)>,
    pub(crate) codata_hints: Vec<(RelId<CompuId>, RelId<CoDataId>)>,
    pub(crate) copattern_matches: Vec<RelId<CompuId>>,
    pub(crate) copattern_pack_pi_binders: Vec<(RelId<CompuId>, RelId<VPatId>)>,
    pub(crate) value_aliases: Vec<(RelDefId, RelId<ValueId>)>,
    pub(crate) package_aliases: Vec<(RelDefId, Vec<RelStaticTermId>)>,
    pub(crate) package_pattern_opened_arity: Vec<(RelId<VPatId>, usize)>,
    pub(crate) type_definitions: Vec<(RelDefId, RelId<TypeId>)>,
    pub(crate) inlinables: Vec<(RelDefId, RelId<ValueId>)>,
    pub(crate) global_defs: Vec<RelDefId>,
    pub(crate) global_terms: Vec<RelTermId>,
    pub(crate) def_hints: Vec<(RelTermId, RelDefId)>,
    pub(crate) annotations_var: Vec<(RelDefId, RelAnnId)>,
    pub(crate) body_defs: Vec<(su::DefId, VarName)>,
    pub(crate) generated_defs: Vec<(su::DefId, VarName)>,
    pub(crate) pat_provenance: Vec<(su::PatId, RelPatId)>,
    pub(crate) term_provenance: Vec<(su::TermId, RelTermId)>,
    pub(crate) term_facts: Vec<(su::TermId, RelTermAnnId)>,
    pub(crate) coverage_errors: Vec<RelCoverageError>,
    pub(crate) observations: Vec<RelObservation>,
    pub(crate) errors: Vec<RelErrorEntry>,
    pub(crate) monadic_families: Vec<(RelId<AbstId>, RelNominalFamily)>,
}

/// Finished separate-compilation product of checking one unit.
#[derive(Clone, Debug)]
pub struct CheckedUnitOutput {
    pub interface: UnitInterface,
    pub body: Arc<UnitBody>,
}

/// One pre-checked dependency supplied to a downstream unit check.
pub struct ImportedUnitDep<'a> {
    pub interface: &'a UnitInterface,
    pub body: Option<&'a UnitBody>,
    pub dep_to_global: &'a [usize],
    pub synthetic_def_space: KeySpaceId,
}

/// Inputs for checking one closed compilation unit against its dependencies.
pub struct UnitCheckInput<'a> {
    pub spans: &'a Arc<SpanArena>,
    pub scoped: &'a Arc<ScopedArena>,
    pub root: su::TermId,
    pub signature_dep: Option<usize>,
    pub has_monadic: bool,
    pub boundary_deps: &'a [(su::TermId, usize)],
    pub deps: &'a [ImportedUnitDep<'a>],
}

/// One unit's checked artifacts supplied for whole-program assembly.
pub struct AssembledUnitInput<'a> {
    pub interface: &'a UnitInterface,
    pub body: &'a UnitBody,
    pub dep_to_global: &'a [usize],
}

/// Inputs for completion checking of one edited root unit against cached dependencies.
pub struct UnitCompletionInput<'a> {
    pub spans: &'a Arc<SpanArena>,
    pub scoped: &'a Arc<ScopedArena>,
    pub root: su::TermId,
    pub signature_dep: Option<usize>,
    pub has_monadic: bool,
    pub boundary_deps: &'a [(su::TermId, usize)],
    pub deps: &'a [ImportedUnitDep<'a>],
    pub target: su::TermId,
    pub candidates: &'a [su::DefId],
}

/// Completion output over a unit check against cached dependencies.
#[derive(Clone, Debug)]
pub struct UnitCompletionOutput {
    pub root_scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub typing: CompletionTyping,
    pub diagnostics: Option<TyckDiagnostics>,
}

fn take_preloaded_state(
    preloaded: &mut transplant::PreloadedUnit, boundary_deps: &[(su::TermId, usize)],
) -> PreloadedState {
    PreloadedState {
        boundaries: boundary_deps
            .iter()
            .map(|&(term, dep_idx)| (term, preloaded.roots[dep_idx]))
            .collect(),
        monadic_families: std::mem::take(&mut preloaded.monadic_families),
        boundary_owned_defs: boundary_deps
            .iter()
            .map(|&(term, dep_idx)| (term, Arc::clone(&preloaded.unit_owned_defs[dep_idx])))
            .collect(),
        dep_value_ranges: std::mem::take(&mut preloaded.dep_value_ranges),
    }
}

/// Check one closed compilation unit against preloaded dependency interfaces (and bodies for `@[monadic]`).
pub fn check_unit(db: &dyn TyckDb, input: UnitCheckInput<'_>) -> CheckedUnitOutput {
    let data = ScopedData::new(db, Arc::clone(input.spans), Arc::clone(input.scoped), input.root);
    let mut preloaded = transplant::preload_for_unit_check(input.deps, input.has_monadic);
    let prechecked = take_preloaded_state(&mut preloaded, input.boundary_deps);
    let local_base = ArenaOffsets::current(&preloaded.statics);
    let mut tycker = Tycker::with_preloaded(
        db,
        data,
        input.spans,
        input.scoped,
        preloaded.statics,
        preloaded.intrinsics,
        prechecked,
    );
    let root =
        run_unit_judgments(&mut tycker, input.root, input.signature_dep, &preloaded.roots).ok();
    tycker.finish_judgments();
    tycker.do_resolve_holes_from(local_base.types);
    tycker.collect_hole_solutions();
    let checked_root = match root {
        | None => None,
        | Some(root) => tycker
            .normalize_and_validate_from(
                local_base.kinds,
                local_base.types,
                local_base.compus,
                local_base.values,
            )
            .ok()
            .map(|()| root),
    };
    let output = extract::extract_unit(
        &tycker,
        checked_root,
        local_base,
        &preloaded.ranges,
        &preloaded.dummy_compus,
        &preloaded.def_spaces,
        input.deps,
    );
    tycker.strip_checker_state();
    output
}

fn run_unit_judgments(
    tycker: &mut Tycker<'_>, root: su::TermId, signature_dep: Option<usize>,
    dep_roots: &[std::result::Result<TermAnnId, ()>],
) -> crate::check::Result<TermAnnId> {
    let Some(sig_idx) = signature_dep else {
        return tycker.run_judgments(root);
    };
    let sig_ty = match dep_roots[sig_idx] {
        | Ok(TermAnnId::Type(sig_ty, _)) => sig_ty,
        | Ok(_) => tycker.err(TyckError::SignatureNotType)?,
        | Err(()) => return Err(ReportedError),
    };
    let checked = tycker.guarded(|tycker| {
        let env = crate::environment::TyEnvT::new(Default::default(), ());
        let inference = InferenceRegion::enter(tycker);
        let checked = env.mk(root).tyck(tycker, Action::ana(AnnId::Type(sig_ty)))?;
        if matches!(checked, TermAnnId::Hole(_)) {
            tycker.err(TyckError::SortMismatch)?
        }
        inference.close(tycker)?;
        Ok(checked)
    });
    if checked.is_err() {
        tycker.check_independent_sources(root);
    }
    checked
}

/// Assemble per-unit interfaces and bodies into one complete `StaticsArena`, render any
/// cross-file diagnostics, and run demand-driven static elaboration at the root.
pub fn assemble_program(
    db: &dyn TyckDb, spans: Arc<SpanArena>, scoped: Arc<ScopedArena>, root_term: su::TermId,
    units: &[AssembledUnitInput<'_>],
) -> TyckOutput {
    let mut assembled = transplant::assemble_for_program(units);
    let any_rejected =
        !assembled.errors.is_empty() || units.iter().any(|unit| !unit.interface.is_accepted());
    if !any_rejected {
        assembled.statics.retain_normalized_annotations();
    }

    let data = ScopedData::new(db, spans.clone(), scoped.clone(), root_term);
    let mut tycker =
        Tycker::from_assembled(db, data, &spans, &scoped, assembled.statics, assembled.intrinsics);
    tycker.errors = assembled.errors;
    tycker.observations = assembled.observations;

    let outcome = if !any_rejected && let Some(root) = assembled.root {
        match tycker.elaborate_static_root(root) {
            | Ok(()) => {
                tycker.strip_checker_state();
                SourceCheckOutcome::Checked(CheckedSource {
                    statics: Arc::new(tycker.statics),
                    root,
                    observations: tycker.observations,
                })
            }
            | Err(ReportedError) => {
                let diagnostics = tycker.error_diagnostics();
                tycker.strip_checker_state();
                SourceCheckOutcome::Rejected(RejectedSource {
                    statics: Arc::new(tycker.statics),
                    diagnostics,
                    observations: tycker.observations,
                })
            }
        }
    } else {
        let diagnostics = tycker.error_diagnostics();
        tycker.strip_checker_state();
        SourceCheckOutcome::Rejected(RejectedSource {
            statics: Arc::new(tycker.statics),
            diagnostics,
            observations: tycker.observations,
        })
    };
    TyckOutput { scoped, outcome }
}

/// Check the edited completion root against cached dependency interfaces and measure compatibility.
pub fn check_unit_completion(
    db: &dyn TyckDb, input: UnitCompletionInput<'_>,
) -> UnitCompletionOutput {
    let data = ScopedData::new(db, Arc::clone(input.spans), Arc::clone(input.scoped), input.root);
    let mut preloaded = transplant::preload_for_unit_check(input.deps, input.has_monadic);
    let prechecked = take_preloaded_state(&mut preloaded, input.boundary_deps);
    let local_base = ArenaOffsets::current(&preloaded.statics);
    let mut tycker = Tycker::with_preloaded(
        db,
        data,
        input.spans,
        input.scoped,
        preloaded.statics,
        preloaded.intrinsics,
        prechecked,
    );
    tycker.set_completion_target(input.target);
    let root =
        run_unit_judgments(&mut tycker, input.root, input.signature_dep, &preloaded.roots).ok();
    tycker.finish_judgments();
    tycker.do_resolve_holes_from(local_base.types);
    tycker.collect_hole_solutions();
    let result = match root {
        | None => Err(ReportedError),
        | Some(root) => tycker
            .normalize_and_validate_from(
                local_base.kinds,
                local_base.types,
                local_base.compus,
                local_base.values,
            )
            .map(|()| root),
    };
    let diagnostics = result.is_err().then(|| tycker.error_diagnostics());
    let typing = tycker.completion_typing(input.candidates).expect("completion capture active");

    tycker.strip_checker_state();
    UnitCompletionOutput {
        root_scoped: Arc::clone(input.scoped),
        statics: Arc::new(tycker.statics),
        typing,
        diagnostics,
    }
}

/// Prepare explicit C library exports from an already-checked and assembled program.
pub fn prepare_assembled_library(
    db: &dyn TyckDb, spans: Arc<SpanArena>, scoped: Arc<ScopedArena>, statics: &StaticsArena,
    root_term: su::TermId, root: TermAnnId, contract: &LibraryContract,
) -> std::result::Result<CheckedLibrary, LibraryCheckError> {
    let data = ScopedData::new(db, spans.clone(), scoped.clone(), root_term);
    let intrinsics = IntrinsicStatics::from_existing(statics);
    let mut tycker = Tycker::from_assembled(db, data, &spans, &scoped, statics.clone(), intrinsics);
    let exports: Vec<CheckedExport> = tycker.prepare_library(root, contract)?;
    tycker.strip_checker_state();
    Ok(CheckedLibrary { exports })
}

/// Prepare a native unit initializer from an already-checked and assembled program.
pub fn prepare_assembled_unit(
    db: &dyn TyckDb, spans: Arc<SpanArena>, scoped: Arc<ScopedArena>, statics: &StaticsArena,
    root_term: su::TermId, root: TermAnnId,
) -> std::result::Result<CheckedUnit, LibraryCheckError> {
    let data = ScopedData::new(db, spans.clone(), scoped.clone(), root_term);
    let intrinsics = IntrinsicStatics::from_existing(statics);
    let mut tycker = Tycker::from_assembled(db, data, &spans, &scoped, statics.clone(), intrinsics);
    let initializer = tycker.prepare_unit(root)?;
    tycker.strip_checker_state();
    Ok(CheckedUnit { initializer })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::tests::TestDb;
    use zydeco_foundation::arena::KeySpaceRegistry;
    use zydeco_surface::scoped::{ResolveFolder, ResolvedUnitOut, UnitResolveConfig};
    use zydeco_surface::textual::{StrictParser, syntax as t};

    fn resolve_unit(source: &str) -> (Arc<SpanArena>, ResolvedUnitOut) {
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(source, &mut parser).expect("test source parses");
        let (spans, textual) = parser.finish();
        let resolved = ResolveFolder::new(&spans, &textual)
            .run_unit(
                unit.root,
                UnitResolveConfig { literals: &[], has_prelude: false, is_signature: false },
            )
            .expect("test source resolves");
        (Arc::new(spans), resolved)
    }

    fn check_standalone(
        db: &dyn TyckDb, source: &str,
    ) -> (Arc<SpanArena>, Arc<ScopedArena>, su::TermId, CheckedUnitOutput) {
        let (spans, resolved) = resolve_unit(source);
        let scoped = Arc::new(resolved.arena.into_inner());
        let out = check_unit(
            db,
            UnitCheckInput {
                spans: &spans,
                scoped: &scoped,
                root: resolved.root,
                signature_dep: None,
                has_monadic: resolved.has_monadic,
                boundary_deps: &[],
                deps: &[],
            },
        );
        (spans, scoped, resolved.root, out)
    }

    fn assemble_two_chain(
        db: &dyn TyckDb, dep_src: &str, consumer_src: &str,
    ) -> SourceCheckOutcome {
        let (dep_spans, dep_scoped, dep_root, dep_out) = check_standalone(db, dep_src);
        let (main_spans, main_resolved) = resolve_unit(consumer_src);
        let main_scoped_base = Arc::new(main_resolved.arena.into_inner());
        let registry: KeySpaceRegistry<usize> = KeySpaceRegistry::default();
        let boundary_term = main_resolved.imports[0].1;
        let deps = [ImportedUnitDep {
            interface: &dep_out.interface,
            body: main_resolved.has_monadic.then_some(dep_out.body.as_ref()),
            dep_to_global: &[],
            synthetic_def_space: registry.intern(0),
        }];
        let main_out = check_unit(
            db,
            UnitCheckInput {
                spans: &main_spans,
                scoped: &main_scoped_base,
                root: main_resolved.root,
                signature_dep: None,
                has_monadic: main_resolved.has_monadic,
                boundary_deps: &[(boundary_term, 0)],
                deps: &deps,
            },
        );
        let mut merged_spans = SpanArena::default();
        merged_spans.merge_rebased_as(&dep_spans, zydeco_foundation::span::BytePos(1), None);
        merged_spans.merge_rebased_as(
            &main_spans,
            zydeco_foundation::span::BytePos(1 + dep_src.len() as u32),
            None,
        );
        let mut merged_scoped = ScopedArena::default();
        merged_scoped.merge_unit(&dep_scoped);
        merged_scoped.merge_unit(&main_scoped_base);
        merged_scoped.patch_term(boundary_term, su::SourceBoundary(dep_root).into());
        let units = [
            AssembledUnitInput {
                interface: &dep_out.interface,
                body: &dep_out.body,
                dep_to_global: &[],
            },
            AssembledUnitInput {
                interface: &main_out.interface,
                body: &main_out.body,
                dep_to_global: &[0],
            },
        ];
        assemble_program(
            db,
            Arc::new(merged_spans),
            Arc::new(merged_scoped),
            main_resolved.root,
            &units,
        )
        .outcome
    }

    const PREAMBLE: &str = "let VType = @(intrinsic(vtype)) in let CType = @(intrinsic(ctype)) in let Thk = @(intrinsic(thk)) in let Ret = @(intrinsic(ret)) in let Unit = @(intrinsic(unit)) in ";

    #[salsa::input]
    struct SeparateTestCase {
        id: u32,
    }

    #[salsa::tracked]
    fn run_separate_test(db: &dyn TyckDb, case: SeparateTestCase) {
        match case.id(db) {
            | 0 => verify_accepted_interface_digest_cuts_off_thunk_body_edits(db),
            | 1 => verify_rejected_interface_digest_distinguishes_distinct_failures(db),
            | 2 => verify_diamond_and_stub_assembly(db),
            | 3 => verify_monadic_unit_preloads_dependency_bodies(db),
            | _ => unreachable!(),
        }
    }

    #[test]
    fn accepted_interface_digest_cuts_off_thunk_body_edits() {
        let db = TestDb::default();
        let case = SeparateTestCase::new(&db, 0);
        run_separate_test(&db, case);
    }

    #[test]
    fn rejected_interface_digest_distinguishes_distinct_failures() {
        let db = TestDb::default();
        let case = SeparateTestCase::new(&db, 1);
        run_separate_test(&db, case);
    }

    #[test]
    fn diamond_and_stub_assembly_preserves_nominal_identity_and_rejects_mismatched_seal() {
        let db = TestDb::default();
        let case = SeparateTestCase::new(&db, 2);
        run_separate_test(&db, case);
    }

    #[test]
    fn monadic_unit_preloads_dependency_bodies() {
        let db = TestDb::default();
        let case = SeparateTestCase::new(&db, 3);
        run_separate_test(&db, case);
    }

    fn verify_accepted_interface_digest_cuts_off_thunk_body_edits(db: &dyn TyckDb) {
        let v1 = check_standalone(
            db,
            &format!("{PREAMBLE}let Flag = data | +Yes : Unit | +No : Unit end in let f : Thk (Flag -> Ret Flag) = {{ fn x => ret x }} in f"),
        )
        .3;
        let v2 = check_standalone(
            db,
            &format!("{PREAMBLE}let Flag = data | +Yes : Unit | +No : Unit end in let f : Thk (Flag -> Ret Flag) = {{ fn x => match x | +Yes(_) => ret +Yes(()) | +No(_) => ret +No(()) end }} in f"),
        )
        .3;
        let v3 = check_standalone(
            db,
            &format!("{PREAMBLE}let Flag = data | +Yes : Unit | +No : Unit end in let f : Thk (Flag -> Ret Unit) = {{ fn _ => ret () }} in f"),
        )
        .3;
        assert!(v1.interface.is_accepted());
        assert_eq!(v1.interface, v2.interface);
        assert_ne!(v1.interface, v3.interface);
    }

    fn verify_rejected_interface_digest_distinguishes_distinct_failures(db: &dyn TyckDb) {
        let err_a = check_standalone(db, &format!("{PREAMBLE}(() : VType)")).3;
        let err_b = check_standalone(
            db,
            &format!(
                "{PREAMBLE}let Flag = data | +Yes : Unit end in let (x : Flag) = () in ret ()"
            ),
        )
        .3;
        let ok = check_standalone(db, &format!("{PREAMBLE}ret ()")).3;
        assert!(!err_a.interface.is_accepted());
        assert!(!err_b.interface.is_accepted());
        assert_ne!(err_a.interface, ok.interface);
        assert_ne!(err_a.interface, err_b.interface);
    }

    fn verify_stub_assembly(db: &dyn TyckDb) {
        let prov_a_src = format!("{PREAMBLE}let mk : Thk (Ret Unit) = {{ ret () }} in mk");
        let res_ok = assemble_two_chain(
            db,
            &prov_a_src,
            "let f = @(import(\"p.zy\")) in do _ <- ! f; ret ()",
        );
        assert!(matches!(res_ok, SourceCheckOutcome::Checked(_)));

        let prov_bad_src = "()";
        let res_bad = assemble_two_chain(
            db,
            prov_bad_src,
            "let f = @(import(\"p.zy\")) in do _ <- ! f; ret ()",
        );
        assert!(matches!(res_bad, SourceCheckOutcome::Rejected(_)));
    }

    fn verify_diamond_assembly(db: &dyn TyckDb) {
        let prov_src = format!(
            "{PREAMBLE}begin def T : VType = data | +Tag : Unit end that let v : T = +Tag(()) in let use : Thk (T -> Ret Unit) = {{ fn _ => ret () }} in (#item = v, #use = use) end"
        );
        let pass_src = "let pkg = @(import(\"s.zy\")) in pkg";
        let (shared_spans, shared_scoped, shared_root, shared_out) =
            check_standalone(db, &prov_src);
        assert!(shared_out.interface.is_accepted());
        let (left_spans, left_res) = resolve_unit(pass_src);
        let (right_spans, right_res) = resolve_unit(pass_src);
        let left_scoped = Arc::new(left_res.arena.into_inner());
        let right_scoped = Arc::new(right_res.arena.into_inner());
        let registry: KeySpaceRegistry<usize> = KeySpaceRegistry::default();
        let shared_dep = [ImportedUnitDep {
            interface: &shared_out.interface,
            body: None,
            dep_to_global: &[],
            synthetic_def_space: registry.intern(0),
        }];
        let check_pass = |spans, scoped, root, boundary| {
            check_unit(
                db,
                UnitCheckInput {
                    spans,
                    scoped,
                    root,
                    signature_dep: None,
                    has_monadic: false,
                    boundary_deps: &[(boundary, 0)],
                    deps: &shared_dep,
                },
            )
        };
        let left_out = check_pass(&left_spans, &left_scoped, left_res.root, left_res.imports[0].1);
        let right_out =
            check_pass(&right_spans, &right_scoped, right_res.root, right_res.imports[0].1);
        assert!(left_out.interface.is_accepted());
        assert!(right_out.interface.is_accepted());

        let main_diamond = format!(
            "{PREAMBLE}let (/item = lv; /use = luse) = @(import(\"l.zy\")) in let (/item = rv; /use = _) = @(import(\"r.zy\")) in let _ = lv in ! luse rv"
        );
        let (main_spans, main_res) = resolve_unit(&main_diamond);
        let main_scoped = Arc::new(main_res.arena.into_inner());
        let diamond_deps = [
            ImportedUnitDep {
                interface: &shared_out.interface,
                body: None,
                dep_to_global: &[],
                synthetic_def_space: registry.intern(0),
            },
            ImportedUnitDep {
                interface: &left_out.interface,
                body: None,
                dep_to_global: &[0],
                synthetic_def_space: registry.intern(1),
            },
            ImportedUnitDep {
                interface: &right_out.interface,
                body: None,
                dep_to_global: &[0],
                synthetic_def_space: registry.intern(2),
            },
        ];
        let main_ok = check_unit(
            db,
            UnitCheckInput {
                spans: &main_spans,
                scoped: &main_scoped,
                root: main_res.root,
                signature_dep: None,
                has_monadic: false,
                boundary_deps: &[(main_res.imports[0].1, 1), (main_res.imports[1].1, 2)],
                deps: &diamond_deps,
            },
        );
        assert!(main_ok.interface.is_accepted(), "shared diamond nominal type must unify");

        let mut merged_spans = SpanArena::default();
        for s in [&shared_spans, &left_spans, &right_spans, &main_spans] {
            merged_spans.merge_rebased_as(s, zydeco_foundation::span::BytePos(1), None);
        }
        let mut merged_scoped = ScopedArena::default();
        merged_scoped.merge_unit(&shared_scoped);
        merged_scoped.merge_unit(&left_scoped);
        merged_scoped.merge_unit(&right_scoped);
        merged_scoped.merge_unit(&main_scoped);
        merged_scoped.patch_term(left_res.imports[0].1, su::SourceBoundary(shared_root).into());
        merged_scoped.patch_term(right_res.imports[0].1, su::SourceBoundary(shared_root).into());
        merged_scoped.patch_term(main_res.imports[0].1, su::SourceBoundary(left_res.root).into());
        merged_scoped.patch_term(main_res.imports[1].1, su::SourceBoundary(right_res.root).into());
        let assembled = assemble_program(
            db,
            Arc::new(merged_spans),
            Arc::new(merged_scoped),
            main_res.root,
            &[
                AssembledUnitInput {
                    interface: &shared_out.interface,
                    body: &shared_out.body,
                    dep_to_global: &[],
                },
                AssembledUnitInput {
                    interface: &left_out.interface,
                    body: &left_out.body,
                    dep_to_global: &[0],
                },
                AssembledUnitInput {
                    interface: &right_out.interface,
                    body: &right_out.body,
                    dep_to_global: &[0],
                },
                AssembledUnitInput {
                    interface: &main_ok.interface,
                    body: &main_ok.body,
                    dep_to_global: &[0, 1, 2],
                },
            ],
        );
        assert!(matches!(assembled.outcome, SourceCheckOutcome::Checked(_)));
    }

    fn verify_diamond_mismatch_rejected(db: &dyn TyckDb) {
        let prov_src = format!(
            "{PREAMBLE}begin def T : VType = data | +Tag : Unit end that let v : T = +Tag(()) in let use : Thk (T -> Ret Unit) = {{ fn _ => ret () }} in (#item = v, #use = use) end"
        );
        let pass_src = "let pkg = @(import(\"s.zy\")) in pkg";
        let (_, _, _, shared_out) = check_standalone(db, &prov_src);
        let (left_spans, left_res) = resolve_unit(pass_src);
        let left_scoped = Arc::new(left_res.arena.into_inner());
        let registry: KeySpaceRegistry<usize> = KeySpaceRegistry::default();
        let left_out = check_unit(
            db,
            UnitCheckInput {
                spans: &left_spans,
                scoped: &left_scoped,
                root: left_res.root,
                signature_dep: None,
                has_monadic: false,
                boundary_deps: &[(left_res.imports[0].1, 0)],
                deps: &[ImportedUnitDep {
                    interface: &shared_out.interface,
                    body: None,
                    dep_to_global: &[],
                    synthetic_def_space: registry.intern(0),
                }],
            },
        );
        let other_out = check_standalone(db, &prov_src).3;
        let main_diamond = format!(
            "{PREAMBLE}let (/item = lv; /use = luse) = @(import(\"l.zy\")) in let (/item = rv; /use = _) = @(import(\"r.zy\")) in let _ = lv in ! luse rv"
        );
        let (main_spans, main_res) = resolve_unit(&main_diamond);
        let main_scoped = Arc::new(main_res.arena.into_inner());
        let mismatch_deps = [
            ImportedUnitDep {
                interface: &shared_out.interface,
                body: None,
                dep_to_global: &[],
                synthetic_def_space: registry.intern(0),
            },
            ImportedUnitDep {
                interface: &left_out.interface,
                body: None,
                dep_to_global: &[0],
                synthetic_def_space: registry.intern(1),
            },
            ImportedUnitDep {
                interface: &other_out.interface,
                body: None,
                dep_to_global: &[],
                synthetic_def_space: registry.intern(3),
            },
        ];
        let main_bad = check_unit(
            db,
            UnitCheckInput {
                spans: &main_spans,
                scoped: &main_scoped,
                root: main_res.root,
                signature_dep: None,
                has_monadic: false,
                boundary_deps: &[(main_res.imports[0].1, 1), (main_res.imports[1].1, 2)],
                deps: &mismatch_deps,
            },
        );
        assert!(!main_bad.interface.is_accepted(), "distinct nominal units must mismatch");
    }

    fn verify_diamond_and_stub_assembly(db: &dyn TyckDb) {
        verify_stub_assembly(db);
        verify_diamond_assembly(db);
        verify_diamond_mismatch_rejected(db);
    }

    fn verify_monadic_unit_preloads_dependency_bodies(db: &dyn TyckDb) {
        let basis_src = format!(
            "{PREAMBLE}let Monad (M : VType -> CType) = codata | .return : forall (A : VType) . A -> M A | .bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B end in let Algebra (M : VType -> CType) (R : CType) = forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R in let M (A : VType) = Ret A in let monad : Thk (Monad M) = {{ comatch | .return => fn A x => ret x | .bind => fn A B m k => do x <- ! m; ! k x end }} in pack (= Monad as Monad) (= Algebra as Algebra) (= M as M) where (#monad = monad) end"
        );
        let (basis_spans, basis_scoped, basis_root, basis_out) = check_standalone(db, &basis_src);
        assert!(basis_out.interface.is_accepted());

        let consumer_src = format!(
            "{PREAMBLE}let (/Monad; /Algebra; /M; /monad) = @(import(\"m.zy\")) in (@[monadic] fn (x : Unit) => ret x) M monad ()"
        );
        let (c_spans, c_res) = resolve_unit(&consumer_src);
        assert!(c_res.has_monadic);
        let c_scoped = Arc::new(c_res.arena.into_inner());
        let registry: KeySpaceRegistry<usize> = KeySpaceRegistry::default();
        let c_out = check_unit(
            db,
            UnitCheckInput {
                spans: &c_spans,
                scoped: &c_scoped,
                root: c_res.root,
                signature_dep: None,
                has_monadic: true,
                boundary_deps: &[(c_res.imports[0].1, 0)],
                deps: &[ImportedUnitDep {
                    interface: &basis_out.interface,
                    body: Some(&basis_out.body),
                    dep_to_global: &[],
                    synthetic_def_space: registry.intern(0),
                }],
            },
        );
        assert!(c_out.interface.is_accepted(), "monadic check against dependency body must pass");

        let mut merged_spans = SpanArena::default();
        merged_spans.merge_rebased_as(&basis_spans, zydeco_foundation::span::BytePos(1), None);
        merged_spans.merge_rebased_as(
            &c_spans,
            zydeco_foundation::span::BytePos(1 + basis_src.len() as u32),
            None,
        );
        let mut merged_scoped = ScopedArena::default();
        merged_scoped.merge_unit(&basis_scoped);
        merged_scoped.merge_unit(&c_scoped);
        merged_scoped.patch_term(c_res.imports[0].1, su::SourceBoundary(basis_root).into());
        let assembled = assemble_program(
            db,
            Arc::new(merged_spans),
            Arc::new(merged_scoped),
            c_res.root,
            &[
                AssembledUnitInput {
                    interface: &basis_out.interface,
                    body: &basis_out.body,
                    dep_to_global: &[],
                },
                AssembledUnitInput {
                    interface: &c_out.interface,
                    body: &c_out.body,
                    dep_to_global: &[0],
                },
            ],
        );
        assert!(matches!(assembled.outcome, SourceCheckOutcome::Checked(_)));
    }
}
