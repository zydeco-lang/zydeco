//! Coarse source and completion checking queries.

use super::*;

/// Take the pending resolved program out of the slot and intern it as a
/// tracked struct, inside the query graph where tracked-struct creation is
/// legal.
#[salsa::tracked(returns(copy))]
pub fn intern_pending<'db>(db: &'db dyn TyckDb) -> ScopedData<'db> {
    let parts = db
        .pending_parts()
        .lock()
        .expect("pending check slot poisoned")
        .take()
        .expect("pending check slot is empty");
    let parts = match std::sync::Arc::try_unwrap(parts) {
        | Ok(parts) => parts,
        | Err(_) => panic!("pending parts are still shared"),
    };
    ScopedData::new(
        db,
        std::sync::Arc::new(parts.spans),
        std::sync::Arc::new(parts.scoped),
        parts.root,
    )
}

/// The complete result of checking one source snapshot.
#[derive(Clone, Debug)]
pub struct TyckOutput {
    /// The immutable name-resolved arena used during checking.
    pub scoped: std::sync::Arc<su::ScopedArena>,
    /// The recoverable checking outcome.
    pub outcome: SourceCheckOutcome,
}

/// Exact resolved cursor identity and definitions to compare in one disposable check.
#[salsa::tracked]
pub struct CompletionInput<'db> {
    #[tracked]
    #[returns(copy)]
    pub target: su::TermId,
    #[tracked]
    #[returns(ref)]
    pub definitions: Vec<su::DefId>,
}

/// Completion evidence and the source arenas owning every annotation it mentions.
#[derive(Clone, Debug)]
pub struct CompletionTyckOutput {
    pub source: TyckOutput,
    pub typing: CompletionTyping,
}

// The outcome owns its arenas and reports and contains no database-tied references.
// The non-Update escape hatch stays until the judgment layer gains structural equality.
//

// `lru = 1` keeps only the most recently used arena memo: the full typed arena of one
// root is hundreds of megabytes, so the database forgets earlier roots whenever the
// session triggers LRU eviction between analyses. Judgments stay memoized separately.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub fn check_source<'db>(db: &'db dyn TyckDb, data: ScopedData<'db>) -> TyckOutput {
    SourceCheckRequest { data, completion: None }.run(db).0
}

/// Compile an explicit boundary without changing the ordinary source-import query.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub fn check_library<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, contract: zydeco_surface::metadata::LibraryContract,
) -> Result<crate::CheckedLibrary, crate::LibraryCheckError> {
    let mut tycker = Tycker::new(db, data, data.spans(db), data.scoped(db));
    let root = tycker.run_judgments(data.root(db));
    tycker.finish_judgments();
    let root = root
        .and_then(|root| tycker.finish_check().map(|()| root))
        .map_err(|_| crate::LibraryCheckError::Checking(tycker.error_diagnostics()))?;
    let exports = tycker.prepare_library(root, &contract)?;
    tycker.strip_checker_state();
    Ok(crate::CheckedLibrary { exports })
}

/// Prepare one native initializer without replacing ordinary source import semantics.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub fn check_unit<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>,
) -> Result<crate::CheckedUnit, crate::LibraryCheckError> {
    let mut tycker = Tycker::new(db, data, data.spans(db), data.scoped(db));
    let root = tycker.run_judgments(data.root(db));
    tycker.finish_judgments();
    let root = root
        .and_then(|root| tycker.finish_check().map(|()| root))
        .map_err(|_| crate::LibraryCheckError::Checking(tycker.error_diagnostics()))?;
    let initializer = tycker.prepare_unit(root)?;
    tycker.strip_checker_state();
    Ok(crate::CheckedUnit { initializer })
}

/// Check current recovered syntax and compare names before transient environments are released.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values), lru = 1)]
pub fn check_completion<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, completion: CompletionInput<'db>,
) -> CompletionTyckOutput {
    let (source, typing) = SourceCheckRequest { data, completion: Some(completion) }.run(db);
    CompletionTyckOutput { source, typing: typing.expect("a completion request captures typing") }
}

struct SourceCheckRequest<'db> {
    data: ScopedData<'db>,
    completion: Option<CompletionInput<'db>>,
}

impl<'db> SourceCheckRequest<'db> {
    fn run(self, db: &'db dyn TyckDb) -> (TyckOutput, Option<CompletionTyping>) {
        // One checker runs the whole pipeline within a single query. Splitting the
        // phases into separate salsa queries required deep-copying the full statics
        // arena across every boundary, which dominated check memory.
        let data = self.data;
        let scoped = std::sync::Arc::clone(data.scoped(db));
        let mut tycker = Tycker::new(db, data, data.spans(db), &scoped);
        if let Some(completion) = self.completion {
            tycker.set_completion_target(completion.target(db));
        }
        let root = tycker.run_judgments(data.root(db)).ok();
        tycker.finish_judgments();
        tycker.resolve_holes_and_collect();
        let result = match root {
            | None => Err(ReportedError),
            | Some(root) => tycker
                .normalize_and_validate()
                .and_then(|()| tycker.elaborate_static_root(root))
                .map(|()| root),
        };
        let diagnostics = result.is_err().then(|| tycker.error_diagnostics());
        let completion =
            self.completion.and_then(|request| tycker.completion_typing(request.definitions(db)));
        tycker.strip_checker_state();
        let statics = std::sync::Arc::new(tycker.statics);
        let outcome = match result {
            | Ok(root) => SourceCheckOutcome::Checked(CheckedSource {
                statics,
                root,
                observations: tycker.observations,
            }),
            | Err(ReportedError) => SourceCheckOutcome::Rejected(RejectedSource {
                statics,
                diagnostics: diagnostics.unwrap(),
                observations: tycker.observations,
            }),
        };
        (TyckOutput { scoped, outcome }, completion)
    }
}
