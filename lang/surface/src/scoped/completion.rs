use super::{ResolveError, ResolveSourceOut, ResolvedUnitOut, Result, ScopeSnapshot, syntax::*};

/// The resolved cursor node and the lexical environment captured at that node.
pub struct CompletionSite {
    pub target: TermId,
    pub scope: ScopeSnapshot,
}

/// Tooling resolution keeps a captured scope even if another construct is rejected.
pub struct CompletionResolution {
    pub site: Option<CompletionSite>,
    pub diagnostics: Vec<ResolveError>,
    pub program: Result<ResolveSourceOut>,
}

/// Per-unit completion resolution retaining unresolved import/prelude boundary sites.
pub struct UnitCompletionResolution {
    pub site: Option<CompletionSite>,
    pub diagnostics: Vec<ResolveError>,
    pub unit: Result<ResolvedUnitOut>,
}

#[cfg(test)]
mod tests;
