//! Field search and selective package opening under deferred typing environments.

use super::*;
use crate::check::binding::Assign;
use crate::check::pattern::{
    CheckedPattern, PatternAction, PatternCheck, PatternSkolems, ValuePatternShape,
};

#[derive(Clone)]
pub(super) struct ValueFieldCandidate {
    route: Vec<ValueFieldStep>,
    pub(super) projected: ss::TypeId,
}

#[derive(Clone)]
struct DeferredValueFieldCandidate {
    route: Vec<DeferredValueFieldStep>,
    projected: DeferredEnvType,
}

#[derive(Clone)]
enum ValueFieldStep {
    Named { name: FieldName, whole: ss::TypeId },
    Product { product: ss::TypeId, components: Vec<ss::TypeId>, position: usize },
}

#[derive(Clone)]
enum DeferredValueFieldStep {
    Named { name: FieldName, whole: DeferredEnvType },
    Product { product: DeferredEnvType, position: usize },
}

/// One structural step of a field route that crosses at least one package telescope.
#[derive(Clone, PartialEq)]
enum PackageFieldStep {
    Product {
        position: usize,
    },
    Named {
        name: FieldName,
    },
    /// Open the telescope of the package at this node.
    Package,
}

/// The occurrence a package-crossing route selects.
#[derive(Clone, PartialEq)]
enum PackageFieldTerminal {
    /// A named value component reached through at least one package.
    Label { name: FieldName },
    /// The telescope entry at `index` of the package opened by the final step.
    Entry { index: usize },
}

/// A field-name occurrence found behind at least one package telescope.
#[derive(Clone)]
struct PackageFieldCandidate {
    steps: Vec<PackageFieldStep>,
    terminal: PackageFieldTerminal,
    /// The route crosses an abstract witness, so only a projection pattern,
    /// which opens the package, may select through it.
    sealed: bool,
}

/// A type together with the environment operation still owed by structural field lookup.
///
/// Applying the environment eagerly at every label and product node recursively copied the
/// entire remaining telescope at each depth. This closure distributes one operation through
/// stable structure without allocating, then materializes only the unique projection route.
#[derive(Clone)]
pub(super) struct DeferredEnvType {
    pub(super) root: ss::TypeId,
    pub(super) environment: ss::TyEnv,
    pending: Option<DeferredEnvOperation>,
}

/// Whether the pending environment application starts a search step or was inherited from an
/// already inspected parent. Descendants must not unroll their payload merely because the parent
/// substituted through it: doing so changes sealed projected types into their definitions.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum DeferredEnvOperation {
    UnrollThenSubstitute,
    Substitute,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct DeferredEnvMaterializationKey {
    root: ss::TypeId,
    environment: ss::TyEnv,
    operation: DeferredEnvOperation,
}

/// Cross-operation field-search results, valid only while mutable type state is unchanged.
#[derive(Default)]
pub(super) struct DeferredEnvMaterializationCache {
    entries: std::collections::HashMap<DeferredEnvMaterializationKey, ss::TypeId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct DeferredEnvIdentity {
    root: ss::TypeId,
    pending: Option<DeferredEnvOperation>,
}

enum DeferredEnvTypeView {
    Label { name: FieldName, whole: DeferredEnvType, projected: DeferredEnvType },
    Product { whole: DeferredEnvType, components: Vec<DeferredEnvType> },
    Package { whole: DeferredEnvType, head: DeferredPackageHead },
    Other(DeferredEnvType),
}

/// The head of a package telescope: one existential or manifest-kind entry.
#[derive(Clone)]
enum DeferredPackageHead {
    ManifestKind(ss::ManifestKind),
    Exists(ss::Exists),
}

#[derive(Clone)]
struct TypeFieldStep {
    name: FieldName,
    whole: ss::KindId,
    projected: ss::KindId,
}

#[derive(Clone)]
pub(super) struct TypeFieldCandidate {
    path: Vec<TypeFieldStep>,
    pub(super) projected: ss::KindId,
}

/// Structural lookup shared by named projections.
///
/// Labels are transparent search nodes. Products contribute runtime path
/// steps, and every other type constructor is an opacity boundary.
pub(super) struct FieldProjectionResolver;

#[derive(Clone)]
pub(super) enum ExistentialProjectionMember {
    Project { source: su::PatId, field: FieldName, payload: su::PatId },
    Whole(su::PatId),
}

/// One selection whose route crosses at least one nested package telescope.
struct PackageBodySelection {
    source: su::PatId,
    payload: su::PatId,
    candidate: PackageFieldCandidate,
}

enum ExistentialProjectionSlot {
    Kind {
        field: Option<FieldName>,
        definition: ss::KindId,
        pattern: ss::KPatId,
    },
    Type {
        field: Option<FieldName>,
        source_pattern: ss::TPatId,
        payload_kind: ss::KindId,
        payload: ss::TypeId,
        /// The payload wrapped in the binder's named-pattern structure.
        /// Whole-package aliases need this form when instantiating a package arrow.
        full_payload: ss::TypeId,
        pattern: ss::TPatId,
        skolem: Option<ss::AbstId>,
    },
}

struct ExistentialProjectionOpening {
    expected: ss::TypeId,
    body: ss::TypeId,
    slots: Vec<ExistentialProjectionSlot>,
    env: ss::TyEnv,
    opened: Vec<ss::AbstId>,
}

/// A package telescope body with its composed substitutions still pending.
///
/// Typing environments only grow while a telescope is opened, so the newest snapshot subsumes
/// every earlier one. Abstract assignments remain ordered because later payloads may depend on
/// earlier witnesses. Descending clears only the outer `unroll`; the substitutions stay shared.
#[derive(Clone)]
pub(super) struct DeferredTelescopeType {
    pub(super) root: ss::TypeId,
    environment: Option<DeferredTelescopeEnvironment>,
    abstracts: rpds::VectorSync<DeferredAbstractAssignment>,
}

#[derive(Clone)]
struct DeferredTelescopeEnvironment {
    value: ss::TyEnv,
    unroll: bool,
}

#[derive(Clone, Copy)]
struct DeferredAbstractAssignment {
    witness: ss::AbstId,
    payload: ss::TypeId,
}

pub(super) enum DeferredTelescopeExistsMode {
    Abstract,
    Manifest(DeferredTelescopeType),
}

pub(super) enum DeferredTelescopeView {
    ManifestKind {
        binder: ss::KPatId,
        definition: ss::KindId,
        body: DeferredTelescopeType,
    },
    Exists {
        binder: ss::TypeBinder,
        mode: DeferredTelescopeExistsMode,
        body: DeferredTelescopeType,
    },
    Other(DeferredTelescopeType),
}

/// Select fields from one package without spelling its static telescope.
///
/// Manifest kind and type entries are substituted transparently, and every
/// abstract witness is opened once under an anonymous or canonical skolem.
/// Selected static fields bind source names to those entries, while selected
/// value fields become ordinary projection patterns over the instantiated
/// package body. Whole-value alias members retain the opened witnesses so the
/// package can be forwarded without reconstructing its telescope.
pub(super) struct ExistentialProjectionPattern;

mod deferred;
mod field;
mod package;
mod telescope;

#[cfg(test)]
mod tests;
