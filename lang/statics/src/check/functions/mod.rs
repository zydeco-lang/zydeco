//! Value and computation function classifiers, package binders, and witness instantiation.

use super::*;
use crate::check::binding::Assign;
use crate::check::judgment::{Action, Switch, Tyck};
use crate::check::pattern::CheckedPatternExt;
use crate::check::pattern::{CheckedPattern, PatternAction, PatternSkolems, ValuePatternShape};
use crate::check::projection::ExistentialProjectionPattern;

/// Reify the package-opening structure of a checked value pattern without
/// retaining source binders in its classifier.
pub(super) struct PackageWitnessProjectionBuilder;

// Todo: use async to cut all tycker functions into small segments (returning futures)
// and achieve better concurrency

// Todo: use hole solution to implement the confluence checker (well-formedness checker)

/// Formation input for a computation function type with an elaborated
/// value-pattern domain.
pub(super) struct ComputationPiFormation {
    pub(super) binder: CheckedPattern,
    pub(super) codomain: su::TermId,
}

/// Formation input for a total value-function classifier.
pub(super) struct ValuePiFormation {
    pub(super) binder: CheckedPattern,
    pub(super) codomain: su::TermId,
}

/// Instantiate the bound witnesses of a package-dependent arrow.
struct PackPiInstantiation {
    signature: PackageSignature,
    witnesses: Vec<ss::StaticTermId>,
}

/// Instantiate package witnesses selected from a structured value-function
/// argument rather than assuming that the whole argument is one package.
pub(super) struct ValuePiInstantiation {
    pub(super) signature: PackageSignature,
    pub(super) projection: ss::PackageWitnessProjection,
    pub(super) argument: ss::ValueId,
}

struct ProjectedPackageArgument {
    domain: ss::TypeId,
    witnesses: Vec<ss::StaticTermId>,
    abstracts: usize,
}

/// The static information carried by a package-dependent computation arrow.
#[derive(Clone)]
pub(super) struct PackageSignature {
    pub(super) domain: ss::TypeId,
    pub(super) witnesses: ss::PackTelescope,
    pub(super) codomain: ss::TypeId,
}

/// State for recursively instantiating a `PackPi` through the physical
/// existential prefix of its package argument.
struct PackPiInstantiationState<'a> {
    domain: ss::TypeId,
    codomain: ss::TypeId,
    canonical: &'a [ss::AbstId],
    actual: &'a [ss::StaticTermId],
    expected: usize,
    found: usize,
}

/// Check a computation abstraction over a package value against a
/// package-dependent computation arrow.
pub(super) struct PackPiIntroduction {
    pub(super) binder: su::PatId,
    pub(super) body: su::TermId,
    pub(super) signature: ss::PackPi,
}

/// Associate a package pattern's leading type components with a `PackPi`
/// telescope.
pub(super) struct PackPiPatternSkolems {
    pub(super) pattern: su::PatId,
    pub(super) signature: PackageSignature,
}

/// Associate package openings inside a structured value-function parameter
/// with the canonical witnesses stored by its `ValPi` classifier.
pub(super) struct ValuePiPatternSkolems {
    pub(super) pattern: su::PatId,
    pub(super) signature: PackageSignature,
    pub(super) projection: ss::PackageWitnessProjection,
}

/// Associate each abstract witness in a package domain with the canonical
/// identity stored by its package-dependent arrow.
struct PackPiWitnessSkolems<'a> {
    witnesses: &'a [ss::AbstId],
    domain: ss::TypeId,
    expected: usize,
}

/// Traverse a package pattern and its domain in lockstep until every abstract
/// witness in a `PackPi` has a corresponding pattern component.
struct PackPiPatternAssignments<'a> {
    items: &'a [su::PatId],
    witnesses: &'a [ss::AbstId],
    domain: ss::TypeId,
    expected: usize,
    found: usize,
}

/// Apply a package-dependent function to a package with manifest witnesses.
pub(super) struct PackPiElimination {
    pub(super) function: ss::CompuId,
    pub(super) argument: su::TermId,
    pub(super) signature: ss::PackPi,
}

mod application;
mod formation;
mod witness;
