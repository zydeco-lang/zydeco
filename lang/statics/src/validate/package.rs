//! Second-class occurrence validation for packages.
//!
//! An existential package may be built by `pack`, bound by a definition or an
//! opening pattern, nested in a product or another package, and applied to a
//! package-dependent or value arrow whose pattern opens it. Every one of those
//! consumers is resolved statically, so the package never needs a runtime
//! representation of its own. The remaining positions would store the package
//! as a first-class value, and the checker rejects them here: constructor and
//! named payloads, computation arguments and returns under plain arrows, and
//! the storing type positions that admit them. The escape hatch for dynamic
//! needs is a product of thunks. The rule is specified in
//! `docs/proposals/package-modularization.md`.

use crate::{arena::StaticsArena, syntax::*};
use std::fmt;
use zydeco_utils::arena::ArenaAccess;

/// One way a package or its classifier reached a first-class position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PackagePosition {
    /// The payload of a data constructor.
    ConstructorPayload,
    /// The payload type of a data or codata declaration arm.
    ConstructorTypePayload,
    /// An argument supplied to a computation under a plain arrow.
    ComputationArgument,
    /// The value returned by a computation.
    ReturnedValue,
    /// The domain of a computation arrow.
    ArrowDomain,
    /// The payload of a returning computation type.
    ReturnType,
}

impl fmt::Display for PackagePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::ConstructorPayload => "stored in a constructor payload",
            | Self::ConstructorTypePayload => "used as a constructor payload type",
            | Self::ComputationArgument => "passed as an argument to a computation",
            | Self::ReturnedValue => "returned by a computation",
            | Self::ArrowDomain => "used as the domain of a computation arrow",
            | Self::ReturnType => "used as the payload of a returning computation type",
        })
    }
}

/// A post-check rejection of one first-class package occurrence.
#[derive(Clone, Debug)]
pub enum PackageError {
    /// A package flowing into a runtime-materialized term position.
    FirstClassValue { value: ValueId, position: PackagePosition },
    /// An `exists` classifier in a type position that stores its inhabitants.
    FirstClassType { ty: TypeId, position: PackagePosition },
}

impl PackageError {
    /// The typed node whose source span blames the occurrence.
    pub fn term(&self) -> TermId {
        match self {
            | Self::FirstClassValue { value, .. } => (*value).into(),
            | Self::FirstClassType { ty, .. } => (*ty).into(),
        }
    }
}

/// Post-check validator confining packages to second-class uses.
pub struct PackageChecker<'a> {
    statics: &'a StaticsArena,
}

impl<'a> PackageChecker<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    /// Reject every package or `exists` classifier that occupies a first-class
    /// position in the finished arena.
    pub fn validate(&self) -> Vec<PackageError> {
        self.value_uses().chain(self.computation_uses()).chain(self.type_uses()).collect()
    }

    fn is_package_type(&self, ty: TypeId) -> bool {
        matches!(self.statics.normalized_at(ty), Some(Type::Exists(_)))
    }

    fn value_position(&self, value: ValueId, position: PackagePosition) -> Option<PackageError> {
        let ty = self.statics.annotations_value.get(&value).copied()?;
        self.is_package_type(ty).then_some(PackageError::FirstClassValue { value, position })
    }

    fn type_position(&self, ty: TypeId, position: PackagePosition) -> Option<PackageError> {
        self.is_package_type(ty).then_some(PackageError::FirstClassType { ty, position })
    }

    /// Storage positions among allocated values. Product, package, and named
    /// component bodies stay legal: nesting packages there is the module
    /// language, and the consumer of each nested component is still a static
    /// projection.
    fn value_uses(&self) -> impl Iterator<Item = PackageError> + '_ {
        self.statics.values.iter().flat_map(|(_value, node)| {
            match node {
                | Value::Ctor(Ctor(_, payload)) => self
                    .value_position(*payload, PackagePosition::ConstructorPayload)
                    .into_iter()
                    .collect(),
                | Value::Hole(_)
                | Value::Var(_)
                | Value::Named(_)
                | Value::Let(_)
                | Value::ValAbs(_)
                | Value::ValApp(_)
                | Value::Thunk(_)
                | Value::Triv(_)
                | Value::VCons(_)
                | Value::SCons(_)
                | Value::Proj(_)
                | Value::Lit(_) => Vec::new(),
            }
            .into_iter()
        })
    }

    /// Storage positions among allocated computations. A package-dependent
    /// arrow opens its argument's witnesses at each application, so applying
    /// one stays second-class; only a plain computation arrow stores the
    /// package it receives. A match on a package is also an opening: coverage
    /// treats packages as a single-constructor head space, so every scrutinee
    /// of package type is destructured by an exhaustive arm.
    fn computation_uses(&self) -> impl Iterator<Item = PackageError> + '_ {
        self.statics.compus.iter().flat_map(|(_compu, node)| match node {
            | Computation::VApp(App(function, argument)) => self
                .stores_argument(*function)
                .then(|| self.value_position(*argument, PackagePosition::ComputationArgument))
                .flatten()
                .into_iter()
                .collect(),
            | Computation::Ret(Return(value)) => {
                self.value_position(*value, PackagePosition::ReturnedValue).into_iter().collect()
            }
            | Computation::Hole(_)
            | Computation::VAbs(_)
            | Computation::TAbs(_)
            | Computation::TApp(_)
            | Computation::Fix(_)
            | Computation::Force(_)
            | Computation::Do(_)
            | Computation::Let(_)
            | Computation::Match(_)
            | Computation::CoMatch(_)
            | Computation::Dtor(_) => Vec::new(),
        })
    }

    /// Whether the function side of an application is a plain computation
    /// arrow, as opposed to a package-dependent arrow that opens its argument.
    fn stores_argument(&self, function: CompuId) -> bool {
        self.statics
            .annotations_compu
            .get(&function)
            .and_then(|&ty| self.statics.normalized_at(ty))
            .is_some_and(|node| matches!(node, Type::Arrow(_)))
    }

    /// Storage positions among allocated types. Products, package bodies, and
    /// the domains of value and package-dependent arrows stay legal: they are
    /// the module language of nested signatures and functors.
    fn type_uses(&self) -> impl Iterator<Item = PackageError> + '_ {
        self.declaration_uses().chain(self.statics.types_pre.iter().filter_map(|(_ty, cell)| {
            let node = match cell {
                | Fillable::Done(node) => node,
                | Fillable::Fill(_) => return None,
            };
            match node {
                | Type::Arrow(Arrow(domain, _)) => {
                    self.type_position(*domain, PackagePosition::ArrowDomain)
                }
                | Type::App(App(function, argument)) => {
                    let returns_value =
                        matches!(self.statics.normalized_at(*function), Some(Type::Ret(_)));
                    returns_value
                        .then(|| self.type_position(*argument, PackagePosition::ReturnType))
                        .flatten()
                }
                | _ => None,
            }
        }))
    }

    /// Storage positions among data and codata declaration payloads, which
    /// side tables attach to their definitions rather than to type nodes.
    fn declaration_uses(&self) -> impl Iterator<Item = PackageError> + '_ {
        let payloads =
            self.statics.datas.iter().flat_map(|(_, node)| node.iter().map(|(_, ty)| *ty)).chain(
                self.statics.codatas.iter().flat_map(|(_, node)| node.iter().map(|(_, ty)| *ty)),
            );
        payloads.filter_map(|ty| self.type_position(ty, PackagePosition::ConstructorTypePayload))
    }
}
