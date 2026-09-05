//! Second-class occurrence validation for value functions.
//!
//! A `val pi` classifier may only classify a definition and only be consumed
//! by application (or by a view pattern, which is application in pattern
//! position). Every other occurrence would materialize the function as a
//! runtime value, so the checker rejects the positions enumerated here:
//! stored components, constructor and package payloads, computation
//! arguments and returns, and the storing type positions that admit them.
//! The rule and its unfolding semantics are specified in
//! `docs/proposals/value-pi.md`.

use crate::{arena::StaticsArena, syntax::*};
use std::fmt;
use zydeco_utils::arena::ArenaAccess;

/// One way a value function or its classifier reached a first-class position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FirstClassPosition {
    /// A component of a product value.
    ProductComponent,
    /// The payload of a data constructor.
    ConstructorPayload,
    /// The runtime body of an existential package.
    PackagePayload,
    /// The payload of a named component.
    NamedPayload,
    /// The payload type of a data or codata declaration arm.
    ConstructorTypePayload,
    /// An argument supplied to a computation function.
    ComputationArgument,
    /// The value returned by a computation.
    ReturnedValue,
    /// The scrutinee of a data match.
    MatchScrutinee,
    /// A component of a product type.
    ProductTypeComponent,
    /// The domain of a computation arrow.
    ArrowDomain,
    /// The runtime domain of another value function.
    ValueFunctionDomain,
    /// The body of an existential package type.
    PackageTypeBody,
    /// The payload of a named classifier.
    NamedTypePayload,
    /// The payload of a returning computation type.
    ReturnType,
    /// The domain of a package-dependent arrow.
    PackageArrowDomain,
}

impl fmt::Display for FirstClassPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::ProductComponent => "stored in a product",
            | Self::ConstructorPayload => "stored in a constructor payload",
            | Self::PackagePayload => "stored in a package",
            | Self::NamedPayload => "wrapped in a named component",
            | Self::ConstructorTypePayload => "used as a constructor payload type",
            | Self::ComputationArgument => "passed as an argument to a computation",
            | Self::ReturnedValue => "returned by a computation",
            | Self::MatchScrutinee => "used as a match scrutinee",
            | Self::ProductTypeComponent => "used as a product component type",
            | Self::ArrowDomain => "used as the domain of a computation arrow",
            | Self::ValueFunctionDomain => "used as the domain of another value function",
            | Self::PackageTypeBody => "used as an existential package body",
            | Self::NamedTypePayload => "used as a named payload type",
            | Self::ReturnType => "used as the payload of a returning computation type",
            | Self::PackageArrowDomain => "used as the domain of a package-dependent arrow",
        })
    }
}

/// A post-check rejection of one first-class value-function occurrence.
#[derive(Clone, Debug)]
pub enum ValueFunctionError {
    /// A value function flowing into a runtime-materialized term position.
    FirstClassValue { value: ValueId, position: FirstClassPosition },
    /// A `val pi` classifier in a type position that stores its inhabitants.
    FirstClassType { ty: TypeId, position: FirstClassPosition },
}

impl ValueFunctionError {
    /// The typed node whose source span blames the occurrence.
    pub fn term(&self) -> TermId {
        match self {
            | Self::FirstClassValue { value, .. } => (*value).into(),
            | Self::FirstClassType { ty, .. } => (*ty).into(),
        }
    }
}

/// Post-check validator confining value functions to second-class uses.
pub struct ValueFunctionChecker<'a> {
    statics: &'a StaticsArena,
}

impl<'a> ValueFunctionChecker<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    /// Reject every value function or `val pi` classifier that occupies a
    /// first-class position in the finished arena.
    pub fn validate(&self) -> Vec<ValueFunctionError> {
        self.value_uses().chain(self.computation_uses()).chain(self.type_uses()).collect()
    }

    fn is_value_function_type(&self, ty: TypeId) -> bool {
        matches!(self.statics.normalized_at(ty), Some(Type::ValPi(_)))
    }

    fn value_position(
        &self, value: ValueId, position: FirstClassPosition,
    ) -> Option<ValueFunctionError> {
        let ty = self.statics.annotations_value.get(&value).copied()?;
        self.is_value_function_type(ty)
            .then_some(ValueFunctionError::FirstClassValue { value, position })
    }

    fn type_position(
        &self, ty: TypeId, position: FirstClassPosition,
    ) -> Option<ValueFunctionError> {
        self.is_value_function_type(ty)
            .then_some(ValueFunctionError::FirstClassType { ty, position })
    }

    /// Storage positions among allocated values.
    fn value_uses(&self) -> impl Iterator<Item = ValueFunctionError> + '_ {
        self.statics.values.iter().flat_map(|(_value, node)| {
            match node {
                | Value::VCons(components) => components
                    .iter()
                    .filter_map(|component| {
                        self.value_position(*component, FirstClassPosition::ProductComponent)
                    })
                    .collect(),
                | Value::Ctor(Ctor(_, payload)) => self
                    .value_position(*payload, FirstClassPosition::ConstructorPayload)
                    .into_iter()
                    .collect(),
                | Value::SCons(ConsN(_, tail)) => self
                    .value_position(*tail, FirstClassPosition::PackagePayload)
                    .into_iter()
                    .collect(),
                | Value::Named(Named(_, inner)) => self
                    .value_position(*inner, FirstClassPosition::NamedPayload)
                    .into_iter()
                    .collect(),
                | Value::Hole(_)
                | Value::Var(_)
                | Value::Let(_)
                | Value::ValAbs(_)
                | Value::ValApp(_)
                | Value::Thunk(_)
                | Value::Triv(_)
                | Value::Proj(_)
                | Value::Lit(_) => Vec::new(),
            }
            .into_iter()
        })
    }

    /// Storage positions among allocated computations.
    fn computation_uses(&self) -> impl Iterator<Item = ValueFunctionError> + '_ {
        self.statics.compus.iter().flat_map(|(_compu, node)| match node {
            | Computation::VApp(App(_, argument)) => self
                .value_position(*argument, FirstClassPosition::ComputationArgument)
                .into_iter()
                .collect(),
            | Computation::Ret(Return(value)) => {
                self.value_position(*value, FirstClassPosition::ReturnedValue).into_iter().collect()
            }
            | Computation::Match(Match { scrut, .. }) => self
                .value_position(*scrut, FirstClassPosition::MatchScrutinee)
                .into_iter()
                .collect(),
            | Computation::Hole(_)
            | Computation::VAbs(_)
            | Computation::TAbs(_)
            | Computation::TApp(_)
            | Computation::Fix(_)
            | Computation::Force(_)
            | Computation::Do(_)
            | Computation::Let(_)
            | Computation::CoMatch(_)
            | Computation::Dtor(_) => Vec::new(),
        })
    }

    /// Storage positions among allocated types.
    fn type_uses(&self) -> impl Iterator<Item = ValueFunctionError> + '_ {
        self.declaration_uses().chain(self.statics.types_pre.iter().filter_map(|(_ty, cell)| {
            let node = match cell {
                | Fillable::Done(node) => node,
                | Fillable::Fill(_) => return None,
            };
            match node {
                | Type::Prod(Prod(components)) => components.iter().find_map(|component| {
                    self.type_position(*component, FirstClassPosition::ProductTypeComponent)
                }),
                | Type::Arrow(Arrow(domain, _)) => {
                    self.type_position(*domain, FirstClassPosition::ArrowDomain)
                }
                | Type::ValPi(inner) => match inner.binder {
                    | ValPiBinder::Value(ValueParameter { domain, .. }) => {
                        self.type_position(domain, FirstClassPosition::ValueFunctionDomain)
                    }
                    | ValPiBinder::Type(_) => None,
                },
                | Type::Exists(inner) => {
                    self.type_position(inner.body, FirstClassPosition::PackageTypeBody)
                }
                | Type::Named(Named(_, payload)) | Type::Label(Label(_, payload)) => {
                    self.type_position(*payload, FirstClassPosition::NamedTypePayload)
                }
                | Type::PackPi(inner) => {
                    self.type_position(inner.domain, FirstClassPosition::PackageArrowDomain)
                }
                | Type::App(App(function, argument)) => {
                    let returns_value =
                        matches!(self.statics.normalized_at(*function), Some(Type::Ret(_)));
                    returns_value
                        .then(|| self.type_position(*argument, FirstClassPosition::ReturnType))
                        .flatten()
                }
                | Type::Var(_)
                | Type::Abst(_)
                | Type::Abs(_)
                | Type::Proj(_)
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_)
                | Type::Forall(_)
                | Type::ManifestKind(_)
                | Type::Data(_)
                | Type::CoData(_) => None,
            }
        }))
    }

    /// Storage positions among data and codata declaration payloads, which
    /// side tables attach to their definitions rather than to type nodes.
    fn declaration_uses(&self) -> impl Iterator<Item = ValueFunctionError> + '_ {
        let payloads =
            self.statics.datas.iter().flat_map(|(_, node)| node.iter().map(|(_, ty)| *ty)).chain(
                self.statics.codatas.iter().flat_map(|(_, node)| node.iter().map(|(_, ty)| *ty)),
            );
        payloads.filter_map(|ty| self.type_position(ty, FirstClassPosition::ConstructorTypePayload))
    }
}
