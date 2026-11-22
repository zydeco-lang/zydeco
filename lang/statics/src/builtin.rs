use super::{
    arena::StaticsArena,
    syntax::{self as ss, Fillable},
};
use std::{
    collections::{BTreeMap, HashSet},
    fmt::{Display, Formatter},
};
use thiserror::Error;
pub use zydeco_abi::{
    BuiltinComputationClassifier, BuiltinOperationAbi, BuiltinValueAtom, BuiltinValueClassifier,
};
use zydeco_foundation::arena::ArenaAccess;
use zydeco_syntax::{BuiltinRole, BuiltinTypeRole, BuiltinValueRole, Named, Prod};

pub use zydeco_ir::builtin::{BuiltinPackagePlan, BuiltinPackageValue};

/// One Builtin role assigned to more than one entry of a package signature.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DuplicateBuiltinRole {
    Type { role: BuiltinTypeRole, witnesses: Vec<ss::AbstId> },
    Value { role: BuiltinValueRole, entries: Vec<ss::TypeId> },
}

impl Display for DuplicateBuiltinRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Type { role, witnesses } => {
                write!(f, "type role `{role}` on {} abstract entries", witnesses.len())
            }
            | Self::Value { role, entries } => {
                write!(f, "operation role `{role}` on {} value entries", entries.len())
            }
        }
    }
}

/// A host operation whose classifier does not implement its assigned role.
#[derive(Clone, Debug, PartialEq, Eq, Error)]
pub enum BuiltinClassifierError {
    #[error("operation role `{role}` requires classifier `{expected}`")]
    Mismatch { role: BuiltinValueRole, entry: ss::TypeId, expected: BuiltinValueClassifier },
}

/// A package signature whose host-role interpretation is ambiguous.
#[derive(Clone, Debug, PartialEq, Eq, Error)]
pub struct BuiltinSignatureError {
    pub duplicates: Vec<DuplicateBuiltinRole>,
    pub classifiers: Vec<BuiltinClassifierError>,
}

impl Display for BuiltinSignatureError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let errors = self
            .duplicates
            .iter()
            .map(|duplicate| format!("repeats {duplicate}"))
            .chain(self.classifiers.iter().map(ToString::to_string))
            .collect::<Vec<_>>()
            .join("; ");
        write!(f, "Builtin signature {errors}")
    }
}

/// Checks package-local uniqueness of typed Builtin roles.
pub struct BuiltinSignatureValidator<'a> {
    statics: &'a StaticsArena,
    visited: HashSet<ss::TypeId>,
    values: BTreeMap<BuiltinValueRole, Vec<ss::TypeId>>,
}

impl<'a> BuiltinSignatureValidator<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics, visited: HashSet::new(), values: BTreeMap::new() }
    }

    pub fn validate(mut self, signature: &ss::PackPi) -> Result<(), BuiltinSignatureError> {
        self.validate_parts(signature.domain, signature.witnesses.iter().copied())
    }

    fn validate_parts(
        &mut self, domain: ss::TypeId, witnesses: impl Iterator<Item = ss::AbstId>,
    ) -> Result<(), BuiltinSignatureError> {
        let types = witnesses.fold(
            BTreeMap::<BuiltinTypeRole, Vec<ss::AbstId>>::new(),
            |mut roles, witness| {
                if let Some(BuiltinRole::Type(role)) = self.statics.builtin_roles.witness(witness) {
                    roles.entry(role).or_default().push(witness);
                }
                roles
            },
        );
        self.collect_values(domain);
        let duplicates = types
            .into_iter()
            .filter(|(_, witnesses)| witnesses.len() > 1)
            .map(|(role, witnesses)| DuplicateBuiltinRole::Type { role, witnesses })
            .chain(self.values.iter().filter(|(_, entries)| entries.len() > 1).map(
                |(role, entries)| DuplicateBuiltinRole::Value {
                    role: *role,
                    entries: entries.clone(),
                },
            ))
            .collect::<Vec<_>>();
        let classifiers = self
            .values
            .iter()
            .flat_map(|(role, entries)| {
                let expected = BuiltinOperationAbi::for_role(*role).into_classifier();
                entries
                    .iter()
                    .copied()
                    .filter(|entry| {
                        !BuiltinClassifierMatcher::new(self.statics)
                            .matches_entry(*entry, &expected)
                    })
                    .map(|entry| BuiltinClassifierError::Mismatch {
                        role: *role,
                        entry,
                        expected: expected.clone(),
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        if duplicates.is_empty() && classifiers.is_empty() {
            Ok(())
        } else {
            Err(BuiltinSignatureError { duplicates, classifiers })
        }
    }

    fn collect_values(&mut self, domain: ss::TypeId) {
        let mut pending = vec![domain];
        std::iter::from_fn(|| {
            let ty = pending.pop()?;
            if !self.visited.insert(ty) {
                return Some(());
            }
            let Some(Fillable::Done(view)) = self.statics.types_pre.get(ty).cloned() else {
                return Some(());
            };
            match view {
                | ss::Type::Exists(exists) => pending.push(exists.body),
                | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => pending.push(body),
                | ss::Type::Prod(Prod(components)) => {
                    pending.extend(components.iter().rev().copied())
                }
                | ss::Type::Named(Named(_, inner)) => pending.push(inner),
                | ss::Type::Label(zydeco_syntax::Label(_, inner)) => {
                    if let Some(role) = self.statics.builtin_roles.value(ty) {
                        self.values.entry(role).or_default().push(ty);
                    } else {
                        pending.push(inner);
                    }
                }
                | ss::Type::Var(def) => {
                    if let Some(ss::AnnId::Type(next)) =
                        self.statics.annotations_var.get(&def).copied()
                    {
                        pending.push(next);
                    }
                }
                | _ => {}
            }
            Some(())
        })
        .for_each(drop);
    }
}

struct BuiltinClassifierMatcher<'a> {
    statics: &'a StaticsArena,
    visiting: HashSet<ss::TypeId>,
    computation_binders: Vec<ss::AbstId>,
}

impl<'a> BuiltinClassifierMatcher<'a> {
    fn new(statics: &'a StaticsArena) -> Self {
        Self { statics, visiting: HashSet::new(), computation_binders: Vec::new() }
    }

    fn matches_entry(&mut self, entry: ss::TypeId, expected: &BuiltinValueClassifier) -> bool {
        match self.type_view(entry) {
            | Some(ss::Type::Label(zydeco_syntax::Label(_, classifier))) => {
                self.matches_value(classifier, expected)
            }
            | Some(ss::Type::Named(Named(_, inner))) => self.matches_entry(inner, expected),
            | _ => false,
        }
    }

    fn matches_value(&mut self, actual: ss::TypeId, expected: &BuiltinValueClassifier) -> bool {
        if !self.visiting.insert(actual) {
            return false;
        }
        let matches = match (self.type_view(actual), expected) {
            | (
                Some(ss::Type::Primitive(ss::PrimitiveTy(actual))),
                BuiltinValueClassifier::Atom(expected),
            ) => expected.primitive() == Some(actual),
            | (Some(ss::Type::Abst(witness)), BuiltinValueClassifier::Atom(expected)) => {
                expected.capability_role().is_some_and(|role| {
                    self.statics.builtin_roles.witness(witness) == Some(BuiltinRole::Type(role))
                })
            }
            | (
                Some(ss::Type::App(zydeco_syntax::App(constructor, body))),
                BuiltinValueClassifier::Thunk(expected),
            ) => {
                self.matches_constructor(constructor, IntrinsicConstructor::Thunk)
                    && self.matches_computation(body, expected)
            }
            | (Some(ss::Type::Named(Named(_, inner))), _) => self.matches_value(inner, expected),
            | (Some(ss::Type::Var(def)), _) => {
                self.variable_type(def).is_some_and(|inner| self.matches_value(inner, expected))
            }
            | _ => false,
        };
        self.visiting.remove(&actual);
        matches
    }

    fn matches_computation(
        &mut self, actual: ss::TypeId, expected: &BuiltinComputationClassifier,
    ) -> bool {
        if !self.visiting.insert(actual) {
            return false;
        }
        let matches = match (self.type_view(actual), expected) {
            | (Some(ss::Type::Abst(witness)), BuiltinComputationClassifier::OS) => {
                self.statics.builtin_roles.witness(witness)
                    == Some(BuiltinRole::Type(BuiltinTypeRole::OS))
            }
            | (Some(ss::Type::Abst(witness)), BuiltinComputationClassifier::Bound(index)) => self
                .computation_binders
                .iter()
                .rev()
                .nth(*index)
                .is_some_and(|expected| expected == &witness),
            | (
                Some(ss::Type::App(zydeco_syntax::App(constructor, body))),
                BuiltinComputationClassifier::Return(expected),
            ) => {
                self.matches_constructor(constructor, IntrinsicConstructor::Return)
                    && self.matches_value(body, expected)
            }
            | (
                Some(ss::Type::Arrow(zydeco_syntax::Arrow(input, output))),
                BuiltinComputationClassifier::Arrow(expected_input, expected_output),
            ) => {
                self.matches_value(input, expected_input)
                    && self.matches_computation(output, expected_output)
            }
            | (
                Some(ss::Type::Forall(ss::Forall(binder, body))),
                BuiltinComputationClassifier::ForallCType(expected_body),
            ) => {
                if self.witness_is_ctype(binder.witness) {
                    self.computation_binders.push(binder.witness);
                    let matches = self.matches_computation(body, expected_body);
                    self.computation_binders.pop();
                    matches
                } else {
                    false
                }
            }
            | (Some(ss::Type::Named(Named(_, inner))), _) => {
                self.matches_computation(inner, expected)
            }
            | (Some(ss::Type::Var(def)), _) => self
                .variable_type(def)
                .is_some_and(|inner| self.matches_computation(inner, expected)),
            | _ => false,
        };
        self.visiting.remove(&actual);
        matches
    }

    fn matches_constructor(&mut self, actual: ss::TypeId, expected: IntrinsicConstructor) -> bool {
        if !self.visiting.insert(actual) {
            return false;
        }
        let matches = match self.type_view(actual) {
            | Some(ss::Type::Thk(_)) => expected == IntrinsicConstructor::Thunk,
            | Some(ss::Type::Ret(_)) => expected == IntrinsicConstructor::Return,
            | Some(ss::Type::Named(Named(_, inner))) => self.matches_constructor(inner, expected),
            | Some(ss::Type::Var(def)) => self
                .variable_type(def)
                .is_some_and(|inner| self.matches_constructor(inner, expected)),
            | _ => false,
        };
        self.visiting.remove(&actual);
        matches
    }

    fn type_view(&self, ty: ss::TypeId) -> Option<ss::Type> {
        match self.statics.types_pre.get(ty)?.to_owned() {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        }
    }

    fn witness_is_ctype(&self, witness: ss::AbstId) -> bool {
        self.statics
            .annotations_abst
            .get(witness)
            .and_then(|kind| self.statics.kinds_pre.get(kind))
            .is_some_and(|kind| matches!(kind, Fillable::Done(ss::Kind::CType(_))))
    }

    fn variable_type(&self, def: ss::DefId) -> Option<ss::TypeId> {
        self.statics
            .type_definitions
            .get(&def)
            .copied()
            .or_else(|| self.statics.annotations_var.get(&def).copied().and_then(Self::as_type))
    }

    fn as_type(annotation: ss::AnnId) -> Option<ss::TypeId> {
        match annotation {
            | ss::AnnId::Type(inner) => Some(inner),
            | ss::AnnId::Set | ss::AnnId::Kind(_) => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IntrinsicConstructor {
    Thunk,
    Return,
}

#[derive(Clone, Debug, Error)]
pub enum BuiltinPackagePlanError {
    #[error(transparent)]
    InvalidSignature(#[from] BuiltinSignatureError),
    #[error("Builtin signature type {ty:?} is unresolved")]
    UnresolvedSignatureType { ty: ss::TypeId },
    #[error("Builtin signature type {ty:?} has no runtime package interpretation")]
    UnsupportedSignatureType { ty: ss::TypeId },
    #[error("Builtin value classifier {entry:?} has no host-operation role")]
    MissingOperationRole { entry: ss::TypeId },
    #[error("Builtin executable signature has no abstract `os` witness")]
    MissingOSWitness,
    #[error("Builtin executable signature has more than one abstract `os` witness")]
    AmbiguousOSWitness,
    #[error("Builtin executable result type {result:?} is not its abstract `os` witness")]
    ResultNotOS { result: ss::TypeId },
}

/// Derives a backend-independent package plan from checked static identities.
pub struct BuiltinPackagePlanner<'a> {
    pub statics: &'a StaticsArena,
}

impl<'a> BuiltinPackagePlanner<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    /// Validate the domain of a library factory independently of process completion.
    pub fn for_value(
        &self, parameter: &ss::ValueParameter,
    ) -> Result<BuiltinPackagePlan, BuiltinPackagePlanError> {
        BuiltinSignatureValidator::new(self.statics).validate_parts(
            parameter.domain,
            parameter
                .witnesses
                .as_ref()
                .into_iter()
                .flat_map(|witnesses| witnesses.iter().copied()),
        )?;
        Ok(BuiltinPackagePlan { value: self.value(parameter.domain)? })
    }

    pub fn for_computation(
        &self, signature: &ss::PackPi,
    ) -> Result<BuiltinPackagePlan, BuiltinPackagePlanError> {
        BuiltinSignatureValidator::new(self.statics).validate(signature)?;
        Ok(BuiltinPackagePlan { value: self.value(signature.domain)? })
    }

    pub fn for_executable(
        &self, signature: &ss::PackPi,
    ) -> Result<BuiltinPackagePlan, BuiltinPackagePlanError> {
        let plan = self.for_computation(signature)?;
        self.validate_executable(signature)?;
        Ok(plan)
    }
    fn value(&self, ty: ss::TypeId) -> Result<BuiltinPackageValue, BuiltinPackagePlanError> {
        match self.type_view(ty)? {
            | ss::Type::Exists(exists) => self.value(exists.body),
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => self.value(body),
            | ss::Type::Prod(Prod(components)) => {
                let components = components
                    .into_iter()
                    .map(|component| self.value(component))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(BuiltinPackageValue::Product(components))
            }
            | ss::Type::Label(zydeco_syntax::Label(_, inner)) => {
                match self.statics.builtin_roles.value(ty) {
                    | Some(role) => Ok(BuiltinPackageValue::Operation(role)),
                    | None => self.value(inner).map_err(|error| match error {
                        | BuiltinPackagePlanError::UnsupportedSignatureType { .. } => {
                            BuiltinPackagePlanError::MissingOperationRole { entry: ty }
                        }
                        | error => error,
                    }),
                }
            }
            | ss::Type::Named(Named(_, inner)) => self.value(inner),
            | ss::Type::Unit(_) => Ok(BuiltinPackageValue::Unit),
            | ss::Type::Var(def) => match self.statics.annotations_var.get(&def).copied() {
                | Some(ss::AnnId::Type(next)) if next != ty => self.value(next),
                | _ => Err(BuiltinPackagePlanError::UnsupportedSignatureType { ty }),
            },
            | _ => Err(BuiltinPackagePlanError::UnsupportedSignatureType { ty }),
        }
    }

    fn validate_executable(&self, signature: &ss::PackPi) -> Result<(), BuiltinPackagePlanError> {
        let witnesses = signature
            .witnesses
            .iter()
            .copied()
            .filter(|witness| {
                self.statics.builtin_roles.witness(*witness)
                    == Some(BuiltinRole::Type(BuiltinTypeRole::OS))
            })
            .collect::<Vec<_>>();
        let os = match witnesses.as_slice() {
            | [] => return Err(BuiltinPackagePlanError::MissingOSWitness),
            | [os] => *os,
            | _ => return Err(BuiltinPackagePlanError::AmbiguousOSWitness),
        };
        if self.type_is_witness(signature.codomain, os)? {
            Ok(())
        } else {
            Err(BuiltinPackagePlanError::ResultNotOS { result: signature.codomain })
        }
    }

    fn type_is_witness(
        &self, ty: ss::TypeId, expected: ss::AbstId,
    ) -> Result<bool, BuiltinPackagePlanError> {
        match self.type_view(ty)? {
            | ss::Type::Abst(found) => Ok(found == expected),
            | ss::Type::Named(Named(_, inner)) => self.type_is_witness(inner, expected),
            | ss::Type::Var(def) => match self.statics.annotations_var.get(&def).copied() {
                | Some(ss::AnnId::Type(next)) if next != ty => self.type_is_witness(next, expected),
                | _ => Ok(false),
            },
            | _ => Ok(false),
        }
    }

    fn type_view(&self, ty: ss::TypeId) -> Result<ss::Type, BuiltinPackagePlanError> {
        match self.statics.types_pre[&ty].to_owned() {
            | Fillable::Done(ty) => Ok(ty),
            | Fillable::Fill(_) => Err(BuiltinPackagePlanError::UnresolvedSignatureType { ty }),
        }
    }
}
