use super::{
    arena::StaticsArena,
    syntax::{self as ss, Fillable},
};
use std::{
    collections::{BTreeMap, HashSet},
    error::Error,
    fmt::{Display, Formatter},
};
use thiserror::Error;
use zydeco_syntax::{BuiltinRole, BuiltinTypeRole, BuiltinValueRole, Named, Prod};
use zydeco_utils::arena::ArenaAccess;

/// Host-provided atomic value types available to foundational operations.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinValueAtom {
    Int,
    Float,
    Char,
    String,
    Bytes,
    Reader,
    Writer,
}

impl BuiltinValueAtom {
    fn role(self) -> BuiltinTypeRole {
        match self {
            | Self::Int => BuiltinTypeRole::Int,
            | Self::Float => BuiltinTypeRole::Float,
            | Self::Char => BuiltinTypeRole::Char,
            | Self::String => BuiltinTypeRole::String,
            | Self::Bytes => BuiltinTypeRole::Bytes,
            | Self::Reader => BuiltinTypeRole::Reader,
            | Self::Writer => BuiltinTypeRole::Writer,
        }
    }
}

impl Display for BuiltinValueAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A value classifier in the foundational Builtin ABI.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinValueClassifier {
    Atom(BuiltinValueAtom),
    Thunk(Box<BuiltinComputationClassifier>),
}

impl Display for BuiltinValueClassifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Atom(atom) => write!(f, "{atom}"),
            | Self::Thunk(body) => write!(f, "Thk({body})"),
        }
    }
}

/// A computation classifier in the foundational Builtin ABI.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinComputationClassifier {
    OS,
    /// A computation classifier bound by an enclosing `forall CType`.
    Bound(usize),
    Return(Box<BuiltinValueClassifier>),
    Arrow(BuiltinValueClassifier, Box<BuiltinComputationClassifier>),
    ForallCType(Box<BuiltinComputationClassifier>),
}

impl Display for BuiltinComputationClassifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::OS => write!(f, "OS"),
            | Self::Bound(index) => write!(f, "C{index}"),
            | Self::Return(value) => write!(f, "Ret {value}"),
            | Self::Arrow(input, output) => write!(f, "{input} -> {output}"),
            | Self::ForallCType(body) => write!(f, "forall CType. {body}"),
        }
    }
}

/// The representation-independent classifier of one host-operation role.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BuiltinOperationAbi {
    classifier: BuiltinValueClassifier,
}

impl BuiltinOperationAbi {
    pub fn for_role(role: BuiltinValueRole) -> Self {
        use BuiltinValueAtom as Atom;
        use BuiltinValueRole as Role;

        let classifier = match role {
            | Role::Add | Role::Sub | Role::Mul | Role::Div | Role::Mod => {
                Self::pure([Atom::Int, Atom::Int], Atom::Int)
            }
            | Role::IntEq | Role::IntLt | Role::IntGt => Self::branch([Atom::Int, Atom::Int]),
            | Role::FloatAdd | Role::FloatSub | Role::FloatMul | Role::FloatDiv => {
                Self::pure([Atom::Float, Atom::Float], Atom::Float)
            }
            | Role::FloatEq | Role::FloatLt | Role::FloatGt => {
                Self::branch([Atom::Float, Atom::Float])
            }
            | Role::FloatToStr => Self::pure([Atom::Float], Atom::String),
            | Role::StrScalarLength | Role::StrByteLength => Self::pure([Atom::String], Atom::Int),
            | Role::StrAppend => Self::pure([Atom::String, Atom::String], Atom::String),
            | Role::StrSplitOnce => Self::optional_pair([Atom::String, Atom::Char]),
            | Role::StrSplitAt => Self::optional_pair([Atom::String, Atom::Int]),
            | Role::StrGet => Self::optional([Atom::String, Atom::Int], Atom::Char),
            | Role::StrEq => Self::branch([Atom::String, Atom::String]),
            | Role::IntToStr => Self::pure([Atom::Int], Atom::String),
            | Role::CharToStr => Self::pure([Atom::Char], Atom::String),
            | Role::CharCodepoint => Self::pure([Atom::Char], Atom::Int),
            | Role::CharFromCodepoint => Self::optional([Atom::Int], Atom::Char),
            | Role::StrParseInt => Self::optional([Atom::String], Atom::Int),
            | Role::BytesEmpty => Self::pure([], Atom::Bytes),
            | Role::BytesLength => Self::pure([Atom::Bytes], Atom::Int),
            | Role::BytesAppend => Self::pure([Atom::Bytes, Atom::Bytes], Atom::Bytes),
            | Role::BytesFromStr => Self::pure([Atom::String], Atom::Bytes),
            | Role::BytesToStr => Self::optional([Atom::Bytes], Atom::String),
            | Role::Stdin => Self::pure([], Atom::Reader),
            | Role::Stdout | Role::Stderr => Self::pure([], Atom::Writer),
            | Role::IoRead => Self::io_effect(
                [Self::atom(Atom::Reader), Self::atom(Atom::Int)],
                Self::continuation(Atom::Bytes),
            ),
            | Role::IoReadAll => {
                Self::io_effect([Self::atom(Atom::Reader)], Self::continuation(Atom::Bytes))
            }
            | Role::IoReadLine => Self::effect([
                Self::atom(Atom::Reader),
                Self::io_error_continuation(),
                Self::os_continuation(),
                Self::continuation(Atom::Bytes),
            ]),
            | Role::IoWriteAll => Self::io_effect(
                [Self::atom(Atom::Writer), Self::atom(Atom::Bytes)],
                Self::os_continuation(),
            ),
            | Role::IoFlush | Role::IoCloseWriter => {
                Self::io_effect([Self::atom(Atom::Writer)], Self::os_continuation())
            }
            | Role::IoCloseReader => {
                Self::io_effect([Self::atom(Atom::Reader)], Self::os_continuation())
            }
            | Role::FsOpenReader => {
                Self::io_effect([Self::atom(Atom::String)], Self::continuation(Atom::Reader))
            }
            | Role::FsCreateWriter | Role::FsAppendWriter => {
                Self::io_effect([Self::atom(Atom::String)], Self::continuation(Atom::Writer))
            }
            | Role::WriteStr => Self::effect([Self::atom(Atom::String), Self::os_continuation()]),
            | Role::WriteInt => Self::effect([Self::atom(Atom::Int), Self::os_continuation()]),
            | Role::WriteLine => Self::effect([Self::atom(Atom::String), Self::os_continuation()]),
            | Role::ReadLine | Role::ReadTillEof => {
                Self::effect([Self::continuation(Atom::String)])
            }
            | Role::ReadLineAsInt => {
                Self::effect([Self::os_continuation(), Self::continuation(Atom::Int)])
            }
            | Role::ArgList => Self::string_fold(),
            | Role::RandomInt => Self::effect([Self::continuation(Atom::Int)]),
            | Role::Exit => Self::effect([Self::atom(Atom::Int)]),
        };
        Self { classifier }
    }

    pub fn into_classifier(self) -> BuiltinValueClassifier {
        self.classifier
    }

    fn pure(
        parameters: impl IntoIterator<Item = BuiltinValueAtom>, result: BuiltinValueAtom,
    ) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Return(Box::new(Self::atom(result)));
        Self::thunk(Self::arrows(parameters.into_iter().map(Self::atom), result))
    }

    fn effect(
        parameters: impl IntoIterator<Item = BuiltinValueClassifier>,
    ) -> BuiltinValueClassifier {
        Self::thunk(Self::arrows(parameters, BuiltinComputationClassifier::OS))
    }

    fn branch(parameters: impl IntoIterator<Item = BuiltinValueAtom>) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let continuation = Self::thunk(result.clone());
        let body = Self::arrows(
            parameters.into_iter().map(Self::atom).chain([continuation.clone(), continuation]),
            result,
        );
        Self::thunk(BuiltinComputationClassifier::ForallCType(Box::new(body)))
    }

    fn optional_pair(
        parameters: impl IntoIterator<Item = BuiltinValueAtom>,
    ) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let when_none = Self::thunk(result.clone());
        let when_some = Self::thunk(Self::arrows(
            [Self::atom(BuiltinValueAtom::String), Self::atom(BuiltinValueAtom::String)],
            result.clone(),
        ));
        let body = Self::arrows(
            parameters.into_iter().map(Self::atom).chain([when_none, when_some]),
            result,
        );
        Self::thunk(BuiltinComputationClassifier::ForallCType(Box::new(body)))
    }

    fn optional(
        parameters: impl IntoIterator<Item = BuiltinValueAtom>, result_atom: BuiltinValueAtom,
    ) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let when_none = Self::thunk(result.clone());
        let when_some = Self::thunk(Self::arrows([Self::atom(result_atom)], result.clone()));
        let body = Self::arrows(
            parameters.into_iter().map(Self::atom).chain([when_none, when_some]),
            result,
        );
        Self::thunk(BuiltinComputationClassifier::ForallCType(Box::new(body)))
    }

    fn string_fold() -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let when_empty = Self::thunk(result.clone());
        let when_item = Self::thunk(Self::arrows(
            [Self::atom(BuiltinValueAtom::String), Self::thunk(result.clone())],
            result.clone(),
        ));
        let body = Self::arrows([when_empty, when_item], result);
        Self::thunk(BuiltinComputationClassifier::ForallCType(Box::new(body)))
    }

    fn atom(atom: BuiltinValueAtom) -> BuiltinValueClassifier {
        BuiltinValueClassifier::Atom(atom)
    }

    fn thunk(body: BuiltinComputationClassifier) -> BuiltinValueClassifier {
        BuiltinValueClassifier::Thunk(Box::new(body))
    }

    fn continuation(argument: BuiltinValueAtom) -> BuiltinValueClassifier {
        Self::continuation_with([argument])
    }

    fn continuation_with(
        arguments: impl IntoIterator<Item = BuiltinValueAtom>,
    ) -> BuiltinValueClassifier {
        Self::thunk(Self::arrows(
            arguments.into_iter().map(Self::atom),
            BuiltinComputationClassifier::OS,
        ))
    }

    fn io_error_continuation() -> BuiltinValueClassifier {
        Self::continuation_with([BuiltinValueAtom::Int, BuiltinValueAtom::String])
    }

    fn io_effect(
        parameters: impl IntoIterator<Item = BuiltinValueClassifier>,
        success: BuiltinValueClassifier,
    ) -> BuiltinValueClassifier {
        Self::effect(parameters.into_iter().chain([Self::io_error_continuation(), success]))
    }

    fn os_continuation() -> BuiltinValueClassifier {
        Self::thunk(BuiltinComputationClassifier::OS)
    }

    fn arrows(
        parameters: impl IntoIterator<Item = BuiltinValueClassifier>,
        result: BuiltinComputationClassifier,
    ) -> BuiltinComputationClassifier {
        parameters
            .into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(result, |output, input| {
                BuiltinComputationClassifier::Arrow(input, Box::new(output))
            })
    }
}

/// The runtime-relevant shape of one product in a checked Builtin package.
#[derive(Clone, Debug)]
pub struct BuiltinProductPlan {
    pub items: Vec<BuiltinPackageValue>,
    pub tail: Box<BuiltinPackageValue>,
}

impl BuiltinProductPlan {
    pub fn into_values(self) -> Vec<BuiltinPackageValue> {
        self.items.into_iter().chain(std::iter::once(*self.tail)).collect()
    }
}

/// One runtime-relevant component of a checked Builtin package.
#[derive(Clone, Debug)]
pub enum BuiltinPackageValue {
    Unit,
    Operation(BuiltinValueRole),
    Product(BuiltinProductPlan),
}

/// Backend-independent instructions for materializing the host Builtin package.
#[derive(Clone, Debug)]
pub struct BuiltinPackagePlan {
    pub value: BuiltinPackageValue,
}

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinClassifierError {
    Mismatch { role: BuiltinValueRole, entry: ss::TypeId, expected: BuiltinValueClassifier },
}

impl Display for BuiltinClassifierError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Mismatch { role, expected, .. } => {
                write!(f, "operation role `{role}` requires classifier `{expected}`")
            }
        }
    }
}

/// A package signature whose host-role interpretation is ambiguous.
#[derive(Clone, Debug, PartialEq, Eq)]
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

impl Error for BuiltinSignatureError {}

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
        let types = signature.witnesses.iter().copied().fold(
            BTreeMap::<BuiltinTypeRole, Vec<ss::AbstId>>::new(),
            |mut roles, witness| {
                if let Some(BuiltinRole::Type(role)) = self.statics.builtin_roles.witness(witness) {
                    roles.entry(role).or_default().push(witness);
                }
                roles
            },
        );
        self.collect_values(signature.domain);
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
            let Some(Fillable::Done(view)) = self.statics.types_pre.get(&ty).cloned() else {
                return Some(());
            };
            match view {
                | ss::Type::Exists(ss::Exists { body, .. })
                | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => pending.push(body),
                | ss::Type::Prod(Prod(head, tail)) => pending.extend([tail, head]),
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
            | (Some(ss::Type::Abst(witness)), BuiltinValueClassifier::Atom(expected)) => {
                self.statics.builtin_roles.witness(witness)
                    == Some(BuiltinRole::Type(expected.role()))
            }
            | (
                Some(ss::Type::App(zydeco_syntax::App(constructor, body))),
                BuiltinValueClassifier::Thunk(expected),
            ) => {
                self.matches_constructor(constructor, IntrinsicConstructor::Thunk)
                    && self.matches_computation(body, expected)
            }
            | (Some(ss::Type::Named(Named(_, inner))), _) => self.matches_value(inner, expected),
            | (Some(ss::Type::Var(def)), _) => self
                .variable_type(actual, def)
                .is_some_and(|inner| self.matches_value(inner, expected)),
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
                .variable_type(actual, def)
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
                .variable_type(actual, def)
                .is_some_and(|inner| self.matches_constructor(inner, expected)),
            | _ => false,
        };
        self.visiting.remove(&actual);
        matches
    }

    fn type_view(&self, ty: ss::TypeId) -> Option<ss::Type> {
        match self.statics.types_pre.get(&ty)?.to_owned() {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        }
    }

    fn witness_is_ctype(&self, witness: ss::AbstId) -> bool {
        self.statics
            .annotations_abst
            .get(&witness)
            .and_then(|kind| self.statics.kinds_pre.get(kind))
            .is_some_and(|kind| matches!(kind, Fillable::Done(ss::Kind::CType(_))))
    }

    fn variable_type(&self, occurrence: ss::TypeId, def: ss::DefId) -> Option<ss::TypeId> {
        self.statics
            .env_type
            .get(&occurrence)
            .and_then(|environment| environment.get(&def).copied())
            .and_then(Self::as_type)
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
struct BuiltinPackagePlanner<'a> {
    statics: &'a StaticsArena,
}

impl BuiltinPackagePlan {
    pub fn for_executable(
        statics: &StaticsArena, signature: &ss::PackPi,
    ) -> Result<Self, BuiltinPackagePlanError> {
        BuiltinSignatureValidator::new(statics).validate(signature)?;
        let planner = BuiltinPackagePlanner { statics };
        planner.validate_executable(signature)?;
        Ok(Self { value: planner.value(signature.domain)? })
    }
}

impl BuiltinPackagePlanner<'_> {
    fn value(&self, ty: ss::TypeId) -> Result<BuiltinPackageValue, BuiltinPackagePlanError> {
        match self.type_view(ty)? {
            | ss::Type::Exists(ss::Exists { body, .. })
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => self.value(body),
            | ss::Type::Prod(Prod(head, tail)) => {
                let head = self.value(head)?;
                let (items, tail) = self.product_tail(tail)?;
                Ok(BuiltinPackageValue::Product(BuiltinProductPlan {
                    items: std::iter::once(head).chain(items).collect(),
                    tail: Box::new(tail),
                }))
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

    fn product_tail(
        &self, ty: ss::TypeId,
    ) -> Result<(Vec<BuiltinPackageValue>, BuiltinPackageValue), BuiltinPackagePlanError> {
        match self.type_view(ty)? {
            | ss::Type::Prod(Prod(head, tail)) => {
                let head = self.value(head)?;
                let (items, tail) = self.product_tail(tail)?;
                Ok((std::iter::once(head).chain(items).collect(), tail))
            }
            | _ => Ok((Vec::new(), self.value(ty)?)),
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
