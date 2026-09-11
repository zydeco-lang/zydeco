//! Partial source protocols carried through SPS rewrites.
//!
//! A continuation describes its accepted value, never the extent of its saved stack.
//! Unknowns permit the existing word transport; compatibility is not type equality or
//! permission to specialize an ABI.

use crate::syntax::DtorIdx;
use std::fmt;
use zydeco_syntax::PrimitiveType;

mod source;
pub(crate) use source::SourceProtocols;
mod agreement;
use agreement::Agreement;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, derive_more::Display)]
#[display("a{_0}")]
pub struct ProtocolParameterId(usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq, derive_more::Display)]
pub enum ProtocolParameterKind {
    #[display("VType")]
    Value,
    #[display("CType")]
    Stack,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum ValueProtocol {
    #[default]
    Unknown,
    Parameter(ProtocolParameterId),
    Unit,
    Primitive(PrimitiveType),
    Product(Vec<ValueProtocol>),
    Thunk(Box<StackProtocol>),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum StackProtocol {
    #[default]
    Unknown,
    Parameter(ProtocolParameterId),
    /// A source type binder. Its occurrences share one argument at each use;
    /// the binder itself consumes no runtime stack component.
    Forall(ProtocolParameterId, Box<StackProtocol>),
    Argument(Box<ValueProtocol>, Box<StackProtocol>),
    /// An installed continuation. Its saved residual stack is existentially hidden.
    Continuation(Box<ValueProtocol>),
    /// A reference to the alternatives of a codata interface in the program's graph.
    Codata(CodataProtocolId),
    /// A particular observation supplied by a stack producer.
    Tag(DtorIdx, Box<StackProtocol>),
}

/// An index local to one program's protocol graph, preserved by structural rebuilding.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, derive_more::Display)]
#[display("p{_0}")]
pub struct CodataProtocolId(usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodataProtocol {
    /// Canonical name order supplies runtime tag numbering, independently of declaration order.
    pub observations: Vec<(DtorIdx, StackProtocol)>,
}

impl CodataProtocol {
    pub fn observation(&self, tag: &DtorIdx) -> Option<&StackProtocol> {
        self.observations
            .get(tag.idx)
            .filter(|(candidate, _)| candidate == tag)
            .map(|(_, stack)| stack)
    }
}

/// Owned, finite descriptions. Cycles pass through codata observations, never a frame boundary.
#[derive(Clone, Debug, Default)]
pub struct ProtocolGraph {
    codatas: Vec<Option<CodataProtocol>>,
    parameters: Vec<ProtocolParameterKind>,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ProtocolGraphError {
    #[error("missing codata protocol definition {0}")]
    MissingDefinition(CodataProtocolId),
    #[error("missing protocol parameter {0}")]
    MissingParameter(ProtocolParameterId),
    #[error("protocol parameter {parameter} has kind {found}, expected {expected}")]
    ParameterKind {
        parameter: ProtocolParameterId,
        expected: ProtocolParameterKind,
        found: ProtocolParameterKind,
    },
}

impl ProtocolGraph {
    fn parameter(&mut self, kind: ProtocolParameterKind) -> ProtocolParameterId {
        let id = ProtocolParameterId(self.parameters.len());
        self.parameters.push(kind);
        id
    }

    pub fn parameter_kind(&self, id: ProtocolParameterId) -> Option<ProtocolParameterKind> {
        self.parameters.get(id.0).copied()
    }

    pub fn parameters(&self) -> impl Iterator<Item = (ProtocolParameterId, ProtocolParameterKind)> {
        self.parameters.iter().enumerate().map(|(id, kind)| (ProtocolParameterId(id), *kind))
    }

    fn validate_parameter(
        &self, parameter: ProtocolParameterId, expected: ProtocolParameterKind,
    ) -> Result<(), ProtocolGraphError> {
        let found = self
            .parameter_kind(parameter)
            .ok_or(ProtocolGraphError::MissingParameter(parameter))?;
        if found != expected {
            return Err(ProtocolGraphError::ParameterKind { parameter, expected, found });
        }
        Ok(())
    }

    fn reserve(&mut self) -> CodataProtocolId {
        let id = CodataProtocolId(self.codatas.len());
        self.codatas.push(None);
        id
    }

    pub fn get(&self, id: CodataProtocolId) -> Option<&CodataProtocol> {
        self.codatas.get(id.0).and_then(Option::as_ref)
    }

    pub fn iter(&self) -> impl Iterator<Item = (CodataProtocolId, &CodataProtocol)> {
        self.codatas
            .iter()
            .enumerate()
            .filter_map(|(index, definition)| Some((CodataProtocolId(index), definition.as_ref()?)))
    }

    pub fn validate(&self) -> Result<(), ProtocolGraphError> {
        for (index, definition) in self.codatas.iter().enumerate() {
            let definition = definition
                .as_ref()
                .ok_or(ProtocolGraphError::MissingDefinition(CodataProtocolId(index)))?;
            for (_, stack) in &definition.observations {
                self.validate_stack(stack)?;
            }
        }
        Ok(())
    }

    pub fn validate_stack(&self, stack: &StackProtocol) -> Result<(), ProtocolGraphError> {
        match stack {
            | StackProtocol::Parameter(id) => {
                self.validate_parameter(*id, ProtocolParameterKind::Stack)
            }
            | StackProtocol::Forall(id, body) => {
                self.parameter_kind(*id).ok_or(ProtocolGraphError::MissingParameter(*id))?;
                self.validate_stack(body)
            }
            | StackProtocol::Codata(id) => {
                self.get(*id).map(|_| ()).ok_or(ProtocolGraphError::MissingDefinition(*id))
            }
            | StackProtocol::Argument(value, rest) => {
                self.validate_value(value)?;
                self.validate_stack(rest)
            }
            | StackProtocol::Continuation(value) => self.validate_value(value),
            | StackProtocol::Tag(_, rest) => self.validate_stack(rest),
            | StackProtocol::Unknown => Ok(()),
        }
    }

    pub fn validate_value(&self, value: &ValueProtocol) -> Result<(), ProtocolGraphError> {
        match value {
            | ValueProtocol::Parameter(id) => {
                self.validate_parameter(*id, ProtocolParameterKind::Value)
            }
            | ValueProtocol::Product(fields) => {
                fields.iter().try_for_each(|field| self.validate_value(field))
            }
            | ValueProtocol::Thunk(stack) => self.validate_stack(stack),
            | _ => Ok(()),
        }
    }

    /// Partial agreement between validated descriptors; unknowns never establish type or ABI identity.
    pub fn stacks_agree(&self, left: &StackProtocol, right: &StackProtocol) -> bool {
        Agreement::new(self).stacks(left, right)
    }

    pub fn values_agree(&self, left: &ValueProtocol, right: &ValueProtocol) -> bool {
        Agreement::new(self).values(left, right)
    }
}

impl ValueProtocol {
    /// Keep producer facts, filling unknown components from a source classifier.
    pub(crate) fn with_evidence(self, evidence: &Self) -> Self {
        match (self, evidence) {
            | (Self::Unknown, evidence) => evidence.clone(),
            | (Self::Product(fields), Self::Product(more)) if fields.len() == more.len() => {
                Self::Product(
                    fields
                        .into_iter()
                        .zip(more)
                        .map(|(field, more)| field.with_evidence(more))
                        .collect(),
                )
            }
            | (Self::Thunk(stack), Self::Thunk(more)) => {
                Self::Thunk(Box::new(stack.with_evidence(more)))
            }
            | (known, _) => known,
        }
    }
}

impl StackProtocol {
    /// Type binders erase before a computation consumes its next runtime component.
    pub(crate) fn runtime_head(&self) -> &Self {
        match self {
            | Self::Forall(_, body) => body.runtime_head(),
            | _ => self,
        }
    }

    pub(crate) fn with_evidence(self, evidence: &Self) -> Self {
        match (self, evidence) {
            | (Self::Unknown, evidence) => evidence.clone(),
            | (Self::Argument(value, rest), Self::Argument(more, tail)) => Self::Argument(
                Box::new(value.with_evidence(more)),
                Box::new(rest.with_evidence(tail)),
            ),
            | (Self::Continuation(value), Self::Continuation(more)) => {
                Self::Continuation(Box::new(value.with_evidence(more)))
            }
            | (known, _) => known,
        }
    }
}

impl fmt::Display for ValueProtocol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Unknown => f.write_str("?"),
            | Self::Parameter(parameter) => write!(f, "{parameter}"),
            | Self::Unit => f.write_str("Unit"),
            | Self::Primitive(ty) => write!(f, "{ty}"),
            | Self::Thunk(stack) => write!(f, "Thk({stack})"),
            | Self::Product(fields) => {
                f.write_str("(")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{field}")?;
                }
                f.write_str(")")
            }
        }
    }
}

impl fmt::Display for StackProtocol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Unknown => f.write_str("?"),
            | Self::Parameter(parameter) => write!(f, "{parameter}"),
            | Self::Forall(parameter, body) => write!(f, "forall {parameter} . {body}"),
            | Self::Argument(value, rest) => write!(f, "{value} :: {rest}"),
            | Self::Continuation(value) => write!(f, "cont({value})"),
            | Self::Codata(id) => write!(f, "{id}"),
            | Self::Tag(tag, rest) => write!(f, "{}#{} :: {rest}", tag.name.0, tag.idx),
        }
    }
}

impl fmt::Display for CodataProtocol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("codata {")?;
        for (tag, stack) in &self.observations {
            write!(f, " {}#{}: {stack};", tag.name.0, tag.idx)?;
        }
        f.write_str(" }")
    }
}

#[cfg(test)]
mod tests;
