//! Partial source protocols carried through SPS rewrites.
//!
//! A continuation describes its accepted value, never the extent of its saved stack.
//! Unknowns permit the existing word transport; compatibility is not type equality or
//! permission to specialize an ABI.

use crate::syntax::DtorIdx;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_syntax::{App, Arrow, DtorName, Named, PrimitiveType, Prod};
use zydeco_utils::arena::ArenaAccess as _;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum ValueProtocol {
    #[default]
    Unknown,
    Unit,
    Primitive(PrimitiveType),
    Product(Vec<ValueProtocol>),
    Thunk(Box<StackProtocol>),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum StackProtocol {
    #[default]
    Unknown,
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
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ProtocolGraphError {
    #[error("missing codata protocol definition {0}")]
    MissingDefinition(CodataProtocolId),
}

impl ProtocolGraph {
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
            | ValueProtocol::Product(fields) => {
                fields.iter().try_for_each(|field| self.validate_value(field))
            }
            | ValueProtocol::Thunk(stack) => self.validate_stack(stack),
            | _ => Ok(()),
        }
    }

    /// Partial agreement between validated descriptors; unknowns never establish type or ABI identity.
    pub fn stacks_agree(&self, left: &StackProtocol, right: &StackProtocol) -> bool {
        Agreement { graph: self, compared: HashSet::new() }.stack(left, right)
    }

    pub fn values_agree(&self, left: &ValueProtocol, right: &ValueProtocol) -> bool {
        Agreement { graph: self, compared: HashSet::new() }.value(left, right)
    }
}

struct Agreement<'a> {
    graph: &'a ProtocolGraph,
    compared: HashSet<(CodataProtocolId, CodataProtocolId)>,
}

impl Agreement<'_> {
    fn value(&mut self, left: &ValueProtocol, right: &ValueProtocol) -> bool {
        match (left, right) {
            | (ValueProtocol::Unknown, _) | (_, ValueProtocol::Unknown) => true,
            | (ValueProtocol::Unit, ValueProtocol::Unit) => true,
            | (ValueProtocol::Primitive(a), ValueProtocol::Primitive(b)) => a == b,
            | (ValueProtocol::Product(a), ValueProtocol::Product(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| self.value(a, b))
            }
            | (ValueProtocol::Thunk(a), ValueProtocol::Thunk(b)) => self.stack(a, b),
            | _ => false,
        }
    }

    fn stack(&mut self, left: &StackProtocol, right: &StackProtocol) -> bool {
        match (left, right) {
            | (StackProtocol::Unknown, _) | (_, StackProtocol::Unknown) => true,
            | (StackProtocol::Argument(a, rest), StackProtocol::Argument(b, tail)) => {
                self.value(a, b) && self.stack(rest, tail)
            }
            | (StackProtocol::Continuation(a), StackProtocol::Continuation(b)) => self.value(a, b),
            | (StackProtocol::Tag(a, rest), StackProtocol::Tag(b, tail)) => {
                a == b && self.stack(rest, tail)
            }
            | (StackProtocol::Codata(a), StackProtocol::Codata(b)) => {
                let (Some(left), Some(right)) = (self.graph.get(*a), self.graph.get(*b)) else {
                    return false;
                };
                // Every recursive edge crosses an observation. Repeated pairs close the
                // coinductive comparison; all other observations still have to agree.
                if !self.compared.insert((*a, *b)) {
                    return true;
                }
                left.observations.len() == right.observations.len()
                    && left
                        .observations
                        .iter()
                        .zip(&right.observations)
                        .all(|((a, rest), (b, tail))| a == b && self.stack(rest, tail))
            }
            | (StackProtocol::Codata(id), StackProtocol::Tag(tag, rest))
            | (StackProtocol::Tag(tag, rest), StackProtocol::Codata(id)) => self
                .graph
                .get(*id)
                .and_then(|definition| definition.observation(tag))
                .is_some_and(|expected| self.stack(expected, rest)),
            | _ => false,
        }
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

/// Translate only structure whose runtime interpretation is already established.
/// Disclosed codata seals become shared references. Unsupported forms stay unknown.
pub(crate) struct SourceProtocols<'a> {
    statics: &'a StaticsArena,
    active: HashSet<ss::TypeId>,
    codatas: HashMap<ss::CoDataId, CodataProtocolId>,
    pub(crate) graph: ProtocolGraph,
}

impl<'a> SourceProtocols<'a> {
    pub(crate) fn new(statics: &'a StaticsArena) -> Self {
        Self {
            statics,
            active: HashSet::new(),
            codatas: HashMap::new(),
            graph: ProtocolGraph::default(),
        }
    }

    pub(crate) fn value(&mut self, ty: ss::TypeId) -> ValueProtocol {
        if !self.active.insert(ty) {
            return ValueProtocol::Unknown;
        }
        let result = match self.statics.normalized_at(ty).cloned() {
            | Some(ss::Type::Unit(_)) => ValueProtocol::Unit,
            | Some(ss::Type::Primitive(ss::PrimitiveTy(ty))) => ValueProtocol::Primitive(ty),
            | Some(ss::Type::Prod(Prod(fields))) => {
                ValueProtocol::Product(fields.into_iter().map(|ty| self.value(ty)).collect())
            }
            | Some(ss::Type::Named(Named(_, inner))) => self.value(inner),
            | Some(ss::Type::App(App(head, body)))
                if matches!(self.statics.normalized_at(head), Some(ss::Type::Thk(_))) =>
            {
                ValueProtocol::Thunk(Box::new(self.stack(body)))
            }
            | _ => ValueProtocol::Unknown,
        };
        self.active.remove(&ty);
        result
    }

    pub(crate) fn stack(&mut self, ty: ss::TypeId) -> StackProtocol {
        // Resolve disclosed heads before the path guard, so a recursive occurrence
        // can find the codata reference reserved before walking its observations.
        let mut ty = ty;
        let mut heads = HashSet::new();
        while heads.insert(ty) {
            match self.statics.normalized_at(ty) {
                | Some(ss::Type::Abst(witness)) => match self.statics.seals.get(witness) {
                    | Some(body) => ty = *body,
                    | None => return StackProtocol::Unknown,
                },
                | Some(ss::Type::Named(Named(_, body))) => ty = *body,
                | Some(ss::Type::CoData(id)) => return self.codata(*id),
                | _ => break,
            }
        }
        if !self.active.insert(ty) {
            return StackProtocol::Unknown;
        }
        let result = match self.statics.normalized_at(ty).cloned() {
            | Some(ss::Type::Arrow(Arrow(input, rest))) => {
                StackProtocol::Argument(Box::new(self.value(input)), Box::new(self.stack(rest)))
            }
            | Some(ss::Type::Forall(ss::Forall(_, body)))
            | Some(ss::Type::Named(Named(_, body))) => self.stack(body),
            | Some(ss::Type::App(App(head, body)))
                if matches!(self.statics.normalized_at(head), Some(ss::Type::Ret(_))) =>
            {
                StackProtocol::Continuation(Box::new(self.value(body)))
            }
            | _ => StackProtocol::Unknown,
        };
        self.active.remove(&ty);
        result
    }

    pub(crate) fn codata(&mut self, source: ss::CoDataId) -> StackProtocol {
        if let Some(id) = self.codatas.get(&source) {
            return StackProtocol::Codata(*id);
        }
        let id = self.graph.reserve();
        self.codatas.insert(source, id);
        let mut arms = self.statics.codatas[&source].clone().into_iter().collect::<Vec<_>>();
        arms.sort_by(|(left, _), (right, _)| left.cmp(right));
        // Crossing an observation permits revisiting a surrounding argument prefix.
        // The reserved codata reference now guards that cycle; path guards still
        // stop unsupported recursion that never reaches a codata definition.
        let active = std::mem::take(&mut self.active);
        let observations = arms
            .into_iter()
            .enumerate()
            .map(|(idx, (name, ty))| (DtorIdx { idx, name }, self.stack(ty)))
            .collect();
        self.active = active;
        self.graph.codatas[id.0] = Some(CodataProtocol { observations });
        StackProtocol::Codata(id)
    }

    /// Both tag producers and consumers use the numbering owned by the descriptor.
    pub(crate) fn tag(&mut self, source: ss::CoDataId, name: DtorName) -> DtorIdx {
        let StackProtocol::Codata(id) = self.codata(source) else { unreachable!() };
        self.graph
            .get(id)
            .expect("source codata extraction completed")
            .observations
            .iter()
            .find(|(tag, _)| tag.name == name)
            .map(|(tag, _)| tag.clone())
            .expect("checked observation belongs to its codata")
    }
}

#[cfg(test)]
mod tests;
