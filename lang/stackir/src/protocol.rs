//! Partial source protocols carried through SPS rewrites.
//!
//! A continuation describes its accepted value, never the extent of its saved stack.
//! Unknowns permit the existing word transport; compatibility is not type equality or
//! permission to specialize an ABI.

use std::{collections::HashSet, fmt};
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_syntax::{App, Arrow, Named, PrimitiveType, Prod};

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

    pub fn agrees(&self, other: &Self) -> bool {
        match (self, other) {
            | (Self::Unknown, _) | (_, Self::Unknown) => true,
            | (Self::Unit, Self::Unit) => true,
            | (Self::Primitive(a), Self::Primitive(b)) => a == b,
            | (Self::Product(a), Self::Product(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.agrees(b))
            }
            | (Self::Thunk(a), Self::Thunk(b)) => a.agrees(b),
            | _ => false,
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

    pub fn agrees(&self, other: &Self) -> bool {
        match (self, other) {
            | (Self::Unknown, _) | (_, Self::Unknown) => true,
            | (Self::Argument(a, rest), Self::Argument(b, tail)) => {
                a.agrees(b) && rest.agrees(tail)
            }
            | (Self::Continuation(a), Self::Continuation(b)) => a.agrees(b),
            | _ => false,
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
        }
    }
}

/// Translate only structure whose runtime interpretation is already established.
/// Quantified witnesses, recursive heads, codata, and unsupported forms stay unknown.
pub(crate) struct SourceProtocols<'a> {
    statics: &'a StaticsArena,
    active: HashSet<ss::TypeId>,
}

impl<'a> SourceProtocols<'a> {
    pub(crate) fn new(statics: &'a StaticsArena) -> Self {
        Self { statics, active: HashSet::new() }
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
}
