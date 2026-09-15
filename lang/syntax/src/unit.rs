//! Closed structural interfaces for the native unit initializer ABI.

use crate::{FloatType, ForeignImport, ForeignTarget, IntegerType, PrimitiveType};
use serde::{Deserialize, Serialize};

/// An exported value's complete classifier in the initial, binder-free unit profile.
/// The public transport is always one runtime word; these are source types, not Rust layouts.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum UnitValueType {
    Unit,
    Integer(IntegerType),
    Float(FloatType),
    Char,
    String,
    Named { name: String, value: Box<Self> },
    Product(Vec<Self>),
    Thunk(Box<UnitStackType>),
}

/// Computation protocols admitted inside exported thunks.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum UnitStackType {
    Arrow(Box<UnitValueType>, Box<Self>),
    Return(Box<UnitValueType>),
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("invalid or excessively deep native unit interface")]
pub struct UnitInterfaceError;

impl UnitValueType {
    pub const MAX_DEPTH: usize = 48;
    pub const MAX_NODES: usize = 4096;

    pub fn validate(&self) -> Result<(), UnitInterfaceError> {
        let mut remaining = Self::MAX_NODES;
        self.validate_at(0, &mut remaining)
    }

    fn validate_at(&self, depth: usize, remaining: &mut usize) -> Result<(), UnitInterfaceError> {
        Self::visit(depth, remaining)?;
        match self {
            | Self::Named { name, value } => {
                let mut chars = name.chars();
                let valid = chars.next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                    && name != "_"
                    && chars.all(|c| c.is_ascii_alphanumeric() || "_'?+*-=~".contains(c));
                if !valid {
                    return Err(UnitInterfaceError);
                }
                value.validate_at(depth + 1, remaining)
            }
            | Self::Product(fields) => {
                if fields.len() < 2 {
                    return Err(UnitInterfaceError);
                }
                fields.iter().try_for_each(|field| field.validate_at(depth + 1, remaining))
            }
            | Self::Thunk(stack) => stack.validate_at(depth + 1, remaining),
            | _ => Ok(()),
        }
    }

    fn visit(depth: usize, remaining: &mut usize) -> Result<(), UnitInterfaceError> {
        if depth > Self::MAX_DEPTH || *remaining == 0 {
            return Err(UnitInterfaceError);
        }
        *remaining -= 1;
        Ok(())
    }

    /// A self-contained source classifier, with no producer-local definitions or ambient prelude.
    pub fn source(&self) -> Result<String, UnitInterfaceError> {
        self.validate()?;
        Ok(self.render())
    }

    fn render(&self) -> String {
        match self {
            | Self::Unit => "(@(intrinsic(unit)))".into(),
            | Self::Integer(integer) => {
                format!("(@(intrinsic({})))", PrimitiveType::Integer(*integer).intrinsic_name())
            }
            | Self::Float(float) => {
                format!("(@(intrinsic({})))", PrimitiveType::Float(*float).intrinsic_name())
            }
            | Self::Char => "(@(intrinsic(char)))".into(),
            | Self::String => "(@(intrinsic(string)))".into(),
            | Self::Named { name, value } => format!("(#{name} :: {})", value.render()),
            | Self::Product(fields) => {
                format!("({})", fields.iter().map(Self::render).collect::<Vec<_>>().join(" * "))
            }
            | Self::Thunk(stack) => format!("((@(intrinsic(thk))) ({}))", stack.render()),
        }
    }
}

impl UnitStackType {
    fn validate_at(&self, depth: usize, remaining: &mut usize) -> Result<(), UnitInterfaceError> {
        UnitValueType::visit(depth, remaining)?;
        match self {
            | Self::Arrow(argument, tail) => {
                argument.validate_at(depth + 1, remaining)?;
                tail.validate_at(depth + 1, remaining)
            }
            | Self::Return(value) => value.validate_at(depth + 1, remaining),
        }
    }

    fn render(&self) -> String {
        match self {
            | Self::Arrow(argument, tail) => format!("{} -> {}", argument.render(), tail.render()),
            | Self::Return(value) => format!("(@(intrinsic(ret))) ({})", value.render()),
        }
    }
}

/// A native initializer returns one value into the caller's existing continuation and instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct UnitImport {
    pub target: ForeignTarget,
    pub exports: UnitValueType,
}

/// Checked source attachment, keeping native word transport distinct from C marshalling.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CheckedImport {
    C(ForeignImport),
    Zydeco(UnitImport),
}

impl CheckedImport {
    pub fn target(&self) -> &ForeignTarget {
        match self {
            | Self::C(import) => &import.target,
            | Self::Zydeco(import) => &import.target,
        }
    }
}
