//! Foreign C calling conventions, symbol validation, and signatures.

use crate::builtin::IntegerType;
use alloc::{string::String, vec::Vec};
use core::fmt;

/// Calling convention used by one foreign import.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum ForeignAbi {
    C,
    Zydeco,
}

impl ForeignAbi {
    pub const VARIANTS: &'static [Self] = &[Self::C, Self::Zydeco];

    pub fn from_source_name(name: &str) -> Option<Self> {
        match name {
            | "c" => Some(Self::C),
            | "zydeco" => Some(Self::Zydeco),
            | _ => None,
        }
    }

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::C => "c",
            | Self::Zydeco => "zydeco",
        }
    }
}

impl fmt::Display for ForeignAbi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.source_name())
    }
}

/// A platform linker name, as used by `-l<name>`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(try_from = "String"))]
pub struct ForeignLibraryName(String);

impl ForeignLibraryName {
    pub fn parse(name: impl Into<String>) -> Option<Self> {
        let name = name.into();
        let mut bytes = name.bytes();
        let first = bytes.next()?;
        ((first.is_ascii_alphanumeric() || first == b'_')
            && bytes.all(|byte| {
                byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-' | b'.' | b'+')
            }))
        .then_some(Self(name))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ForeignLibraryName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// An unmangled C external symbol.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(try_from = "String"))]
pub struct ForeignSymbolName(String);

impl ForeignSymbolName {
    pub fn parse(name: impl Into<String>) -> Option<Self> {
        let name = name.into();
        let mut bytes = name.bytes();
        let first = bytes.next()?;
        ((first.is_ascii_alphabetic() || first == b'_')
            && bytes.all(|byte| byte.is_ascii_alphanumeric() || byte == b'_'))
        .then_some(Self(name))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ForeignSymbolName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Validation failure for foreign library or C symbol identifiers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForeignNameError {
    Library,
    Symbol,
}

impl fmt::Display for ForeignNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Library => f.write_str("invalid foreign library name"),
            | Self::Symbol => f.write_str("invalid C symbol name"),
        }
    }
}

impl core::error::Error for ForeignNameError {}

impl TryFrom<String> for ForeignLibraryName {
    type Error = ForeignNameError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::parse(value).ok_or(ForeignNameError::Library)
    }
}

impl TryFrom<String> for ForeignSymbolName {
    type Error = ForeignNameError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::parse(value).ok_or(ForeignNameError::Symbol)
    }
}

/// Link-time identity of a foreign function, before its Zydeco classifier is interpreted.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ForeignTarget {
    pub abi: ForeignAbi,
    pub library: ForeignLibraryName,
    pub symbol: ForeignSymbolName,
}

/// One source-level parameter whose C representation is known to the compiler.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ForeignParameter {
    /// Pass an unmanaged `Addr` as one C data pointer. Its validity is a caller obligation.
    Address,
    Integer(IntegerType),
}

impl ForeignParameter {
    pub fn components(self) -> impl Iterator<Item = ForeignComponent> {
        core::iter::once(match self {
            | Self::Address => ForeignComponent::MemoryPointer,
            | Self::Integer(integer) => ForeignComponent::Integer(integer),
        })
    }
}

/// A scalar C argument extracted from one source value, in declaration order.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ForeignComponent {
    MemoryPointer,
    Integer(IntegerType),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ForeignArgument {
    /// Zero-based source parameter index. A window supplies one pointer; length is explicit.
    pub parameter: usize,
    pub component: ForeignComponent,
}

/// One source-level foreign result whose C representation is known to the compiler.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ForeignResult {
    Integer(IntegerType),
    /// C `void` resumes the Zydeco continuation with the unit value.
    Unit,
}

/// The marshalling protocol derived from a checked CBPV classifier.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(try_from = "ForeignSignatureData"))]
pub struct ForeignSignature {
    parameters: Vec<ForeignParameter>,
    result: ForeignResult,
}

#[cfg(feature = "serde")]
#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct ForeignSignatureData {
    parameters: Vec<ForeignParameter>,
    result: ForeignResult,
}

#[cfg(feature = "serde")]
impl TryFrom<ForeignSignatureData> for ForeignSignature {
    type Error = ForeignSignatureError;

    fn try_from(data: ForeignSignatureData) -> Result<Self, Self::Error> {
        Self::new(data.parameters, data.result)
    }
}

impl ForeignSignature {
    /// The initial common subset fits six integer/pointer argument registers on SysV / AAPCS64.
    pub const MAX_C_ARGUMENTS: usize = 6;

    pub fn new(
        parameters: Vec<ForeignParameter>, result: ForeignResult,
    ) -> Result<Self, ForeignSignatureError> {
        let signature = Self { parameters, result };
        let found = signature.arguments().count();
        if found > Self::MAX_C_ARGUMENTS {
            return Err(ForeignSignatureError::TooManyArguments {
                found,
                maximum: Self::MAX_C_ARGUMENTS,
            });
        }
        Ok(signature)
    }

    pub fn parameters(&self) -> &[ForeignParameter] {
        &self.parameters
    }

    pub fn result(&self) -> ForeignResult {
        self.result
    }

    pub fn arguments(&self) -> impl Iterator<Item = ForeignArgument> + '_ {
        self.parameters.iter().enumerate().flat_map(|(parameter, representation)| {
            representation
                .components()
                .map(move |component| ForeignArgument { parameter, component })
        })
    }
}

/// Validation failure when constructing a [`ForeignSignature`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForeignSignatureError {
    TooManyArguments { found: usize, maximum: usize },
}

impl fmt::Display for ForeignSignatureError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::TooManyArguments { found, maximum } => write!(
                f,
                "C ffi supports at most {maximum} flattened C arguments, but this classifier needs {found}"
            ),
        }
    }
}

impl core::error::Error for ForeignSignatureError {}

/// A validated foreign target paired with its source-to-C marshalling protocol.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ForeignImport {
    pub target: ForeignTarget,
    pub signature: ForeignSignature,
}
