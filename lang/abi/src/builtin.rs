//! Declarative catalog of intrinsics, builtin types, operations, and CBPV classifiers.

use crate::host::{HostCallMode, SpareBox};
use alloc::{borrow::ToOwned, boxed::Box, format, string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};
use zydeco_machine::word::{RuntimeWord, ScalarRepresentation};

/// Total integer leaves for value computation. Layout laws are source functions.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ValueIntOp {
    Add,
    Sub,
    And,
    Compare,
}

impl ValueIntOp {
    pub const ALL: &'static [Self] = &[Self::Add, Self::Sub, Self::And, Self::Compare];

    pub const fn intrinsic_name(self) -> &'static str {
        match self {
            | Self::Add => "val/int/add",
            | Self::Sub => "val/int/sub",
            | Self::And => "val/int/and",
            | Self::Compare => "val/int/compare",
        }
    }

    pub fn evaluate(self, [left, right]: [i64; 2]) -> i64 {
        RuntimeWord::wrap_signed(match self {
            | Self::Add => left.wrapping_add(right),
            | Self::Sub => left.wrapping_sub(right),
            | Self::And => left & right,
            | Self::Compare => match left.cmp(&right) {
                | core::cmp::Ordering::Less => -1,
                | core::cmp::Ordering::Equal => 0,
                | core::cmp::Ordering::Greater => 1,
            },
        })
    }
}

/// Compiler-defined identities for the intrinsic CBPV structure and pure primitive operations.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntrinsicRole {
    VType,
    CType,
    Thk,
    Ret,
    Unit,
    Primitive(PrimitiveType),
    ValueInt(ValueIntOp),
    Operation(IntrinsicOp),
}

impl IntrinsicRole {
    const BASE_VARIANTS: &'static [Self] =
        &[Self::VType, Self::CType, Self::Thk, Self::Ret, Self::Unit];

    /// Every source-spellable intrinsic role, in completion order.
    pub fn all() -> impl Iterator<Item = Self> {
        Self::BASE_VARIANTS
            .iter()
            .copied()
            .chain(PrimitiveType::all().map(Self::Primitive))
            .chain(ValueIntOp::ALL.iter().copied().map(Self::ValueInt))
            .chain(IntrinsicOp::all().map(Self::Operation))
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::all().find(|role| role.source_name() == name)
    }

    pub fn source_name(self) -> String {
        match self {
            | Self::VType => "vtype".to_owned(),
            | Self::CType => "ctype".to_owned(),
            | Self::Thk => "thk".to_owned(),
            | Self::Ret => "ret".to_owned(),
            | Self::Unit => "unit".to_owned(),
            | Self::Primitive(primitive) => primitive.intrinsic_name().to_owned(),
            | Self::ValueInt(operation) => operation.intrinsic_name().to_owned(),
            | Self::Operation(operation) => operation.source_name(),
        }
    }
}

impl Display for IntrinsicRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.source_name())
    }
}

/// Compiler-defined roles for host-provided abstract types in the Builtin package signature.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinTypeRole {
    Addr,
    Reader,
    Writer,
    OS,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinTypeUniverse {
    Value,
    Computation,
}

impl BuiltinTypeRole {
    pub const VARIANTS: &'static [Self] = &[Self::Addr, Self::Reader, Self::Writer, Self::OS];

    pub fn from_source_name(name: &str) -> Option<Self> {
        match name {
            | "addr" => Some(Self::Addr),
            | "reader" => Some(Self::Reader),
            | "writer" => Some(Self::Writer),
            | "os" => Some(Self::OS),
            | _ => None,
        }
    }

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Addr => "addr",
            | Self::Reader => "reader",
            | Self::Writer => "writer",
            | Self::OS => "os",
        }
    }

    pub const fn universe(self) -> BuiltinTypeUniverse {
        match self {
            | Self::Addr | Self::Reader | Self::Writer => BuiltinTypeUniverse::Value,
            | Self::OS => BuiltinTypeUniverse::Computation,
        }
    }
}

impl Display for BuiltinTypeRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.source_name())
    }
}

/// The concrete representation selected for an integer literal or operation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum IntegerType {
    Int8,
    Int16,
    Int32,
    Int64,
    Int,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt,
}

impl IntegerType {
    pub const VARIANTS: &'static [Self] = &[
        Self::Int8,
        Self::Int16,
        Self::Int32,
        Self::Int64,
        Self::Int,
        Self::UInt8,
        Self::UInt16,
        Self::UInt32,
        Self::UInt64,
        Self::UInt,
    ];

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Int8 => "int8",
            | Self::Int16 => "int16",
            | Self::Int32 => "int32",
            | Self::Int64 => "int64",
            | Self::Int => "int",
            | Self::UInt8 => "uint8",
            | Self::UInt16 => "uint16",
            | Self::UInt32 => "uint32",
            | Self::UInt64 => "uint64",
            | Self::UInt => "uint",
        }
    }

    pub const fn type_name(self) -> &'static str {
        match self {
            | Self::Int8 => "Int8",
            | Self::Int16 => "Int16",
            | Self::Int32 => "Int32",
            | Self::Int64 => "Int64",
            | Self::Int => "Int",
            | Self::UInt8 => "UInt8",
            | Self::UInt16 => "UInt16",
            | Self::UInt32 => "UInt32",
            | Self::UInt64 => "UInt64",
            | Self::UInt => "UInt",
        }
    }

    pub const fn c_type_name(self) -> &'static str {
        match self {
            | Self::Int8 => "int8_t",
            | Self::Int16 => "int16_t",
            | Self::Int32 => "int32_t",
            | Self::Int64 | Self::Int => "int64_t",
            | Self::UInt8 => "uint8_t",
            | Self::UInt16 => "uint16_t",
            | Self::UInt32 => "uint32_t",
            | Self::UInt64 | Self::UInt => "uint64_t",
        }
    }

    pub const fn is_signed(self) -> bool {
        matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64 | Self::Int)
    }

    pub const fn bits(self) -> u8 {
        match self {
            | Self::Int8 | Self::UInt8 => 8,
            | Self::Int16 | Self::UInt16 => 16,
            | Self::Int32 | Self::UInt32 => 32,
            | Self::Int64 | Self::UInt64 => 64,
            | Self::Int | Self::UInt => RuntimeWord::INTEGER_BITS,
        }
    }

    /// The general value ABI follows the type, independent of the payload's value.
    pub const fn representation(self) -> ScalarRepresentation {
        match self {
            | Self::Int64 | Self::UInt64 => ScalarRepresentation::OpaqueBox,
            | Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int
            | Self::UInt8
            | Self::UInt16
            | Self::UInt32
            | Self::UInt => ScalarRepresentation::Immediate,
        }
    }

    /// Byte storage and C transport round the payload width to a whole scalar carrier.
    /// Int/UInt use a 64-bit carrier in the current runtime profile, with checked decoding.
    pub const fn storage_bits(self) -> u8 {
        self.bits().next_power_of_two()
    }
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.type_name())
    }
}

/// The concrete IEEE-754 representation selected for a floating-point literal or operation.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum FloatType {
    Float32,
    #[default]
    Float64,
}

impl FloatType {
    pub const VARIANTS: &'static [Self] = &[Self::Float32, Self::Float64];

    pub const fn representation(self) -> ScalarRepresentation {
        match self {
            | Self::Float32 => ScalarRepresentation::Immediate,
            | Self::Float64 => ScalarRepresentation::OpaqueBox,
        }
    }

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Float32 => "float32",
            | Self::Float64 => "float64",
        }
    }

    pub const fn type_name(self) -> &'static str {
        match self {
            | Self::Float32 => "Float32",
            | Self::Float64 => "Float64",
        }
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.type_name())
    }
}

/// A fixed host representation with an applicative static identity.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    Integer(IntegerType),
    Float(FloatType),
    Char,
    String,
}

impl PrimitiveType {
    pub fn all() -> impl Iterator<Item = Self> {
        IntegerType::VARIANTS
            .iter()
            .copied()
            .map(Self::Integer)
            .chain(FloatType::VARIANTS.iter().copied().map(Self::Float))
            .chain([Self::Char, Self::String])
    }

    pub fn from_intrinsic_name(name: &str) -> Option<Self> {
        Self::all().find(|primitive| primitive.intrinsic_name() == name)
    }

    pub const fn intrinsic_name(self) -> &'static str {
        match self {
            | Self::Integer(IntegerType::Int8) => "i8",
            | Self::Integer(IntegerType::Int16) => "i16",
            | Self::Integer(IntegerType::Int32) => "i32",
            | Self::Integer(IntegerType::Int64) => "i64",
            | Self::Integer(IntegerType::Int) => "int",
            | Self::Integer(IntegerType::UInt8) => "u8",
            | Self::Integer(IntegerType::UInt16) => "u16",
            | Self::Integer(IntegerType::UInt32) => "u32",
            | Self::Integer(IntegerType::UInt64) => "u64",
            | Self::Integer(IntegerType::UInt) => "uint",
            | Self::Float(FloatType::Float32) => "f32",
            | Self::Float(FloatType::Float64) => "f64",
            | Self::Char => "char",
            | Self::String => "string",
        }
    }

    pub const fn type_name(self) -> &'static str {
        match self {
            | Self::Integer(integer) => integer.type_name(),
            | Self::Float(float) => float.type_name(),
            | Self::Char => "Char",
            | Self::String => "String",
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.type_name())
    }
}

/// Operations exported per [`IntegerType`].
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerOperation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    ToString,
    StoreLe,
    LoadLe,
}

impl IntegerOperation {
    pub const VARIANTS: &'static [Self] = &[
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::Mod,
        Self::Eq,
        Self::Lt,
        Self::Gt,
        Self::ToString,
        Self::StoreLe,
        Self::LoadLe,
    ];

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Add => "add",
            | Self::Sub => "sub",
            | Self::Mul => "mul",
            | Self::Div => "div",
            | Self::Mod => "mod",
            | Self::Eq => "eq",
            | Self::Lt => "lt",
            | Self::Gt => "gt",
            | Self::ToString => "to_string",
            | Self::StoreLe => "store_le",
            | Self::LoadLe => "load_le",
        }
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::VARIANTS.iter().copied().find(|op| op.source_name() == name)
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
            | Self::StoreLe => 3,
            | Self::LoadLe => 2,
        }
    }

    pub const fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt | Self::StoreLe | Self::LoadLe)
    }
}

/// Operations exported per [`FloatType`].
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatOperation {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    ToString,
    StoreLe,
    LoadLe,
}

impl FloatOperation {
    pub const VARIANTS: &'static [Self] = &[
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::Eq,
        Self::Lt,
        Self::Gt,
        Self::ToString,
        Self::StoreLe,
        Self::LoadLe,
    ];

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Add => "add",
            | Self::Sub => "sub",
            | Self::Mul => "mul",
            | Self::Div => "div",
            | Self::Eq => "eq",
            | Self::Lt => "lt",
            | Self::Gt => "gt",
            | Self::ToString => "to_string",
            | Self::StoreLe => "store_le",
            | Self::LoadLe => "load_le",
        }
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::VARIANTS.iter().copied().find(|op| op.source_name() == name)
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
            | Self::StoreLe => 3,
            | Self::LoadLe => 2,
        }
    }

    pub const fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt | Self::StoreLe | Self::LoadLe)
    }
}

/// Atomic value types available to foundational host operations.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinValueAtom {
    Integer(IntegerType),
    Float(FloatType),
    Char,
    String,
    Addr,
    Reader,
    Writer,
}

impl BuiltinValueAtom {
    pub const fn capability_role(self) -> Option<BuiltinTypeRole> {
        match self {
            | Self::Addr => Some(BuiltinTypeRole::Addr),
            | Self::Reader => Some(BuiltinTypeRole::Reader),
            | Self::Writer => Some(BuiltinTypeRole::Writer),
            | Self::Integer(_) | Self::Float(_) | Self::Char | Self::String => None,
        }
    }

    pub const fn primitive(self) -> Option<PrimitiveType> {
        Some(match self {
            | Self::Integer(integer) => PrimitiveType::Integer(integer),
            | Self::Float(float) => PrimitiveType::Float(float),
            | Self::Char => PrimitiveType::Char,
            | Self::String => PrimitiveType::String,
            | Self::Addr | Self::Reader | Self::Writer => return None,
        })
    }
}

impl Display for BuiltinValueAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

/// A value classifier in the foundational Builtin ABI.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinValueClassifier {
    Atom(BuiltinValueAtom),
    Thunk(Box<BuiltinComputationClassifier>),
}

impl Display for BuiltinValueClassifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

    fn memory_success(
        parameters: impl IntoIterator<Item = BuiltinValueAtom>,
        output: impl IntoIterator<Item = BuiltinValueAtom>,
    ) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let success = Self::thunk(Self::arrows(output.into_iter().map(Self::atom), result.clone()));
        let body = Self::arrows(parameters.into_iter().map(Self::atom).chain([success]), result);
        Self::thunk(BuiltinComputationClassifier::ForallCType(Box::new(body)))
    }

    fn memory_operation(
        parameters: impl IntoIterator<Item = BuiltinValueAtom>,
        output: impl IntoIterator<Item = BuiltinValueAtom>,
    ) -> BuiltinValueClassifier {
        let result = BuiltinComputationClassifier::Bound(0);
        let error = Self::thunk(Self::arrows(
            [Self::atom(BuiltinValueAtom::Integer(IntegerType::Int))],
            result.clone(),
        ));
        let success = Self::thunk(Self::arrows(output.into_iter().map(Self::atom), result.clone()));
        let body =
            Self::arrows(parameters.into_iter().map(Self::atom).chain([error, success]), result);
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
        Self::continuation_with([
            BuiltinValueAtom::Integer(IntegerType::Int),
            BuiltinValueAtom::String,
        ])
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

#[derive(Clone, Copy)]
struct NamedBuiltinSpec {
    role: BuiltinValueRole,
    source_name: &'static str,
    host_name: &'static str,
    arity: usize,
    call_mode: HostCallMode,
    spare_box: Option<SpareBox>,
    may_allocate_gc: bool,
}

type Atom = BuiltinValueAtom;
const INT: Atom = Atom::Integer(IntegerType::Int);

macro_rules! define_builtins {
    (
        $(
            ($variant:ident, $src:expr, $host:expr, $arity:expr, $mode:ident, $spare:expr, $gc:expr, $abi:expr)
        ),* $(,)?
    ) => {
        /// Compiler-defined roles that may be assigned to host-provided value entries
        /// in the Builtin package signature.
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum BuiltinValueRole {
            Integer(IntegerType, IntegerOperation),
            Float(FloatType, FloatOperation),
            $( $variant, )*
        }

        const NAMED_SPECS: &[NamedBuiltinSpec] = &[
            $(
                NamedBuiltinSpec {
                    role: BuiltinValueRole::$variant,
                    source_name: $src,
                    host_name: $host,
                    arity: $arity,
                    call_mode: HostCallMode::$mode,
                    spare_box: $spare,
                    may_allocate_gc: $gc,
                },
            )*
        ];

        impl BuiltinValueRole {
            pub const NAMED_VARIANTS: &'static [Self] = &[ $( Self::$variant, )* ];

            const fn named_spec(self) -> &'static NamedBuiltinSpec {
                #[allow(non_camel_case_types)]
                #[repr(usize)]
                enum NamedSlot { $( $variant, )* }

                let index = match self {
                    | Self::Integer(..) | Self::Float(..) => unreachable!(),
                    $( | Self::$variant => NamedSlot::$variant as usize, )*
                };
                &NAMED_SPECS[index]
            }

            pub(crate) fn named_abi(self) -> BuiltinValueClassifier {
                match self {
                    | Self::Integer(..) | Self::Float(..) => unreachable!(),
                    $( | Self::$variant => $abi, )*
                }
            }
        }
    };
}

define_builtins! {
    (Int64FromInt, "int64_from_int", "int64_from_int", 1, Returning, Some(SpareBox::Opaque), false, BuiltinOperationAbi::pure([INT], Atom::Integer(IntegerType::Int64))),
    (Int64ToInt, "int64_to_int", "int64_to_int_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::Integer(IntegerType::Int64)], INT)),
    (UInt64FromUInt, "uint64_from_uint", "uint64_from_uint", 1, Returning, Some(SpareBox::Opaque), false, BuiltinOperationAbi::pure([Atom::Integer(IntegerType::UInt)], Atom::Integer(IntegerType::UInt64))),
    (UInt64ToUInt, "uint64_to_uint", "uint64_to_uint_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::Integer(IntegerType::UInt64)], Atom::Integer(IntegerType::UInt))),
    (StrScalarLength, "str_scalar_length", "str_scalar_length", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::String], INT)),
    (StrByteLength, "str_byte_length", "str_byte_length", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::String], INT)),
    (StrAppend, "str_append", "str_append", 2, Returning, None, true, BuiltinOperationAbi::pure([Atom::String, Atom::String], Atom::String)),
    (StrSplitOnce, "str_split_once", "str_split_once_branch", 4, Control, None, true, BuiltinOperationAbi::optional_pair([Atom::String, Atom::Char])),
    (StrSplitAt, "str_split_at", "str_split_at_branch", 4, Control, None, true, BuiltinOperationAbi::optional_pair([Atom::String, INT])),
    (StrEq, "str_eq", "str_eq_branch", 4, Control, None, false, BuiltinOperationAbi::branch([Atom::String, Atom::String])),
    (StrGet, "str_get", "str_get_branch", 4, Control, None, false, BuiltinOperationAbi::optional([Atom::String, INT], Atom::Char)),
    (CharToStr, "char_to_str", "char_to_str", 1, Returning, None, true, BuiltinOperationAbi::pure([Atom::Char], Atom::String)),
    (CharCodepoint, "char_codepoint", "char_codepoint", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::Char], INT)),
    (CharFromCodepoint, "char_from_codepoint", "char_from_codepoint_branch", 3, Control, None, false, BuiltinOperationAbi::optional([INT], Atom::Char)),
    (StrParseInt, "str_parse_int", "str_parse_int_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::String], INT)),
    (MemoryNull, "memory_null", "memory_null", 0, Returning, None, false, BuiltinOperationAbi::pure([], Atom::Addr)),
    (MemoryCopy, "memory_copy", "memory_copy", 4, Control, None, false, BuiltinOperationAbi::memory_success([Atom::Addr, Atom::Addr, INT], None)),
    (MemoryFill, "memory_fill", "memory_fill", 4, Control, None, false, BuiltinOperationAbi::memory_success([Atom::Addr, INT, Atom::Integer(IntegerType::UInt8)], None)),
    (MemoryAllocate, "memory_allocate", "memory_allocate", 4, Control, None, false, BuiltinOperationAbi::memory_operation([INT, INT], Some(Atom::Addr))),
    (MemoryFree, "memory_free", "memory_free", 5, Control, None, false, BuiltinOperationAbi::memory_operation([Atom::Addr, INT, INT], None)),
    (MemoryRetain, "memory_retain", "memory_retain", 5, Control, None, false, BuiltinOperationAbi::memory_operation([Atom::Addr, INT, INT], None)),
    (MemoryFromString, "memory_from_string", "memory_from_string", 3, Control, None, false, BuiltinOperationAbi::memory_operation([Atom::String], [Atom::Addr, INT])),
    (MemoryToString, "memory_to_string", "memory_to_string", 4, Control, None, true, BuiltinOperationAbi::memory_operation([Atom::Addr, INT], Some(Atom::String))),
    (MemoryOffset, "memory_offset", "memory_offset", 2, Returning, None, false, BuiltinOperationAbi::pure([Atom::Addr, INT], Atom::Addr)),
    (MemoryLoadAddr, "memory_load_addr", "memory_load_addr", 2, Control, None, false, BuiltinOperationAbi::memory_success([Atom::Addr], Some(Atom::Addr))),
    (MemoryStoreAddr, "memory_store_addr", "memory_store_addr", 3, Control, None, false, BuiltinOperationAbi::memory_success([Atom::Addr, Atom::Addr], None)),
    (Stdin, "stdin", "stdin", 0, Returning, None, false, BuiltinOperationAbi::pure([], Atom::Reader)),
    (Stdout, "stdout", "stdout", 0, Returning, None, false, BuiltinOperationAbi::pure([], Atom::Writer)),
    (Stderr, "stderr", "stderr", 0, Returning, None, false, BuiltinOperationAbi::pure([], Atom::Writer)),
    (IoRead, "io_read", "io_read", 4, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Reader), BuiltinOperationAbi::atom(INT)], BuiltinOperationAbi::continuation_with([Atom::Addr, INT]))),
    (IoReadLine, "io_read_line", "io_read_line", 4, Control, None, true, BuiltinOperationAbi::effect([BuiltinOperationAbi::atom(Atom::Reader), BuiltinOperationAbi::io_error_continuation(), BuiltinOperationAbi::os_continuation(), BuiltinOperationAbi::continuation_with([Atom::Addr, INT])])),
    (IoReadAll, "io_read_all", "io_read_all", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Reader)], BuiltinOperationAbi::continuation_with([Atom::Addr, INT]))),
    (IoWriteAll, "io_write_all", "io_write_all", 5, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Writer), BuiltinOperationAbi::atom(Atom::Addr), BuiltinOperationAbi::atom(INT)], BuiltinOperationAbi::os_continuation())),
    (IoFlush, "io_flush", "io_flush", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Writer)], BuiltinOperationAbi::os_continuation())),
    (IoCloseReader, "io_close_reader", "io_close_reader", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Reader)], BuiltinOperationAbi::os_continuation())),
    (IoCloseWriter, "io_close_writer", "io_close_writer", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::Writer)], BuiltinOperationAbi::os_continuation())),
    (FsOpenReader, "fs_open_reader", "fs_open_reader", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::String)], BuiltinOperationAbi::continuation(Atom::Reader))),
    (FsCreateWriter, "fs_create_writer", "fs_create_writer", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::String)], BuiltinOperationAbi::continuation(Atom::Writer))),
    (FsAppendWriter, "fs_append_writer", "fs_append_writer", 3, Control, None, true, BuiltinOperationAbi::io_effect([BuiltinOperationAbi::atom(Atom::String)], BuiltinOperationAbi::continuation(Atom::Writer))),
    (WriteStr, "write_str", "write_str", 2, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::atom(Atom::String), BuiltinOperationAbi::os_continuation()])),
    (WriteInt, "write_int", "write_int", 2, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::atom(INT), BuiltinOperationAbi::os_continuation()])),
    (WriteLine, "write_line", "write_line", 2, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::atom(Atom::String), BuiltinOperationAbi::os_continuation()])),
    (ReadLine, "read_line", "read_line", 1, Control, None, true, BuiltinOperationAbi::effect([BuiltinOperationAbi::continuation(Atom::String)])),
    (ReadLineAsInt, "read_line_as_int", "read_line_as_int_branch", 2, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::os_continuation(), BuiltinOperationAbi::continuation(INT)])),
    (ReadTillEof, "read_till_eof", "read_till_eof", 1, Control, None, true, BuiltinOperationAbi::effect([BuiltinOperationAbi::continuation(Atom::String)])),
    (ArgAt, "arg_at", "arg_at", 3, Control, None, false, BuiltinOperationAbi::optional([INT], Atom::String)),
    (RandomInt, "random_int", "random_int", 1, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::continuation(INT)])),
    (Exit, "exit", "exit", 1, Control, None, false, BuiltinOperationAbi::effect([BuiltinOperationAbi::atom(INT)])),
}

impl BuiltinValueRole {
    pub fn all() -> impl Iterator<Item = Self> {
        IntegerType::VARIANTS
            .iter()
            .copied()
            .flat_map(|integer| {
                IntegerOperation::VARIANTS
                    .iter()
                    .copied()
                    .map(move |operation| Self::Integer(integer, operation))
            })
            .chain(FloatType::VARIANTS.iter().copied().flat_map(|float| {
                FloatOperation::VARIANTS
                    .iter()
                    .copied()
                    .map(move |operation| Self::Float(float, operation))
            }))
            .chain(Self::NAMED_VARIANTS.iter().copied())
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        let numeric = IntegerType::VARIANTS.iter().copied().find_map(|integer| {
            name.strip_prefix(integer.source_name())
                .and_then(|suffix| suffix.strip_prefix('_'))
                .and_then(IntegerOperation::from_source_name)
                .map(|operation| Self::Integer(integer, operation))
        });
        if numeric.is_some() {
            return numeric;
        }
        let numeric = FloatType::VARIANTS.iter().copied().find_map(|float| {
            name.strip_prefix(float.source_name())
                .and_then(|suffix| suffix.strip_prefix('_'))
                .and_then(FloatOperation::from_source_name)
                .map(|operation| Self::Float(float, operation))
        });
        if numeric.is_some() {
            return numeric;
        }
        NAMED_SPECS.iter().find(|spec| spec.source_name == name).map(|spec| spec.role)
    }

    pub fn source_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) => {
                format!("{}_{}", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) => {
                format!("{}_{}", float.source_name(), operation.source_name())
            }
            | role => role.named_spec().source_name.to_owned(),
        }
    }

    /// Runtime symbol used when the role is materialized in the foundational Builtin package.
    pub fn host_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) if operation.is_branch() => {
                format!("{}_{}_branch", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) if operation.is_branch() => {
                format!("{}_{}_branch", float.source_name(), operation.source_name())
            }
            | Self::Integer(..) | Self::Float(..) => self.source_name(),
            | role => role.named_spec().host_name.to_owned(),
        }
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Integer(_, operation) => operation.arity(),
            | Self::Float(_, operation) => operation.arity(),
            | role => role.named_spec().arity,
        }
    }

    /// Whether the host operation returns one word or selects a continuation.
    pub const fn call_mode(self) -> HostCallMode {
        match self {
            | Self::Integer(_, operation) => {
                if operation.is_branch() {
                    HostCallMode::Control
                } else {
                    HostCallMode::Returning
                }
            }
            | Self::Float(_, operation) => {
                if operation.is_branch() {
                    HostCallMode::Control
                } else {
                    HostCallMode::Returning
                }
            }
            | role => role.named_spec().call_mode,
        }
    }

    /// Hidden spare-box argument passed after explicit source arguments.
    pub const fn spare_box(self) -> Option<SpareBox> {
        match self {
            | Self::Integer(..) | Self::Float(..) => None,
            | role => role.named_spec().spare_box,
        }
    }

    /// Whether executing this host operation can allocate a managed GC object (e.g. a `String`).
    pub const fn may_allocate_gc(self) -> bool {
        match self {
            | Self::Integer(_, operation) => matches!(operation, IntegerOperation::ToString),
            | Self::Float(_, operation) => matches!(operation, FloatOperation::ToString),
            | role => role.named_spec().may_allocate_gc,
        }
    }
}

impl Display for BuiltinValueRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.source_name())
    }
}

impl BuiltinOperationAbi {
    pub fn for_role(role: BuiltinValueRole) -> Self {
        let classifier = match role {
            | BuiltinValueRole::Integer(integer, operation) => {
                let atom = BuiltinValueAtom::Integer(integer);
                match operation {
                    | IntegerOperation::Add
                    | IntegerOperation::Sub
                    | IntegerOperation::Mul
                    | IntegerOperation::Div
                    | IntegerOperation::Mod => Self::pure([atom, atom], atom),
                    | IntegerOperation::Eq | IntegerOperation::Lt | IntegerOperation::Gt => {
                        Self::branch([atom, atom])
                    }
                    | IntegerOperation::ToString => Self::pure([atom], BuiltinValueAtom::String),
                    | IntegerOperation::StoreLe => {
                        Self::memory_success([BuiltinValueAtom::Addr, atom], None)
                    }
                    | IntegerOperation::LoadLe => {
                        Self::memory_success([BuiltinValueAtom::Addr], Some(atom))
                    }
                }
            }
            | BuiltinValueRole::Float(float, operation) => {
                let atom = BuiltinValueAtom::Float(float);
                match operation {
                    | FloatOperation::Add
                    | FloatOperation::Sub
                    | FloatOperation::Mul
                    | FloatOperation::Div => Self::pure([atom, atom], atom),
                    | FloatOperation::Eq | FloatOperation::Lt | FloatOperation::Gt => {
                        Self::branch([atom, atom])
                    }
                    | FloatOperation::ToString => Self::pure([atom], BuiltinValueAtom::String),
                    | FloatOperation::StoreLe => {
                        Self::memory_success([BuiltinValueAtom::Addr, atom], None)
                    }
                    | FloatOperation::LoadLe => {
                        Self::memory_success([BuiltinValueAtom::Addr], Some(atom))
                    }
                }
            }
            | named => named.named_abi(),
        };
        Self { classifier }
    }
}

/// The typed meaning of one `@[builtin(...)]` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinRole {
    Type(BuiltinTypeRole),
    Value(BuiltinValueRole),
}

impl BuiltinRole {
    /// Every source-spellable Builtin package role, in completion order.
    pub fn all() -> impl Iterator<Item = Self> {
        BuiltinTypeRole::VARIANTS
            .iter()
            .copied()
            .map(Self::Type)
            .chain(BuiltinValueRole::all().map(Self::Value))
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        BuiltinTypeRole::from_source_name(name)
            .map(Self::Type)
            .or_else(|| BuiltinValueRole::from_source_name(name).map(Self::Value))
    }

    pub fn source_name(self) -> String {
        match self {
            | Self::Type(role) => role.source_name().to_owned(),
            | Self::Value(role) => role.source_name(),
        }
    }
}

impl Display for BuiltinRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.source_name())
    }
}

/// Pure integer thunk operations exposed through `@(intrinsic(<ty>/<op>))`.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerIntrinsicOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    ToString,
}

impl IntegerIntrinsicOp {
    pub const VARIANTS: &'static [Self] = &[
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::Mod,
        Self::Eq,
        Self::Lt,
        Self::Gt,
        Self::ToString,
    ];

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Add => "add",
            | Self::Sub => "sub",
            | Self::Mul => "mul",
            | Self::Div => "div",
            | Self::Mod => "mod",
            | Self::Eq => "eq",
            | Self::Lt => "lt",
            | Self::Gt => "gt",
            | Self::ToString => "to_string",
        }
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::VARIANTS.iter().copied().find(|op| op.source_name() == name)
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
        }
    }

    pub const fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt)
    }
}

/// Pure floating-point thunk operations exposed through `@(intrinsic(<ty>/<op>))`.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatIntrinsicOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    ToString,
}

impl FloatIntrinsicOp {
    pub const VARIANTS: &'static [Self] =
        &[Self::Add, Self::Sub, Self::Mul, Self::Div, Self::Eq, Self::Lt, Self::Gt, Self::ToString];

    pub const fn source_name(self) -> &'static str {
        match self {
            | Self::Add => "add",
            | Self::Sub => "sub",
            | Self::Mul => "mul",
            | Self::Div => "div",
            | Self::Eq => "eq",
            | Self::Lt => "lt",
            | Self::Gt => "gt",
            | Self::ToString => "to_string",
        }
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::VARIANTS.iter().copied().find(|op| op.source_name() == name)
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
        }
    }

    pub const fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt)
    }
}

#[derive(Clone, Copy)]
struct NamedIntrinsicSpec {
    op: IntrinsicOp,
    source_name: &'static str,
    host_name: &'static str,
    arity: usize,
    call_mode: HostCallMode,
    spare_box: Option<SpareBox>,
    may_allocate_gc: bool,
}

macro_rules! define_intrinsic_ops {
    (
        $(
            ($variant:ident, $src:expr, $host:expr, $arity:expr, $mode:ident, $spare:expr, $gc:expr, $abi:expr)
        ),* $(,)?
    ) => {
        /// Pure primitive thunk operations exposed via canonical `@(intrinsic(...))` forms.
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum IntrinsicOp {
            Integer(IntegerType, IntegerIntrinsicOp),
            Float(FloatType, FloatIntrinsicOp),
            $( $variant, )*
        }

        const NAMED_INTRINSIC_SPECS: &[NamedIntrinsicSpec] = &[
            $(
                NamedIntrinsicSpec {
                    op: IntrinsicOp::$variant,
                    source_name: $src,
                    host_name: $host,
                    arity: $arity,
                    call_mode: HostCallMode::$mode,
                    spare_box: $spare,
                    may_allocate_gc: $gc,
                },
            )*
        ];

        impl IntrinsicOp {
            pub const NAMED_VARIANTS: &'static [Self] = &[ $( Self::$variant, )* ];

            const fn named_spec(self) -> &'static NamedIntrinsicSpec {
                #[allow(non_camel_case_types)]
                #[repr(usize)]
                enum NamedSlot { $( $variant, )* }

                let index = match self {
                    | Self::Integer(..) | Self::Float(..) => unreachable!(),
                    $( | Self::$variant => NamedSlot::$variant as usize, )*
                };
                &NAMED_INTRINSIC_SPECS[index]
            }

            pub(crate) fn named_abi(self) -> BuiltinValueClassifier {
                match self {
                    | Self::Integer(..) | Self::Float(..) => unreachable!(),
                    $( | Self::$variant => $abi, )*
                }
            }
        }
    };
}

define_intrinsic_ops! {
    (Int64FromInt, "int64/from_int", "int64_from_int", 1, Returning, Some(SpareBox::Opaque), false, BuiltinOperationAbi::pure([INT], Atom::Integer(IntegerType::Int64))),
    (Int64ToInt, "int64/to_int", "int64_to_int_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::Integer(IntegerType::Int64)], INT)),
    (UInt64FromUInt, "uint64/from_uint", "uint64_from_uint", 1, Returning, Some(SpareBox::Opaque), false, BuiltinOperationAbi::pure([Atom::Integer(IntegerType::UInt)], Atom::Integer(IntegerType::UInt64))),
    (UInt64ToUInt, "uint64/to_uint", "uint64_to_uint_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::Integer(IntegerType::UInt64)], Atom::Integer(IntegerType::UInt))),
    (CharToString, "char/to_string", "char_to_str", 1, Returning, None, true, BuiltinOperationAbi::pure([Atom::Char], Atom::String)),
    (CharCodepoint, "char/codepoint", "char_codepoint", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::Char], INT)),
    (CharFromCodepoint, "char/from_codepoint", "char_from_codepoint_branch", 3, Control, None, false, BuiltinOperationAbi::optional([INT], Atom::Char)),
    (StringLength, "string/length", "str_scalar_length", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::String], INT)),
    (StringByteLength, "string/byte_length", "str_byte_length", 1, Returning, None, false, BuiltinOperationAbi::pure([Atom::String], INT)),
    (StringAppend, "string/append", "str_append", 2, Returning, None, true, BuiltinOperationAbi::pure([Atom::String, Atom::String], Atom::String)),
    (StringSplitOnce, "string/split_once", "str_split_once_branch", 4, Control, None, true, BuiltinOperationAbi::optional_pair([Atom::String, Atom::Char])),
    (StringSplitAt, "string/split_at", "str_split_at_branch", 4, Control, None, true, BuiltinOperationAbi::optional_pair([Atom::String, INT])),
    (StringEq, "string/eq", "str_eq_branch", 4, Control, None, false, BuiltinOperationAbi::branch([Atom::String, Atom::String])),
    (StringGet, "string/get", "str_get_branch", 4, Control, None, false, BuiltinOperationAbi::optional([Atom::String, INT], Atom::Char)),
    (StringParseInt, "string/parse_int", "str_parse_int_branch", 3, Control, None, false, BuiltinOperationAbi::optional([Atom::String], INT)),
}

impl IntrinsicOp {
    pub fn all() -> impl Iterator<Item = Self> {
        IntegerType::VARIANTS
            .iter()
            .copied()
            .flat_map(|integer| {
                IntegerIntrinsicOp::VARIANTS
                    .iter()
                    .copied()
                    .map(move |operation| Self::Integer(integer, operation))
            })
            .chain(FloatType::VARIANTS.iter().copied().flat_map(|float| {
                FloatIntrinsicOp::VARIANTS
                    .iter()
                    .copied()
                    .map(move |operation| Self::Float(float, operation))
            }))
            .chain(Self::NAMED_VARIANTS.iter().copied())
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        if let Some(named) =
            NAMED_INTRINSIC_SPECS.iter().find(|spec| spec.source_name == name).map(|spec| spec.op)
        {
            return Some(named);
        }
        let (prefix, suffix) = name.split_once('/')?;
        if let Some(integer) =
            IntegerType::VARIANTS.iter().copied().find(|ty| ty.source_name() == prefix)
            && let Some(op) = IntegerIntrinsicOp::from_source_name(suffix)
        {
            return Some(Self::Integer(integer, op));
        }
        if let Some(float) =
            FloatType::VARIANTS.iter().copied().find(|ty| ty.source_name() == prefix)
            && let Some(op) = FloatIntrinsicOp::from_source_name(suffix)
        {
            return Some(Self::Float(float, op));
        }
        None
    }

    pub fn source_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) => {
                format!("{}/{}", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) => {
                format!("{}/{}", float.source_name(), operation.source_name())
            }
            | op => op.named_spec().source_name.to_owned(),
        }
    }

    /// Runtime symbol shared with compiled backends and host thunks.
    pub fn host_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) if operation.is_branch() => {
                format!("{}_{}_branch", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) if operation.is_branch() => {
                format!("{}_{}_branch", float.source_name(), operation.source_name())
            }
            | Self::Integer(integer, operation) => {
                format!("{}_{}", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) => {
                format!("{}_{}", float.source_name(), operation.source_name())
            }
            | op => op.named_spec().host_name.to_owned(),
        }
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Integer(_, operation) => operation.arity(),
            | Self::Float(_, operation) => operation.arity(),
            | op => op.named_spec().arity,
        }
    }

    pub const fn call_mode(self) -> HostCallMode {
        match self {
            | Self::Integer(_, operation) => {
                if operation.is_branch() {
                    HostCallMode::Control
                } else {
                    HostCallMode::Returning
                }
            }
            | Self::Float(_, operation) => {
                if operation.is_branch() {
                    HostCallMode::Control
                } else {
                    HostCallMode::Returning
                }
            }
            | op => op.named_spec().call_mode,
        }
    }

    pub const fn spare_box(self) -> Option<SpareBox> {
        match self {
            | Self::Integer(..) | Self::Float(..) => None,
            | op => op.named_spec().spare_box,
        }
    }

    pub const fn may_allocate_gc(self) -> bool {
        match self {
            | Self::Integer(_, operation) => matches!(operation, IntegerIntrinsicOp::ToString),
            | Self::Float(_, operation) => matches!(operation, FloatIntrinsicOp::ToString),
            | op => op.named_spec().may_allocate_gc,
        }
    }
}

impl Display for IntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.source_name())
    }
}

impl BuiltinOperationAbi {
    pub fn for_intrinsic(op: IntrinsicOp) -> Self {
        let classifier = match op {
            | IntrinsicOp::Integer(integer, operation) => {
                let atom = BuiltinValueAtom::Integer(integer);
                match operation {
                    | IntegerIntrinsicOp::Add
                    | IntegerIntrinsicOp::Sub
                    | IntegerIntrinsicOp::Mul
                    | IntegerIntrinsicOp::Div
                    | IntegerIntrinsicOp::Mod => Self::pure([atom, atom], atom),
                    | IntegerIntrinsicOp::Eq | IntegerIntrinsicOp::Lt | IntegerIntrinsicOp::Gt => {
                        Self::branch([atom, atom])
                    }
                    | IntegerIntrinsicOp::ToString => Self::pure([atom], BuiltinValueAtom::String),
                }
            }
            | IntrinsicOp::Float(float, operation) => {
                let atom = BuiltinValueAtom::Float(float);
                match operation {
                    | FloatIntrinsicOp::Add
                    | FloatIntrinsicOp::Sub
                    | FloatIntrinsicOp::Mul
                    | FloatIntrinsicOp::Div => Self::pure([atom, atom], atom),
                    | FloatIntrinsicOp::Eq | FloatIntrinsicOp::Lt | FloatIntrinsicOp::Gt => {
                        Self::branch([atom, atom])
                    }
                    | FloatIntrinsicOp::ToString => Self::pure([atom], BuiltinValueAtom::String),
                }
            }
            | named => named.named_abi(),
        };
        Self { classifier }
    }
}

/// Compiler-supplied host operation invoked by the runtime evaluator or a compiled backend.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum HostOp {
    Intrinsic(IntrinsicOp),
    Builtin(BuiltinValueRole),
}

impl From<IntrinsicOp> for HostOp {
    fn from(op: IntrinsicOp) -> Self {
        Self::Intrinsic(op)
    }
}

impl From<BuiltinValueRole> for HostOp {
    fn from(role: BuiltinValueRole) -> Self {
        Self::Builtin(role)
    }
}

impl HostOp {
    pub fn host_name(self) -> String {
        match self {
            | Self::Intrinsic(op) => op.host_name(),
            | Self::Builtin(role) => role.host_name(),
        }
    }

    pub const fn arity(self) -> usize {
        match self {
            | Self::Intrinsic(op) => op.arity(),
            | Self::Builtin(role) => role.arity(),
        }
    }

    pub const fn call_mode(self) -> HostCallMode {
        match self {
            | Self::Intrinsic(op) => op.call_mode(),
            | Self::Builtin(role) => role.call_mode(),
        }
    }

    pub const fn spare_box(self) -> Option<SpareBox> {
        match self {
            | Self::Intrinsic(op) => op.spare_box(),
            | Self::Builtin(role) => role.spare_box(),
        }
    }

    pub const fn may_allocate_gc(self) -> bool {
        match self {
            | Self::Intrinsic(op) => op.may_allocate_gc(),
            | Self::Builtin(role) => role.may_allocate_gc(),
        }
    }
}

impl Display for HostOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Intrinsic(op) => op.fmt(f),
            | Self::Builtin(role) => role.fmt(f),
        }
    }
}
