pub mod fmt;
pub use fmt::*;

pub mod span;
pub use span::*;

pub mod text;
pub use text::*;

mod impls;
use derive_more::From;
use strum::{IntoEnumIterator, VariantArray as _};

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymName(pub String);
/// `+C`, including the plus
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CtorName(pub String);
/// `.dtor`, including the dot
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DtorName(pub String);

/* ------------------------------- Structural ------------------------------- */

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ann<Tm, Ty> {
    pub tm: Tm,
    pub ty: Ty,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Hole;

/// `field = inner` introduces a term carrying `field`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Named<Tag, Inner>(pub Tag, pub Inner);

/// `field :: inner` classifies a term carrying `field`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Label<Tag, Inner>(pub Tag, pub Inner);

/// any binding structure
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Abs<S, T>(pub S, pub T);
/// `e1 e2` shaped application
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct App<S, T>(pub S, pub T);

/// a -> b shaped arrow
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Arrow<S, T>(pub S, pub T);

/// unary arrow that applies the same component
pub type ArrowU<T> = Arrow<T, T>;

/// A binary cons cell used by internal structures.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Cons<S, T>(pub S, pub T);

/// `()` as the nullary value constructor.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Triv;

/// A non-empty n-ary cons cell with a distinguished final element.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConsN<S, T>(pub Vec<S>, pub T);

/// Several patterns that all observe the same bindee, in source order.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Alias<T>(pub ConsN<T, T>);

/// `/field = pattern` searches one bindee for a uniquely named payload.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ProjectionPattern<Tag, Inner>(pub Tag, pub Inner);

/// an infix n-ary product: `a * b * c` is one product over its components
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Prod<T>(pub Vec<T>);

/// sealed term which is abstract, only eq to itself during tyck
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Sealed<T>(pub T);

/* ---------------------------------- Meta ---------------------------------- */

/// Compiler-defined identities for the intrinsic CBPV structure.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumDiscriminants,
    strum::EnumIter,
)]
#[strum_discriminants(derive(strum::IntoStaticStr))]
#[strum_discriminants(strum(serialize_all = "lowercase"))]
#[strum_discriminants(vis(pub(crate)))]
pub enum IntrinsicRole {
    VType,
    CType,
    Thk,
    Ret,
    Unit,
    #[strum(disabled)]
    Primitive(PrimitiveType),
}

impl IntrinsicRole {
    /// Every source-spellable intrinsic role, in completion order.
    pub fn all() -> impl Iterator<Item = Self> {
        Self::iter().chain(PrimitiveType::all().map(Self::Primitive))
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        Self::all().find(|role| role.source_name() == name)
    }

    pub fn source_name(self) -> &'static str {
        match self {
            | Self::Primitive(primitive) => primitive.intrinsic_name(),
            | role => IntrinsicRoleDiscriminants::from(role).into(),
        }
    }
}

impl std::fmt::Display for IntrinsicRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
    }
}

/// Compiler-defined roles for host-provided abstract types in the Builtin
/// package signature.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinTypeRole {
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
    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }

    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn universe(self) -> BuiltinTypeUniverse {
        match self {
            | Self::Reader | Self::Writer => BuiltinTypeUniverse::Value,
            | Self::OS => BuiltinTypeUniverse::Computation,
        }
    }
}

impl std::fmt::Display for BuiltinTypeRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
    }
}

/// The concrete representation selected for an integer literal or operation.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum IntegerType {
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
}

impl IntegerType {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn type_name(self) -> &'static str {
        match self {
            | Self::Int8 => "Int8",
            | Self::Int16 => "Int16",
            | Self::Int32 => "Int32",
            | Self::Int64 => "Int64",
            | Self::UInt8 => "UInt8",
            | Self::UInt16 => "UInt16",
            | Self::UInt32 => "UInt32",
            | Self::UInt64 => "UInt64",
        }
    }

    pub fn is_signed(self) -> bool {
        matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }
}

impl std::fmt::Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

/// The concrete IEEE-754 representation selected for a floating-point literal
/// or operation.
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum FloatType {
    Float32,
    #[default]
    Float64,
}

impl FloatType {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn type_name(self) -> &'static str {
        match self {
            | Self::Float32 => "Float32",
            | Self::Float64 => "Float64",
        }
    }
}

impl std::fmt::Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

/// A fixed host representation with an applicative static identity.
///
/// Repeating an intrinsic primitive splice denotes the same type. This lets
/// independently assembled packages share exact Rust-compatible scalar and
/// buffer representations without placing every type in one generative
/// existential telescope.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumDiscriminants,
    strum::EnumIter,
)]
#[strum_discriminants(derive(strum::IntoStaticStr))]
#[strum_discriminants(strum(serialize_all = "lowercase"))]
#[strum_discriminants(vis(pub(crate)))]
pub enum PrimitiveType {
    #[strum(disabled)]
    Integer(IntegerType),
    #[strum(disabled)]
    Float(FloatType),
    Char,
    String,
    Bytes,
}

impl PrimitiveType {
    pub fn all() -> impl Iterator<Item = Self> {
        IntegerType::VARIANTS
            .iter()
            .copied()
            .map(Self::Integer)
            .chain(FloatType::VARIANTS.iter().copied().map(Self::Float))
            .chain(Self::iter())
    }

    pub fn from_intrinsic_name(name: &str) -> Option<Self> {
        Self::all().find(|primitive| primitive.intrinsic_name() == name)
    }

    pub fn intrinsic_name(self) -> &'static str {
        match self {
            | Self::Integer(IntegerType::Int8) => "i8",
            | Self::Integer(IntegerType::Int16) => "i16",
            | Self::Integer(IntegerType::Int32) => "i32",
            | Self::Integer(IntegerType::Int64) => "i64",
            | Self::Integer(IntegerType::UInt8) => "u8",
            | Self::Integer(IntegerType::UInt16) => "u16",
            | Self::Integer(IntegerType::UInt32) => "u32",
            | Self::Integer(IntegerType::UInt64) => "u64",
            | Self::Float(FloatType::Float32) => "f32",
            | Self::Float(FloatType::Float64) => "f64",
            | primitive => PrimitiveTypeDiscriminants::from(primitive).into(),
        }
    }

    pub fn type_name(self) -> &'static str {
        match self {
            | Self::Integer(integer) => integer.type_name(),
            | Self::Float(float) => float.type_name(),
            | Self::Char => "Char",
            | Self::String => "String",
            | Self::Bytes => "Bytes",
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "snake_case")]
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
}

impl IntegerOperation {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }

    pub fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
        }
    }

    pub fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt)
    }
}

#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "snake_case")]
pub enum FloatOperation {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    ToString,
}

impl FloatOperation {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }

    pub fn arity(self) -> usize {
        match self {
            | Self::Add | Self::Sub | Self::Mul | Self::Div => 2,
            | Self::Eq | Self::Lt | Self::Gt => 4,
            | Self::ToString => 1,
        }
    }

    pub fn is_branch(self) -> bool {
        matches!(self, Self::Eq | Self::Lt | Self::Gt)
    }
}

/// Compiler-defined roles that may be assigned to host-provided value entries
/// in the Builtin package signature.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumDiscriminants,
    strum::EnumIter,
)]
#[strum_discriminants(derive(strum::IntoStaticStr))]
#[strum_discriminants(strum(serialize_all = "snake_case"))]
#[strum_discriminants(vis(pub(crate)))]
pub enum BuiltinValueRole {
    #[strum(disabled)]
    Integer(IntegerType, IntegerOperation),
    #[strum(disabled)]
    Float(FloatType, FloatOperation),
    StrScalarLength,
    StrByteLength,
    StrAppend,
    StrSplitOnce,
    StrSplitAt,
    StrEq,
    StrGet,
    CharToStr,
    CharCodepoint,
    CharFromCodepoint,
    StrParseInt,
    BytesEmpty,
    BytesLength,
    BytesAppend,
    BytesFromStr,
    BytesToStr,
    BytesGet,
    BytesSlice,
    BytesSingleton,
    BytesEq,
    BytesLt,
    Stdin,
    Stdout,
    Stderr,
    IoRead,
    IoReadLine,
    IoReadAll,
    IoWriteAll,
    IoFlush,
    IoCloseReader,
    IoCloseWriter,
    FsOpenReader,
    FsCreateWriter,
    FsAppendWriter,
    WriteStr,
    WriteInt,
    WriteLine,
    ReadLine,
    ReadLineAsInt,
    ReadTillEof,
    ArgList,
    RandomInt,
    Exit,
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
            .chain(Self::iter())
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
        Self::iter().find(|role| {
            let source_name: &'static str = BuiltinValueRoleDiscriminants::from(*role).into();
            source_name == name
        })
    }

    pub fn source_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) => {
                format!("{}_{}", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) => {
                format!("{}_{}", float.source_name(), operation.source_name())
            }
            | role => {
                let source_name: &'static str = BuiltinValueRoleDiscriminants::from(role).into();
                source_name.to_owned()
            }
        }
    }

    /// Runtime symbol used when the role is materialized in the foundational
    /// Builtin package. This may differ from the source annotation name when
    /// the package uses a representation-independent classifier.
    pub fn host_name(self) -> String {
        match self {
            | Self::Integer(integer, operation) if operation.is_branch() => {
                format!("{}_{}_branch", integer.source_name(), operation.source_name())
            }
            | Self::Float(float, operation) if operation.is_branch() => {
                format!("{}_{}_branch", float.source_name(), operation.source_name())
            }
            | Self::StrSplitOnce => "str_split_once_branch".to_owned(),
            | Self::StrSplitAt => "str_split_at_branch".to_owned(),
            | Self::StrEq => "str_eq_branch".to_owned(),
            | Self::StrGet => "str_get_branch".to_owned(),
            | Self::CharFromCodepoint => "char_from_codepoint_branch".to_owned(),
            | Self::StrParseInt => "str_parse_int_branch".to_owned(),
            | Self::BytesToStr => "bytes_to_str_branch".to_owned(),
            | Self::BytesGet => "bytes_get_branch".to_owned(),
            | Self::BytesSlice => "bytes_slice_branch".to_owned(),
            | Self::BytesEq => "bytes_eq_branch".to_owned(),
            | Self::BytesLt => "bytes_lt_branch".to_owned(),
            | Self::ReadLineAsInt => "read_line_as_int_branch".to_owned(),
            | Self::ArgList => "arg_fold".to_owned(),
            | role => role.source_name(),
        }
    }

    pub fn arity(self) -> usize {
        match self {
            | Self::Integer(_, operation) => operation.arity(),
            | Self::Float(_, operation) => operation.arity(),
            | Self::BytesEmpty | Self::Stdin | Self::Stdout | Self::Stderr => 0,
            | Self::StrScalarLength
            | Self::StrByteLength
            | Self::CharToStr
            | Self::CharCodepoint
            | Self::BytesLength
            | Self::BytesFromStr
            | Self::BytesSingleton
            | Self::ReadLine
            | Self::ReadTillEof
            | Self::RandomInt
            | Self::Exit => 1,
            | Self::StrAppend
            | Self::BytesAppend
            | Self::WriteStr
            | Self::WriteInt
            | Self::WriteLine
            | Self::ReadLineAsInt
            | Self::ArgList => 2,
            | Self::CharFromCodepoint
            | Self::StrParseInt
            | Self::BytesToStr
            | Self::IoReadAll
            | Self::IoFlush
            | Self::IoCloseReader
            | Self::IoCloseWriter
            | Self::FsOpenReader
            | Self::FsCreateWriter
            | Self::FsAppendWriter => 3,
            | Self::StrSplitOnce
            | Self::StrSplitAt
            | Self::StrEq
            | Self::StrGet
            | Self::BytesGet
            | Self::BytesEq
            | Self::BytesLt
            | Self::IoRead
            | Self::IoReadLine
            | Self::IoWriteAll => 4,
            | Self::BytesSlice => 5,
        }
    }
}

impl std::fmt::Display for BuiltinValueRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
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

impl std::fmt::Display for BuiltinRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
    }
}

/* ---------------------------------- FFI ----------------------------------- */

/// Calling convention used by one foreign import.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum ForeignAbi {
    C,
}

impl ForeignAbi {
    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }

    pub fn source_name(self) -> &'static str {
        self.into()
    }
}

impl std::fmt::Display for ForeignAbi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.source_name())
    }
}

/// A platform linker name, as used by `-l<name>`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

impl std::fmt::Display for ForeignLibraryName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// An unmangled C external symbol.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

impl std::fmt::Display for ForeignSymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Link-time identity of a foreign function, before its Zydeco classifier is interpreted.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ForeignTarget {
    pub abi: ForeignAbi,
    pub library: ForeignLibraryName,
    pub symbol: ForeignSymbolName,
}

/// One source-level parameter whose C representation is known to the compiler.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ForeignParameter {
    /// Borrow an immutable `Bytes` value as `const void *` plus `size_t` for the duration of a call.
    BorrowedBytes,
    UInt64,
}

/// One source-level foreign result whose C representation is known to the compiler.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ForeignResult {
    UInt64,
}

/// The marshalling protocol derived from a checked CBPV classifier.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ForeignSignature {
    pub parameters: Vec<ForeignParameter>,
    pub result: ForeignResult,
}

/// A validated foreign target paired with its source-to-C marshalling protocol.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ForeignImport {
    pub target: ForeignTarget,
    pub signature: ForeignSignature,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Meta {
    Ident(String),
    String(String),
    Integer(i64),
    Apply { callee: String, args: Vec<Meta> },
}

/// Assigns a downstream, typed meaning to structurally parsed metadata.
///
/// The syntax crate recognizes only a callee and recursively structured
/// arguments. Implementors choose a callee name and validate its arguments
/// without adding concrete annotation variants to [`Meta`].
pub trait SpecializeMeta: Sized {
    type Error;

    fn name() -> &'static str;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error>;
}

impl Meta {
    pub fn ident(name: impl Into<String>) -> Self {
        Self::Ident(name.into())
    }

    pub fn string(value: impl Into<String>) -> Self {
        Self::String(value.into())
    }

    pub fn integer(value: i64) -> Self {
        Self::Integer(value)
    }

    pub fn apply(callee: impl Into<String>, args: impl IntoIterator<Item = Self>) -> Self {
        Self::Apply { callee: callee.into(), args: args.into_iter().collect() }
    }

    pub fn callee(&self) -> Option<&str> {
        match self {
            | Self::Ident(name) | Self::Apply { callee: name, .. } => Some(name),
            | Self::String(_) | Self::Integer(_) => None,
        }
    }

    pub fn arguments(&self) -> &[Self] {
        match self {
            | Self::Apply { args, .. } => args,
            | Self::Ident(_) | Self::String(_) | Self::Integer(_) => &[],
        }
    }

    pub fn is(&self, name: &str) -> bool {
        self.callee() == Some(name)
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            | Self::String(value) => Some(value),
            | Self::Ident(_) | Self::Integer(_) | Self::Apply { .. } => None,
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            | Self::Integer(value) => Some(*value),
            | Self::Ident(_) | Self::String(_) | Self::Apply { .. } => None,
        }
    }

    pub fn as_ident(&self) -> Option<&str> {
        match self {
            | Self::Ident(value) => Some(value),
            | Self::String(_) | Self::Integer(_) | Self::Apply { .. } => None,
        }
    }

    pub fn specialize<S>(&self) -> Result<Option<S>, S::Error>
    where
        S: SpecializeMeta,
    {
        if self.is(S::name()) { S::from_arguments(self.arguments()).map(Some) } else { Ok(None) }
    }
}

impl std::fmt::Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Ident(name) => write!(f, "{name}"),
            | Self::String(value) => write!(f, "{value:?}"),
            | Self::Integer(value) => write!(f, "{value}"),
            | Self::Apply { callee, args } => write!(
                f,
                "{callee}({})",
                args.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")
            ),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MetaT<T>(pub Meta, pub T);

/// A compiler-internal boundary around one imported source term.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SourceBoundary<T>(pub T);

/// A compiler-internal boundary requiring one signature source to be a type term.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SignatureBoundary<T>(pub T);

/* --------------------------------- Common --------------------------------- */

/// `{ b }` has type `Thk B`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Thunk<Tm>(pub Tm);
/// `! a` has type `B` where `A = Thk B`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Force<Tm>(pub Tm);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Return<Tm>(pub Tm);
/// `do x <- b; ...`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bind<Br, Be, Tail> {
    pub binder: Br,
    pub bindee: Be,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Let<Br, Be, Tail> {
    pub binder: Br,
    pub bindee: Be,
    pub tail: Tail,
}

/// `fix (x: A) => b`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Fix<P, Tm>(pub P, pub Tm);

/// `C(a_1, ...)`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ctor<Tag, Tail>(pub Tag, pub Tail);

/// `match a | p => b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match<Sc, Br, Tail> {
    pub scrut: Sc,
    pub arms: Vec<Matcher<Br, Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Br, Tail> {
    pub binder: Br,
    pub tail: Tail,
}

/// `comatch | .d_1 => b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch<Tag, Tail> {
    pub arms: Vec<CoMatcher<Tag, Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tag, Tail> {
    pub dtor: Tag,
    pub tail: Tail,
}

/// `b .d_i`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Dtor<Head, Tag>(pub Head, pub Tag);

/// `term/field`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Proj<Head, Tag>(pub Head, pub Tag);

/// literals in term
#[derive(From, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Literal {
    Integer(IntegerLiteral),
    Float(FloatLiteral),
    String(Utf8String),
    Char(char),
}

/// An integer literal represented by the corresponding Rust integer type after
/// checking. `Unresolved` exists only between parsing and type checking.
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum IntegerLiteral {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Unresolved(i128),
}

impl IntegerLiteral {
    pub fn new(value: i128) -> Self {
        Self::Unresolved(value)
    }

    pub fn with_type(self, integer_type: IntegerType) -> Option<Self> {
        let value = self.value();
        Some(match integer_type {
            | IntegerType::Int8 => Self::Int8(value.try_into().ok()?),
            | IntegerType::Int16 => Self::Int16(value.try_into().ok()?),
            | IntegerType::Int32 => Self::Int32(value.try_into().ok()?),
            | IntegerType::Int64 => Self::Int64(value.try_into().ok()?),
            | IntegerType::UInt8 => Self::UInt8(value.try_into().ok()?),
            | IntegerType::UInt16 => Self::UInt16(value.try_into().ok()?),
            | IntegerType::UInt32 => Self::UInt32(value.try_into().ok()?),
            | IntegerType::UInt64 => Self::UInt64(value.try_into().ok()?),
        })
    }

    pub fn from_value(value: i128, integer_type: IntegerType) -> Self {
        Self::new(value)
            .with_type(integer_type)
            .expect("integer primitive produced a value outside its representation")
    }

    pub fn value(self) -> i128 {
        match self {
            | Self::Int8(value) => value.into(),
            | Self::Int16(value) => value.into(),
            | Self::Int32(value) => value.into(),
            | Self::Int64(value) => value.into(),
            | Self::UInt8(value) => value.into(),
            | Self::UInt16(value) => value.into(),
            | Self::UInt32(value) => value.into(),
            | Self::UInt64(value) => value.into(),
            | Self::Unresolved(value) => value,
        }
    }

    pub fn integer_type(self) -> Option<IntegerType> {
        Some(match self {
            | Self::Int8(_) => IntegerType::Int8,
            | Self::Int16(_) => IntegerType::Int16,
            | Self::Int32(_) => IntegerType::Int32,
            | Self::Int64(_) => IntegerType::Int64,
            | Self::UInt8(_) => IntegerType::UInt8,
            | Self::UInt16(_) => IntegerType::UInt16,
            | Self::UInt32(_) => IntegerType::UInt32,
            | Self::UInt64(_) => IntegerType::UInt64,
            | Self::Unresolved(_) => return None,
        })
    }

    pub fn to_word_bits(self) -> u64 {
        match self {
            | Self::Int8(value) => value as u8 as u64,
            | Self::Int16(value) => value as u16 as u64,
            | Self::Int32(value) => value as u32 as u64,
            | Self::Int64(value) => value as u64,
            | Self::UInt8(value) => value.into(),
            | Self::UInt16(value) => value.into(),
            | Self::UInt32(value) => value.into(),
            | Self::UInt64(value) => value,
            | Self::Unresolved(_) => panic!("unresolved integer literal reached lowering"),
        }
    }
}

impl From<i64> for IntegerLiteral {
    fn from(value: i64) -> Self {
        Self::Int64(value)
    }
}

impl std::fmt::Debug for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value().fmt(f)
    }
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value().fmt(f)
    }
}

/// An IEEE-754 binary32 or binary64 literal with bitwise equality and hashing.
///
/// Keeping the bits as the structural representation preserves signed zero and
/// NaN payloads while allowing literals to participate in syntax identities.
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum FloatLiteral {
    Float32(u32),
    Float64(u64),
}

impl Default for FloatLiteral {
    fn default() -> Self {
        Self::from(0.0)
    }
}

impl FloatLiteral {
    pub fn from_bits(bits: u64) -> Self {
        Self::Float64(bits)
    }

    pub fn from_f32_bits(bits: u32) -> Self {
        Self::Float32(bits)
    }

    pub fn to_bits(self) -> u64 {
        match self {
            | Self::Float32(bits) => bits.into(),
            | Self::Float64(bits) => bits,
        }
    }

    pub fn value(self) -> f64 {
        match self {
            | Self::Float32(bits) => f32::from_bits(bits).into(),
            | Self::Float64(bits) => f64::from_bits(bits),
        }
    }

    pub fn float_type(self) -> FloatType {
        match self {
            | Self::Float32(_) => FloatType::Float32,
            | Self::Float64(_) => FloatType::Float64,
        }
    }

    pub fn with_type(self, float_type: FloatType) -> Option<Self> {
        let value = self.value();
        match float_type {
            | FloatType::Float32 => {
                let narrowed = value as f32;
                (!value.is_finite() || narrowed.is_finite())
                    .then(|| Self::from_f32_bits(narrowed.to_bits()))
            }
            | FloatType::Float64 => Some(Self::from_bits(value.to_bits())),
        }
    }
}

impl From<f64> for FloatLiteral {
    fn from(value: f64) -> Self {
        Self::from_bits(value.to_bits())
    }
}

impl std::fmt::Debug for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value().fmt(f)
    }
}

impl std::fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value().fmt(f)
    }
}

#[cfg(test)]
mod numeric_tests {
    use super::*;

    #[test]
    fn integer_literals_use_exact_rust_carriers() {
        assert_eq!(
            IntegerLiteral::new(-128).with_type(IntegerType::Int8),
            Some(IntegerLiteral::Int8(i8::MIN))
        );
        assert_eq!(
            IntegerLiteral::new(u64::MAX.into()).with_type(IntegerType::UInt64),
            Some(IntegerLiteral::UInt64(u64::MAX))
        );
        assert_eq!(IntegerLiteral::new(128).with_type(IntegerType::Int8), None);
        assert_eq!(IntegerLiteral::new(-1).with_type(IntegerType::UInt8), None);
    }

    #[test]
    fn float_literals_preserve_the_selected_rust_width() {
        let literal = FloatLiteral::from(1.5);
        let narrowed = literal.with_type(FloatType::Float32).unwrap();

        assert_eq!(narrowed, FloatLiteral::Float32(1.5_f32.to_bits()));
        assert_eq!(narrowed.float_type(), FloatType::Float32);
        assert_eq!(literal.float_type(), FloatType::Float64);
    }

    #[test]
    fn fixed_primitive_builtin_type_roles_are_retired() {
        [
            "int", "float", "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32",
            "uint64", "float32", "float64", "char", "string", "bytes",
        ]
        .into_iter()
        .for_each(|name| {
            assert_eq!(BuiltinTypeRole::from_source_name(name), None);
        });
        ["add", "int_eq", "float_add"].into_iter().for_each(|name| {
            assert_eq!(BuiltinValueRole::from_source_name(name), None);
        });
    }
}
