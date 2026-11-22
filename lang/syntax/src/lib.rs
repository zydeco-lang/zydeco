pub mod fmt;
pub use fmt::*;

pub mod span;
pub use span::*;

pub mod text;
pub use text::*;

pub mod word;
pub use word::*;

pub mod primitive;
pub use primitive::*;
pub mod scalar;
pub mod memory;

pub mod unit {
    pub use zydeco_abi::unit::*;
}
pub use zydeco_abi::*;

mod impls;
use derive_more::From;
use zydeco_foundation::symbol::Sym;

/* --------------------------------- Binder --------------------------------- */

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct VarName(Sym);
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct FieldName(Sym);
/// `+C`, including the plus
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CtorName(Sym);
/// `.dtor`, including the dot
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct DtorName(Sym);

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
    Int(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt(u64),
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
            | IntegerType::Int => {
                let value = value.try_into().ok()?;
                zydeco_machine::word::RuntimeWord::signed(value)?;
                Self::Int(value)
            }
            | IntegerType::UInt8 => Self::UInt8(value.try_into().ok()?),
            | IntegerType::UInt16 => Self::UInt16(value.try_into().ok()?),
            | IntegerType::UInt32 => Self::UInt32(value.try_into().ok()?),
            | IntegerType::UInt64 => Self::UInt64(value.try_into().ok()?),
            | IntegerType::UInt => {
                let value = value.try_into().ok()?;
                zydeco_machine::word::RuntimeWord::unsigned(value)?;
                Self::UInt(value)
            }
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
            | Self::Int(value) => value.into(),
            | Self::UInt8(value) => value.into(),
            | Self::UInt16(value) => value.into(),
            | Self::UInt32(value) => value.into(),
            | Self::UInt64(value) => value.into(),
            | Self::UInt(value) => value.into(),
            | Self::Unresolved(value) => value,
        }
    }

    pub fn integer_type(self) -> Option<IntegerType> {
        Some(match self {
            | Self::Int8(_) => IntegerType::Int8,
            | Self::Int16(_) => IntegerType::Int16,
            | Self::Int32(_) => IntegerType::Int32,
            | Self::Int64(_) => IntegerType::Int64,
            | Self::Int(_) => IntegerType::Int,
            | Self::UInt8(_) => IntegerType::UInt8,
            | Self::UInt16(_) => IntegerType::UInt16,
            | Self::UInt32(_) => IntegerType::UInt32,
            | Self::UInt64(_) => IntegerType::UInt64,
            | Self::UInt(_) => IntegerType::UInt,
            | Self::Unresolved(_) => return None,
        })
    }

    pub fn to_word_bits(self) -> u64 {
        match self {
            | Self::Int8(value) => value as u8 as u64,
            | Self::Int16(value) => value as u16 as u64,
            | Self::Int32(value) => value as u32 as u64,
            | Self::Int64(value) => value as u64,
            | Self::Int(value) => value as u64,
            | Self::UInt8(value) => value.into(),
            | Self::UInt16(value) => value.into(),
            | Self::UInt32(value) => value.into(),
            | Self::UInt64(value) => value,
            | Self::UInt(value) => value,
            | Self::Unresolved(_) => panic!("unresolved integer literal reached lowering"),
        }
    }
}

impl From<i64> for IntegerLiteral {
    fn from(value: i64) -> Self {
        Self::from_value(value.into(), IntegerType::Int)
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
    fn machine_and_exact_width_integers_have_distinct_representations() {
        for (name, integer) in [("int", IntegerType::Int), ("uint", IntegerType::UInt)] {
            assert_eq!(
                PrimitiveType::from_intrinsic_name(name),
                Some(PrimitiveType::Integer(integer))
            );
            assert_eq!(integer.bits(), 63);
            assert_eq!(integer.storage_bits(), 64);
            for &operation in IntegerOperation::VARIANTS {
                assert_eq!(BuiltinValueRole::Integer(integer, operation).spare_box(), None);
            }
        }
        for (name, integer) in [("i64", IntegerType::Int64), ("u64", IntegerType::UInt64)] {
            assert_eq!(
                PrimitiveType::from_intrinsic_name(name),
                Some(PrimitiveType::Integer(integer))
            );
            assert_eq!(integer.bits(), 64);
            assert_eq!(integer.storage_bits(), 64);
            for &operation in IntegerOperation::VARIANTS {
                assert_eq!(BuiltinValueRole::Integer(integer, operation).spare_box(), None);
            }
        }
        assert_eq!(
            BuiltinValueRole::Float(FloatType::Float64, FloatOperation::Add).spare_box(),
            None
        );
        assert_eq!(BuiltinValueRole::Int64FromInt.spare_box(), Some(SpareBox::Opaque));
        assert_eq!(BuiltinValueRole::UInt64FromUInt.spare_box(), Some(SpareBox::Opaque));
    }

    #[test]
    fn integer_literals_check_payload_ranges() {
        assert_eq!(
            IntegerLiteral::new(-128).with_type(IntegerType::Int8),
            Some(IntegerLiteral::Int8(i8::MIN))
        );
        assert_eq!(
            IntegerLiteral::new(zydeco_machine::word::RuntimeWord::UNSIGNED_MAX.into())
                .with_type(IntegerType::UInt),
            Some(IntegerLiteral::UInt(zydeco_machine::word::RuntimeWord::UNSIGNED_MAX))
        );
        assert_eq!(IntegerLiteral::new(u64::MAX.into()).with_type(IntegerType::UInt), None);
        assert_eq!(IntegerLiteral::new(i64::MAX.into()).with_type(IntegerType::Int), None);
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
            "int", "float", "int8", "int16", "int32", "uint8", "uint16", "uint32", "uint",
            "float32", "float64", "char", "string", "bytes",
        ]
        .into_iter()
        .for_each(|name| {
            assert_eq!(BuiltinTypeRole::from_source_name(name), None);
        });
        ["add", "int128_eq", "uint128_add", "float_add"].into_iter().for_each(|name| {
            assert_eq!(BuiltinValueRole::from_source_name(name), None);
        });
    }
}
