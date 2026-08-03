pub mod fmt;
pub use fmt::*;

pub mod span;
pub use span::*;

mod impls;
use derive_more::From;

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

/// a * b shaped product
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Prod<S, T>(pub S, pub T);

/// unary product that applies the same component
pub type ProdU<T> = Prod<T, T>;

/// sealed term which is abstract, only eq to itself during tyck
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Sealed<T>(pub T);

/* ---------------------------------- Meta ---------------------------------- */

/// Compiler-defined identities for the intrinsic CBPV structure.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntrinsicRole {
    VType,
    CType,
    Thk,
    Ret,
    Unit,
}

impl IntrinsicRole {
    pub fn from_source_name(name: &str) -> Option<Self> {
        match name {
            | "vtype" => Some(Self::VType),
            | "ctype" => Some(Self::CType),
            | "thk" => Some(Self::Thk),
            | "ret" => Some(Self::Ret),
            | "unit" => Some(Self::Unit),
            | _ => None,
        }
    }

    pub fn source_name(self) -> &'static str {
        match self {
            | Self::VType => "vtype",
            | Self::CType => "ctype",
            | Self::Thk => "thk",
            | Self::Ret => "ret",
            | Self::Unit => "unit",
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
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinTypeRole {
    Int,
    Char,
    String,
    OS,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinTypeUniverse {
    Value,
    Computation,
}

impl BuiltinTypeRole {
    pub fn from_source_name(name: &str) -> Option<Self> {
        match name {
            | "int" => Some(Self::Int),
            | "char" => Some(Self::Char),
            | "string" => Some(Self::String),
            | "os" => Some(Self::OS),
            | _ => None,
        }
    }

    pub fn source_name(self) -> &'static str {
        match self {
            | Self::Int => "int",
            | Self::Char => "char",
            | Self::String => "string",
            | Self::OS => "os",
        }
    }

    pub fn universe(self) -> BuiltinTypeUniverse {
        match self {
            | Self::Int | Self::Char | Self::String => BuiltinTypeUniverse::Value,
            | Self::OS => BuiltinTypeUniverse::Computation,
        }
    }
}

impl std::fmt::Display for BuiltinTypeRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
    }
}

/// Compiler-defined roles that may be assigned to host-provided value entries
/// in the Builtin package signature.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinValueRole {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    IntEq,
    IntLt,
    IntGt,
    StrLength,
    StrAppend,
    StrSplitOnce,
    StrSplitN,
    StrEq,
    StrIndex,
    IntToStr,
    CharToStr,
    CharToInt,
    StrToInt,
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
    pub const ALL: &'static [Self] = &[
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::Mod,
        Self::IntEq,
        Self::IntLt,
        Self::IntGt,
        Self::StrLength,
        Self::StrAppend,
        Self::StrSplitOnce,
        Self::StrSplitN,
        Self::StrEq,
        Self::StrIndex,
        Self::IntToStr,
        Self::CharToStr,
        Self::CharToInt,
        Self::StrToInt,
        Self::WriteStr,
        Self::WriteInt,
        Self::WriteLine,
        Self::ReadLine,
        Self::ReadLineAsInt,
        Self::ReadTillEof,
        Self::ArgList,
        Self::RandomInt,
        Self::Exit,
    ];

    pub fn from_source_name(name: &str) -> Option<Self> {
        match name {
            | "add" => Some(Self::Add),
            | "sub" => Some(Self::Sub),
            | "mul" => Some(Self::Mul),
            | "div" => Some(Self::Div),
            | "mod" => Some(Self::Mod),
            | "int_eq" => Some(Self::IntEq),
            | "int_lt" => Some(Self::IntLt),
            | "int_gt" => Some(Self::IntGt),
            | "str_length" => Some(Self::StrLength),
            | "str_append" => Some(Self::StrAppend),
            | "str_split_once" => Some(Self::StrSplitOnce),
            | "str_split_n" => Some(Self::StrSplitN),
            | "str_eq" => Some(Self::StrEq),
            | "str_index" => Some(Self::StrIndex),
            | "int_to_str" => Some(Self::IntToStr),
            | "char_to_str" => Some(Self::CharToStr),
            | "char_to_int" => Some(Self::CharToInt),
            | "str_to_int" => Some(Self::StrToInt),
            | "write_str" => Some(Self::WriteStr),
            | "write_int" => Some(Self::WriteInt),
            | "write_line" => Some(Self::WriteLine),
            | "read_line" => Some(Self::ReadLine),
            | "read_line_as_int" => Some(Self::ReadLineAsInt),
            | "read_till_eof" => Some(Self::ReadTillEof),
            | "arg_list" => Some(Self::ArgList),
            | "random_int" => Some(Self::RandomInt),
            | "exit" => Some(Self::Exit),
            | _ => None,
        }
    }

    pub fn source_name(self) -> &'static str {
        match self {
            | Self::Add => "add",
            | Self::Sub => "sub",
            | Self::Mul => "mul",
            | Self::Div => "div",
            | Self::Mod => "mod",
            | Self::IntEq => "int_eq",
            | Self::IntLt => "int_lt",
            | Self::IntGt => "int_gt",
            | Self::StrLength => "str_length",
            | Self::StrAppend => "str_append",
            | Self::StrSplitOnce => "str_split_once",
            | Self::StrSplitN => "str_split_n",
            | Self::StrEq => "str_eq",
            | Self::StrIndex => "str_index",
            | Self::IntToStr => "int_to_str",
            | Self::CharToStr => "char_to_str",
            | Self::CharToInt => "char_to_int",
            | Self::StrToInt => "str_to_int",
            | Self::WriteStr => "write_str",
            | Self::WriteInt => "write_int",
            | Self::WriteLine => "write_line",
            | Self::ReadLine => "read_line",
            | Self::ReadLineAsInt => "read_line_as_int",
            | Self::ReadTillEof => "read_till_eof",
            | Self::ArgList => "arg_list",
            | Self::RandomInt => "random_int",
            | Self::Exit => "exit",
        }
    }

    /// Runtime symbol used when the role is materialized in the foundational
    /// Builtin package. This may differ from the legacy external name when the
    /// package uses a representation-independent classifier.
    pub fn host_name(self) -> &'static str {
        match self {
            | Self::IntEq => "int_eq_branch",
            | Self::IntLt => "int_lt_branch",
            | Self::IntGt => "int_gt_branch",
            | Self::StrSplitOnce => "str_split_once_branch",
            | Self::StrSplitN => "str_split_n_branch",
            | Self::StrEq => "str_eq_branch",
            | Self::ReadLineAsInt => "read_line_as_int_branch",
            | Self::ArgList => "arg_fold",
            | role => role.source_name(),
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
    pub fn from_source_name(name: &str) -> Option<Self> {
        BuiltinTypeRole::from_source_name(name)
            .map(Self::Type)
            .or_else(|| BuiltinValueRole::from_source_name(name).map(Self::Value))
    }

    pub fn source_name(self) -> &'static str {
        match self {
            | Self::Type(role) => role.source_name(),
            | Self::Value(role) => role.source_name(),
        }
    }
}

impl std::fmt::Display for BuiltinRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_name())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Meta {
    Ident(String),
    String(String),
    Apply { callee: String, args: Vec<Meta> },
}

/// Assigns a downstream, typed meaning to structurally parsed metadata.
///
/// The syntax crate recognizes only a callee and recursively structured
/// arguments. Implementors choose a callee name and validate its arguments
/// without adding concrete annotation variants to [`Meta`].
pub trait SpecializeMeta: Sized {
    const NAME: &'static str;
    type Error;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error>;
}

impl Meta {
    pub fn ident(name: impl Into<String>) -> Self {
        Self::Ident(name.into())
    }

    pub fn string(value: impl Into<String>) -> Self {
        Self::String(value.into())
    }

    pub fn apply(callee: impl Into<String>, args: impl IntoIterator<Item = Self>) -> Self {
        Self::Apply { callee: callee.into(), args: args.into_iter().collect() }
    }

    pub fn callee(&self) -> Option<&str> {
        match self {
            | Self::Ident(name) | Self::Apply { callee: name, .. } => Some(name),
            | Self::String(_) => None,
        }
    }

    pub fn arguments(&self) -> &[Self] {
        match self {
            | Self::Apply { args, .. } => args,
            | Self::Ident(_) | Self::String(_) => &[],
        }
    }

    pub fn is(&self, name: &str) -> bool {
        self.callee() == Some(name)
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            | Self::String(value) => Some(value),
            | Self::Ident(_) | Self::Apply { .. } => None,
        }
    }

    pub fn as_ident(&self) -> Option<&str> {
        match self {
            | Self::Ident(value) => Some(value),
            | Self::String(_) | Self::Apply { .. } => None,
        }
    }

    pub fn specialize<S>(&self) -> Result<Option<S>, S::Error>
    where
        S: SpecializeMeta,
    {
        if self.is(S::NAME) { S::from_arguments(self.arguments()).map(Some) } else { Ok(None) }
    }
}

impl std::fmt::Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Ident(name) => write!(f, "{name}"),
            | Self::String(value) => write!(f, "{value:?}"),
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

/// `fix (x: A) -> b`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Fix<P, Tm>(pub P, pub Tm);

/// `C(a_1, ...)`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ctor<Tag, Tail>(pub Tag, pub Tail);

/// `match a | p -> b_1 | ... end`
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

/// `comatch | .d_1 -> b_1 | ... end`
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
    Int(i64),
    // Note: for real dude?
    String(Vec<char>),
    Char(char),
}
