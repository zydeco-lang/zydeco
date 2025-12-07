//! There are three sorts in syntax of ZASM:
//! 
//! - Variables, whose values are dynamically assigned.
//! - Symbols, whose values are statically assigned.
//! - Programs, which are sequences of instructions.

pub use super::arena::*;
pub use zydeco_syntax::*;
pub use zydeco_utils::arena::*;

use derive_more::From;

new_key_type! {
    pub struct VarId;
    pub struct SymId;
    pub struct ProgId;
}

/// Computations in ZIR are programs in ZASM.
#[derive(From, Clone, Debug)]
pub enum Program {
    Instruction(Instruction, ProgId),
    Jump(Jump),
    PopJump(PopJump),
    Branch(Branch),
    Panic(Panic),
    /* ----------------------- these will be compiled away ---------------------- */
    Call(Call),
    Return(Return<Atom>),
    Bind(Bind<(), ProgId, ProgId>),
}

#[derive(Clone, Debug)]
pub struct PopJump;

#[derive(Clone, Debug)]
pub struct Panic;

/// Stack transformations in ZIR.
#[derive(Clone, Debug)]
pub enum Instruction {
    PackProduct(Pack<Product>),
    UnpackProduct(Unpack<Product>),
    PackContext(Pack<Context>),
    UnpackContext(Unpack<Context>),
    PackClosure(Pack<Closure>),
    UnpackClosure(Unpack<Closure>),
    PushArg(Push<Atom>),
    PopArg(Pop<VarId>),
    /// Destructed by [`Branch`]
    PushTag(Push<Tag>),
    Clear(Context),
}

#[derive(Clone, Debug)]
pub struct Pack<T>(pub T);
#[derive(Clone, Debug)]
pub struct Unpack<T>(pub T);
#[derive(Clone, Debug)]
pub struct Push<T>(pub T);
#[derive(Clone, Debug)]
pub struct Pop<T>(pub T);

#[derive(Clone, Debug)]
pub struct Product;

#[derive(Clone, Debug)]
pub struct Closure(pub Context);

#[derive(Clone, Debug)]
pub struct Call;

#[derive(Clone, Debug)]
pub struct Jump(pub ProgId);
#[derive(Clone, Debug)]
pub struct Branch(pub Vec<(Tag, ProgId)>);

#[derive(Clone, Debug)]
pub struct Label(pub String);
#[derive(Clone, Debug)]
pub struct Tag {
    pub idx: usize,
    pub name: Option<String>,
}

/// Values in ZIR.
#[derive(Clone, Debug)]
pub enum Atom {
    Var(VarId),
    Label(ProgId),
    Literal(Literal),
    External(String),
}

/// Contexts are ordered sets of variables.
pub type Context = zydeco_utils::context::Context<VarId>;

/// CoContexts are unordered sets of variables.
pub type CoContext = zydeco_utils::context::CoContext<VarId>;
