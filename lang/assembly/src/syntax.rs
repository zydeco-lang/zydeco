pub use zydeco_syntax::*;
pub use zydeco_utils::arena::*;
pub use super::arena::*;

new_key_type! {
    pub struct VarId;
    pub struct ProgId;
}

/// Computations in ZIR.
#[derive(Clone, Debug)]
pub enum Program {
    Instruction(Instruction, Box<Program>),
    Jump(Jump),
    /// Effectively an x86 return.
    PopJump(PopJump),
    Branch(Branch),
    // Do-binding and return?
    // Bind(Bind<VarId, ProgId, Box<Program>>),
    // Return(Return<Atom>),
    Panic(Panic),
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
    PushArg(Atom),
    PopArg(VarId),
    PushTag(Tag),
    Clear(Context),
}

#[derive(Clone, Debug)]
pub struct Pack<T>(pub T);
#[derive(Clone, Debug)]
pub struct Unpack<T>(pub T);

#[derive(Clone, Debug)]
pub struct Product;

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
}

/// Contexts are ordered sets of variables.
#[derive(Clone, Debug)]
pub struct Context(pub Vec<VarId>);
