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

#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub enum DefId {
    Var(VarId),
    Sym(SymId),
}

/// Computations in ZIR are programs in ZASM.
#[derive(From, Clone, Debug)]
pub enum Program {
    Instruction(Instruction, ProgId),
    /// Unconditional jump to a program.
    Jump(Jump),
    /// Pop two values off the stack, compare them, and
    /// jump to the target program if they are equal.
    EqJump(EqJump),
    /// Pop the top value off the stack, and dynamically jump to it.
    PopJump(PopJump),
    /// A jump table.
    PopBranch(PopBranch),
    /// Panic.
    Panic(Panic),
    /* ----------------------- these will be compiled away ---------------------- */
    // Call(Call),
    // Return(Return<Atom>),
    // Bind(Bind<(), ProgId, ProgId>),
}

/// Stack transformations in ZIR.
#[derive(From, Clone, Debug)]
pub enum Instruction {
    /// Construct a pair. Pop two values off the stack, and push one "pair" value onto the stack.
    PackProduct(Pack<Product>),
    /// Destruct a pair. Pop a "pair" value off the stack, and push two values back onto the stack.
    UnpackProduct(Unpack<Product>),
    /// Save current context. Push the pointer to the current context onto the stack.
    PushContext(Push<ContextMarker>),
    /// Restore current context. Pop a pointer to the context off the stack, and replace the current context with it.
    PopContext(Pop<ContextMarker>),
    /// Function application. Push the argument onto the stack.
    /// Destructed by [`Instruction::PopArg`].
    PushArg(Push<Atom>),
    /// Function abstraction. Pop an argument off the stack, and include it into the context.
    PopArg(Pop<VarId>),
    /// Push a tag onto the stack.
    /// Destructed by [`PopBranch`].
    PushTag(Push<Tag>),
    /// Rotate the top values on the stack, moving the most top values to the bottom of the range.
    /// The offset is always one.
    ///
    /// e.g. top [a, b, c, d] bottom with `rotate 3` becomes top [b, c, a, d] bottom.
    ///
    /// In our implementation, the only range used is three, which appears during return to a continuation.
    Rotate(Rotate),
    /// Clear specified variables from the current context.
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
/// The number of values to rotate, i.e. the range of rotation.
///
/// The offset is always one.
#[derive(Clone, Debug)]
pub struct Rotate(pub usize);

#[derive(Clone, Debug)]
pub struct Product;
#[derive(Clone, Debug)]
pub struct ContextMarker;
#[derive(Clone, Debug)]
pub struct Call;

#[derive(Clone, Debug)]
pub struct Jump(pub ProgId);
#[derive(Clone, Debug)]
pub struct EqJump(pub ProgId);
#[derive(Clone, Debug)]
pub struct PopJump;
#[derive(Clone, Debug)]
pub struct PopBranch(pub Vec<(Tag, ProgId)>);
#[derive(Clone, Debug)]
pub struct Panic;

#[derive(Clone, Debug)]
pub struct Label(pub String);
impl From<&str> for Label {
    fn from(name: &str) -> Self {
        Label(name.to_string())
    }
}
#[derive(Clone, Debug)]
pub struct Tag {
    pub idx: usize,
    pub name: Option<String>,
}

/// Values in ZIR.
#[derive(Clone, Debug)]
pub enum Atom {
    Var(VarId),
    Sym(SymId),
}

#[derive(From, Clone, Debug)]
pub enum Symbol {
    Triv(Triv),
    Prog(ProgId),
    Extern(Extern),
    Literal(Literal),
}

#[derive(Clone, Debug)]
pub struct Extern;

/// Contexts are ordered sets of variables.
pub type Context = zydeco_utils::context::Context<VarId>;

/// CoContexts are unordered sets of variables.
pub type CoContext = zydeco_utils::context::CoContext<VarId>;
