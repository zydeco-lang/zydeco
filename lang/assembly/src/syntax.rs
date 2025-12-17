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
    /// Pop the top value (an address) off the stack, and dynamically jump to it.
    PopJump(PopJump),
    /// Pop-jump the second value off the stack, keeping the first value at the top.
    LeapJump(LeapJump),
    /// A jump table.
    PopBranch(PopBranch),
    /// Panic.
    Panic(Panic),
}

/// Stack transformations in ZIR.
#[derive(From, Clone, Debug)]
pub enum Instruction {
    /// Construct a pair. Pop two values off the stack, and push one "pair" value onto the stack.
    PackProduct(Pack<ProductMarker>),
    /// Destruct a pair. Pop a "pair" value off the stack, and push two values back onto the stack.
    UnpackProduct(Unpack<ProductMarker>),
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
    /// Swap the top two values on the stack.
    Swap(Swap),
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
#[derive(Clone, Debug)]
pub struct Swap;

#[derive(Clone, Debug)]
pub struct ProductMarker;
#[derive(Clone, Debug)]
pub struct ContextMarker;

#[derive(Clone, Debug)]
pub struct Jump(pub ProgId);
#[derive(Clone, Debug)]
pub struct EqJump(pub ProgId);
#[derive(Clone, Debug)]
pub struct PopJump;
#[derive(Clone, Debug)]
pub struct LeapJump;
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
impl Label {
    pub fn plain(&self) -> &str {
        &self.0
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

/// Symbols represent statically determined values.
///
/// In our implementation, we track the following statically known symbols:
/// - Triv, the unit value
/// - Program, which are labelled blocks
/// - External functions
/// - Literals
///
/// Symbols are guaranteed to be evaluated and generated at compile time.
#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub inner: SymbolInner,
}
#[derive(From, Clone, Debug)]
pub enum SymbolInner {
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
