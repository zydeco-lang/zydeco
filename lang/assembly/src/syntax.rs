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
#[derive(Clone, Debug)]
pub struct Context(pub Vec<VarId>);
mod impls_context {
    use super::*;
    impl FromIterator<VarId> for Context {
        fn from_iter<T: IntoIterator<Item = VarId>>(iter: T) -> Self {
            Context(iter.into_iter().collect())
        }
    }
    impl IntoIterator for Context {
        type Item = VarId;
        type IntoIter = std::vec::IntoIter<VarId>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
    impl<'a> IntoIterator for &'a Context {
        type Item = &'a VarId;
        type IntoIter = std::slice::Iter<'a, VarId>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.iter()
        }
    }
    impl Context {
        pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
            self.into_iter()
        }
    }
}

#[derive(Clone, Debug)]
pub struct CoContext(pub im::HashSet<VarId>);
mod impls_co_context {
    use super::*;
    impl FromIterator<VarId> for CoContext {
        fn from_iter<T: IntoIterator<Item = VarId>>(iter: T) -> Self {
            CoContext(iter.into_iter().collect())
        }
    }
    impl IntoIterator for CoContext {
        type Item = VarId;
        type IntoIter = im::hashset::ConsumingIter<VarId>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
    impl<'a> IntoIterator for &'a CoContext {
        type Item = &'a VarId;
        type IntoIter = im::hashset::Iter<'a, VarId>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.iter()
        }
    }
    impl CoContext {
        pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
            self.into_iter()
        }
    }
}
