pub use super::arena::*;
pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use zydeco_utils::cells::SingCell;

/* --------------------------- Contextual program --------------------------- */

/// The term site that contributed a binding to a contextual term.
pub type BindingId = TermId;

zydeco_utils::new_key_type! {
    /// Identifier for a node in the condensation DAG of context bindings.
    pub struct ContextNodeId;
}

/// A definition contributing a type or value to the surrounding context.
#[derive(Clone, Debug)]
pub struct Definition {
    pub binder: PatId,
    pub bindee: TermId,
}

/// A parameter contributed by `param ... that ...`.
#[derive(Clone, Debug)]
pub struct Parameter {
    pub binder: PatId,
}

/// A single binding stored in a contextual term.
#[derive(Clone, Debug)]
pub struct Binding {
    pub id: BindingId,
    pub inner: BindingForm,
    pub metas: im::Vector<Meta>,
    pub(crate) source_order: usize,
}

/// The forms admitted by contextual terms.
#[derive(Clone, Debug)]
pub enum BindingForm {
    Parameter(Parameter),
    Definition(Definition),
}

impl Binding {
    pub fn from_term(source: TermId, inner: BindingForm, source_order: usize) -> Self {
        Self { id: source, inner, metas: im::Vector::new(), source_order }
    }

    pub fn source_order(&self) -> usize {
        self.source_order
    }
}

/// One strongly connected component in a contextual term.
///
/// `Acyclic` is a singleton component without a self edge. `Recursive`
/// therefore contains either one self-recursive binding or several mutually
/// recursive bindings.
#[derive(Clone, Debug)]
pub enum ContextNode {
    Acyclic(Binding),
    Recursive(Vec<Binding>),
}

impl ContextNode {
    pub fn bindings(&self) -> &[Binding] {
        match self {
            | ContextNode::Acyclic(binding) => std::slice::from_ref(binding),
            | ContextNode::Recursive(bindings) => bindings,
        }
    }

    pub fn source_order(&self) -> usize {
        self.bindings()
            .iter()
            .map(Binding::source_order)
            .min()
            .expect("a context node must contain at least one binding")
    }
}

/// A term together with the context that it inhabits.
#[derive(Clone, Debug)]
pub struct ContextualTerm<C, B> {
    pub context: C,
    pub body: B,
}

/// The source residual and dependency-ordered elaboration of a `begin` block.
#[derive(Clone, Debug)]
pub struct BlockBody {
    pub residual: TermId,
    pub elaborated: TermId,
}

/* --------------------------------- Context -------------------------------- */

/// Context is what variables we *can use* at a given term site.
pub type Context = zydeco_utils::context::Context<DefId>;

/* -------------------------------- CoContext ------------------------------- */

/// CoContext is what variables we *have used* at a given term site.
pub type CoContext = zydeco_utils::context::CoContext<DefId>;

/* -------------------------------- Primitive ------------------------------- */

/// Legacy lexical identities consulted by a few internal monadic
/// transformations.
///
/// The canonical Builtin signature introduces intrinsic constructors and
/// assigns host types through typed roles, so name resolution does not
/// populate these cells from distinguished source spellings.
#[derive(Default)]
pub struct PrimDefs {
    pub vtype: SingCell<DefId>,
    pub ctype: SingCell<DefId>,
    pub thk: SingCell<DefId>,
    pub ret: SingCell<DefId>,
    pub unit: SingCell<DefId>,
    pub os: SingCell<DefId>,
    pub monad: SingCell<DefId>,
    pub algebra: SingCell<DefId>,
}
