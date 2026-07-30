pub use super::arena::*;
pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use zydeco_utils::cells::SingCell;

/* --------------------------- Contextual program --------------------------- */

/// Context bindings retain the identity of the source declaration that
/// introduced them, but declarations themselves do not survive name
/// resolution.
pub type BindingId = DeclId;

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

/// An externally supplied binding with an optional classifier.
#[derive(Clone, Debug)]
pub struct External {
    pub binder: PatId,
    pub classifier: Option<TermId>,
}

/// A single binding stored in a contextual term.
#[derive(Clone, Debug)]
pub struct Binding {
    pub id: BindingId,
    pub inner: BindingForm,
    pub metas: im::Vector<Meta>,
    pub(crate) source_order: usize,
}

/// The two binding forms admitted by the legacy top-level syntax.
#[derive(Clone, Debug)]
pub enum BindingForm {
    Definition(Definition),
    External(External),
}

impl Binding {
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

/// The executable body of a contextual term.
#[derive(Clone, Debug)]
pub struct ContextBody {
    pub id: DeclId,
    pub term: TermId,
    pub metas: im::Vector<Meta>,
}

/// A term together with the context that it inhabits.
///
/// The context representation is supplied by the owning compiler arena. The
/// optional body permits checking libraries that do not define an entry point.
#[derive(Debug)]
pub struct ContextualTerm<C> {
    pub context: C,
    pub body: Option<ContextBody>,
}

/* --------------------------------- Context -------------------------------- */

/// Context is what variables we *can use* at a given term site.
pub type Context = zydeco_utils::context::Context<DefId>;

/* -------------------------------- CoContext ------------------------------- */

/// CoContext is what variables we *have used* at a given term site.
pub type CoContext = zydeco_utils::context::CoContext<DefId>;

/* -------------------------------- Primitive ------------------------------- */

/// Primitive definitions
///
/// Collects the primitive definitions from the surface syntax.
/// To add a new primitive form:
/// 1. Add a new field to this struct.
/// 2. Check if the form can be introduced during desugaring, e.g. annotations.
///    If so, add it to [`crate::bitter::syntax::PrimTerms`] too.
/// 3. Implement the `check` method to ensure all fields are filled.
#[derive(Default)]
pub struct PrimDefs {
    pub vtype: SingCell<DefId>,
    pub ctype: SingCell<DefId>,
    pub thk: SingCell<DefId>,
    pub ret: SingCell<DefId>,
    pub unit: SingCell<DefId>,
    pub int: SingCell<DefId>,
    pub char: SingCell<DefId>,
    pub string: SingCell<DefId>,
    pub os: SingCell<DefId>,
    pub monad: SingCell<DefId>,
    pub algebra: SingCell<DefId>,
}

mod impls {
    use super::*;
    use crate::scoped::err::*;
    impl PrimDefs {
        /// Ensure all primitive definitions are provided by extern declarations.
        pub fn check(&self) -> Result<()> {
            self.vtype.get_or_else(|| ResolveError::MissingPrim("VType"))?;
            self.ctype.get_or_else(|| ResolveError::MissingPrim("CType"))?;
            self.thk.get_or_else(|| ResolveError::MissingPrim("Thk"))?;
            self.ret.get_or_else(|| ResolveError::MissingPrim("Ret"))?;
            self.unit.get_or_else(|| ResolveError::MissingPrim("Unit"))?;
            self.int.get_or_else(|| ResolveError::MissingPrim("Int"))?;
            self.char.get_or_else(|| ResolveError::MissingPrim("Char"))?;
            self.string.get_or_else(|| ResolveError::MissingPrim("String"))?;
            self.os.get_or_else(|| ResolveError::MissingPrim("OS"))?;
            self.monad.get_or_else(|| ResolveError::MissingPrim("Monad"))?;
            self.algebra.get_or_else(|| ResolveError::MissingPrim("Algebra"))?;
            Ok(())
        }
    }
}
