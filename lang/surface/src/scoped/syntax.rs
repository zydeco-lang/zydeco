pub use super::arena::*;
pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use zydeco_utils::cells::SingCell;

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
