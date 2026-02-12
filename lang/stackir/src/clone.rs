use super::syntax::*;
pub use zydeco_surface::scoped::arena::ScopedArena;
/// Map old def id to new def id.
pub type DefMap = std::collections::HashMap<DefId, DefId>;
