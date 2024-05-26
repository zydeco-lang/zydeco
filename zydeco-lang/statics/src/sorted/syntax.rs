pub use crate::syntax::*;

use zydeco_utils::arena::ArenaAssoc;

/* ---------------------------------- Level --------------------------------- */

#[derive(Debug, Clone)]
pub enum Level {
    Term,
    Type,
    Kind,
}
impl Into<usize> for Level {
    fn into(self) -> usize {
        match self {
            Level::Term => 0,
            Level::Type => 1,
            Level::Kind => 2,
        }
    }
}
impl From<usize> for Level {
    fn from(n: usize) -> Self {
        match n {
            0 => Level::Term,
            1 => Level::Type,
            2 => Level::Kind,
            _ => panic!("Invalid level: {}", n),
        }
    }
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct SortedArena {
    pub levels: ArenaAssoc<TermId, Level>,
}
