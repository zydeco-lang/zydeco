//! Substitution, inference solving, and definitional normalization.
//!
//! Local inference mutates solutions; source finalization resolves and
//! normalizes the resulting graph with shared pass-wide caches.

use crate::*;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use zydeco_utils::arena::ArenaAccess;

mod scope;
mod substitution;
mod reduction;
mod inference;
mod resolution;
mod filled;

pub(crate) use filled::FilledNormalizer;
pub(crate) use resolution::HoleResolver;
