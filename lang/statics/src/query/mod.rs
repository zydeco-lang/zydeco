//! Salsa-backed source checking and typed judgment producers.
//!
//! The source driver owns checker orchestration and publication. Each judgment
//! family owns its interned inputs and site-keyed typed fragments; shared keys
//! live in `input`. Public query names are exported at this boundary.

use crate::TyEnv;
use crate::alloc::QUERY_DERIVATION_TAG;
use crate::check::{
    CheckedSource, CompletionTyping, KontFailure, RejectedSource, SourceCheckOutcome, Tycker,
};
use crate::surface_syntax as su;
use crate::syntax as ss;
use zydeco_surface::arena::ArenaId;
use zydeco_utils::arena::ArenaAccess;
use zydeco_utils::arena::{KeySpaceId, derived_id};

mod input;
pub use input::*;
mod intrinsic;
pub use intrinsic::*;
mod atomic;
pub use atomic::*;
mod pattern;
pub use pattern::*;
mod structure;
pub use structure::*;
mod computation;
pub use computation::*;
mod function;
pub use function::*;
mod package;
pub use package::*;
mod data;
pub use data::*;
mod source;
pub use source::*;
