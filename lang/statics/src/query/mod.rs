//! Salsa-backed coarse source checking queries.

use crate::check::{
    CheckedSource, CompletionTyping, RejectedSource, ReportedError, SourceCheckOutcome, Tycker,
};
use crate::surface_syntax as su;
use crate::syntax as ss;

mod input;
pub use input::*;
mod source;
pub use source::*;
