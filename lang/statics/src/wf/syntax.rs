pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::tyck::syntax as ss;

pub type DefId = ss::DefId;
pub type KindId = ss::KindId;
pub type TPatId = ss::TPatId;
pub type TypeId = ss::TypeId;
pub type VPatId = ss::VPatId;
pub type ValueId = ss::ValueId;
pub type CompuId = ss::CompuId;

pub type AbstId = ss::AbstId;
// no FillId in wellformed syntax
pub type DataId = ss::DataId;
pub type CoDataId = ss::CoDataId;

pub type Kind = ss::Kind;
pub type TypePattern = ss::TypePattern;
pub type Type = ss::Type;
pub type ValuePattern = ss::ValuePattern;
pub type Value = ss::Value;
pub type Computation = ss::Computation;
pub type Declaration = ss::Declaration;
