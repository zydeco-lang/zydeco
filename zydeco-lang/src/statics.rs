pub mod syntax;
mod resolve;
mod err;
mod legacy;
mod elab;
mod tyck;

pub use self::legacy::{ctx::Ctx, err::TypeCheckError, tyck::TypeCheck};
