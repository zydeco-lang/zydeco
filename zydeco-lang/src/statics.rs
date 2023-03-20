pub mod syntax;
mod resolve;
mod err;
mod legacy;
mod elab;
mod tyck;
mod ctx;

pub use self::{err::TypeCheckError, legacy::{ctx::Ctx, tyck::TypeCheck}};
