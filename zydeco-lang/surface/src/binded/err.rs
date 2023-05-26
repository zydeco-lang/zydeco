use crate::textual::syntax::*;

pub enum ResolveError {
    UnboundVar(NameRef<VarName>),
}
