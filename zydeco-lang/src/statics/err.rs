use super::resolve::*;
use crate::parse::syntax::*;

#[derive(Clone, Debug)]
pub enum THetero<Ann> {
    TVal(TValue<Ann>),
    TComp(TCompute<Ann>),
}

impl<Ann> From<TValue<Ann>> for THetero<Ann> {
    fn from(x: TValue<Ann>) -> Self {
        THetero::TVal(x)
    }
}
impl<Ann> From<TCompute<Ann>> for THetero<Ann> {
    fn from(x: TCompute<Ann>) -> Self {
        THetero::TComp(x)
    }
}

#[derive(Clone, Debug)]
pub enum TypeCheckError<Ann> {
    UnboundVar { var: VVar<Ann>, ann: Ann },
    TypeMismatch { expected: THetero<Ann>, found: THetero<Ann> },
    TypeExpected { expected: String, found: THetero<Ann> },
    InconsistentBranches(Vec<TCompute<Ann>>),
    NameResolve(NameResolveError<Ann>),
    Explosion(String),
}
use TypeCheckError::*;

use std::fmt;
impl<Ann> fmt::Display for TypeCheckError<Ann>
where
    Ann: std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnboundVar { var, ann } => {
                write!(f, "Unbound variable {} (Info: {:?})", var, ann)
            }
            TypeMismatch { expected, found } => write!(
                f,
                "Type mismatch, expected {:?}, but got {:?}",
                expected, found
            ),
            TypeExpected { expected, found } => {
                write!(f, "Type {} expected, but got {:?}", expected, found)
            }
            InconsistentBranches(types) => {
                write!(f, "Branches have mismatched types: {:?}", types)
            }
            NameResolve(nr) => write!(f, "{}", nr),
            Explosion(s) => write!(f, "explosion, whatever that means: {}", s),
        }
    }
}
