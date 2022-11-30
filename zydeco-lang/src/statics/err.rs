use super::resolve::*;
use crate::{parse::syntax::*, utils::ann::Ann};

#[derive(Clone, Debug)]
pub enum THetero {
    TVal(TValue),
    TComp(TCompute),
}

impl std::fmt::Display for THetero {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            THetero::TComp(b) => write!(f, "{}", b),
            THetero::TVal(a) => write!(f, "{}", a),
        }
    }
}

impl From<TValue> for THetero {
    fn from(x: TValue) -> Self {
        THetero::TVal(x)
    }
}
impl From<TCompute> for THetero {
    fn from(x: TCompute) -> Self {
        THetero::TComp(x)
    }
}

#[derive(Clone, Debug)]
pub enum TypeCheckError {
    UnboundVar { var: VVar, ann: Ann },
    TypeMismatch { expected: THetero, found: THetero },
    TypeExpected { expected: String, found: THetero },
    ArityMismatch { context: String, expected: usize, found: usize },
    InconsistentBranches(Vec<TCompute>),
    NameResolve(NameResolveError),
    WrongMain { found: TCompute },
    ErrStr(String),
}
use TypeCheckError::*;

use std::fmt;
impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnboundVar { var, ann } => {
                write!(f, "Unbound variable {} (Info: {:?})", var, ann)
            }
            TypeMismatch { expected, found } => write!(
                f,
                "Type mismatch, expected {}, but got {}",
                expected, found
            ),
            TypeExpected { expected, found } => {
                write!(f, "Type {} expected, but got {}", expected, found)
            }
            ArityMismatch { context, expected, found } => write!(
                f,
                "In {}, expected {} arguments but got {}",
                context, expected, found
            ),
            InconsistentBranches(types) => {
                write!(f, "Branches have mismatched types: {:?}", types)
            }
            NameResolve(nr) => write!(f, "{}", nr),
            WrongMain { found } => write!(
                f,
                "The type of the main expression should be OS but got {}",
                found
            ),
            ErrStr(s) => write!(f, "{}", s),
        }
    }
}
