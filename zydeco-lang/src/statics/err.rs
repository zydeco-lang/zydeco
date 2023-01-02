use super::resolve::*;
use crate::{parse::syntax::*, utils::ann::Ann};

#[derive(Clone, Debug)]
pub enum TypeCheckError {
    UnboundVar { var: VVar, ann: Ann },
    KindMismatch { context: String, expected: Kind },
    TypeMismatchCtx { context: String, expected: Type, found: Type },
    TypeMismatch { expected: Type, found: Type },
    TypeExpected { expected: String, found: Type },
    TypeExpectedCtx { context: String, expected: String, found: Type },
    ArityMismatch { context: String, expected: usize, found: usize },
    NeedAnnotation { content: String, },
    InconsistentBranches(Vec<Type>),
    NameResolve(NameResolveError),
    WrongMain { found: Type },
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
            KindMismatch { context, expected } => write!(
                f,
                "Kind mismatch, In {}, expected {}, but got {}",
                context,
                expected,
                match expected {
                    Kind::CompType => Kind::ValType,
                    Kind::ValType => Kind::CompType,
                }
            ),
            TypeMismatch { expected, found } => write!(
                f,
                "Type mismatch, expected {}, but got {}",
                expected, found
            ),
            TypeMismatchCtx { context, expected, found } => write!(
                f,
                "Type mismatch. In {}, expected {}, but got {}",
                context, expected, found
            ),
            TypeExpected { expected, found } => {
                write!(f, "Type {} expected, but got {}", expected, found)
            }
            TypeExpectedCtx { context, expected, found } => {
                write!(
                    f,
                    "In {}, expected {}, but got {}",
                    context, expected, found
                )
            }
            ArityMismatch { context, expected, found } => write!(
                f,
                "In {}, expected {} arguments but got {}",
                context, expected, found
            ),
            NeedAnnotation { content } => {
                write!(f, "Need annotation for {}", content)
            }
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
