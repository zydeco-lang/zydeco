use super::resolve::*;
use crate::{parse::legacy::syntax::*, syntax::binder::*};

#[derive(Clone, Debug)]
pub enum TypeCheckError {
    UnboundVar { var: TermV },
    KindMismatch { context: String, expected: Kind, found: Kind },
    TypeMismatch { context: String, expected: Type, found: Type },
    TypeExpected { context: String, expected: String, found: Type },
    ArityMismatch { context: String, expected: usize, found: usize },
    NeedAnnotation { content: String },
    Subsumption,
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
            UnboundVar { var, .. } => {
                write!(f, "Unbound variable {}", var)
            }
            KindMismatch { context, expected, found } => write!(
                f,
                "Kind mismatch, In {}, expected {}, but got {}",
                context, expected, found
            ),
            TypeMismatch { context, expected, found } => write!(
                f,
                "Type mismatch. In {}, expected {}, but got {}",
                context, expected, found
            ),
            TypeExpected { context, expected, found } => {
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
            Subsumption => write!(
                f,
                "Subsumption failed.",
            ),
            InconsistentBranches(types) => {
                writeln!(f, "Branches have mismatched types:")?;
                for t in types {
                    writeln!(f, "\t- {}, ", t)?;
                }
                Ok(())
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
