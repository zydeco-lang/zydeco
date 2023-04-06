use super::{resolve::*, syntax::*, tyck::Trace};
use crate::utils::{fmt::FmtArgs, Span};
use std::fmt;

#[derive(Clone, Debug)]
pub struct TyckError {
    pub item: TyckErrorItem,
    pub trace: Trace,
}
impl Span<TyckErrorItem> {
    pub fn traced(self, trace: Trace) -> Span<TyckError> {
        self.map(|item| TyckError { item, trace })
    }
}

impl fmt::Display for TyckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.item)?;
        writeln!(f, "Trace:")?;
        writeln!(f, "{}", self.trace)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum TyckErrorItem {
    UnboundVar { var: TermV },
    KindMismatch { context: String, expected: Kind, found: Kind },
    TypeMismatch { context: String, expected: Type, found: Type },
    TypeExpected { context: String, expected: String, found: Type },
    ArityMismatch { context: String, expected: usize, found: usize },
    NeedAnnotation { content: String },
    Subsumption { sort: &'static str },
    InconsistentMatchers { unexpected: Vec<CtorV>, missing: Vec<CtorV> },
    InconsistentComatchers { unexpected: Vec<DtorV>, missing: Vec<DtorV> },
    InconsistentBranches(Vec<Type>),
    NameResolve(NameResolveError),
    WrongMain { found: Type },
}

use TyckErrorItem::*;
impl fmt::Display for TyckErrorItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnboundVar { var } => {
                write!(f, "Unbound variable {}", var)
            }
            KindMismatch { context, expected, found } => write!(
                f,
                "Kind mismatch. In {}, expected {}, but got {}",
                context,
                expected.fmt(),
                found.fmt()
            ),
            TypeMismatch { context, expected, found } => write!(
                f,
                "Type mismatch. In {}, expected {}, but got {}",
                context,
                expected.fmt(),
                found.fmt()
            ),
            TypeExpected { context, expected, found } => {
                write!(
                    f,
                    "In {}, expected {}, but got {}",
                    context,
                    expected,
                    found.fmt()
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
            Subsumption { sort } => {
                write!(f, "Subsumption of sort {} failed", sort)
            }
            InconsistentMatchers { unexpected, missing } => {
                writeln!(f, "Inconsistent matchers:")?;
                if !unexpected.is_empty() {
                    writeln!(f, "Unexpected:")?;
                    for c in unexpected {
                        writeln!(f, "\t- {}", c)?;
                    }
                }
                if !missing.is_empty() {
                    writeln!(f, "Missing:")?;
                    for c in missing {
                        writeln!(f, "\t- {}", c)?;
                    }
                }
                Ok(())
            }
            InconsistentComatchers { unexpected, missing } => {
                writeln!(f, "Inconsistent matchers:")?;
                if !unexpected.is_empty() {
                    writeln!(f, "Unexpected:")?;
                    for c in unexpected {
                        writeln!(f, "\t- {}", c)?;
                    }
                }
                if !missing.is_empty() {
                    writeln!(f, "Missing:")?;
                    for c in missing {
                        writeln!(f, "\t- {}", c)?;
                    }
                }
                Ok(())
            }
            InconsistentBranches(types) => {
                writeln!(f, "Branches have mismatched types:")?;
                for t in types {
                    writeln!(f, "\t- {}, ", t.fmt())?;
                }
                Ok(())
            }
            NameResolve(nr) => write!(f, "{}", nr),
            WrongMain { found } => write!(
                f,
                "The type of the main expression should be OS but got {}",
                found.fmt()
            ),
        }
    }
}
