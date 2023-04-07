use super::{resolve::*, syntax::*};
use crate::utils::{fmt::FmtArgs, Span, SpanInfo, SpanView};
use std::fmt;

#[derive(Clone, Debug)]
pub struct TyckError {
    pub item: Span<TyckErrorItem>,
    pub trace: Trace,
}

impl fmt::Display for TyckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.item.inner_ref())?;
        writeln!(f, "\t({})", self.item.span())?;
        writeln!(f)?;
        writeln!(f, "Trace:")?;
        write!(f, "{}", self.trace)?;
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
    InconsistentBranches { tys: Vec<Type> },
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
                write!(f, "In {}, expected {}, but got {}", context, expected, found.fmt())
            }
            ArityMismatch { context, expected, found } => {
                write!(f, "In {}, expected {} arguments but got {}", context, expected, found)
            }
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
            InconsistentBranches { tys } => {
                writeln!(f, "Branches have mismatched types:")?;
                for t in tys {
                    writeln!(f, "\t- {}, ", t.fmt())?;
                }
                Ok(())
            }
            NameResolve(nr) => write!(f, "{}", nr),
            WrongMain { found } => {
                write!(f, "The type of the main expression should be OS but got {}", found.fmt())
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Trace(pub im::Vector<Frame>);

impl Trace {
    pub fn push(&mut self, frame: Frame) {
        self.0.push_back(frame);
    }
}

impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (_i, frame) in self.0.iter().enumerate().rev() {
            write!(f, "- {}", frame)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub tycker_src: String,
    pub sort: String,
    pub term: String,
    pub info: SpanInfo,
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "When {}:", self.sort)?;
        writeln!(f, "\t{}", self.term)?;
        writeln!(f, "\t({})", self.info)?;
        // writeln!(f, "\t@({})", self.tycker_src)?;
        Ok(())
    }
}
