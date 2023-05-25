use super::syntax::*;
use crate::{prelude::*, resolve::err::NameResolveError};
use std::fmt;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct TyckError {
    pub item: Sp<TyckErrorItem>,
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

#[derive(Error, Clone, Debug)]
pub enum TyckErrorItem {
    #[error("Unbound variable {var}")]
    UnboundVar { var: TermV },
    #[error("Kind mismatch. In {context}, expected {}, but got {}", .expected.fmt(), .found.fmt())]
    KindMismatch { context: String, expected: Kind, found: Kind },
    #[error("Type mismatch. In {context}, expected {}, but got {}", .expected.fmt(), .found.fmt())]
    TypeMismatch { context: String, expected: Type, found: Type },
    #[error("In {context}, expected {expected}, but got {}", .found.fmt())]
    TypeExpected { context: String, expected: String, found: Type },
    #[error("In {context}, expected {expected} arguments but got {found}")]
    ArityMismatch { context: String, expected: usize, found: usize },
    #[error("Applying type arguments to {}, which is not a type abstraction", .found.fmt())]
    ApplyToNonTypeAbs { found: Type },
    #[error("Need annotation for {content}")]
    NeedAnnotation { content: String },
    #[error("Subsumption for sort {sort} failed")]
    Subsumption { sort: &'static str },
    #[error("Inconsistent matchers.\nUnexpected:\n{unexpected:?}Missing:\n{missing:?}")]
    InconsistentMatchers { unexpected: Vec<CtorV>, missing: Vec<CtorV> },
    #[error("Inconsistent comatchers.\nUnexpected:\n{unexpected:?}, Missing:\n{missing:?}")]
    InconsistentComatchers { unexpected: Vec<DtorV>, missing: Vec<DtorV> },
    #[error("Inconsistent branches. Expected: {tys:?}")]
    InconsistentBranches { tys: Vec<Type> },
    #[error(transparent)]
    NameResolve(#[from] NameResolveError),
    #[error("No main entry is defined")]
    NoMainEntry,
    #[error("Multiple main entries are defined")]
    MultipleMainEntries,
    #[error("The main entry should be defined in the top level module")]
    MainEntryInModule,
    #[error("The type of the main expression should be OS but got {}", .found.fmt())]
    WrongMain { found: Type },
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
    pub blame: String,
    pub context: String,
    pub term: String,
    pub info: Span,
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "When {}:", self.context)?;
        writeln!(f, "\t{}", self.term)?;
        writeln!(f, "\t({})", self.info)?;
        // writeln!(f, "\t@({})", self.blame)?;
        Ok(())
    }
}
