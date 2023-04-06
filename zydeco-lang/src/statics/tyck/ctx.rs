use super::*;
use std::fmt;

#[derive(Clone, Default)]
pub struct Ctx {
    pub abst_ctx: im::Vector<Kind>,
    pub type_ctx: im::HashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: im::HashMap<TermV, Type>,
    pub type_env: Env<TypeV, Type>,
    pub data_env: im::HashMap<TypeV, Data<TypeV, Kind, CtorV, RcType>>,
    pub coda_env: im::HashMap<TypeV, Codata<TypeV, Kind, DtorV, RcType>>,
    pub trace: Trace,
}
impl Ctx {
    pub(super) fn fresh(&mut self, kd: Kind) -> AbstVar {
        self.abst_ctx.push_back(kd);
        AbstVar(self.abst_ctx.len() - 1)
    }
}

#[derive(Clone, Default)]
pub struct Trace(pub im::Vector<Frame>);

impl Trace {
    pub fn push(&mut self, frame: Frame) {
        self.0.push_back(frame);
    }
}

impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, frame) in self.0.iter().enumerate().rev() {
            writeln!(f, "{}. {}", i, frame)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Frame {
    pub tycker_src: String,
    pub sort: String,
    pub term: String,
    pub info: SpanInfo,
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "In {} checking ({}):", self.sort, self.tycker_src)?;
        writeln!(f, "{}", self.term)?;
        writeln!(f, "{}", self.info)?;
        Ok(())
    }
}
