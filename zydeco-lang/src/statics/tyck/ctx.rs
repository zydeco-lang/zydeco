use super::*;

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

#[derive(Clone)]
pub struct Frame {
    pub tycker_src: String,
    pub sort: String,
    pub term: String,
    pub info: SpanInfo,
}

impl Trace {
    pub fn push(&mut self, frame: Frame) {
        self.0.push_back(frame);
    }
}
