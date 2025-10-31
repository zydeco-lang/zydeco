use super::syntax::*;
use crate::*;
use zydeco_statics::{surface_syntax as su, tyck::syntax as ss};
use zydeco_utils::scc::SccGroup;

pub trait Lower {
    type Out;
    fn lower(&self, lower: &mut Lowerer) -> Self::Out;
}

pub struct Lowerer<'a> {
    pub object: Object,
    pub spans: &'a su::SpanArena,
    pub scoped: &'a su::ScopedArena,
    pub statics: &'a ss::StaticsArena,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a su::SpanArena, scoped: &'a su::ScopedArena,
        statics: &'a ss::StaticsArena,
    ) -> Self {
        let object = Object::new(alloc);
        Self { object, spans, scoped, statics }
    }
    pub fn run(mut self) -> Object {
        let mut scc = self.scoped.top.clone();
        loop {
            let groups = scc.top();
            match groups.into_iter().next() {
                | None => break self.object,
                | Some(group) => {
                    let assignments = group.lower(&mut self);
                    scc.release(group);
                }
            }
            // // if no more groups are at the top, we're done
            // if groups.is_empty() {
            //     break self.object
            // }
            // for group in groups {
            //     // each group should be type checked on its own
            //     group.lower(self);
            //     // move on
            //     scc.release(group);
            // }
        }
    }
}

pub struct Assign<Br, Be>(pub Br, pub Be);

impl Lower for SccGroup<su::DeclId> {
    type Out = Vec<Assign<VarId, Atom>>;

    fn lower(&self, lower: &mut Lowerer) -> Self::Out {
        self.into_iter().map(|decl| decl.lower(lower)).collect()
    }
}

impl Lower for su::DeclId {
    type Out = Assign<VarId, Atom>;

    fn lower(&self, lower: &mut Lowerer) -> Self::Out {
        todo!()
    }
}
