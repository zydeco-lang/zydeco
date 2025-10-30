use crate::*;
use super::syntax::*;

pub trait WellFormed {
    type Out;
    fn check_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        self.check_inner_k(tycker)
    }
    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out>;
}

impl Tycker {
    pub fn check(&mut self) -> ResultKont<()> {
        let mut scc = self.scoped.top.clone();
        let mut env = TyEnvT::new(());
        loop {
            let groups = scc.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                match env.mk(SccGroup(&group)).check_k(self) {
                    | Ok(new_env) => {
                        // move on
                        env = new_env;
                        scc.release(group);
                    }
                    | Err(()) => {
                        // mark all decls in the group and those that depend on them unreachable
                        scc.obliviate(group);
                        self.stack.clear();
                    }
                }
            }
        }
        Ok(())
    }
}

impl WellFormed for TyEnvT<SccGroup<'_>> {
    type Out = TyEnvT<()>;

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}

impl WellFormed for TyEnvT<TPatId> {
    type Out = TyEnvT<()>;

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}

impl WellFormed for TyEnvT<TypeId> {
    type Out = ();

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}

impl WellFormed for TyEnvT<VPatId> {
    type Out = ();

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}

impl WellFormed for TyEnvT<ValueId> {
    type Out = ();

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}

impl WellFormed for TyEnvT<CompuId> {
    type Out = ();

    fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        todo!()
    }
}
