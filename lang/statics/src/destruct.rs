use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl TPatId {
    pub fn destruct(&self, tycker: &mut Tycker) -> (Option<DefId>, KindId) {
        match tycker.statics.tpats[self].clone() {
            | TypePattern::Hole(Ann { tm: Hole, ty: kd }) => (None, kd),
            | TypePattern::Var(def) => {
                let AnnId::Kind(kd) = tycker.statics.annotations_var[&def] else { unreachable!() };
                (Some(def), kd)
            }
        }
    }
    pub fn destruct_def(&self, tycker: &mut Tycker) -> (DefId, KindId) {
        let (def, kd) = self.destruct(tycker);
        match def {
            | Some(def) => (def, kd),
            | None => {
                let def = Alloc::alloc(tycker, VarName("_".to_owned()), kd.into());
                (def, kd)
            }
        }
    }
}

impl TypeId {
    pub fn destruct_abs(&self, tycker: &mut Tycker) -> Option<(TPatId, TypeId)> {
        match tycker.statics.types.get(self)?.to_owned() {
            | Type::Abs(Abs(tpat, ty)) => Some((tpat, ty)),
            | _ => None,
        }
    }
}
