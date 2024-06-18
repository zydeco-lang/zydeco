use crate::*;
use ss::*;

impl Tycker {
    pub fn extract_tpat(&mut self, tpat: TPatId) -> (Option<DefId>, KindId) {
        match self.statics.tpats[&tpat].clone() {
            | TypePattern::Hole(Hole) => todo!(),
            | TypePattern::Var(_def) => {
                todo!()
            }
        }
    }
}
