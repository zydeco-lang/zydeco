pub mod syntax;

use zydeco_statics::{arena::*, syntax::*};

pub fn run(scoped: ScopedArena, statics: StaticsArena) -> String {
    let mut seq = Vec::new();
    let mut scc = scoped.top.clone();
    loop {
        let groups = scc.top();
        if groups.is_empty() {
            break;
        }
        for group in groups {
            seq.extend(group.iter().cloned());
            scc.release(group);
        }
    }
    format!("{:?}", seq)
}
