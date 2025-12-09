use super::syntax::*;
use zydeco_utils::deps::DepGraph;

pub trait _Normalize: Sized {
    fn beta(self, object: &mut AssemblyArena) -> Option<Self>;
    fn eta(self, object: &mut AssemblyArena) -> Option<Self>;
}

pub struct Normalizer {
    pub arena: AssemblyArena,
    pub deps: DepGraph<ProgId>,
}

impl Normalizer {
    pub fn new(object: AssemblyArena) -> Self {
        let mut deps = DepGraph::new();
        for (id, prog) in object.programs.iter() {
            let id = id.clone();
            match prog.clone() {
                | Program::Instruction(_, dep) => deps.add(id, [dep]),
                | Program::Jump(Jump(dep)) => deps.add(id, [dep]),
                | Program::PopJump(PopJump) => {}
                | Program::Branch(Branch(brs)) => {
                    deps.add(id, brs.iter().map(|(_, dep)| dep.clone()))
                }
                | Program::Panic(Panic) => {}
                // | Program::Call(Call) => {}
                // | Program::Return(Return(_)) => {}
                // | Program::Bind(Bind { binder: _, bindee: a, tail: b }) => {
                //     deps.add(id, [a, b]);
                // }
            }
        }
        Self { arena: object, deps }
    }
}
