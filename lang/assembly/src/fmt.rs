//! Formatters for the assembly language.

use super::syntax::*;

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    arena: &'arena AssemblyArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena AssemblyArena) -> Self {
        Formatter { arena }
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for ProgId {
    fn ugly(&self, f: &'a Formatter) -> String {
        f.arena.programs[self].ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarId {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("{}", f.arena.variables[self])
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Program {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Program::Instruction(instr, next) => format!("{}; {}", instr.ugly(f), next.ugly(f)),
            | Program::Jump(jump) => jump.ugly(f),
            | Program::PopJump(pop_jump) => pop_jump.ugly(f),
            | Program::Branch(branch) => branch.ugly(f),
            | Program::Panic(panic) => panic.ugly(f),
            | Program::Call(call) => call.ugly(f),
            | Program::Return(ret) => ret.ugly(f),
            | Program::Bind(bind) => bind.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for PopJump {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "popjmp".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Panic {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "panic".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Call {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "call".to_string()
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Return<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("ret {}", self.0.ugly(f))
    }
}

impl<'a, Be, Tail> Ugly<'a, Formatter<'a>> for Bind<(), Be, Tail>
where
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("do ({}); {}", self.bindee.ugly(f), self.tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Instruction {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Instruction::PackProduct(pack) => pack.ugly(f),
            | Instruction::UnpackProduct(unpack) => unpack.ugly(f),
            | Instruction::PackContext(pack) => pack.ugly(f),
            | Instruction::UnpackContext(unpack) => unpack.ugly(f),
            | Instruction::PackClosure(pack) => pack.ugly(f),
            | Instruction::UnpackClosure(unpack) => unpack.ugly(f),
            | Instruction::PushArg(push) => push.ugly(f),
            | Instruction::PopArg(pop) => pop.ugly(f),
            | Instruction::PushTag(push) => push.ugly(f),
            | Instruction::Clear(context) => context.ugly(f),
        }
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Pack<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("pack {}", self.0.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Unpack<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("unpack {}", self.0.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Push<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("push {}", self.0.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Pop<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("pop {}", self.0.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Product {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "<product>".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Closure {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("<closure {}>", self.0.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Jump {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("jmp {}", f.arena.labels[&self.0].0)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Branch {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!(
            "br {{{}}}",
            self.0
                .iter()
                .map(|(tag, prog)| format!("{} {}", tag.ugly(f), prog.ugly(f)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Tag {
    fn ugly(&self, _f: &'a Formatter) -> String {
        format!(
            "{}{}",
            self.idx,
            self.name.as_ref().map_or_else(|| String::new(), |name| format!(":{}", name))
        )
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Atom {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Atom::Var(var) => var.ugly(f),
            | Atom::Symbol(sym_id) => {
                match &f.arena.symbols[sym_id] {
                    | Symbol::Label(label) => label.ugly(f),
                    | Symbol::Literal(literal) => literal.ugly(f),
                    | Symbol::External => "<external>".to_string(),
                }
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Label {
    fn ugly(&self, _f: &'a Formatter) -> String {
        format!("{}", self.0)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Literal {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let mut s = String::new();
        match self {
            | Literal::Int(i) => s += &format!("{:?}", i),
            // Fixme: escape string
            | Literal::String(str) => s += &format!("{:?}", str),
            | Literal::Char(c) => s += &format!("{:?}", c),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Context {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("{{{}}}", self.0.iter().map(|var| var.ugly(f)).collect::<Vec<_>>().join(", "))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ugly() {
        let mut arena = AssemblyArena::new(ArcGlobalAlloc::new());
        let prog = arena.prog_anon(Program::Panic(Panic));
        let prog = arena.prog_anon(
            Program::Instruction(Instruction::PackProduct(Pack(Product)), prog),
        );
        let fmter = Formatter::new(&arena);
        assert_eq!(prog.ugly(&fmter), "pack <product>; panic");
    }
}
