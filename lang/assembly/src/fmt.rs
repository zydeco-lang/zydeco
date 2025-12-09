//! Formatters for the assembly language.

use super::syntax::*;

pub use zydeco_syntax::{Pretty, Ugly};
pub struct Formatter<'arena> {
    arena: &'arena AssemblyArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena AssemblyArena) -> Self {
        Formatter { arena }
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for AssemblyArena {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();

        // Print all programs
        for (prog_id, prog) in self.programs.iter() {
            if let Some(label) = self.block_name(*prog_id) {
                s += &format!("[label:{}{}]\n", label.0, prog_id.concise());
                s += &format!("\t{}\n", prog.ugly(f));
            }
        }

        // Print all symbols
        for (sym_id, sym) in self.symbols.iter() {
            s += &format!(
                "[sym{}] {}{}\n",
                sym_id.concise(),
                sym.ugly(f),
                if let Symbol::Prog(prog_id) = sym {
                    format!(" (label: {})", prog_id.ugly(f))
                } else {
                    String::new()
                }
            );
        }

        // Print entry point
        for (prog_id, _) in self.entry.iter() {
            s += "[entry]\n";
            if let Some(label) = self.labels.get(prog_id) {
                s += &format!("\t{}\n", label.0);
            } else {
                s += &format!("\t{}\n", prog_id.ugly(f));
            }
        }

        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ProgId {
    fn ugly(&self, f: &'a Formatter) -> String {
        // If the program is nominated, we should stop here and use the label.
        if let Some(label) = f.arena.block_name(*self) {
            label.ugly(f)
        } else {
            f.arena.programs[self].ugly(f)
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarId {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("{}{}", f.arena.variables[self], self.concise())
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for SymId {
    fn ugly(&self, f: &'a Formatter) -> String {
        f.arena.symbols[self].ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Program {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Program::Instruction(instr, next) => format!("{}; {}", instr.ugly(f), next.ugly(f)),
            | Program::Jump(jump) => jump.ugly(f),
            | Program::EqJump(eq_jump) => eq_jump.ugly(f),
            | Program::PopJump(pop_jump) => pop_jump.ugly(f),
            | Program::PopBranch(branch) => branch.ugly(f),
            | Program::Panic(panic) => panic.ugly(f),
            // | Program::Call(call) => call.ugly(f),
            // | Program::Return(ret) => ret.ugly(f),
            // | Program::Bind(bind) => bind.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Jump {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("jmp {}", f.arena.labels[&self.0].0)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for EqJump {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("eqjmp {}", self.0.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for PopJump {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "popjmp".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for PopBranch {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!(
            "popbr {{{}}}",
            self.0
                .iter()
                .map(|(tag, prog)| format!("{} -> {}", tag.ugly(f), prog.ugly(f)))
                .collect::<Vec<_>>()
                .join(", ")
        )
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
            | Instruction::PushContext(pack) => pack.ugly(f),
            | Instruction::PopContext(unpack) => unpack.ugly(f),
            | Instruction::PushArg(push) => push.ugly(f),
            | Instruction::PopArg(pop) => pop.ugly(f),
            | Instruction::PushTag(push) => push.ugly(f),
            | Instruction::Rotate(rotate) => rotate.ugly(f),
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

impl<'a> Ugly<'a, Formatter<'a>> for Rotate {
    fn ugly(&self, _f: &'a Formatter) -> String {
        format!("rotate {}", self.0)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ProductMarker {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "<product>".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ContextMarker {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "<context>".to_string()
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

impl<'a> Ugly<'a, Formatter<'a>> for Symbol {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Symbol::Triv(triv) => triv.ugly(f),
            | Symbol::Prog(prog_id) => {
                let label_part = f
                    .arena
                    .labels
                    .get(prog_id)
                    .map(|label| label.0.clone())
                    .unwrap_or_else(|| "<prog>".to_string());
                format!("{}{}", label_part, prog_id.concise())
            }
            | Symbol::Literal(literal) => literal.ugly(f),
            | Symbol::Extern(ext) => ext.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Triv {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "()".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Atom {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Atom::Var(var) => var.ugly(f),
            | Atom::Sym(sym_id) => sym_id.ugly(f),
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

impl<'a> Ugly<'a, Formatter<'a>> for Extern {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "<extern>".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Context {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("{{{}}}", self.0.iter().map(|var| var.ugly(f)).collect::<Vec<_>>().join(", "))
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for AssemblyArena {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let mut doc = RcDoc::nil();

        // Print all programs
        for (prog_id, prog) in self.programs.iter() {
            if let Some(label) = self.block_name(*prog_id) {
                doc = doc
                    .append(RcDoc::text(format!("[label:{}]", label.ugly(f))))
                    .append(RcDoc::concat([RcDoc::line(), prog.pretty(f)]).nest(1))
                    .append(RcDoc::line());
            }
        }

        // Print all symbols
        for (sym_id, sym) in self.symbols.iter() {
            doc = doc
                .append(RcDoc::text(format!("[sym{}", sym_id.concise(),)))
                .append(RcDoc::text("]"))
                .append(RcDoc::space())
                .append(sym.pretty(f));
            if let Symbol::Prog(prog_id) = sym {
                doc = doc.append(RcDoc::concat([
                    RcDoc::space(),
                    RcDoc::text("(label: "),
                    prog_id.pretty(f),
                    RcDoc::text(")"),
                ]));
            }
            doc = doc.append(RcDoc::line());
        }

        // Print entry point
        for (prog_id, _) in self.entry.iter() {
            doc = doc.append(RcDoc::text("[entry]"));
            if let Some(label) = self.block_name(*prog_id) {
                doc = doc
                    .append(RcDoc::concat([RcDoc::line(), RcDoc::text(label.0.clone())]).nest(1));
            } else {
                doc = doc.append(RcDoc::concat([RcDoc::line(), prog_id.pretty(f)]).nest(1));
            }
            doc = doc.append(RcDoc::line());
        }

        doc
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProgId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        // If the program is nominated, we should stop here and use the label.
        if let Some(label) = f.arena.block_name(*self) {
            label.pretty(f)
        } else {
            f.arena.programs[self].pretty(f)
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(format!("{}{}", f.arena.variables[self], self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SymId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.arena.symbols[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Program {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Program::Instruction(instr, next) => {
                RcDoc::concat([instr.pretty(f), RcDoc::text(";"), RcDoc::line(), next.pretty(f)])
            }
            | Program::Jump(jump) => jump.pretty(f),
            | Program::EqJump(eq_jump) => eq_jump.pretty(f),
            | Program::PopJump(pop_jump) => pop_jump.pretty(f),
            | Program::PopBranch(branch) => branch.pretty(f),
            | Program::Panic(panic) => panic.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Jump {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("jmp"),
            RcDoc::space(),
            RcDoc::text(f.arena.labels[&self.0].0.clone()),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for EqJump {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("eqjmp"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PopJump {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("popjmp")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PopBranch {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let branches: Vec<_> = self
            .0
            .iter()
            .map(|(tag, prog)| {
                RcDoc::concat([
                    tag.pretty(f),
                    RcDoc::space(),
                    RcDoc::text("->"),
                    RcDoc::space(),
                    prog.pretty(f),
                ])
            })
            .collect();
        RcDoc::concat([
            RcDoc::text("popbr"),
            RcDoc::concat(
                branches
                    .iter()
                    .flat_map(|doc| vec![RcDoc::line(), doc.clone()])
                    .collect::<Vec<_>>(),
            )
            .nest(1),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Panic {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("panic")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Call {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("call")
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Return<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("ret"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a, Be, Tail> Pretty<'a, Formatter<'a>> for Bind<(), Be, Tail>
where
    Be: Pretty<'a, Formatter<'a>>,
    Tail: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("do"),
            RcDoc::space(),
            RcDoc::text("("),
            self.bindee.pretty(f),
            RcDoc::text(")"),
            RcDoc::text(";"),
            RcDoc::space(),
            self.tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Instruction {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Instruction::PackProduct(pack) => pack.pretty(f),
            | Instruction::UnpackProduct(unpack) => unpack.pretty(f),
            | Instruction::PushContext(pack) => pack.pretty(f),
            | Instruction::PopContext(unpack) => unpack.pretty(f),
            | Instruction::PushArg(push) => push.pretty(f),
            | Instruction::PopArg(pop) => pop.pretty(f),
            | Instruction::PushTag(push) => push.pretty(f),
            | Instruction::Rotate(rotate) => rotate.pretty(f),
            | Instruction::Clear(context) => context.pretty(f),
        }
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Pack<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("pack"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Unpack<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("unpack"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Push<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("push"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a, T> Pretty<'a, Formatter<'a>> for Pop<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("pop"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Rotate {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("rotate"), RcDoc::space(), RcDoc::text(self.0.to_string())])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProductMarker {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<product>")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ContextMarker {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<context>")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Tag {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match &self.name {
            | Some(name) => RcDoc::concat([
                RcDoc::text(self.idx.to_string()),
                RcDoc::text(":"),
                RcDoc::text(name.clone()),
            ]),
            | None => RcDoc::text(self.idx.to_string()),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Symbol {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Symbol::Triv(triv) => triv.pretty(f),
            | Symbol::Prog(prog_id) => {
                let label_part = f
                    .arena
                    .labels
                    .get(prog_id)
                    .map(|label| label.0.clone())
                    .unwrap_or_else(|| "<prog>".to_string());
                RcDoc::concat([RcDoc::text(label_part), RcDoc::text(prog_id.concise())])
            }
            | Symbol::Literal(literal) => literal.pretty(f),
            | Symbol::Extern(ext) => ext.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Triv {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("()")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Atom {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Atom::Var(var) => var.pretty(f),
            | Atom::Sym(sym_id) => sym_id.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Label {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.0.clone())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Literal {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Literal::Int(i) => RcDoc::text(format!("{:?}", i)),
            | Literal::String(str) => RcDoc::text(format!("{:?}", str)),
            | Literal::Char(c) => RcDoc::text(format!("{:?}", c)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Extern {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<extern>")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Context {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let vars: Vec<_> = self.0.iter().map(|var| var.pretty(f)).collect();
        RcDoc::concat([
            RcDoc::text("{"),
            RcDoc::concat(
                vars.iter()
                    .enumerate()
                    .flat_map(|(i, doc)| {
                        if i == 0 {
                            vec![doc.clone()]
                        } else {
                            vec![RcDoc::text(", "), doc.clone()]
                        }
                    })
                    .collect::<Vec<_>>(),
            ),
            RcDoc::text("}"),
        ])
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ugly() {
        let mut arena = AssemblyArena::new(ArcGlobalAlloc::new());
        let prog = arena.prog_anon(Program::Panic(Panic));
        let prog = arena
            .prog_anon(Program::Instruction(Instruction::PackProduct(Pack(ProductMarker)), prog));
        let fmter = Formatter::new(&arena);
        assert_eq!(prog.ugly(&fmter), "pack <product>; panic");
    }
}
