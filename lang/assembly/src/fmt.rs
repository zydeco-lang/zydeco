//! Formatters for the assembly language.

use super::syntax::*;

pub use zydeco_syntax::{Pretty, Ugly};
pub struct Formatter<'arena> {
    arena: &'arena AssemblyArena,
    pub indent: isize,
}
impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena AssemblyArena) -> Self {
        Formatter { arena, indent: 2 }
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for AssemblyArena {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let mut doc = RcDoc::nil();

        // Print all symbols
        for (sym_id, sym) in self.symbols.iter() {
            doc = doc
                .append(RcDoc::text(format!("[sym:{}{}]", sym.name, sym_id.concise(),)))
                .append(RcDoc::space())
                .append(sym.inner.pretty(f));
            if let Symbol::Prog(prog_id) = sym.inner {
                doc = doc.append(RcDoc::concat([RcDoc::line(), prog_id.pretty(f)]).nest(f.indent));
            }
            doc = doc.append(RcDoc::line());
        }

        // Print entry point
        for (prog_id, _) in self.entry.iter() {
            doc = doc.append(RcDoc::text("[entry]"));
            doc = doc.append(RcDoc::concat([RcDoc::line(), prog_id.pretty(f)]).nest(f.indent));
            doc = doc.append(RcDoc::line());
        }

        doc
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProgId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.arena.programs[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let trailing = f
            .arena
            .defs
            .back(&DefId::Var(*self))
            .map_or_else(|| String::new(), |def| format!("/{}", def.concise_inner()));
        RcDoc::text(format!("{}[{}{}]", f.arena.variables[self], self.concise_inner(), trailing))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SymId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(f.arena.sym_label(self))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Program {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Program::Instruction(instr, next) => {
                RcDoc::concat([instr.pretty(f), RcDoc::text(";"), RcDoc::line(), next.pretty(f)])
            }
            | Program::Jump(jump) => jump.pretty(f),
            | Program::PopJump(pop_jump) => pop_jump.pretty(f),
            | Program::LeapJump(leap_jump) => leap_jump.pretty(f),
            | Program::PopBranch(branch) => branch.pretty(f),
            | Program::Extern(ext) => ext.pretty(f),
            | Program::Panic(panic) => panic.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Jump {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("jmp"),
            RcDoc::space(),
            RcDoc::text(f.arena.prog_label(&self.0).unwrap_or_else(|| self.0.concise())),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for PopJump {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("popjmp")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for LeapJump {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("leapjmp")
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
                    RcDoc::text(f.arena.prog_label(prog).unwrap()),
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
            .nest(f.indent),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Panic {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("panic")
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
            | Instruction::Intrinsic(builtin) => builtin.pretty(f),
            | Instruction::Swap(swap) => swap.pretty(f),
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

impl<'a> Pretty<'a, Formatter<'a>> for Swap {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("swap")
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
            | Symbol::Undefined(undefined) => undefined.pretty(f),
            | Symbol::Prog(prog_id) => {
                let label = f.arena.prog_label(prog_id).unwrap();
                RcDoc::text(format!("<label:{}>", label))
            }
            | Symbol::StringLiteral(s) => RcDoc::text(format!("{:?}", s)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Undefined {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<undefined>")
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
            | Atom::Imm(imm) => imm.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Imm {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Imm::Triv(triv) => triv.pretty(f),
            | Imm::Int(i) => RcDoc::text(format!("{:?}", i)),
            | Imm::Char(c) => RcDoc::text(format!("{:?}", c)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Intrinsic {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.name)
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
        RcDoc::text(format!("<extern:{}/{}>", self.name, self.arity))
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
