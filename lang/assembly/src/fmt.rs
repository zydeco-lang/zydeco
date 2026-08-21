//! Formatters for the assembly language.

use super::syntax::*;
use crate::analyze::StackAnalysisScope;

pub use zydeco_syntax::{Pretty, Ugly};
pub struct Formatter<'arena> {
    arena: &'arena AssemblyArena,
    layouts: Option<&'arena ArenaAssoc<ProgId, Layout>>,
    slots: Option<&'arena ArenaSparse<StackAnalysisScope, SlotId>>,
    pub indent: isize,
}
impl<'arena> Formatter<'arena> {
    pub fn new(
        arena: &'arena AssemblyArena, layouts: Option<&'arena ArenaAssoc<ProgId, Layout>>,
        slots: Option<&'arena ArenaSparse<StackAnalysisScope, SlotId>>,
    ) -> Self {
        Formatter { arena, layouts, slots, indent: 2 }
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for AssemblyProgram {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let mut doc = RcDoc::nil();

        // Print all symbols
        for (sym_id, sym) in self.arena.symbols.iter() {
            doc = doc
                .append(RcDoc::text(format!("[sym:{}{}]", sym.name, sym_id.concise(),)))
                .append(RcDoc::space())
                .append(sym.inner.pretty(f));
            if let Symbol::Prog(prog_id) = sym.inner {
                doc = doc.append(RcDoc::concat([RcDoc::line(), prog_id.pretty(f)]).nest(f.indent));
            }
            doc = doc.append(RcDoc::line());
        }

        doc = doc.append(RcDoc::text("[root]"));
        doc = doc.append(RcDoc::concat([RcDoc::line(), self.root.pretty(f)]).nest(f.indent));
        doc = doc.append(RcDoc::line());

        doc
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProgId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let layout = f.layouts.and_then(|layouts| layouts.get(self).cloned());
        RcDoc::concat([
            layout.map(|layout| layout.pretty(f)).unwrap_or_else(RcDoc::nil),
            f.arena.programs[self].pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        // let trailing = f
        //     .arena
        //     .defs
        //     .back(&DefId::Var(*self))
        //     .map_or_else(String::new, |def| format!("/{}", def.concise_inner()));
        // RcDoc::text(format!("{}[{}{}]", f.arena.variables[self], self.concise_inner(), trailing))
        RcDoc::text(format!("{}{}", f.arena.variables[self], self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SymId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(f.arena.sym_label(self))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SlotId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.slots
            .and_then(|slots| slots.get(self).map(|slot| slot.pretty(f)))
            .unwrap_or_else(RcDoc::nil)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Program {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Program::Instruction(instr, next) => {
                RcDoc::concat([instr.pretty(f), RcDoc::text(";"), RcDoc::line(), next.pretty(f)])
            }
            | Program::Terminator(t) => t.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Terminator {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Terminator::Jump(jump) => jump.pretty(f),
            | Terminator::PopJump(pop_jump) => pop_jump.pretty(f),
            | Terminator::PopBranch(branch) => branch.pretty(f),
            | Terminator::Extern(ext) => ext.pretty(f),
            | Terminator::Abort(panic) => panic.pretty(f),
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

impl<'a> Pretty<'a, Formatter<'a>> for Abort {
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
            | Instruction::AllocContext(alloc) => alloc.pretty(f),
            | Instruction::PushArg(push) => push.pretty(f),
            | Instruction::PopArg(pop) => pop.pretty(f),
            | Instruction::PushTag(push) => push.pretty(f),
            | Instruction::Intrinsic(builtin) => builtin.pretty(f),
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

impl<'a, T> Pretty<'a, Formatter<'a>> for Alloc<T>
where
    T: Pretty<'a, Formatter<'a>>,
{
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([RcDoc::text("alloc"), RcDoc::space(), self.0.pretty(f)])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ProductLayout {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(format!("<product:{}/{}>", self.elements, self.arity))
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
            | Imm::Integer(i) => RcDoc::text(format!("{:?}", i)),
            | Imm::Float(value) => RcDoc::text(format!("{:?}", value)),
            | Imm::Char(c) => RcDoc::text(format!("{:?}", c)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Intrinsic {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(self.name.clone())
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Literal {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Literal::Integer(i) => RcDoc::text(format!("{:?}", i)),
            | Literal::Float(value) => RcDoc::text(format!("{:?}", value)),
            | Literal::String(str) => RcDoc::text(format!("{:?}", str)),
            | Literal::Char(c) => RcDoc::text(format!("{:?}", c)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Extern {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(format!("<extern:{}/{}:{:?}>", self.name, self.arity, self.mode))
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

impl<'a> Pretty<'a, Formatter<'a>> for Layout {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let control = self.control.iter().map(|slot| slot.pretty(f));
        let context = self
            .context
            .iter()
            .map(|(var, slot)| RcDoc::concat([var.pretty(f), RcDoc::text("="), slot.pretty(f)]));
        RcDoc::concat([
            RcDoc::concat([
                RcDoc::text("["),
                RcDoc::text("control:"),
                RcDoc::space(),
                RcDoc::intersperse(control, RcDoc::text(", ")),
                RcDoc::text("]"),
            ])
            .group(),
            RcDoc::concat([
                RcDoc::line(),
                RcDoc::text("["),
                RcDoc::text("context:"),
                RcDoc::space(),
                RcDoc::intersperse(context, RcDoc::text(", ")),
                RcDoc::text("]"),
                RcDoc::line(),
            ])
            .group(),
        ])
        .nest(f.indent)
        .nest(f.indent)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Slot {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Slot::Sym(sym) => sym.pretty(f),
            | Slot::Imm(imm) => imm.pretty(f),
            | Slot::Product(items) | Slot::StackProduct(items) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
            | Slot::Unknown => RcDoc::text("<?>"),
        }
    }
}
