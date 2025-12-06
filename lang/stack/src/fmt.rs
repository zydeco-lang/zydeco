//! Formatters for the Zydeco Intermediate Representation (ZIR).

use super::syntax::*;
use zydeco_statics::tyck::syntax as ss;
use zydeco_surface::scoped::syntax::ScopedArena;

/* -------------------------------- Formatter ------------------------------- */

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    arena: &'arena StackArena,
    scoped: &'arena ScopedArena,
    statics: &'arena ss::StaticsArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(
        arena: &'arena StackArena, scoped: &'arena ScopedArena, statics: &'arena ss::StaticsArena,
    ) -> Self {
        Formatter { arena, scoped, statics }
    }

    fn statics_fmt(&self) -> zydeco_statics::tyck::fmt::Formatter<'arena> {
        zydeco_statics::tyck::fmt::Formatter::new(self.scoped, self.statics)
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let name = &f.scoped.defs[self];
        let statics_fmt = f.statics_fmt();
        s += &name.ugly(&statics_fmt);
        s += &self.concise();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        self.ugly(&statics_fmt)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let value = &f.arena.values[self];
        value.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Value {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        match self {
            | Value::Hole(Hole) => "_".to_string(),
            | Value::Var(def) => def.ugly(f),
            | Value::Clo(Clo { capture, stack, body }) => {
                let mut s = String::new();
                if !capture.is_empty() {
                    s += " [";
                    s += &capture.iter().map(|d| d.ugly(f)).collect::<Vec<_>>().join(", ");
                    s += "] ";
                }
                s += &format!("{{ {} -> {} }}", stack.ugly(f), body.ugly(f));
                s
            }
            | Value::Ctor(Ctor(name, val)) => {
                format!("{}({})", name.ugly(&statics_fmt), val.ugly(f))
            }
            | Value::Triv(Triv) => "()".to_string(),
            | Value::VCons(Cons(a, b)) => format!("({}, {})", a.ugly(f), b.ugly(f)),
            | Value::Lit(lit) => lit.ugly(&statics_fmt),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for StackId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let stack = &f.arena.stacks[self];
        stack.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Stack {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        match self {
            | Stack::Kont(s) => format!("({})", s.ugly(f)),
            | Stack::Var(s) => s.ugly(f),
            | Stack::Arg(Cons(val, stack)) => format!("arg({}) :: {}", val.ugly(f), stack.ugly(f)),
            | Stack::Tag(Cons(dtor, stack)) => {
                format!("tag({}) :: {}", dtor.ugly(&statics_fmt), stack.ugly(f))
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Bullet {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "•".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Kont {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("kont {} -> {}", self.binder.ugly(f), self.body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CompuId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let compu = &f.arena.compus[self];
        compu.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Computation {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Computation::Hole(Hole) => "_".to_string(),
            | Computation::Fix(Fix(param, body)) => {
                format!("fix {} -> {}", param.ugly(f), body.ugly(f))
            }
            | Computation::Force(SForce { thunk, stack }) => {
                format!("{} ! {}", thunk.ugly(f), stack.ugly(f))
            }
            | Computation::Ret(SReturn { stack, value }) => {
                format!("{} @ {}", stack.ugly(f), value.ugly(f))
            }
            | Computation::Case(Match { scrut, arms }) => {
                let mut s = String::new();
                s += &format!("case {}", scrut.ugly(f));
                for Matcher { binder, tail } in arms.iter() {
                    s += &format!(" | {} -> {}", binder.ugly(f), tail.ugly(f));
                }
                s += " end";
                s
            }
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Computation::LetStack(Let { binder, bindee, tail }) => {
                format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Computation::LetArg(Let { binder, bindee, tail }) => {
                let Cons(param, Bullet) = binder;
                format!(
                    "let arg({}) :: {} = {} in {}",
                    param.ugly(f),
                    Bullet.ugly(f),
                    bindee.ugly(f),
                    tail.ugly(f)
                )
            }
            | Computation::CoCase(CoMatch { arms }) => {
                let mut s = String::new();
                let statics_fmt = f.statics_fmt();
                s += "cocase";
                for CoMatcher { dtor, tail } in arms.iter() {
                    let Cons(dtor_name, Bullet) = dtor;
                    s += &format!(" | tag({}) -> {}", dtor_name.ugly(&statics_fmt), tail.ugly(f));
                }
                s += " end";
                s
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | TermId::Value(v) => v.ugly(f),
            | TermId::Compu(c) => c.ugly(f),
            | TermId::Stack(s) => s.ugly(f),
        }
    }
}
