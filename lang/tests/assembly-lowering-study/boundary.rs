//! Temporary measurement boundary; absent from the production checkout.
#![allow(dead_code)]

mod original;
mod jobs;

use crate::{arena::AssemblyBuild, lower::Lowerer, representation::Local, syntax::*};
use std::fmt::Write as _;
use zydeco_stackir::SpsLowProgram;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::fold::{Explicit, Recursive};

#[derive(Clone, Copy, Debug)]
pub enum Variant {
    Original,
    Jobs,
    Explicit,
    Recursive,
}

impl Variant {
    pub const ALL: [Self; 4] = [Self::Original, Self::Jobs, Self::Explicit, Self::Recursive];
    pub fn name(self) -> &'static str {
        match self {
            | Self::Original => "original",
            | Self::Jobs => "jobs",
            | Self::Explicit => "explicit",
            | Self::Recursive => "recursive",
        }
    }
}

pub struct Input<'a> {
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub program: &'a SpsLowProgram,
}

enum PreparedInner<'a> {
    Original(original::lower::Lowerer<'a>),
    Jobs(jobs::lower::Lowerer<'a>),
    Explicit(Lowerer<'a>),
    Recursive(Lowerer<'a>),
}

pub struct Prepared<'a>(PreparedInner<'a>);
pub struct Output(AssemblyBuild);

impl<'a> Input<'a> {
    pub fn prepare(&self, variant: Variant, native: bool) -> Prepared<'a> {
        Prepared(match variant {
            | Variant::Original => {
                let lo = original::lower::Lowerer::with_policy(
                    self.spans,
                    self.scoped,
                    self.statics,
                    self.program,
                    &Local,
                );
                PreparedInner::Original(if native { lo.with_native_frames() } else { lo })
            }
            | Variant::Jobs => {
                let lo = jobs::lower::Lowerer::with_policy(
                    self.spans,
                    self.scoped,
                    self.statics,
                    self.program,
                    &Local,
                );
                PreparedInner::Jobs(if native { lo.with_native_frames() } else { lo })
            }
            | Variant::Explicit | Variant::Recursive => {
                let lo = Lowerer::with_policy(
                    self.spans,
                    self.scoped,
                    self.statics,
                    self.program,
                    &Local,
                );
                let lo = if native { lo.with_native_frames() } else { lo };
                if matches!(variant, Variant::Explicit) {
                    PreparedInner::Explicit(lo)
                } else {
                    PreparedInner::Recursive(lo)
                }
            }
        })
    }
}

impl Prepared<'_> {
    #[inline(never)]
    pub fn run(self) -> Output {
        Output(match self.0 {
            | PreparedInner::Original(lo) => lo.run(),
            | PreparedInner::Jobs(lo) => lo.run(),
            | PreparedInner::Explicit(lo) => lo.run_with_driver::<Explicit>(),
            | PreparedInner::Recursive(lo) => lo.run_with_driver::<Recursive>(),
        })
    }
}

impl Output {
    pub fn counts(&self) -> [usize; 4] {
        let arena = &self.0.arena;
        [
            arena.programs.len(),
            arena.variables.len(),
            arena.symbols.len(),
            arena.frame_entries.len(),
        ]
    }

    pub fn snapshot(&self) -> String {
        let arena = &self.0.arena;
        let mut text = format!("root {:?}\n", self.0.root);
        let mut programs = arena.programs.iter().collect::<Vec<_>>();
        programs.sort_by_key(|(id, _)| *id);
        for (id, program) in programs {
            let mut deps = arena.deps.query(id);
            deps.sort_unstable();
            writeln!(text, "{id:?}: {program:?}; context {:?}; deps {deps:?}", arena.contexts[id])
                .unwrap();
        }
        let mut variables = arena.variables.iter().collect::<Vec<_>>();
        variables.sort_by_key(|(id, _)| *id);
        for (id, name) in variables {
            writeln!(text, "{id:?}: {name:?}").unwrap();
        }
        let mut symbols = arena.symbols.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(id, _)| *id);
        for (id, symbol) in symbols {
            writeln!(text, "{id:?}: {symbol:?}").unwrap();
        }
        let mut defs = arena.defs.iter().collect::<Vec<_>>();
        defs.sort_by_key(|(id, _)| *id);
        let mut labels = arena.labels.iter().collect::<Vec<_>>();
        labels.sort_by_key(|(id, _)| *id);
        writeln!(
            text,
            "defs {defs:?}\nlabels {labels:?}\nframes {:?}\nexterns {:?}",
            arena.frame_entries, arena.externs
        )
        .unwrap();
        for sort in ["ProgId", "VarId", "SymId"] {
            text = text.replace(
                &format!("{sort}({:?}, ", self.0.root.key_space()),
                &format!("{sort}(_, "),
            );
        }
        text
    }
}
