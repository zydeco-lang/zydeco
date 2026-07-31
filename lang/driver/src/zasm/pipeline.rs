use crate::{Result, Verbosity, prelude::*};
use zydeco_utils::pass::CompilerPass;

/// The assembly lowering pipeline shared by package and single-term compilation.
pub(crate) struct AssemblyPipeline<'a> {
    spans: &'a t::SpanArena,
    scoped: &'a sc::ScopedArena,
    statics: &'a ss::StaticsArena,
    stackir: &'a sk::StackirArena,
    verbosity: Verbosity,
}

impl<'a> AssemblyPipeline<'a> {
    pub fn new(
        spans: &'a t::SpanArena, scoped: &'a sc::ScopedArena, statics: &'a ss::StaticsArena,
        stackir: &'a sk::StackirArena, verbosity: Verbosity,
    ) -> Self {
        Self { spans, scoped, statics, stackir, verbosity }
    }

    pub fn run(self) -> Result<sa::AssemblyArena> {
        let mut assembly = zydeco_assembly::lower::Lowerer::new(
            self.spans,
            self.scoped,
            self.statics,
            self.stackir,
        )
        .run();
        self.dump(&assembly);

        let analyzer = zydeco_assembly::analyze::StackAnalyzer::new(&mut assembly).run()?;
        if self.verbosity.enables_stage_dumps() && log::log_enabled!(log::Level::Trace) {
            use zydeco_assembly::fmt::*;
            let fmt =
                Formatter::new(analyzer.arena, Some(&analyzer.layouts), Some(&analyzer.slots));
            let doc = analyzer.arena.pretty(&fmt);
            let mut buf = String::new();
            doc.render_fmt(100, &mut buf).unwrap();
            log::trace!("ZASM after inlining:\n{}", buf);
        }
        Ok(assembly)
    }

    fn dump(&self, assembly: &sa::AssemblyArena) {
        if !self.verbosity.enables_stage_dumps() || !log::log_enabled!(log::Level::Trace) {
            return;
        }
        use zydeco_assembly::fmt::*;
        let fmt = Formatter::new(assembly, None, None);
        let doc = assembly.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        log::trace!("ZASM:\n{}", buf);
    }
}
