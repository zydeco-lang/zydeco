use crate::{PipelineConf, Result, Verbosity, prelude::*};
use zydeco_utils::pass::CompilerPass;

/// The Stack IR middle-end shared by package and single-term compilation.
pub(crate) struct StackPipeline<'a> {
    spans: &'a t::SpanArena,
    scoped: &'a mut sc::ScopedArena,
    statics: &'a ss::StaticsArena,
    config: &'a PipelineConf,
    verbosity: Verbosity,
}

impl<'a> StackPipeline<'a> {
    const NORMALIZATION_PASSES: usize = 11;

    pub fn new(
        spans: &'a t::SpanArena, scoped: &'a mut sc::ScopedArena, statics: &'a ss::StaticsArena,
        config: &'a PipelineConf, verbosity: Verbosity,
    ) -> Self {
        Self { spans, scoped, statics, config, verbosity }
    }

    pub fn run(self, mut stackir: sk::StackirArena) -> Result<sk::StackirArena> {
        self.dump_stackir("right after lowering", &stackir);

        zydeco_stackir::sps::inline::Inliner { stackir: &mut stackir, scoped: self.scoped }
            .run()?;
        self.dump_stackir("after inlining", &stackir);
        zydeco_stackir::sps::check::check(&stackir, self.scoped);

        if self.config.enable_cps {
            zydeco_stackir::CpsTranslator::new(&mut stackir, self.scoped).translate();
            self.dump_stackir("after CPS translation", &stackir);
            zydeco_stackir::sps::check::check(&stackir, self.scoped);
        } else if self.verbosity.enables_stage_dumps() {
            log::trace!("ZIR CPS translation skipped");
        }

        zydeco_stackir::ClosureConverter::new(&mut stackir, self.scoped, self.statics).convert();
        self.dump_stackir("after closure conversion", &stackir);
        zydeco_stackir::sps::check::check(&stackir, self.scoped);

        (0..Self::NORMALIZATION_PASSES).try_fold(stackir, |stackir, _| {
            let sk::StackirArena { admin, mut inner } = stackir;
            let snorm =
                zydeco_stackir::Elaborator::new(admin, self.spans, self.statics, &mut inner)
                    .run()?;
            self.dump_snorm(&snorm, &inner);

            let sn::SNormArena { admin, mut inner } = snorm;
            let stackir =
                zydeco_stackir::Substitutor::new(admin, &mut inner, self.scoped, self.statics)
                    .run()?;
            self.dump_stackir("after substitution", &stackir);
            zydeco_stackir::sps::check::check(&stackir, self.scoped);
            Ok(stackir)
        })
    }

    fn dump_stackir(&self, stage: &str, stackir: &sk::StackirArena) {
        if !self.verbosity.enables_stage_dumps() {
            return;
        }
        use zydeco_stackir::sps::fmt::*;
        let fmt = Formatter::new(&stackir.admin, &stackir.inner, self.scoped, self.statics);
        let doc = stackir.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        log::trace!("ZIR {}:\n{}", stage, buf);
    }

    fn dump_snorm(&self, snorm: &sn::SNormArena, stackir: &sk::StackirInnerArena) {
        if !self.verbosity.enables_stage_dumps() {
            return;
        }
        use zydeco_stackir::snorm::fmt::*;
        let fmt = Formatter::new(&snorm.admin, &snorm.inner, stackir, self.scoped, self.statics);
        let doc = snorm.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        log::trace!("Normalized ZIR:\n{}", buf);
    }
}
