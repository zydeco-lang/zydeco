use crate::{
    ClosureConverter, CpsTranslator, Elaborator, SNormArena, SNormProgram, StackirArena,
    StackirProgram, Substitutor,
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum CpsMode {
    #[default]
    Enabled,
    Disabled,
}

/// The deterministic Stack IR optimization schedule.
pub struct OptimizationPipeline<'a> {
    spans: &'a SpanArena,
    scoped: &'a mut ScopedArena,
    statics: &'a StaticsArena,
    cps: CpsMode,
}

impl<'a> OptimizationPipeline<'a> {
    const NORMALIZATION_PASSES: usize = 11;

    pub fn new(
        spans: &'a SpanArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena, cps: CpsMode,
    ) -> Self {
        Self { spans, scoped, statics, cps }
    }

    pub fn run(self, mut stackir: StackirProgram) -> StackirProgram {
        zydeco_stackir_infallible(
            crate::sps::inline::Inliner::new(&mut stackir, self.scoped).run(),
        );
        crate::sps::check::check(&stackir, self.scoped);

        if self.cps == CpsMode::Enabled {
            CpsTranslator::new(&mut stackir, self.scoped).translate();
            crate::sps::check::check(&stackir, self.scoped);
        }

        ClosureConverter::new(&mut stackir, self.scoped, self.statics).convert();
        crate::sps::check::check(&stackir, self.scoped);

        (0..Self::NORMALIZATION_PASSES).fold(stackir, |stackir, _| {
            let StackirProgram { arena: StackirArena { admin, inner }, root } = stackir;
            let snorm = zydeco_stackir_infallible(
                Elaborator::new(admin, self.spans, self.statics, &inner, root).run(),
            );
            let SNormProgram { arena: SNormArena { admin, mut inner }, root } = snorm;
            let stackir = zydeco_stackir_infallible(
                Substitutor::new(admin, &mut inner, self.scoped, self.statics, root).run(),
            );
            crate::sps::check::check(&stackir, self.scoped);
            stackir
        })
    }
}

fn zydeco_stackir_infallible<T>(result: Result<T, std::convert::Infallible>) -> T {
    match result {
        | Ok(value) => value,
        | Err(never) => match never {},
    }
}
