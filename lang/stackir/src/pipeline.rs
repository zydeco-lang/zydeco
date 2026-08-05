use crate::{ClosureConverter, CpsTranslator, Elaborator, StackirArena, Substitutor};
use zydeco_statics::tyck::arena::StaticsArena;
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

    pub fn run(self, mut stackir: StackirArena) -> StackirArena {
        zydeco_stackir_infallible(
            crate::sps::inline::Inliner { stackir: &mut stackir, scoped: self.scoped }.run(),
        );
        crate::sps::check::check(&stackir, self.scoped);

        if self.cps == CpsMode::Enabled {
            CpsTranslator::new(&mut stackir, self.scoped).translate();
            crate::sps::check::check(&stackir, self.scoped);
        }

        ClosureConverter::new(&mut stackir, self.scoped, self.statics).convert();
        crate::sps::check::check(&stackir, self.scoped);

        (0..Self::NORMALIZATION_PASSES).fold(stackir, |stackir, _| {
            let StackirArena { admin, mut inner } = stackir;
            let snorm = zydeco_stackir_infallible(
                Elaborator::new(admin, self.spans, self.statics, &mut inner).run(),
            );
            let crate::snorm::arena::SNormArena { admin, mut inner } = snorm;
            let stackir = zydeco_stackir_infallible(
                Substitutor::new(admin, &mut inner, self.scoped, self.statics).run(),
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
