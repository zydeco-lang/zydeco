use crate::source::{ProgramAssembly, SourceLowerError};
use crate::{
    BuildConf, PipelineConf, Verbosity,
    amd64::{Amd64Pipeline, PackageAmd64},
    llvm::{LlvmPipeline, PackageLlvm},
    zasm::AssemblyPipeline,
    zir::StackPipeline,
};
use std::{collections::HashMap, path::PathBuf};
use zydeco_assembly::syntax::AssemblyArena;
use zydeco_dynamics::{BuiltinRootLinker, RootLinker, syntax::DynamicsArena};
use zydeco_stackir::{BuiltinRootLowerer, RootLowerer, StackirArena};
use zydeco_statics::tyck::{
    CheckedSource, TyckReports, Tycker,
    arena::StaticsArena,
    syntax::{Fillable, TermAnnId, Type},
};
use zydeco_surface::{
    bitter::{
        DesugarError, SourceDesugarOut, SourceUnitDesugarer,
        arena::BitterArena,
        syntax::{PrimTerms, TermId as BitterTermId},
    },
    scoped::{
        ResolveError, ResolveSourceOut, Resolver,
        arena::ScopedArena,
        syntax::{PrimDefs, TermId as ScopedTermId},
    },
    textual::syntax as t,
};
use zydeco_utils::pass::CompilerPass;

/// One assembled source term after surface desugaring.
pub struct SourceBitter {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub arena: BitterArena,
    pub prim: PrimTerms,
    pub root: BitterTermId,
}

/// One assembled source term after hygienic name resolution.
pub struct SourceScoped {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub arena: ScopedArena,
    pub prim: PrimDefs,
    pub root: ScopedTermId,
}

/// One assembled source term with its classified static identity.
pub struct SourceChecked {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub root: TermAnnId,
}

/// A directly linked dynamic computation root.
pub struct SourceDynamics {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub arena: DynamicsArena,
}

/// A directly lowered Stack IR computation root.
pub struct SourceStack {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub stackir: StackirArena,
}

/// One single-term source program lowered to analyzed assembly.
pub struct SourceAssembly {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub stackir: StackirArena,
    pub assembly: AssemblyArena,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, derive_more::Display)]
pub enum SourceAssemblyOutcome {
    #[display("Program exited with code 0")]
    Exit,
    #[display("Program panicked")]
    Panic,
}

impl ProgramAssembly {
    pub fn desugar(self) -> Result<SourceBitter, DesugarError> {
        let ProgramAssembly { sources, spans, arena: textual, unit } = self;
        let SourceDesugarOut { arena, prim, root } =
            SourceUnitDesugarer::new(&spans, &textual, unit, Default::default()).run()?;
        Ok(SourceBitter { sources, spans, arena, prim, root })
    }
}

impl SourceBitter {
    pub fn resolve(self) -> Result<SourceScoped, Box<ResolveError>> {
        let Self { sources, spans, arena, prim, root } = self;
        let ResolveSourceOut { prim, arena, root } =
            Resolver::new(&spans, arena, prim).run_source(root)?;
        Ok(SourceScoped { sources, spans, arena, prim, root })
    }
}

impl SourceScoped {
    pub fn check(self) -> Result<SourceChecked, TyckReports> {
        let Self { sources, spans, mut arena, prim, root } = self;
        let CheckedSource { statics, root } =
            Tycker::new(&spans, &prim, &mut arena).check_source(root)?;
        Ok(SourceChecked { sources, spans, scoped: arena, statics, root })
    }
}

impl SourceChecked {
    pub fn dynamics(self) -> Result<SourceDynamics, SourceLowerError> {
        let Self { sources, spans, scoped, statics, root } = self;
        let TermAnnId::Compu(root, _) = root else {
            return Err(SourceLowerError::NonComputation { found: root.into() });
        };
        let arena = RootLinker { scoped, statics, root }.run();
        Ok(SourceDynamics { sources, spans, arena })
    }

    pub fn dynamics_with_builtin(self) -> Result<SourceDynamics, SourceLowerError> {
        let Self { sources, spans, scoped, statics, root } = self;
        let TermAnnId::Compu(root, ty) = root else {
            return Err(SourceLowerError::NonComputation { found: root.into() });
        };
        let Fillable::Done(Type::PackPi(signature)) = statics.types_pre[&ty].to_owned() else {
            return Err(SourceLowerError::NonBuiltinExecutable { found: ty });
        };
        let arena = BuiltinRootLinker { scoped, statics, root, signature }.run()?;
        Ok(SourceDynamics { sources, spans, arena })
    }

    pub fn stackir(self) -> Result<SourceStack, SourceLowerError> {
        let Self { sources, spans, mut scoped, statics, root } = self;
        let TermAnnId::Compu(root, _) = root else {
            return Err(SourceLowerError::NonComputation { found: root.into() });
        };
        let stackir = RootLowerer::new(&spans, &mut scoped, &statics, root)
            .run()
            .expect("root Stack IR lowering is infallible");
        Ok(SourceStack { sources, spans, scoped, statics, stackir })
    }

    pub fn stackir_with_builtin(self) -> Result<SourceStack, SourceLowerError> {
        let Self { sources, spans, mut scoped, statics, root } = self;
        let TermAnnId::Compu(root, ty) = root else {
            return Err(SourceLowerError::NonComputation { found: root.into() });
        };
        let Fillable::Done(Type::PackPi(signature)) = statics.types_pre[&ty].to_owned() else {
            return Err(SourceLowerError::NonBuiltinExecutable { found: ty });
        };
        let stackir =
            BuiltinRootLowerer::new(&spans, &mut scoped, &statics, root, signature).run()?;
        Ok(SourceStack { sources, spans, scoped, statics, stackir })
    }
}

impl SourceStack {
    pub fn optimize(
        self, config: &PipelineConf, verbosity: Verbosity,
    ) -> crate::Result<SourceStack> {
        let Self { sources, spans, mut scoped, statics, stackir } = self;
        let stackir =
            StackPipeline::new(&spans, &mut scoped, &statics, config, verbosity).run(stackir)?;
        Ok(SourceStack { sources, spans, scoped, statics, stackir })
    }

    pub fn assemble(self, verbosity: Verbosity) -> crate::Result<SourceAssembly> {
        let Self { sources, spans, scoped, statics, stackir } = self;
        let assembly =
            AssemblyPipeline::new(&spans, &scoped, &statics, &stackir, verbosity).run()?;
        Ok(SourceAssembly { sources, spans, scoped, statics, stackir, assembly })
    }

    pub fn render(&self) -> String {
        use zydeco_stackir::sps::fmt::*;
        let fmt =
            Formatter::new(&self.stackir.admin, &self.stackir.inner, &self.scoped, &self.statics);
        let doc = self.stackir.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        buf
    }
}

impl SourceAssembly {
    pub fn emit_amd64(
        self, name: String, build_conf: BuildConf, verbosity: Verbosity,
    ) -> crate::Result<PackageAmd64> {
        let Self { sources: _, spans, scoped, statics, stackir, assembly } = self;
        Amd64Pipeline::new(&spans, &scoped, &statics, &stackir, &assembly, verbosity)
            .run(name, build_conf)
    }

    pub fn emit_llvm(
        self, name: String, build_conf: BuildConf, verbosity: Verbosity,
    ) -> crate::Result<PackageLlvm> {
        let Self { sources: _, spans, scoped, statics, stackir, assembly } = self;
        LlvmPipeline::new(&spans, &scoped, &statics, &stackir, &assembly, verbosity)
            .run(name, build_conf)
    }

    pub fn execute(self) -> crate::Result<SourceAssemblyOutcome> {
        match zydeco_assembly::interp::Interpreter::new(self.assembly).run()? {
            | zydeco_assembly::interp::Output::Exit => Ok(SourceAssemblyOutcome::Exit),
            | zydeco_assembly::interp::Output::Panic => Ok(SourceAssemblyOutcome::Panic),
        }
    }

    pub fn render(&self) -> String {
        use zydeco_assembly::fmt::*;
        let fmt = Formatter::new(&self.assembly, None, None);
        let doc = self.assembly.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        buf
    }
}
