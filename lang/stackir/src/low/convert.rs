//! Closure conversion from lexical SPS to first-order SPSLow.

use super::{check::SpsLowProgram, syntax as low};
use crate::arena::{Construct as _, DefinitionNames as _};
use crate::high::{
    check::FragmentProgram, syntax as high, traverse::Traversal, variables::Variables,
};
use crate::protocol::{StackProtocol, ValueProtocol};
use crate::scheme::SpsScheme;
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible};
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::{
    arena::ArenaAccess as _,
    context::Context,
    fold::{Driver, Explicit},
    pass::CompilerPass,
};

mod pattern;
mod procedures;
mod fold;

/// Convert lexical SPS into first-order SPSLow with fresh construction state.
pub struct SpsLowConverter<'a> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
}

impl CompilerPass<FragmentProgram> for SpsLowConverter<'_> {
    type Output = SpsLowProgram;
    type Error = Infallible;

    fn run(&mut self, program: FragmentProgram) -> Result<Self::Output, Self::Error> {
        self.run_with_driver::<Explicit>(program)
    }
}

impl SpsLowConverter<'_> {
    /// Select continuation storage for reconstruction and pattern translation.
    pub fn run_with_driver<D: Driver>(
        &mut self, program: FragmentProgram,
    ) -> Result<SpsLowProgram, Infallible> {
        Ok(ClosureConversion::new(program, self.scoped, self.statics).convert::<D>())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct RenameEnvId(usize);

#[derive(Debug)]
struct RenameEnv {
    parent: Option<RenameEnvId>,
    bindings: HashMap<high::DefId, high::DefId>,
}

struct PatternTranslation {
    pattern: low::VPatId,
    bindings: Vec<(high::DefId, high::DefId)>,
}

/// Consume SPS and construct a fresh first-order SPSLow program.
#[derive(AsRef, AsMut)]
struct ClosureConversion<'a> {
    source: high::SpsArena,
    #[as_ref(low::SpsLowArena)]
    #[as_mut(low::SpsLowArena)]
    arena: low::SpsLowArena,
    root: high::CompuId,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    envs: Vec<RenameEnv>,
    variables: Variables,
    /// The label kind of each procedure binding, or `None` for a procedure that no call reaches.
    procedures: HashMap<high::CompuId, Option<low::LabelKind>>,
    /// The variables that every jump to a converted label pushes, in order; none for a local label.
    labels: HashMap<high::DefId, Vec<high::DefId>>,
    /// Conversion is homomorphic on stack lets, so the output keeps the input's fragment.
    scheme: SpsScheme,
}

impl<'a> ClosureConversion<'a> {
    fn new(program: FragmentProgram, scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        let scheme = program.scheme();
        let high::SpsRebuild { source, target, root } = program.into_program().into_rebuild();
        let mut variables = Variables::default();
        Traversal { arena: &source.inner }.run(root.into(), &mut variables);
        let procedures = procedures::Placement::analyze(&source.inner, root);
        let arena = low::SpsLowArena {
            admin: low::SpsLowAdminArena::from_high(target.admin),
            inner: low::SpsLowInnerArena {
                protocols: target.inner.protocols,
                ..Default::default()
            },
        };
        Self {
            source,
            arena,
            root,
            scoped,
            statics,
            envs: vec![RenameEnv { parent: None, bindings: HashMap::new() }],
            variables,
            procedures,
            labels: HashMap::new(),
            scheme,
        }
    }

    fn convert<D: Driver>(mut self) -> SpsLowProgram {
        let source = self.root;
        let root = fold::ConversionFolder::<D>::new(&mut self).run(source);
        SpsLowProgram::try_new(self.arena, root, self.scheme)
            .expect("closure conversion produces closed first-order SPSLow")
    }

    fn extend_env(
        &mut self, parent: RenameEnvId,
        bindings: impl IntoIterator<Item = (high::DefId, high::DefId)>,
    ) -> RenameEnvId {
        let id = RenameEnvId(self.envs.len());
        self.envs
            .push(RenameEnv { parent: Some(parent), bindings: bindings.into_iter().collect() });
        id
    }

    fn renamed_def(&self, mut env: RenameEnvId, def: high::DefId) -> high::DefId {
        loop {
            let RenameEnv { parent, bindings } = &self.envs[env.0];
            if let Some(renamed) = bindings.get(&def) {
                return *renamed;
            }
            match parent {
                | Some(parent) => env = *parent,
                | None => return def,
            }
        }
    }

    fn alloc_def(&mut self, name: VarName) -> high::DefId {
        let id = self.arena.admin.fresh_def();
        self.arena.admin.insert_def(id, name);
        id
    }

    fn alloc_like(&mut self, original: high::DefId) -> high::DefId {
        self.alloc_def(self.arena.admin.def_name(self.scoped, self.statics, &original).clone())
    }

    fn alloc_capture(&mut self, captured: high::DefId) -> high::DefId {
        let VarName(name) = self.arena.admin.def_name(self.scoped, self.statics, &captured).clone();
        self.alloc_def(VarName(format!("{name}#cap")))
    }

    fn alloc_label(&mut self, role: &str) -> high::DefId {
        self.alloc_def(VarName(format!("__{role}_code__")))
    }

    /// The variables a body reads under `env`. A call to a closed procedure reads the variables
    /// that its jump pushes; a label itself is static code, never captured.
    fn sorted_free_vars(
        &self, body: high::CompuId, excluded: Context<high::DefId>, env: RenameEnvId,
    ) -> Vec<high::DefId> {
        let free =
            self.variables.free_variables(body.into()).expect("validated closure body").clone()
                - excluded;
        let mut vars: Vec<_> = free
            .into_iter()
            .flat_map(|def| match self.labels.get(&self.renamed_def(env, def)) {
                | Some(variables) => variables.clone(),
                | None => vec![def],
            })
            .collect();
        vars.sort_unstable();
        vars.dedup();
        vars
    }

    fn capture_bindings(&mut self, captures: &[high::DefId]) -> Vec<(high::DefId, high::DefId)> {
        captures.iter().map(|capture| (*capture, self.alloc_capture(*capture))).collect()
    }

    fn build_product_pattern(&mut self, items: Vec<low::VPatId>) -> low::VPatId {
        let arity = items.len();
        if arity == 0 {
            return low::Triv.build(self, None);
        }
        low::VCons::new(items, low::ProductLayout { arity }).build(self, None)
    }

    fn build_product_value(
        &mut self, items: Vec<low::ValueId>, site: Option<ss::TermId>,
    ) -> low::ValueId {
        let arity = items.len();
        if arity == 0 {
            return low::Triv.build(self, site);
        }
        low::VCons::new(items, low::ProductLayout { arity }).build(self, site)
    }

    fn translated_var(
        &mut self, def: high::DefId, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::ValueId {
        self.renamed_def(env, def).build(self, site)
    }

    fn captured_pattern(&mut self, bindings: &[(high::DefId, high::DefId)]) -> low::VPatId {
        let patterns = bindings.iter().map(|(_, captured)| captured.build(self, None)).collect();
        self.build_product_pattern(patterns)
    }

    fn captured_value_inside(
        &mut self, bindings: &[(high::DefId, high::DefId)], site: Option<ss::TermId>,
    ) -> low::ValueId {
        let values = bindings.iter().map(|(_, captured)| captured.build(self, site)).collect();
        self.build_product_value(values, site)
    }

    fn captured_value_outside(
        &mut self, captures: &[high::DefId], env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::ValueId {
        let values =
            captures.iter().map(|capture| self.translated_var(*capture, env, site)).collect();
        self.build_product_value(values, site)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::Construct;
    use zydeco_utils::arena::ArenaAccess;

    struct Fixture {
        arena: high::SpsArena,
        scoped: ScopedArena,
    }

    impl Fixture {
        fn new() -> Self {
            Self { arena: high::SpsArena::default(), scoped: ScopedArena::default() }
        }

        fn def(&mut self, name: &str) -> high::DefId {
            let def = self.arena.admin.fresh();
            self.arena.admin.insert_def(def, VarName(name.into()));
            def
        }

        fn build<U, S, T>(&mut self, node: U) -> T
        where
            U: Construct<S, T, high::SpsArena>,
        {
            node.build(&mut self.arena, None)
        }

        fn convert(self, root: high::CompuId) -> SpsLowProgram {
            let statics = StaticsArena::default();
            fold::tests::Drivers::convert(&self.arena, root, &self.scoped, &statics)
        }
    }

    #[test]
    fn closure_conversion_introduces_a_value_package_and_block() {
        let mut fixture = Fixture::new();
        let captured = fixture.def("captured");
        let captured_use: high::ValueId = fixture.build(captured);
        let closure_stack = fixture.build(high::StackVar);
        let closure_body =
            fixture.build(high::SReturn { stack: closure_stack, value: captured_use });
        let closure = fixture.build(high::Closure { stack: high::StackVar, body: closure_body });
        let call_stack = fixture.build(high::StackVar);
        let call = fixture.build(high::SForce { thunk: closure, stack: call_stack });
        let binder: high::VPatId = fixture.build(captured);
        let bindee = fixture.build(high::Triv);
        let root = fixture.build(high::Let { binder, bindee, tail: call });

        let program = fixture.convert(root);
        let arena = program.arena();
        assert_eq!(arena.admin.defs[&captured].plain(), "captured");
        assert!(
            arena
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
        );
        let block = arena
            .inner
            .values
            .iter()
            .find_map(|(_, value)| match value {
                | low::Value::Block(block) => Some(block),
                | _ => None,
            })
            .unwrap();
        assert!(matches!(block.entry, low::EntryParameters::Closure { .. }));
        assert_eq!(
            block.entry.words().map(|(role, _)| role).collect::<Vec<_>>(),
            vec![low::EntryRole::Environment]
        );
        assert!(matches!(arena.inner.compus[&block.body], low::Computation::OpenContinuation(_)));
        assert!(
            arena
                .inner
                .compus
                .iter()
                .any(|(_, compu)| matches!(compu, low::Computation::OpenClosure(_)))
        );
    }

    #[test]
    fn continuation_conversion_introduces_a_stack_package() {
        let mut fixture = Fixture::new();
        let returned = fixture.def("returned");
        let binder: high::VPatId = fixture.build(returned);
        let returned_value: high::ValueId = fixture.build(returned);
        let body_stack = fixture.build(high::StackVar);
        let body = fixture.build(high::SReturn { stack: body_stack, value: returned_value });
        let continuation = fixture.build(high::Kont { binder, body });
        let value = fixture.build(high::Triv);
        let root = fixture.build(high::SReturn { stack: continuation, value });

        let program = fixture.convert(root);
        let low::Computation::OpenContinuation(low::OpenContinuation { package, .. }) =
            &program.arena().inner.compus[&program.root()]
        else {
            panic!("return must open a continuation package")
        };
        let low::Stack::ContinuationPackage(low::ContinuationPackage { code, .. }) =
            program.arena().inner.stacks[package]
        else {
            panic!("return must retain its continuation package")
        };
        let low::Value::Block(block) = &program.arena().inner.values[&code] else {
            panic!("continuation must have a block entry")
        };
        assert_eq!(
            block.entry.words().map(|(role, _)| role).collect::<Vec<_>>(),
            vec![low::EntryRole::Result, low::EntryRole::Environment]
        );
        assert!(matches!(
            program.arena().inner.compus[&block.body],
            low::Computation::OpenContinuation(_)
        ));
        let package = *package;
        assert!(program.arena().inner.continuations.get(&package).is_some());
        let (mut arena, root) = program.into_parts();
        let wrong_result = low::Hole.build(&mut arena, None);
        arena.inner.continuations[&package].result = wrong_result;
        assert!(matches!(SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin),
            Err(crate::low::check::SpsLowValidationError::ContinuationContext { stack }) if stack == package));
    }

    #[test]
    fn fix_conversion_uses_a_self_named_block() {
        let mut fixture = Fixture::new();
        let recursive = fixture.def("recursive");
        let recursive_value: high::ValueId = fixture.build(recursive);
        let body_stack = fixture.build(high::StackVar);
        let body = fixture.build(high::SForce { thunk: recursive_value, stack: body_stack });
        let root_stack = fixture.build(high::StackVar);
        let root = fixture.build(high::SFix { param: recursive, stack: root_stack, body });

        let program = fixture.convert(root);
        let low::Computation::Jump(low::Jump { target, .. }) =
            &program.arena().inner.compus[&program.root()]
        else {
            panic!("fix must enter a first-order block")
        };
        assert!(matches!(program.arena().inner.values[target], low::Value::Block(_)));
        assert!(
            program
                .arena()
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
        );
    }

    impl Fixture {
        /// `let y = () in letproc p • = • @ y in` the computation that `tail` builds.
        fn procedure_reading(
            &mut self, tail: impl FnOnce(&mut Self, high::DefId) -> high::CompuId,
        ) -> high::CompuId {
            let y = self.def("y");
            let procedure = self.def("p");
            let read: high::ValueId = self.build(y);
            let stack = self.build(high::StackVar);
            let body = self.build(high::SReturn { stack, value: read });
            let tail = tail(self, procedure);
            let bound = self.build(high::LetProc { procedure, stack: high::StackVar, body, tail });
            let binder: high::VPatId = self.build(y);
            let bindee = self.build(high::Triv);
            self.build(high::Let { binder, bindee, tail: bound })
        }

        fn call(&mut self, procedure: high::DefId, stack: high::StackId) -> high::CompuId {
            self.build(high::CallProc { procedure, stack })
        }
    }

    /// Every label kind and every jump's stack in a converted program.
    fn labels(program: &SpsLowProgram) -> (Vec<low::LabelKind>, Vec<low::StackId>) {
        let inner = &program.arena().inner;
        let kinds = inner
            .compus
            .iter()
            .filter_map(|(_, compu)| match compu {
                | low::Computation::LetLabel(low::LetLabel { kind, .. }) => Some(*kind),
                | _ => None,
            })
            .collect();
        let jumps = inner
            .compus
            .iter()
            .filter_map(|(_, compu)| match compu {
                | low::Computation::JumpLabel(low::JumpLabel { stack, .. }) => Some(*stack),
                | _ => None,
            })
            .collect();
        (kinds, jumps)
    }

    #[test]
    fn a_procedure_called_from_its_block_becomes_a_local_label_over_that_frame() {
        let mut fixture = Fixture::new();
        let root = fixture.procedure_reading(|fixture, procedure| {
            let stack = fixture.build(high::StackVar);
            fixture.call(procedure, stack)
        });
        let program = fixture.convert(root);
        let inner = &program.arena().inner;
        let (kinds, jumps) = labels(&program);
        assert_eq!(kinds, vec![low::LabelKind::Local]);
        assert_eq!(jumps.len(), 1);
        assert!(matches!(inner.stacks[&jumps[0]], low::Stack::Var(_)));
        assert!(
            !inner.values.iter().any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
        );
    }

    #[test]
    fn a_procedure_called_inside_a_continuation_becomes_a_closed_label() {
        // let y = () in letproc p • = • @ y in p ! (kont _ -> p ! •)
        let mut fixture = Fixture::new();
        let root = fixture.procedure_reading(|fixture, procedure| {
            let stack = fixture.build(high::StackVar);
            let resumed = fixture.call(procedure, stack);
            let binder = fixture.build(high::Hole);
            let kont = fixture.build(high::Kont { binder, body: resumed });
            fixture.call(procedure, kont)
        });
        let program = fixture.convert(root);
        let inner = &program.arena().inner;
        let (kinds, jumps) = labels(&program);
        assert_eq!(kinds, vec![low::LabelKind::Closed]);
        // Both jumps push the variable that the closed body pops, and the continuation captures it.
        assert_eq!(jumps.len(), 2);
        for stack in jumps {
            assert!(matches!(inner.stacks[&stack], low::Stack::Arg(_)));
        }
        let entry = inner.continuations.iter().map(|(_, entry)| entry).next().unwrap();
        assert_eq!(entry.captures.len(), 1);
        assert!(
            !inner.values.iter().any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
        );
    }

    #[test]
    fn a_procedure_without_calls_is_dropped() {
        let mut fixture = Fixture::new();
        let root = fixture.procedure_reading(|fixture, _| {
            let value = fixture.build(high::Triv);
            let stack = fixture.build(high::StackVar);
            fixture.build(high::SReturn { stack, value })
        });
        let program = fixture.convert(root);
        assert_eq!(labels(&program), (Vec::new(), Vec::new()));
    }
}
