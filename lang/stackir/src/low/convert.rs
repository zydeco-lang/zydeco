//! Closure conversion from lexical high SPS to first-order SPSLow.

use super::{check::SpsLowProgram, syntax as low};
use crate::arena::{Construct as _, DefinitionNames as _};
use crate::high::{
    check::BranchJoinProgram, syntax as high, traverse::Traversal, variables::Variables,
};
use crate::protocol::{StackProtocol, ValueProtocol};
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
mod fold;

/// Convert lexical high SPS into first-order SPSLow with fresh construction state.
pub struct SpsLowConverter<'a> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
}

impl CompilerPass<BranchJoinProgram> for SpsLowConverter<'_> {
    type Output = SpsLowProgram;
    type Error = Infallible;

    fn run(&mut self, program: BranchJoinProgram) -> Result<Self::Output, Self::Error> {
        self.run_with_driver::<Explicit>(program)
    }
}

impl SpsLowConverter<'_> {
    /// Select continuation storage for reconstruction and pattern translation.
    pub fn run_with_driver<D: Driver>(
        &mut self, program: BranchJoinProgram,
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

/// Consume high SPS and construct a fresh first-order SPSLow program.
#[derive(AsRef, AsMut)]
struct ClosureConversion<'a> {
    source: high::StackirArena,
    #[as_ref(low::SpsLowArena)]
    #[as_mut(low::SpsLowArena)]
    arena: low::SpsLowArena,
    root: high::CompuId,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    envs: Vec<RenameEnv>,
    variables: Variables,
    /// Thunks bound by a `let` to a plain variable, so their blocks can carry that name.
    thunk_owners: HashMap<high::ValueId, high::DefId>,
}

impl<'a> ClosureConversion<'a> {
    fn new(program: BranchJoinProgram, scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        let high::StackirRebuild { source, target, root } = program.into_program().into_rebuild();
        let mut variables = Variables::default();
        Traversal { arena: &source.inner }.run(root.into(), &mut variables);
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
            thunk_owners: HashMap::new(),
        }
    }

    fn convert<D: Driver>(mut self) -> SpsLowProgram {
        let source = self.root;
        let root = fold::ConversionFolder::<D>::new(&mut self).run(source);
        SpsLowProgram::try_new(self.arena, root)
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

    /// Label a block `owner/kind`: the binder its code belongs to, or `role` when it has
    /// none, and the kind of package the code pointer lives in.
    fn alloc_label(&mut self, owner: Option<high::DefId>, role: &str, kind: &str) -> high::DefId {
        let owner = match owner {
            | Some(def) => self.arena.admin.def_name(self.scoped, self.statics, &def).plain(),
            | None => role.to_owned(),
        };
        self.alloc_def(VarName(format!("{owner}/{kind}")))
    }

    /// The variable a pattern binds directly, if it is a plain variable pattern.
    fn plain_binder(&self, pattern: low::VPatId) -> Option<high::DefId> {
        match &self.arena.inner.vpats[&pattern] {
            | low::ValuePattern::Var(def) => Some(*def),
            | _ => None,
        }
    }

    fn compu_site(&self, id: high::CompuId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&high::TermId::Compu(id)).copied()
    }

    fn value_site(&self, id: high::ValueId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&high::TermId::Value(id)).copied()
    }

    fn stack_site(&self, id: high::StackId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&high::TermId::Stack(id)).copied()
    }

    fn pattern_site(&self, id: high::VPatId) -> Option<ss::PatId> {
        self.source.admin.pats.back(&id).copied()
    }

    fn sorted_free_vars(
        &self, body: high::CompuId, excluded: Context<high::DefId>,
    ) -> Vec<high::DefId> {
        let mut vars: Vec<_> =
            (self.variables.free_variables(body.into()).expect("validated closure body").clone()
                - excluded)
                .into_iter()
                .collect();
        vars.sort_unstable();
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
        arena: high::StackirArena,
        scoped: ScopedArena,
    }

    impl Fixture {
        fn new() -> Self {
            Self { arena: high::StackirArena::default(), scoped: ScopedArena::default() }
        }

        fn def(&mut self, name: &str) -> high::DefId {
            let def = self.arena.admin.fresh();
            self.arena.admin.insert_def(def, VarName(name.into()));
            def
        }

        fn build<U, S, T>(&mut self, node: U) -> T
        where
            U: Construct<S, T, high::StackirArena>,
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
        let closure_stack = fixture.build(high::Bullet);
        let closure_body =
            fixture.build(high::SReturn { stack: closure_stack, value: captured_use });
        let closure = fixture.build(high::Closure { stack: high::Bullet, body: closure_body });
        let call_stack = fixture.build(high::Bullet);
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
        let body_stack = fixture.build(high::Bullet);
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
        assert!(matches!(SpsLowProgram::try_new(arena, root),
            Err(crate::low::check::SpsLowError::ContinuationContext { stack }) if stack == package));
    }

    #[test]
    fn fix_conversion_uses_a_self_named_block() {
        let mut fixture = Fixture::new();
        let recursive = fixture.def("recursive");
        let recursive_value: high::ValueId = fixture.build(recursive);
        let body_stack = fixture.build(high::Bullet);
        let body = fixture.build(high::SForce { thunk: recursive_value, stack: body_stack });
        let root_stack = fixture.build(high::Bullet);
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
}
