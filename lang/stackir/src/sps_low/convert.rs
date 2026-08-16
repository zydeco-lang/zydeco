//! Closure conversion from lexical high SPS to first-order SPSLow.

use super::{arena::Construct as _, check::SpsLowProgram, syntax as low};
use crate::sps::{
    check::BranchJoinProgram,
    syntax as high,
    variables::{FreeVars as _, Vars as _},
};
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible};
use zydeco_statics::syntax as ss;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::{context::Context, pass::CompilerPass};

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
pub struct SpsLowConverter<'a> {
    source: high::StackirArena,
    #[as_ref(low::SpsLowArena)]
    #[as_mut(low::SpsLowArena)]
    arena: low::SpsLowArena,
    root: high::CompuId,
    #[as_mut(ScopedArena)]
    scoped: &'a mut ScopedArena,
    envs: Vec<RenameEnv>,
}

impl<'a> SpsLowConverter<'a> {
    pub fn new(program: BranchJoinProgram, scoped: &'a mut ScopedArena) -> Self {
        let high::StackirRebuild { source, target, root } = program.into_program().into_rebuild();
        let arena = low::SpsLowArena {
            admin: low::SpsLowAdminArena::from_high(target.admin),
            inner: low::SpsLowInnerArena::default(),
        };
        Self {
            source,
            arena,
            root,
            scoped,
            envs: vec![RenameEnv { parent: None, bindings: HashMap::new() }],
        }
    }

    pub fn convert(mut self) -> SpsLowProgram {
        let root = self.translate_compu(self.root, RenameEnvId(0));
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
        self.scoped.insert_def(id, name);
        id
    }

    fn alloc_like(&mut self, original: high::DefId) -> high::DefId {
        self.alloc_def(self.scoped.defs[&original].clone())
    }

    fn alloc_capture(&mut self, captured: high::DefId) -> high::DefId {
        let VarName(name) = self.scoped.defs[&captured].clone();
        self.alloc_def(VarName(format!("{name}#cap")))
    }

    fn alloc_label(&mut self, role: &str) -> high::DefId {
        self.alloc_def(VarName(format!("__{role}_code__")))
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
        let mut vars: Vec<_> = (body.free_vars(&self.source) - excluded).into_iter().collect();
        vars.sort_unstable();
        vars
    }

    fn capture_bindings(&mut self, captures: &[high::DefId]) -> Vec<(high::DefId, high::DefId)> {
        captures.iter().map(|capture| (*capture, self.alloc_capture(*capture))).collect()
    }

    fn build_product_pattern(&mut self, items: Vec<low::VPatId>) -> low::VPatId {
        let arity = items.len();
        match low::ConsN::from_vec(items) {
            | Some(items) => {
                low::VCons::new(items, low::ProductLayout::conservative(arity)).build(self, None)
            }
            | None => low::Triv.build(self, None),
        }
    }

    fn build_product_value(
        &mut self, items: Vec<low::ValueId>, site: Option<ss::TermId>,
    ) -> low::ValueId {
        let arity = items.len();
        match low::ConsN::from_vec(items) {
            | Some(items) => {
                low::VCons::new(items, low::ProductLayout::conservative(arity)).build(self, site)
            }
            | None => low::Triv.build(self, site),
        }
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

    fn translate_pattern(&mut self, id: high::VPatId) -> PatternTranslation {
        let site = self.pattern_site(id);
        match self.source.inner.vpats[&id].clone() {
            | high::ValuePattern::Hole(high::Hole) => {
                PatternTranslation { pattern: low::Hole.build(self, site), bindings: Vec::new() }
            }
            | high::ValuePattern::Var(def) => {
                let translated = self.alloc_like(def);
                PatternTranslation {
                    pattern: translated.build(self, site),
                    bindings: vec![(def, translated)],
                }
            }
            | high::ValuePattern::Ctor(high::Ctor(ctor, body)) => {
                let PatternTranslation { pattern: body, bindings } = self.translate_pattern(body);
                PatternTranslation { pattern: low::Ctor(ctor, body).build(self, site), bindings }
            }
            | high::ValuePattern::Alias(high::Alias(patterns)) => {
                let (patterns, bindings): (Vec<_>, Vec<_>) = patterns
                    .into_iter()
                    .map(|pattern| {
                        let PatternTranslation { pattern, bindings } =
                            self.translate_pattern(pattern);
                        (pattern, bindings)
                    })
                    .unzip();
                PatternTranslation {
                    pattern: low::Alias(
                        low::ConsN::from_vec(patterns).expect("an alias pattern is non-empty"),
                    )
                    .build(self, site),
                    bindings: bindings.into_iter().flatten().collect(),
                }
            }
            | high::ValuePattern::Triv(high::Triv) => {
                PatternTranslation { pattern: low::Triv.build(self, site), bindings: Vec::new() }
            }
            | high::ValuePattern::VCons(high::VCons {
                items: high::ConsN(items, tail),
                layout,
            }) => {
                let (items, bindings): (Vec<_>, Vec<_>) = items
                    .into_iter()
                    .chain([tail])
                    .map(|item| {
                        let PatternTranslation { pattern, bindings } = self.translate_pattern(item);
                        (pattern, bindings)
                    })
                    .unzip();
                PatternTranslation {
                    pattern: low::VCons::new(
                        low::ConsN::from_vec(items).expect("a product pattern is non-empty"),
                        layout,
                    )
                    .build(self, site),
                    bindings: bindings.into_iter().flatten().collect(),
                }
            }
        }
    }

    fn translate_value(&mut self, id: high::ValueId, env: RenameEnvId) -> low::ValueId {
        let site = self.value_site(id);
        match self.source.inner.values[&id].clone() {
            | high::Value::Hole(high::Hole) => low::Hole.build(self, site),
            | high::Value::Var(def) => self.translated_var(def, env, site),
            | high::Value::Closure(high::Closure { stack: high::Bullet, body }) => {
                self.translate_closure(body, env, site)
            }
            | high::Value::Ctor(high::Ctor(ctor, body)) => {
                let body = self.translate_value(body, env);
                low::Ctor(ctor, body).build(self, site)
            }
            | high::Value::Triv(high::Triv) => low::Triv.build(self, site),
            | high::Value::VCons(high::VCons { items: high::ConsN(items, tail), layout }) => {
                let items = items.into_iter().map(|item| self.translate_value(item, env)).collect();
                let tail = self.translate_value(tail, env);
                low::VCons::new(low::ConsN(items, tail), layout).build(self, site)
            }
            | high::Value::Literal(literal) => literal.build(self, site),
            | high::Value::Complex(high::Complex { operator, operands }) => {
                let operands = operands
                    .into_iter()
                    .map(|operand| self.translate_value(operand, env))
                    .collect();
                low::Complex { operator, operands }.build(self, site)
            }
        }
    }

    fn translate_closure(
        &mut self, body: high::CompuId, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::ValueId {
        let captures = self.sorted_free_vars(body, Context::new());
        let capture_bindings = self.capture_bindings(&captures);
        let body_env = self.extend_env(env, capture_bindings.iter().copied());
        let body = self.translate_compu(body, body_env);
        let environment_pattern = self.captured_pattern(&capture_bindings);
        let incoming = low::Bullet.build(self, site);
        let body =
            low::LetArg { binder: environment_pattern, bindee: incoming, body }.build(self, site);
        let label = self.alloc_label("closure");
        let code = low::Block { label, body }.build(self, site);
        let environment = self.captured_value_outside(&captures, env, site);
        low::ClosurePackage { environment, code }.build(self, site)
    }

    fn translate_stack(&mut self, id: high::StackId, env: RenameEnvId) -> low::StackId {
        let site = self.stack_site(id);
        match self.source.inner.stacks[&id].clone() {
            | high::Stack::Kont(high::Kont { binder, body }) => {
                self.translate_continuation(binder, body, env, site)
            }
            | high::Stack::Var(high::Bullet) => low::Bullet.build(self, site),
            | high::Stack::Arg(high::Cons(value, stack)) => {
                let value = self.translate_value(value, env);
                let stack = self.translate_stack(stack, env);
                low::Cons(value, stack).build(self, site)
            }
            | high::Stack::Tag(high::Cons(dtor, stack)) => {
                let stack = self.translate_stack(stack, env);
                low::Cons(dtor, stack).build(self, site)
            }
        }
    }

    fn translate_continuation(
        &mut self, binder: high::VPatId, body: high::CompuId, env: RenameEnvId,
        site: Option<ss::TermId>,
    ) -> low::StackId {
        let captures = self.sorted_free_vars(body, binder.vars(&self.source));
        let capture_bindings = self.capture_bindings(&captures);
        let PatternTranslation { pattern: binder, bindings: binder_bindings } =
            self.translate_pattern(binder);
        let capture_env = self.extend_env(env, capture_bindings.iter().copied());
        let body_env = self.extend_env(capture_env, binder_bindings);
        let body = self.translate_compu(body, body_env);

        let environment_pattern = self.captured_pattern(&capture_bindings);
        let incoming_environment = low::Bullet.build(self, site);
        let body = low::LetArg { binder: environment_pattern, bindee: incoming_environment, body }
            .build(self, site);
        let incoming_value = low::Bullet.build(self, site);
        let body = low::LetArg { binder, bindee: incoming_value, body }.build(self, site);
        let label = self.alloc_label("continuation");
        let code = low::Block { label, body }.build(self, site);

        let environment = self.captured_value_outside(&captures, env, site);
        let ambient = low::Bullet.build(self, site);
        let residual = low::Cons(environment, ambient).build(self, site);
        low::ContinuationPackage { code, residual }.build(self, site)
    }

    fn translate_compu(&mut self, id: high::CompuId, env: RenameEnvId) -> low::CompuId {
        let site = self.compu_site(id);
        match self.source.inner.compus[&id].clone() {
            | high::Computation::Hole(high::SHole(stack)) => {
                let stack = self.translate_stack(stack, env);
                low::SHole(stack).build(self, site)
            }
            | high::Computation::Force(force) => self.translate_force(force, env, site),
            | high::Computation::Ret(ret) => self.translate_return(ret, env, site),
            | high::Computation::Fix(fix) => self.translate_fix(fix, env, site),
            | high::Computation::ProductMatch(high::SProductMatch { scrut, binder, body }) => {
                let scrut = self.translate_value(scrut, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let body_env = self.extend_env(env, bindings);
                let body = self.translate_compu(body, body_env);
                low::SProductMatch { scrut, binder, body }.build(self, site)
            }
            | high::Computation::CoprodMatch(high::SCoprodMatch { scrut, arms }) => {
                let scrut = self.translate_value(scrut, env);
                let arms = arms
                    .into_iter()
                    .map(|high::Matcher { binder, tail }| {
                        let PatternTranslation { pattern: binder, bindings } =
                            self.translate_pattern(binder);
                        let body_env = self.extend_env(env, bindings);
                        low::Matcher { binder, tail: self.translate_compu(tail, body_env) }
                    })
                    .collect();
                low::SCoprodMatch { scrut, arms }.build(self, site)
            }
            | high::Computation::Join(high::LetJoin::Value(high::Let { binder, bindee, tail })) => {
                let bindee = self.translate_value(bindee, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let body_env = self.extend_env(env, bindings);
                let body = self.translate_compu(tail, body_env);
                low::LetValue { binder, bindee, body }.build(self, site)
            }
            | high::Computation::Join(high::LetJoin::Stack(high::Let {
                binder: high::Bullet,
                bindee,
                tail,
            })) => {
                let bindee = self.translate_stack(bindee, env);
                let body = self.translate_compu(tail, env);
                low::LetStack { bindee, body }.build(self, site)
            }
            | high::Computation::LetArg(high::Let {
                binder: high::Cons(binder, high::Bullet),
                bindee,
                tail,
            }) => {
                let bindee = self.translate_stack(bindee, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let body_env = self.extend_env(env, bindings);
                let body = self.translate_compu(tail, body_env);
                low::LetArg { binder, bindee, body }.build(self, site)
            }
            | high::Computation::CoCase(high::SCoMatch { scrut, arms }) => {
                let scrut = self.translate_stack(scrut, env);
                let arms = arms
                    .into_iter()
                    .map(|high::CoMatcher { dtor, tail }| low::CoMatcher {
                        dtor,
                        tail: self.translate_compu(tail, env),
                    })
                    .collect();
                low::SCoMatch { scrut, arms }.build(self, site)
            }
            | high::Computation::ExternCall(high::ExternCall { function, stack }) => {
                let stack = self.translate_stack(stack, env);
                low::ExternCall { function, stack }.build(self, site)
            }
        }
    }

    fn translate_force(
        &mut self, force: high::SForce, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::CompuId {
        let package = self.translate_value(force.thunk, env);
        let stack = self.translate_stack(force.stack, env);
        let environment_def = self.alloc_def(VarName("__environment__".into()));
        let code_def = self.alloc_def(VarName("__closure_code__".into()));
        let environment: low::VPatId = environment_def.build(self, None);
        let code: low::VPatId = code_def.build(self, None);
        let environment_value: low::ValueId = environment_def.build(self, site);
        let code_value: low::ValueId = code_def.build(self, site);
        let stack = low::Cons(environment_value, stack).build(self, site);
        let body = low::Jump { target: code_value, stack }.build(self, site);
        low::OpenClosure { package, environment, code, body }.build(self, site)
    }

    fn translate_return(
        &mut self, ret: high::SReturn, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::CompuId {
        let package = self.translate_stack(ret.stack, env);
        let value = self.translate_value(ret.value, env);
        let code_def = self.alloc_def(VarName("__continuation_code__".into()));
        let code: low::VPatId = code_def.build(self, None);
        let code_value: low::ValueId = code_def.build(self, site);
        let residual = low::Bullet.build(self, site);
        let stack = low::Cons(value, residual).build(self, site);
        let body = low::Jump { target: code_value, stack }.build(self, site);
        low::OpenContinuation { package, code, body }.build(self, site)
    }

    fn translate_fix(
        &mut self, fix: high::SFix, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> low::CompuId {
        let stack = self.translate_stack(fix.stack, env);
        let captures = self.sorted_free_vars(fix.body, Context::singleton(fix.param));
        let capture_bindings = self.capture_bindings(&captures);
        let recursive_closure = self.alloc_like(fix.param);
        let label = self.alloc_label("fix");
        let body_env = self.extend_env(
            env,
            capture_bindings.iter().copied().chain([(fix.param, recursive_closure)]),
        );
        let body = self.translate_compu(fix.body, body_env);

        let environment_inside = self.captured_value_inside(&capture_bindings, site);
        let code_inside: low::ValueId = label.build(self, site);
        let closure = low::ClosurePackage { environment: environment_inside, code: code_inside }
            .build(self, site);
        let closure_pattern: low::VPatId = recursive_closure.build(self, None);
        let body =
            low::LetValue { binder: closure_pattern, bindee: closure, body }.build(self, site);
        let environment_pattern = self.captured_pattern(&capture_bindings);
        let incoming = low::Bullet.build(self, site);
        let body =
            low::LetArg { binder: environment_pattern, bindee: incoming, body }.build(self, site);
        let block = low::Block { label, body }.build(self, site);

        let environment = self.captured_value_outside(&captures, env, site);
        let stack = low::Cons(environment, stack).build(self, site);
        low::Jump { target: block, stack }.build(self, site)
    }
}

impl CompilerPass for SpsLowConverter<'_> {
    type Arena = low::SpsLowArena;
    type Out = SpsLowProgram;
    type Error = Infallible;

    fn run(self) -> Result<Self::Out, Self::Error> {
        Ok(self.convert())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sps::arena::Construct as HighConstruct;

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
            self.scoped.insert_def(def, VarName(name.into()));
            def
        }

        fn build<U, S, T>(&mut self, node: U) -> T
        where
            U: HighConstruct<S, T, high::StackirArena>,
        {
            HighConstruct::build(node, &mut self.arena, None)
        }

        fn convert(self, root: high::CompuId) -> SpsLowProgram {
            let program =
                BranchJoinProgram::try_new(high::StackirProgram { arena: self.arena, root })
                    .unwrap();
            let mut scoped = self.scoped;
            SpsLowConverter::new(program, &mut scoped).convert()
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
        assert!(
            arena
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
        );
        assert!(arena.inner.values.iter().any(|(_, value)| matches!(value, low::Value::Block(_))));
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
        assert!(matches!(
            program.arena().inner.stacks[package],
            low::Stack::ContinuationPackage(_)
        ));
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
