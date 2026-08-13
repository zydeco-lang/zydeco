//! Closure conversion.
//!
//! This pass rebuilds the reachable Stack IR graph instead of rewriting arena
//! nodes in place.  A source node can be shared by several lexical occurrences,
//! so mutating it while substituting captures would let one occurrence change
//! the meaning of another.  The translation rebuilds every reachable occurrence
//! as lexical syntax in a fresh output arena; source storage is read-only and is
//! dropped with the consumed input program.

use super::{check::BranchJoinProgram, syntax::*, variables::FreeVars as _};
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible};
use {
    zydeco_statics::syntax as ss, zydeco_surface::scoped::arena::ScopedArena,
    zydeco_syntax::VarName, zydeco_utils::prelude::CompilerPass,
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct RenameEnvId(usize);

#[derive(Debug)]
struct RenameEnv {
    parent: Option<RenameEnvId>,
    bindings: HashMap<DefId, DefId>,
}

struct PatternTranslation {
    pattern: VPatId,
    bindings: Vec<(DefId, DefId)>,
}

/// Make thunk captures explicit without mutating any reachable source node.
///
/// With the transitional CPS pass enabled, continuation captures arrive here as
/// thunk captures too. A distinct `SPS_l` pass will replace that encoding.
#[derive(AsRef, AsMut)]
pub struct ClosureConverter<'a> {
    source: StackirArena,
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    arena: StackirArena,
    root: CompuId,
    #[as_mut(ScopedArena)]
    scoped: &'a mut ScopedArena,
    envs: Vec<RenameEnv>,
}

impl<'a> ClosureConverter<'a> {
    pub fn new(program: BranchJoinProgram, scoped: &'a mut ScopedArena) -> Self {
        let StackirRebuild { source, target, root } = program.into_program().into_rebuild();
        Self {
            source,
            arena: target,
            root,
            scoped,
            envs: vec![RenameEnv { parent: None, bindings: HashMap::new() }],
        }
    }

    pub fn convert(mut self) -> BranchJoinProgram {
        let root_env = RenameEnvId(0);
        let root = self.translate_compu(self.root, root_env);
        BranchJoinProgram::try_new(StackirProgram { arena: self.arena, root })
            .expect("closure conversion preserves lexical branch-join Stack IR")
    }

    fn extend_env(
        &mut self, parent: RenameEnvId, bindings: impl IntoIterator<Item = (DefId, DefId)>,
    ) -> RenameEnvId {
        let id = RenameEnvId(self.envs.len());
        self.envs
            .push(RenameEnv { parent: Some(parent), bindings: bindings.into_iter().collect() });
        id
    }

    fn renamed_def(&self, mut env: RenameEnvId, def: DefId) -> DefId {
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

    fn alloc_def(&mut self, name: VarName) -> DefId {
        let id = self.arena.admin.fresh();
        self.scoped.insert_def(id, name);
        id
    }

    fn alloc_capture(&mut self, captured: DefId) -> DefId {
        let VarName(name) = self.scoped.defs[&captured].clone();
        self.alloc_def(VarName(format!("{name}#cap")))
    }

    fn alloc_like(&mut self, original: DefId) -> DefId {
        self.alloc_def(self.scoped.defs[&original].clone())
    }

    fn alloc_closure(&mut self, param: DefId) -> DefId {
        let VarName(name) = self.scoped.defs[&param].clone();
        self.alloc_def(VarName(format!("{name}#clo")))
    }

    fn compu_site(&self, id: CompuId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Compu(id)).copied()
    }

    fn value_site(&self, id: ValueId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Value(id)).copied()
    }

    fn stack_site(&self, id: StackId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Stack(id)).copied()
    }

    fn pattern_site(&self, id: VPatId) -> Option<ss::PatId> {
        self.source.admin.pats.back(&id).copied()
    }

    fn sorted_free_vars(&self, body: CompuId) -> Vec<DefId> {
        let mut vars: Vec<_> = body.free_vars(&self.source).into_iter().collect();
        vars.sort_unstable();
        vars
    }

    fn build_product_pattern(&mut self, items: Vec<VPatId>) -> VPatId {
        let arity = items.len();
        match ConsN::from_vec(items) {
            | Some(items) => VCons::new(items, ProductLayout { arity }).build(self, None),
            | None => Triv.build(self, None),
        }
    }

    fn build_product_value(&mut self, items: Vec<ValueId>, site: Option<ss::TermId>) -> ValueId {
        let arity = items.len();
        match ConsN::from_vec(items) {
            | Some(items) => VCons::new(items, ProductLayout { arity }).build(self, site),
            | None => Triv.build(self, site),
        }
    }

    fn translated_var(
        &mut self, def: DefId, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> ValueId {
        self.renamed_def(env, def).build(self, site)
    }

    fn translate_pattern(&mut self, id: VPatId) -> PatternTranslation {
        let site = self.pattern_site(id);
        let pattern = self.source.inner.vpats[&id].clone();
        match pattern {
            | ValuePattern::Hole(Hole) => {
                PatternTranslation { pattern: Hole.build(self, site), bindings: Vec::new() }
            }
            | ValuePattern::Var(def) => {
                let translated = self.alloc_like(def);
                PatternTranslation {
                    pattern: translated.build(self, site),
                    bindings: vec![(def, translated)],
                }
            }
            | ValuePattern::Ctor(Ctor(ctor, body)) => {
                let PatternTranslation { pattern: body, bindings } = self.translate_pattern(body);
                PatternTranslation { pattern: Ctor(ctor, body).build(self, site), bindings }
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                let (patterns, bindings): (Vec<_>, Vec<_>) = patterns
                    .into_iter()
                    .map(|pattern| {
                        let PatternTranslation { pattern, bindings } =
                            self.translate_pattern(pattern);
                        (pattern, bindings)
                    })
                    .unzip();
                let patterns = ConsN::from_vec(patterns).expect("an alias pattern is non-empty");
                PatternTranslation {
                    pattern: Alias(patterns).build(self, site),
                    bindings: bindings.into_iter().flatten().collect(),
                }
            }
            | ValuePattern::Triv(Triv) => {
                PatternTranslation { pattern: Triv.build(self, site), bindings: Vec::new() }
            }
            | ValuePattern::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let (items, bindings): (Vec<_>, Vec<_>) = items
                    .into_iter()
                    .chain([tail])
                    .map(|item| {
                        let PatternTranslation { pattern, bindings } = self.translate_pattern(item);
                        (pattern, bindings)
                    })
                    .unzip();
                let items = ConsN::from_vec(items).expect("a product pattern is non-empty");
                PatternTranslation {
                    pattern: VCons::new(items, layout).build(self, site),
                    bindings: bindings.into_iter().flatten().collect(),
                }
            }
        }
    }

    fn translate_value(&mut self, id: ValueId, env: RenameEnvId) -> ValueId {
        let site = self.value_site(id);
        let value = self.source.inner.values[&id].clone();
        match value {
            | Value::Hole(Hole) => Hole.build(self, site),
            | Value::Var(def) => self.translated_var(def, env, site),
            | Value::Closure(Closure { stack: Bullet, body }) => {
                self.translate_closure(body, env, site)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = self.translate_value(body, env);
                Ctor(ctor, body).build(self, site)
            }
            | Value::Triv(Triv) => Triv.build(self, site),
            | Value::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.into_iter().map(|item| self.translate_value(item, env)).collect();
                let tail = self.translate_value(tail, env);
                VCons::new(ConsN(items, tail), layout).build(self, site)
            }
            | Value::Literal(literal) => literal.build(self, site),
            | Value::Complex(Complex { operator, operands }) => {
                let operands = operands
                    .into_iter()
                    .map(|operand| self.translate_value(operand, env))
                    .collect();
                Complex { operator, operands }.build(self, site)
            }
        }
    }

    fn translate_closure(
        &mut self, body: CompuId, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> ValueId {
        let captures = self.sorted_free_vars(body);
        let capture_bindings = captures
            .iter()
            .map(|capture| (*capture, self.alloc_capture(*capture)))
            .collect::<Vec<_>>();
        let body_env = self.extend_env(env, capture_bindings.iter().copied());
        let body = self.translate_compu(body, body_env);

        let capture_patterns: Vec<VPatId> =
            capture_bindings.iter().map(|(_, captured)| captured.build(self, None)).collect();
        let capture_pattern = self.build_product_pattern(capture_patterns);
        let capture_values =
            captures.into_iter().map(|capture| self.translated_var(capture, env, site)).collect();
        let captures = self.build_product_value(capture_values, site);

        let incoming = Bullet.build(self, site);
        let body = Let { binder: Cons(capture_pattern, Bullet), bindee: incoming, tail: body }
            .build(self, site);
        let code = Closure { stack: Bullet, body }.build(self, site);
        VCons::new(ConsN(vec![captures], code), ProductLayout { arity: 2 }).build(self, site)
    }

    fn translate_stack(&mut self, id: StackId, env: RenameEnvId) -> StackId {
        let site = self.stack_site(id);
        let stack = self.source.inner.stacks[&id].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let body_env = self.extend_env(env, bindings);
                let body = self.translate_compu(body, body_env);
                Kont { binder, body }.build(self, site)
            }
            | Stack::Var(Bullet) => Bullet.build(self, site),
            | Stack::Arg(Cons(value, stack)) => {
                let value = self.translate_value(value, env);
                let stack = self.translate_stack(stack, env);
                Cons(value, stack).build(self, site)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = self.translate_stack(stack, env);
                Cons(dtor, stack).build(self, site)
            }
        }
    }

    fn translate_compu(&mut self, id: CompuId, env: RenameEnvId) -> CompuId {
        let site = self.compu_site(id);
        let compu = self.source.inner.compus[&id].clone();
        match compu {
            | Computation::Hole(SHole(stack)) => {
                let stack = self.translate_stack(stack, env);
                SHole(stack).build(self, site)
            }
            | Computation::Force(force) => self.translate_force(force, env, site),
            | Computation::Ret(SReturn { stack, value }) => {
                let stack = self.translate_stack(stack, env);
                let value = self.translate_value(value, env);
                SReturn { stack, value }.build(self, site)
            }
            | Computation::Fix(fix) => self.translate_fix(fix, env, site),
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                let scrut = self.translate_value(scrut, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let body_env = self.extend_env(env, bindings);
                let body = self.translate_compu(body, body_env);
                SProductMatch { scrut, binder, body }.build(self, site)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                let scrut = self.translate_value(scrut, env);
                let arms = arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| {
                        let PatternTranslation { pattern: binder, bindings } =
                            self.translate_pattern(binder);
                        let body_env = self.extend_env(env, bindings);
                        Matcher { binder, tail: self.translate_compu(tail, body_env) }
                    })
                    .collect();
                SCoprodMatch { scrut, arms }.build(self, site)
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                let bindee = self.translate_value(bindee, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let tail_env = self.extend_env(env, bindings);
                let tail = self.translate_compu(tail, tail_env);
                Let { binder, bindee, tail }.build(self, site)
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                let bindee = self.translate_stack(bindee, env);
                let tail = self.translate_compu(tail, env);
                Let { binder: Bullet, bindee, tail }.build(self, site)
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                let bindee = self.translate_stack(bindee, env);
                let PatternTranslation { pattern: binder, bindings } =
                    self.translate_pattern(binder);
                let tail_env = self.extend_env(env, bindings);
                let tail = self.translate_compu(tail, tail_env);
                Let { binder: Cons(binder, Bullet), bindee, tail }.build(self, site)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = self.translate_stack(scrut, env);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let branch_env = self.extend_env(env, []);
                        CoMatcher { dtor, tail: self.translate_compu(tail, branch_env) }
                    })
                    .collect();
                SCoMatch { scrut, arms }.build(self, site)
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let stack = self.translate_stack(stack, env);
                ExternCall { function, stack }.build(self, site)
            }
        }
    }

    fn translate_force(
        &mut self, force: SForce, env: RenameEnvId, site: Option<ss::TermId>,
    ) -> CompuId {
        let thunk = self.translate_value(force.thunk, env);
        let stack = self.translate_stack(force.stack, env);

        let captures = self.alloc_def(VarName("__env__".into()));
        let code = self.alloc_def(VarName("__code__".into()));
        let pair_pattern = VCons::new(
            ConsN(vec![captures.build(self, None)], code.build(self, None)),
            ProductLayout { arity: 2 },
        )
        .build(self, None);

        let captures: ValueId = captures.build(self, site);
        let code: ValueId = code.build(self, site);
        let stack = Cons(captures, stack).build(self, site);
        let invoke = SForce { thunk: code, stack }.build(self, site);
        Let { binder: pair_pattern, bindee: thunk, tail: invoke }.build(self, site)
    }

    fn translate_fix(&mut self, fix: SFix, env: RenameEnvId, site: Option<ss::TermId>) -> CompuId {
        let stack = self.translate_stack(fix.stack, env);
        let captures = self
            .sorted_free_vars(fix.body)
            .into_iter()
            .filter(|capture| *capture != fix.param)
            .collect::<Vec<_>>();
        let capture_bindings = captures
            .iter()
            .map(|capture| (*capture, self.alloc_capture(*capture)))
            .collect::<Vec<_>>();
        let closure = self.alloc_closure(fix.param);
        let code_param = self.alloc_like(fix.param);
        let body_env =
            self.extend_env(env, capture_bindings.iter().copied().chain([(fix.param, closure)]));
        let body = self.translate_compu(fix.body, body_env);

        let capture_patterns: Vec<VPatId> =
            capture_bindings.iter().map(|(_, captured)| captured.build(self, None)).collect();
        let capture_pattern = self.build_product_pattern(capture_patterns);
        let captured_values: Vec<ValueId> =
            capture_bindings.iter().map(|(_, captured)| captured.build(self, site)).collect();
        let captured_values = self.build_product_value(captured_values, site);
        let code: ValueId = code_param.build(self, site);
        let recursive_closure =
            VCons::new(ConsN(vec![captured_values], code), ProductLayout { arity: 2 })
                .build(self, site);
        let closure_pattern: VPatId = closure.build(self, None);
        let body = Let { binder: closure_pattern, bindee: recursive_closure, tail: body }
            .build(self, site);
        let incoming = Bullet.build(self, site);
        let body = Let { binder: Cons(capture_pattern, Bullet), bindee: incoming, tail: body }
            .build(self, site);

        let captured_values =
            captures.into_iter().map(|capture| self.translated_var(capture, env, site)).collect();
        let captured_values = self.build_product_value(captured_values, site);
        let stack = Cons(captured_values, stack).build(self, site);
        SFix { param: code_param, stack, body }.build(self, site)
    }
}

impl CompilerPass for ClosureConverter<'_> {
    type Arena = StackirArena;
    type Out = BranchJoinProgram;
    type Error = Infallible;

    fn run(self) -> Result<Self::Out, Self::Error> {
        Ok(self.convert())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closure_conversion_builds_a_fresh_output_arena() {
        let mut arena = StackirArena::default();
        let mut scoped = ScopedArena::default();
        let captured = arena.admin.fresh();
        scoped.insert_def(captured, VarName("captured".into()));

        let captured_use: ValueId = captured.build(&mut arena, None);
        let closure_stack = Bullet.build(&mut arena, None);
        let closure_body =
            SReturn { stack: closure_stack, value: captured_use }.build(&mut arena, None);
        let closure = Closure { stack: Bullet, body: closure_body }.build(&mut arena, None);
        let root_stack = Bullet.build(&mut arena, None);
        let tail = SReturn { stack: root_stack, value: closure }.build(&mut arena, None);
        let binder: VPatId = captured.build(&mut arena, None);
        let bindee = Triv.build(&mut arena, None);
        let root = Let { binder, bindee, tail }.build(&mut arena, None);
        let program = BranchJoinProgram::try_new(StackirProgram { arena, root }).unwrap();

        super::super::check::check(program.as_program(), &scoped);
        let program = ClosureConverter::new(program, &mut scoped).convert();
        let output = program.as_program();

        assert_ne!(output.root, root);
        assert!(output.arena.inner.compus.get(&root).is_none());
        assert!(output.arena.inner.compus.get(&closure_body).is_none());
        assert!(output.arena.inner.values.get(&closure).is_none());
        assert!(output.arena.inner.values.get(&captured_use).is_none());
        super::super::check::check(output, &scoped);
    }
}
