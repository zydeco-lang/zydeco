//! Administrative reductions and known primitive calls in lexical high SPS.
//!
//! Facts describe suspended external calls and product structure; they never
//! evaluate a computation. Only an external call on the thunk's incoming stack
//! can replace a force. General closures and recursion remain residual.

use super::{check::BranchJoinProgram, syntax::*, variables::Vars as _};
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, rc::Rc};
use zydeco_statics::syntax as ss;

#[derive(Clone, Default)]
enum KnownValue {
    #[default]
    Unknown,
    External(ExternalFunction),
    Product(Vec<Rc<KnownValue>>),
    Constructor(CtorIdx, Rc<KnownValue>),
}

#[derive(Clone, Copy)]
struct EnvId(usize);

struct Environment {
    parent: Option<EnvId>,
    bindings: HashMap<DefId, Rc<KnownValue>>,
}

/// A rebuilt node with the free variables of its surviving subtree. Keeping
/// these facts bottom-up avoids rescanning every tail when dropping dead lets.
struct Residual<T> {
    node: T,
    free: CoContext<DefId>,
}

/// Consume a lexical program and rebuild its normalized tree with fresh nodes.
/// No source body is duplicated, so definitions retain their lexical identity.
#[derive(AsRef, AsMut)]
pub struct Normalizer {
    source: StackirArena,
    #[as_ref]
    #[as_mut]
    arena: StackirArena,
    root: CompuId,
    envs: Vec<Environment>,
}

impl Normalizer {
    pub fn new(program: BranchJoinProgram) -> Self {
        let StackirRebuild { source, target: arena, root } = program.into_program().into_rebuild();
        let envs = vec![Environment { parent: None, bindings: HashMap::new() }];
        Self { source, arena, root, envs }
    }

    pub fn run(mut self) -> BranchJoinProgram {
        let root = self.compu(self.root, EnvId(0)).node;
        BranchJoinProgram::try_new(StackirProgram::new(self.arena, root))
            .expect("normalization preserves lexical ownership and branch joins")
    }

    fn lookup(&self, mut env: EnvId, def: DefId) -> Rc<KnownValue> {
        loop {
            let Environment { parent, bindings } = &self.envs[env.0];
            if let Some(value) = bindings.get(&def) {
                return value.clone();
            }
            match parent {
                | Some(parent) => env = *parent,
                | None => return Rc::default(),
            }
        }
    }

    fn extend(
        &mut self, parent: EnvId, bindings: impl IntoIterator<Item = (DefId, Rc<KnownValue>)>,
    ) -> EnvId {
        let env = EnvId(self.envs.len());
        self.envs
            .push(Environment { parent: Some(parent), bindings: bindings.into_iter().collect() });
        env
    }

    fn bind(&mut self, env: EnvId, binder: VPatId, value: Rc<KnownValue>) -> EnvId {
        let bindings = self.pattern_facts(binder, value);
        self.extend(env, bindings)
    }

    fn pattern_facts(&self, id: VPatId, value: Rc<KnownValue>) -> Vec<(DefId, Rc<KnownValue>)> {
        match &self.source.inner.vpats[&id] {
            | ValuePattern::Var(def) => vec![(*def, value)],
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => Vec::new(),
            | ValuePattern::Alias(Alias(patterns)) => patterns
                .iter()
                .flat_map(|pattern| self.pattern_facts(*pattern, value.clone()))
                .collect(),
            | ValuePattern::Ctor(Ctor(ctor, body)) => {
                let value = match value.as_ref() {
                    | KnownValue::Constructor(tag, body) if tag == ctor => body.clone(),
                    | _ => Rc::default(),
                };
                self.pattern_facts(*body, value)
            }
            | ValuePattern::VCons(VCons { items, layout }) => items
                .iter()
                .enumerate()
                .flat_map(|(position, pattern)| {
                    let field = match value.as_ref() {
                        | KnownValue::Product(fields) if fields.len() == layout.arity => {
                            if position + 1 == items.len() && items.len() < layout.arity {
                                Rc::new(KnownValue::Product(fields[position..].to_vec()))
                            } else {
                                fields[position].clone()
                            }
                        }
                        | _ => Rc::default(),
                    };
                    self.pattern_facts(*pattern, field)
                })
                .collect(),
        }
    }

    fn known(&self, id: ValueId, env: EnvId) -> Rc<KnownValue> {
        match &self.source.inner.values[&id] {
            | Value::Var(def) => self.lookup(env, *def),
            | Value::Closure(Closure { body, .. }) => {
                if let Computation::ExternCall(ExternCall { function, stack }) =
                    &self.source.inner.compus[body]
                    && matches!(self.source.inner.stacks[stack], Stack::Var(Bullet))
                {
                    Rc::new(KnownValue::External(function.clone()))
                } else {
                    Rc::default()
                }
            }
            | Value::VCons(VCons { items, layout }) => {
                let fields = items.iter().map(|item| self.known(*item, env)).collect::<Vec<_>>();
                let fields = if fields.len() < layout.arity {
                    let prefix = fields.len() - 1;
                    let tail = match fields[prefix].as_ref() {
                        | KnownValue::Product(tail) if tail.len() == layout.arity - prefix => {
                            tail.clone()
                        }
                        | _ => vec![Rc::default(); layout.arity - prefix],
                    };
                    fields.into_iter().take(prefix).chain(tail).collect()
                } else {
                    fields
                };
                Rc::new(KnownValue::Product(fields))
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                Rc::new(KnownValue::Constructor(ctor.clone(), self.known(*body, env)))
            }
            | Value::Hole(_) | Value::Triv(_) | Value::Literal(_) | Value::Complex(_) => {
                Rc::default()
            }
        }
    }

    fn discardable(&self, id: ValueId) -> bool {
        match &self.source.inner.values[&id] {
            | Value::Var(_) | Value::Triv(_) | Value::Literal(_) | Value::Closure(_) => true,
            | Value::Ctor(Ctor(_, body)) => self.discardable(*body),
            | Value::VCons(VCons { items, .. }) => items.iter().all(|item| self.discardable(*item)),
            // An intrinsic value can trap; a dead result must not suppress it.
            | Value::Hole(_) | Value::Complex(_) => false,
        }
    }

    fn pattern(&mut self, id: VPatId) -> VPatId {
        let site = self.source.admin.pats.back(&id).copied();
        let pattern: ValuePattern = match self.source.inner.vpats[&id].clone() {
            | ValuePattern::Ctor(Ctor(ctor, body)) => Ctor(ctor, self.pattern(body)).into(),
            | ValuePattern::Alias(Alias(patterns)) => Alias(
                ConsN::from_vec(
                    patterns.into_iter().map(|pattern| self.pattern(pattern)).collect(),
                )
                .expect("an alias pattern is nonempty"),
            )
            .into(),
            | ValuePattern::VCons(VCons { items, layout }) => {
                VCons::new(items.into_iter().map(|item| self.pattern(item)).collect(), layout)
                    .into()
            }
            | pattern => pattern,
        };
        pattern.build(self, site)
    }

    fn value(&mut self, id: ValueId, env: EnvId) -> Residual<ValueId> {
        let site = self.source.admin.terms.back(&TermId::Value(id)).copied();
        let (value, free): (Value, _) = match self.source.inner.values[&id].clone() {
            | Value::Var(def) => (def.into(), CoContext::singleton(def)),
            | Value::Closure(Closure { stack, body }) => {
                let body = self.compu(body, env);
                (Closure { stack, body: body.node }.into(), body.free)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = self.value(body, env);
                (Ctor(ctor, body.node).into(), body.free)
            }
            | Value::VCons(VCons { items, layout }) => {
                let (items, free) = self.values(items, env);
                (VCons::new(items, layout).into(), free)
            }
            | Value::Complex(Complex { operator, operands }) => {
                let (operands, free) = self.values(operands, env);
                (Complex { operator, operands }.into(), free)
            }
            | value => (value, CoContext::new()),
        };
        Residual { node: value.build(self, site), free }
    }

    fn values(&mut self, values: Vec<ValueId>, env: EnvId) -> (Vec<ValueId>, CoContext<DefId>) {
        let (values, free): (Vec<_>, Vec<_>) = values
            .into_iter()
            .map(|value| {
                let Residual { node, free } = self.value(value, env);
                (node, free)
            })
            .unzip();
        (values, free.into_iter().flatten().collect())
    }

    fn stack(&mut self, id: StackId, env: EnvId) -> Residual<StackId> {
        let site = self.source.admin.terms.back(&TermId::Stack(id)).copied();
        let (stack, free): (Stack, _) = match self.source.inner.stacks[&id].clone() {
            | Stack::Var(bullet) => (bullet.into(), CoContext::new()),
            | Stack::Arg(Cons(value, stack)) => {
                let value = self.value(value, env);
                let stack = self.stack(stack, env);
                (Cons(value.node, stack.node).into(), value.free + stack.free)
            }
            | Stack::Tag(Cons(tag, stack)) => {
                let stack = self.stack(stack, env);
                (Cons(tag, stack.node).into(), stack.free)
            }
            | Stack::Kont(Kont { binder, body }) => {
                let body_env = self.bind(env, binder, Rc::default());
                let body = self.compu(body, body_env);
                let free = body.free - binder.vars(&self.source);
                let binder = self.pattern(binder);
                (Kont { binder, body: body.node }.into(), free)
            }
        };
        Residual { node: stack.build(self, site), free }
    }

    fn binding(
        &mut self, binder: VPatId, bindee: ValueId, tail: CompuId, env: EnvId,
        site: Option<ss::TermId>,
    ) -> Residual<CompuId> {
        let body_env = self.bind(env, binder, self.known(bindee, env));
        let tail = self.compu(tail, body_env);
        let bound = binder.vars(&self.source);
        if !tail.free.iter().any(|def| bound.iter().any(|bound| bound == def))
            && self.discardable(bindee)
        {
            return tail;
        }
        let bindee = self.value(bindee, env);
        let binder = self.pattern(binder);
        let free = (tail.free - bound) + bindee.free;
        let node = Let { binder, bindee: bindee.node, tail: tail.node }.build(self, site);
        Residual { node, free }
    }

    fn compu(&mut self, id: CompuId, env: EnvId) -> Residual<CompuId> {
        let site = self.source.admin.terms.back(&TermId::Compu(id)).copied();
        let (compu, free): (Computation<LetJoin>, _) = match self.source.inner.compus[&id].clone() {
            | Computation::Hole(SHole(stack)) => {
                let stack = self.stack(stack, env);
                (SHole(stack.node).into(), stack.free)
            }
            | Computation::Force(SForce { thunk, stack }) => {
                let known = self.known(thunk, env);
                let stack = self.stack(stack, env);
                if let KnownValue::External(function) = known.as_ref() {
                    (
                        ExternCall { function: function.clone(), stack: stack.node }.into(),
                        stack.free,
                    )
                } else {
                    let thunk = self.value(thunk, env);
                    (
                        SForce { thunk: thunk.node, stack: stack.node }.into(),
                        thunk.free + stack.free,
                    )
                }
            }
            | Computation::Ret(SReturn { stack, value }) => {
                if let Stack::Kont(Kont { binder, body }) = self.source.inner.stacks[&stack] {
                    return self.binding(binder, value, body, env, site);
                }
                let stack = self.stack(stack, env);
                let value = self.value(value, env);
                (SReturn { stack: stack.node, value: value.node }.into(), stack.free + value.free)
            }
            | Computation::Fix(SFix { param, stack, body }) => {
                let stack = self.stack(stack, env);
                let body_env = self.extend(env, [(param, Rc::default())]);
                let body = self.compu(body, body_env);
                (
                    SFix { param, stack: stack.node, body: body.node }.into(),
                    stack.free + (body.free - &param),
                )
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                return self.binding(binder, scrut, body, env, site);
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                return self.binding(binder, bindee, tail, env, site);
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                let known = self.known(scrut, env);
                let scrut = self.value(scrut, env);
                let (arms, free): (Vec<_>, Vec<_>) = arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| {
                        let body_env = self.bind(env, binder, known.clone());
                        let tail = self.compu(tail, body_env);
                        let free = tail.free - binder.vars(&self.source);
                        let binder = self.pattern(binder);
                        (Matcher { binder, tail: tail.node }, free)
                    })
                    .unzip();
                (
                    SCoprodMatch { scrut: scrut.node, arms }.into(),
                    scrut.free + free.into_iter().flatten(),
                )
            }
            | Computation::Join(LetJoin::Stack(Let { binder, bindee, tail })) => {
                let bindee = self.stack(bindee, env);
                let tail = self.compu(tail, env);
                (
                    Let { binder, bindee: bindee.node, tail: tail.node }.into(),
                    bindee.free + tail.free,
                )
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                let known = match self.source.inner.stacks[&bindee] {
                    | Stack::Arg(Cons(value, rest)) => {
                        // Consuming a freshly pushed argument leaves the same
                        // ambient stack, so this is an ordinary value binding.
                        if matches!(self.source.inner.stacks[&rest], Stack::Var(Bullet)) {
                            return self.binding(binder, value, tail, env, site);
                        }
                        self.known(value, env)
                    }
                    | _ => Rc::default(),
                };
                let body_env = self.bind(env, binder, known);
                let tail = self.compu(tail, body_env);
                let free = tail.free - binder.vars(&self.source);
                let bindee = self.stack(bindee, env);
                let binder = Cons(self.pattern(binder), Bullet);
                (Let { binder, bindee: bindee.node, tail: tail.node }.into(), bindee.free + free)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = self.stack(scrut, env);
                let (arms, free): (Vec<_>, Vec<_>) = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = self.compu(tail, env);
                        (CoMatcher { dtor, tail: tail.node }, tail.free)
                    })
                    .unzip();
                (
                    SCoMatch { scrut: scrut.node, arms }.into(),
                    scrut.free + free.into_iter().flatten(),
                )
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let stack = self.stack(stack, env);
                (ExternCall { function, stack: stack.node }.into(), stack.free)
            }
        };
        Residual { node: compu.build(self, site), free }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sps::variables::FreeVars as _;

    #[derive(Default)]
    struct Fixture {
        arena: StackirArena,
    }

    impl Fixture {
        fn def(&mut self, name: &str) -> DefId {
            let def = self.arena.admin.fresh();
            self.arena.admin.insert_def(def, VarName(name.into()));
            def
        }

        fn build<U, S, T>(&mut self, node: U) -> T
        where
            U: Construct<S, T, StackirArena>,
        {
            node.build(&mut self.arena, None)
        }

        fn primitive(&mut self) -> ValueId {
            let stack = self.build(Bullet);
            let body = self
                .build(ExternCall { function: ExternalFunction::Host("int64_add".into()), stack });
            self.build(Closure { stack: Bullet, body })
        }

        fn force(&mut self, thunk: ValueId) -> CompuId {
            let stack = self.build(Bullet);
            self.build(SForce { thunk, stack })
        }

        fn ret(&mut self, value: ValueId) -> CompuId {
            let stack = self.build(Bullet);
            self.build(SReturn { stack, value })
        }

        fn normalize(self, root: CompuId) -> StackirProgram {
            let program =
                BranchJoinProgram::try_new(StackirProgram::new(self.arena, root)).unwrap();
            let normalized = Normalizer::new(program).run().into_program();
            assert!(normalized.root().free_vars(normalized.arena()).is_empty());
            normalized
        }
    }

    #[test]
    fn known_primitive_calls_discard_thunks_and_package_projection_bindings() {
        let mut fixture = Fixture::default();
        let tail_def = fixture.def("tail");
        let operation = fixture.def("operation");
        let alias = fixture.def("alias");
        let thunk = fixture.primitive();
        let unit: ValueId = fixture.build(Triv);
        let pair: ValueId =
            fixture.build(VCons::new(vec![thunk, unit], ProductLayout { arity: 2 }));
        let unit: ValueId = fixture.build(Triv);
        // A flattened three-field product written with a two-field suffix.
        let package = fixture.build(VCons::new(vec![unit, pair], ProductLayout { arity: 3 }));
        let skip: VPatId = fixture.build(Hole);
        let tail_pat: VPatId = fixture.build(tail_def);
        let package_pat =
            fixture.build(VCons::new(vec![skip, tail_pat], ProductLayout { arity: 3 }));
        let operation_pat: VPatId = fixture.build(operation);
        let skip: VPatId = fixture.build(Hole);
        let projection =
            fixture.build(VCons::new(vec![operation_pat, skip], ProductLayout { arity: 2 }));
        let alias_value = fixture.build(alias);
        let force = fixture.force(alias_value);
        let alias_pat = fixture.build(alias);
        let continuation = fixture.build(Kont { binder: alias_pat, body: force });
        let operation_value = fixture.build(operation);
        let returned = fixture.build(SReturn { stack: continuation, value: operation_value });
        let tail_value = fixture.build(tail_def);
        let projected =
            fixture.build(SProductMatch { scrut: tail_value, binder: projection, body: returned });
        let ambient = fixture.build(Bullet);
        let stack = fixture.build(Cons(package, ambient));
        let root = fixture.build(Let {
            binder: Cons(package_pat, Bullet),
            bindee: stack,
            tail: projected,
        });

        let program = fixture.normalize(root);
        assert!(matches!(
            &program.arena().inner.compus[&program.root()],
            Computation::ExternCall(ExternCall { function: ExternalFunction::Host(name), .. })
                if name == "int64_add"
        ));
        assert_eq!(program.arena().inner.compus.iter().count(), 1);
        assert_eq!(program.arena().inner.values.iter().count(), 0);
    }

    #[test]
    fn an_escaping_primitive_remains_suspended() {
        let mut fixture = Fixture::default();
        let thunk = fixture.primitive();
        let root = fixture.ret(thunk);

        let program = fixture.normalize(root);
        let Computation::Ret(SReturn { value, .. }) =
            &program.arena().inner.compus[&program.root()]
        else {
            panic!("returning a primitive must not invoke it")
        };
        assert!(matches!(program.arena().inner.values[value], Value::Closure(_)));
    }

    #[test]
    fn unknown_callees_and_recursive_calls_remain_indirect() {
        for recursive in [false, true] {
            let mut fixture = Fixture::default();
            let function = fixture.def("function");
            let thunk = fixture.build(function);
            let body = fixture.force(thunk);
            let stack = fixture.build(Bullet);
            let root = if recursive {
                fixture.build(SFix { param: function, stack, body })
            } else {
                let binder = fixture.build(function);
                fixture.build(Let { binder: Cons(binder, Bullet), bindee: stack, tail: body })
            };

            let program = fixture.normalize(root);
            assert!(
                program
                    .arena()
                    .inner
                    .compus
                    .iter()
                    .any(|(_, node)| matches!(node, Computation::Force(_)))
            );
            assert!(
                !program
                    .arena()
                    .inner
                    .compus
                    .iter()
                    .any(|(_, node)| matches!(node, Computation::ExternCall(_)))
            );
            assert!(
                matches!(program.arena().inner.compus[&program.root()], Computation::Fix(_))
                    == recursive
            );
        }
    }

    #[test]
    fn a_thunk_that_changes_the_incoming_stack_is_not_a_primitive_forwarder() {
        let mut fixture = Fixture::default();
        let value: ValueId = fixture.build(Triv);
        let ambient = fixture.build(Bullet);
        let stack = fixture.build(Cons(value, ambient));
        let body = fixture
            .build(ExternCall { function: ExternalFunction::Host("int64_add".into()), stack });
        let thunk = fixture.build(Closure { stack: Bullet, body });
        let root = fixture.force(thunk);

        let program = fixture.normalize(root);
        assert!(matches!(program.arena().inner.compus[&program.root()], Computation::Force(_)));
    }

    #[test]
    fn return_beta_reduction_preserves_runtime_value_sharing() {
        let mut fixture = Fixture::default();
        let shared = fixture.def("shared");
        let left = fixture.build(shared);
        let right = fixture.build(shared);
        let pair = fixture.build(VCons::new(vec![left, right], ProductLayout { arity: 2 }));
        let body = fixture.ret(pair);
        let binder = fixture.build(shared);
        let stack = fixture.build(Kont { binder, body });
        let first = fixture.build(Triv);
        let second = fixture.build(Triv);
        let value = fixture.build(VCons::new(vec![first, second], ProductLayout { arity: 2 }));
        let root = fixture.build(SReturn { stack, value });

        let program = fixture.normalize(root);
        assert!(matches!(
            program.arena().inner.compus[&program.root()],
            Computation::Join(LetJoin::Value(_))
        ));
        assert_eq!(
            program
                .arena()
                .inner
                .values
                .iter()
                .filter(|(_, node)| matches!(node, Value::VCons(_)))
                .count(),
            2
        );
        assert_eq!(
            program
                .arena()
                .inner
                .values
                .iter()
                .filter(|(_, node)| matches!(node, Value::Var(def) if *def == shared))
                .count(),
            2
        );
        assert!(
            !program.arena().inner.stacks.iter().any(|(_, stack)| matches!(stack, Stack::Kont(_)))
        );
    }

    #[test]
    fn unused_intrinsic_results_still_execute() {
        let mut fixture = Fixture::default();
        let left = fixture.build(Triv);
        let right = fixture.build(Triv);
        let bindee = fixture.build(Complex { operator: "div".into(), operands: vec![left, right] });
        let value = fixture.build(Triv);
        let tail = fixture.ret(value);
        let binder: VPatId = fixture.build(Hole);
        let root = fixture.build(Let { binder, bindee, tail });

        let program = fixture.normalize(root);
        assert!(matches!(
            program.arena().inner.compus[&program.root()],
            Computation::Join(LetJoin::Value(_))
        ));
        assert!(
            program
                .arena()
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, Value::Complex(_)))
        );
    }
}
