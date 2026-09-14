//! Sharing-preserving local reductions and demand-driven rebuilding of high SPS.
//!
//! Producer facts flow into consumers; surviving consumers return field demands.
//! Stack substitution stops at binders, including branch joins. General closure
//! bodies move only from literal forces or singly used variable bindings.

use super::{
    check::BranchJoinProgram,
    demand::{Demand, Demands},
    syntax::*,
    variables::Vars as _,
};
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible, rc::Rc};
use zydeco_statics::syntax as ss;
use zydeco_utils::{
    fold::{Driver, Explicit},
    pass::CompilerPass,
};

mod fold;
mod pattern;

use fold::NormalizationFolder;

/// Normalize high SPS using fresh construction state for each input program.
pub struct Normalizer;

impl CompilerPass<BranchJoinProgram> for Normalizer {
    type Output = BranchJoinProgram;
    type Error = Infallible;

    fn run(&mut self, program: BranchJoinProgram) -> Result<Self::Output, Self::Error> {
        Ok(Normalization::new(program).run())
    }
}

#[derive(Clone, Default)]
enum KnownValue {
    #[default]
    Unknown,
    Alias(DefId),
    Literal(Literal),
    Triv,
    External(ExternalFunction),
    Closure {
        body: CompuId,
        env: EnvId,
    },
    Product(Vec<Rc<KnownValue>>),
    Constructor(CtorIdx, Rc<KnownValue>),
}

#[derive(Clone, Copy)]
struct EnvId(usize);

struct Environment {
    parent: Option<EnvId>,
    bindings: HashMap<DefId, Rc<KnownValue>>,
}

#[derive(Clone, Copy)]
struct ScopedStackId(usize);

/// A delayed stack retains the value and ambient-stack scopes of its producer.
#[derive(Clone)]
struct ScopedStack {
    node: StackId,
    scope: Scope,
}

#[derive(Clone)]
struct Scope {
    values: EnvId,
    stack: Option<ScopedStackId>,
}

#[derive(Clone, Copy)]
struct ScopedValue {
    node: ValueId,
    env: EnvId,
}

struct Residual<T> {
    node: T,
    demands: Demands,
}

/// Consume a lexical program and rebuild its normalized tree with fresh nodes.
/// Shared bodies stay bound, so moving a body preserves its definition IDs.
#[derive(AsRef, AsMut)]
struct Normalization {
    source: StackirArena,
    #[as_ref]
    #[as_mut]
    arena: StackirArena,
    root: CompuId,
    envs: Vec<Environment>,
    delayed_stacks: Vec<ScopedStack>,
    /// Conservative syntactic occurrence counts, including currently dead uses.
    occurrences: HashMap<DefId, usize>,
}

impl Normalization {
    fn new(program: BranchJoinProgram) -> Self {
        let StackirRebuild { source, target: arena, root } = program.into_program().into_rebuild();
        let occurrences =
            source.inner.values.iter().fold(HashMap::new(), |mut counts, (_, value)| {
                if let Value::Var(def) = value {
                    *counts.entry(*def).or_default() += 1;
                }
                counts
            });
        let envs = vec![Environment { parent: None, bindings: HashMap::new() }];
        Self { source, arena, root, envs, delayed_stacks: Vec::new(), occurrences }
    }

    fn run(mut self) -> BranchJoinProgram {
        let source_root = self.root;
        let root = NormalizationFolder::new(&mut self).run(source_root);
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
        self.extend(env, self.pattern_facts(binder, value))
    }

    fn pattern_facts(&self, id: VPatId, value: Rc<KnownValue>) -> Vec<(DefId, Rc<KnownValue>)> {
        let mut pending = vec![(id, value)];
        let mut facts = Vec::new();
        while let Some((id, value)) = pending.pop() {
            match &self.source.inner.vpats[&id] {
                | ValuePattern::Var(def) => facts.push((*def, value)),
                | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Alias(Alias(patterns)) => {
                    pending.extend(patterns.iter().rev().map(|pattern| (*pattern, value.clone())));
                }
                | ValuePattern::Ctor(Ctor(ctor, body)) => {
                    let value = match value.as_ref() {
                        | KnownValue::Constructor(tag, body) if tag == ctor => body.clone(),
                        | _ => Rc::default(),
                    };
                    pending.push((*body, value));
                }
                | ValuePattern::VCons(VCons { items, layout }) => {
                    pending.extend(items.iter().enumerate().rev().map(|(position, pattern)| {
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
                        (*pattern, field)
                    }));
                }
            }
        }
        facts
    }

    /// Products and aliases of patterns can share a closure among several names.
    /// Only a simple, singly used binding may transfer ownership of a body.
    fn shared(&self, id: ValueId, env: EnvId) -> Rc<KnownValue> {
        let known = self.known(id, env);
        if matches!(known.as_ref(), KnownValue::Closure { .. }) { Rc::default() } else { known }
    }

    fn known(&self, id: ValueId, env: EnvId) -> Rc<KnownValue> {
        match &self.source.inner.values[&id] {
            | Value::Var(def) => {
                let known = self.lookup(env, *def);
                if matches!(known.as_ref(), KnownValue::Unknown) {
                    Rc::new(KnownValue::Alias(*def))
                } else {
                    known
                }
            }
            | Value::Closure(Closure { body, .. }) => match &self.source.inner.compus[body] {
                | Computation::ExternCall(ExternCall { function, stack })
                    if matches!(self.source.inner.stacks[stack], Stack::Var(Bullet)) =>
                {
                    Rc::new(KnownValue::External(function.clone()))
                }
                | Computation::Force(SForce { thunk, stack })
                    if matches!(self.source.inner.stacks[stack], Stack::Var(Bullet))
                        && matches!(self.source.inner.values[thunk], Value::Var(_)) =>
                {
                    self.known(*thunk, env)
                }
                | _ => Rc::default(),
            },
            | Value::VCons(VCons { items, layout }) => {
                let fields = items.iter().map(|item| self.shared(*item, env)).collect::<Vec<_>>();
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
                Rc::new(KnownValue::Constructor(ctor.clone(), self.shared(*body, env)))
            }
            | Value::Triv(_) => Rc::new(KnownValue::Triv),
            | Value::Literal(literal) => Rc::new(KnownValue::Literal(literal.clone())),
            | Value::Primitive(Primitive { operation, operands }) => self
                .fold_primitive(*operation, operands.map(|node| ScopedValue { node, env }))
                .map_or_else(Rc::default, |literal| Rc::new(KnownValue::Literal(literal))),
            | Value::Hole(_) => Rc::default(),
        }
    }

    fn binding_fact(&self, binder: VPatId, value: ScopedValue) -> Rc<KnownValue> {
        let known = self.known(value.node, value.env);
        let single = matches!(self.source.inner.vpats[&binder], ValuePattern::Var(def)
            if self.occurrences.get(&def) == Some(&1));
        if single {
            if matches!(known.as_ref(), KnownValue::Unknown)
                && let Value::Closure(Closure { body, .. }) = self.source.inner.values[&value.node]
            {
                return Rc::new(KnownValue::Closure { body, env: value.env });
            }
            known
        } else if matches!(known.as_ref(), KnownValue::Closure { .. }) {
            Rc::default()
        } else {
            known
        }
    }

    /// Split matching introductions and eliminations into bindings in value
    /// evaluation order. Alias patterns keep their single shared scrutinee.
    fn components(&self, binder: VPatId, value: ScopedValue) -> Option<Vec<(VPatId, ScopedValue)>> {
        match (&self.source.inner.vpats[&binder], &self.source.inner.values[&value.node]) {
            | (ValuePattern::VCons(pattern), Value::VCons(product))
                if pattern.layout == product.layout
                    && pattern.items.len() == product.items.len() =>
            {
                Some(
                    pattern
                        .items
                        .iter()
                        .zip(&product.items)
                        .map(|(binder, node)| (*binder, ScopedValue { node: *node, ..value }))
                        .collect(),
                )
            }
            | (ValuePattern::Ctor(Ctor(left, binder)), Value::Ctor(Ctor(right, node)))
                if left == right =>
            {
                Some(vec![(*binder, ScopedValue { node: *node, ..value })])
            }
            | _ => None,
        }
    }

    fn binding_facts(&self, binder: VPatId, value: ScopedValue) -> Vec<(DefId, Rc<KnownValue>)> {
        let mut pending = vec![(binder, value)];
        let mut facts = Vec::new();
        while let Some((binder, value)) = pending.pop() {
            if let Some(components) = self.components(binder, value) {
                pending.extend(components.into_iter().rev());
            } else {
                facts.extend(self.pattern_facts(binder, self.binding_fact(binder, value)));
            }
        }
        facts
    }

    fn discardable(&self, id: ValueId) -> bool {
        let mut pending = vec![id];
        while let Some(id) = pending.pop() {
            match &self.source.inner.values[&id] {
                | Value::Var(_) | Value::Triv(_) | Value::Literal(_) | Value::Closure(_) => {}
                | Value::Ctor(Ctor(_, body)) => pending.push(*body),
                | Value::VCons(VCons { items, .. }) => pending.extend(items.iter().rev().copied()),
                | Value::Primitive(Primitive { operation, operands }) => {
                    if operation.may_trap() {
                        return false;
                    }
                    pending.extend(operands.iter().rev().copied());
                }
                | Value::Hole(_) => return false,
            }
        }
        true
    }

    /// Substitution may delay constructing frames until their consumer runs.
    /// Only total value construction can move across an intervening computation.
    /// Continuation bodies stay suspended at this boundary.
    fn movable_stack(&self, mut id: StackId) -> bool {
        loop {
            match &self.source.inner.stacks[&id] {
                | Stack::Var(_) | Stack::Kont(_) => return true,
                | Stack::Arg(Cons(value, rest)) => {
                    if !self.discardable(*value) {
                        return false;
                    }
                    id = *rest;
                }
                | Stack::Tag(Cons(_, rest)) => id = *rest,
            }
        }
    }

    fn pattern(&mut self, id: VPatId) -> VPatId {
        Explicit::run(&mut pattern::PatternFolder { norm: self }, id)
    }

    fn delay_stack(&mut self, stack: ScopedStack) -> ScopedStackId {
        let id = ScopedStackId(self.delayed_stacks.len());
        self.delayed_stacks.push(stack);
        id
    }

    fn resolve_stack(&self, mut stack: ScopedStack) -> ScopedStack {
        while matches!(self.source.inner.stacks[&stack.node], Stack::Var(Bullet)) {
            match stack.scope.stack {
                | Some(ambient) => stack = self.delayed_stacks[ambient.0].clone(),
                | None => break,
            }
        }
        stack
    }

    /// A decision is useful only when no earlier arm could match instead.
    fn matches(&self, binder: VPatId, known: &KnownValue) -> Option<bool> {
        match &self.source.inner.vpats[&binder] {
            | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => Some(true),
            | ValuePattern::Ctor(Ctor(ctor, body)) => match known {
                | KnownValue::Constructor(tag, value) if tag == ctor => self.matches(*body, value),
                | KnownValue::Constructor(..) => Some(false),
                | _ => None,
            },
            | ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().try_fold(true, |matched, pattern| {
                    if matched { self.matches(*pattern, known) } else { Some(false) }
                })
            }
            | ValuePattern::VCons(VCons { items, layout }) => match known {
                | KnownValue::Product(fields) if fields.len() == layout.arity => {
                    items.iter().enumerate().try_fold(true, |matched, (position, pattern)| {
                        if !matched {
                            return Some(false);
                        }
                        if position + 1 == items.len() && items.len() < layout.arity {
                            self.matches(
                                *pattern,
                                &KnownValue::Product(fields[position..].to_vec()),
                            )
                        } else {
                            self.matches(*pattern, &fields[position])
                        }
                    })
                }
                | _ => None,
            },
        }
    }

    fn fold_primitive(
        &self, operation: PrimitiveOp, operands: [ScopedValue; 2],
    ) -> Option<Literal> {
        let [first, second] = operands.map(|value| self.known(value.node, value.env));
        let (KnownValue::Literal(first), KnownValue::Literal(second)) =
            (first.as_ref(), second.as_ref())
        else {
            return None;
        };
        operation.evaluate(&[first.clone(), second.clone()]).ok()
    }

    /// Read arguments through ambient-stack substitutions without moving any trapping frames.
    fn primitive_arguments(&self, stack: ScopedStack) -> Option<([ScopedValue; 2], ScopedStack)> {
        let first = self.resolve_stack(stack);
        let Stack::Arg(Cons(value, rest)) = self.source.inner.stacks[&first.node] else {
            return None;
        };
        let first_value = ScopedValue { node: value, env: first.scope.values };
        let second = self.resolve_stack(ScopedStack { node: rest, scope: first.scope });
        let Stack::Arg(Cons(value, rest)) = self.source.inner.stacks[&second.node] else {
            return None;
        };
        let second_value = ScopedValue { node: value, env: second.scope.values };
        self.movable_stack(rest).then_some((
            [first_value, second_value],
            self.resolve_stack(ScopedStack { node: rest, scope: second.scope }),
        ))
    }
}

#[cfg(test)]
mod depth_tests;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::high::variables::FreeVars as _;

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
            let body = self.build(ExternCall {
                function: ExternalFunction::Host(BuiltinValueRole::Integer(
                    IntegerType::Int64,
                    IntegerOperation::Add,
                )),
                stack,
            });
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

        fn returning_thunk(&mut self) -> ValueId {
            let value = self.build(Triv);
            let body = self.ret(value);
            self.build(Closure { stack: Bullet, body })
        }

        fn external(&mut self, role: BuiltinValueRole) -> CompuId {
            let stack = self.build(Bullet);
            self.build(ExternCall { function: ExternalFunction::Host(role), stack })
        }

        fn trap(&mut self, operation: &str) -> ValueId {
            let first =
                self.build(Literal::Integer(IntegerLiteral::Int64(if operation == "left" {
                    1
                } else {
                    2
                })));
            let second = self.build(Literal::Integer(IntegerLiteral::Int64(0)));
            self.build(Primitive {
                operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Div),
                operands: [first, second],
            })
        }

        fn effect_before_popping_argument(&mut self) -> CompuId {
            let tail = self.external(BuiltinValueRole::WriteLine);
            let binder = self.build(Hole);
            let ambient = self.build(Bullet);
            let body = self.build(Let { binder: Cons(binder, Bullet), bindee: ambient, tail });
            let binder = self.build(Hole);
            let stack = self.build(Kont { binder, body });
            self.build(ExternCall {
                function: ExternalFunction::Host(BuiltinValueRole::WriteStr),
                stack,
            })
        }

        fn ctor(index: usize) -> CtorIdx {
            CtorIdx { idx: index, name: CtorName(format!("C{index}")) }
        }

        fn normalize(self, root: CompuId) -> StackirProgram {
            let program =
                BranchJoinProgram::try_new(StackirProgram::new(self.arena, root)).unwrap();
            let normalized = Normalizer.run_infallible(program).into_program();
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
        let arena = &program.arena().inner;
        let Computation::LetArg(Let { tail, .. }) = arena.compus[&program.root()] else {
            panic!("a primitive with an ambient argument stack must pop its first argument")
        };
        let Computation::LetArg(Let { tail, .. }) = arena.compus[&tail] else {
            panic!("the primitive must pop its second argument")
        };
        let Computation::Ret(SReturn { value, .. }) = arena.compus[&tail] else {
            panic!("the primitive must return its inline result")
        };
        assert!(matches!(arena.values[&value], Value::Primitive(_)));
        assert_eq!(arena.compus.iter().count(), 3);
        assert_eq!(arena.values.iter().count(), 3);
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
    fn unused_primitive_calls_disappear_only_when_their_evaluation_is_total() {
        for (arithmetic, divisor, trapping_operand, retained) in [
            (IntegerArithmetic::Add, 0, false, false),
            (IntegerArithmetic::Div, 2, false, false),
            (IntegerArithmetic::Div, 0, false, true),
            (IntegerArithmetic::Mod, 0, false, true),
            (IntegerArithmetic::Add, 1, true, true),
        ] {
            let mut fixture = Fixture::default();
            let role = PrimitiveOp::Integer(IntegerType::Int64, arithmetic).builtin();
            let thunk = ExternalFunction::Host(role).make_function(&mut fixture.arena);
            let first = if trapping_operand {
                fixture.trap("operand")
            } else {
                fixture.build(Literal::Integer(IntegerLiteral::Int64(7)))
            };
            let second = fixture.build(Literal::Integer(IntegerLiteral::Int64(divisor)));
            let after = fixture.external(BuiltinValueRole::WriteLine);
            let binder = fixture.build(Hole);
            let rest = fixture.build(Kont { binder, body: after });
            let stack: StackId = fixture.build(Cons(second, rest));
            let stack = fixture.build(Cons(first, stack));
            let call = fixture.build(SForce { thunk, stack });
            let binder = fixture.build(Hole);
            let stack = fixture.build(Kont { binder, body: call });
            let root = fixture.build(ExternCall {
                function: ExternalFunction::Host(BuiltinValueRole::WriteStr),
                stack,
            });

            let program = fixture.normalize(root);
            let arena = &program.arena().inner;
            let Computation::ExternCall(ExternCall {
                function: ExternalFunction::Host(name),
                stack,
            }) = &arena.compus[&program.root()]
            else {
                panic!("the preceding effect must remain first")
            };
            assert_eq!(*name, BuiltinValueRole::WriteStr);
            let Stack::Kont(Kont { body, .. }) = arena.stacks[stack] else {
                panic!("the effect must resume its consumer")
            };
            let body = if retained {
                let Computation::Join(LetJoin::Value(Let { bindee, tail, .. })) =
                    arena.compus[&body]
                else {
                    panic!("trapping arithmetic must remain bound before the following effect")
                };
                assert!(matches!(arena.values[&bindee], Value::Primitive(_)));
                tail
            } else {
                assert!(
                    !arena.values.iter().any(|(_, value)| matches!(value, Value::Primitive(_)))
                );
                body
            };
            assert!(
                matches!(&arena.compus[&body], Computation::ExternCall(ExternCall { function: ExternalFunction::Host(name), .. }) if *name == BuiltinValueRole::WriteLine)
            );
        }
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
    fn closure_beta_preserves_changes_to_the_incoming_stack() {
        let mut fixture = Fixture::default();
        let value: ValueId = fixture.build(Triv);
        let ambient = fixture.build(Bullet);
        let stack = fixture.build(Cons(value, ambient));
        let body = fixture.build(ExternCall {
            function: ExternalFunction::Host(BuiltinValueRole::WriteStr),
            stack,
        });
        let thunk = fixture.build(Closure { stack: Bullet, body });
        let root = fixture.force(thunk);

        let program = fixture.normalize(root);
        let Computation::ExternCall(ExternCall { stack, .. }) =
            program.arena().inner.compus[&program.root()]
        else {
            panic!("a direct closure should reduce to its body")
        };
        assert!(matches!(program.arena().inner.stacks[&stack], Stack::Arg(_)));
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
    fn unused_trapping_primitive_results_still_execute() {
        let mut fixture = Fixture::default();
        let left = fixture.build(Triv);
        let right = fixture.build(Triv);
        let bindee = fixture.build(Primitive {
            operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Div),
            operands: [left, right],
        });
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
                .any(|(_, value)| matches!(value, Value::Primitive(_)))
        );
    }

    #[test]
    fn field_demand_retains_nested_and_suffix_shapes_but_prunes_their_contents() {
        for suffix in [false, true] {
            for escape in [false, true] {
                let mut fixture = Fixture::default();
                let package = fixture.def("package");
                let selected = fixture.def("selected");
                let dead_a = fixture.returning_thunk();
                let dead_b = fixture.returning_thunk();
                let nested =
                    fixture.build(VCons::new(vec![dead_a, dead_b], ProductLayout { arity: 2 }));
                let live = fixture.returning_thunk();
                let layout = ProductLayout { arity: if suffix { 3 } else { 2 } };
                let product = fixture.build(VCons::new(vec![live, nested], layout));
                let value = fixture.build(package);
                let tail = if escape {
                    fixture.ret(value)
                } else {
                    let selected_value = fixture.build(selected);
                    let body = fixture.ret(selected_value);
                    let first = fixture.build(selected);
                    let hole_a = fixture.build(Hole);
                    let hole_b = fixture.build(Hole);
                    let nested =
                        fixture.build(VCons::new(vec![hole_a, hole_b], ProductLayout { arity: 2 }));
                    let binder = fixture.build(VCons::new(vec![first, nested], layout));
                    fixture.build(SProductMatch { scrut: value, binder, body })
                };
                let binder = fixture.build(package);
                let root = fixture.build(Let { binder, bindee: product, tail });
                let program = fixture.normalize(root);
                let values = &program.arena().inner.values;
                assert_eq!(
                    values.iter().filter(|(_, value)| matches!(value, Value::VCons(_))).count(),
                    2
                );
                assert_eq!(
                    values.iter().filter(|(_, value)| matches!(value, Value::Closure(_))).count(),
                    if escape { 3 } else { 1 }
                );
            }
        }
    }

    #[test]
    fn product_beta_keeps_trapping_fields_in_evaluation_order() {
        let mut fixture = Fixture::default();
        let left = fixture.trap("left");
        let right = fixture.trap("right");
        let bindee = fixture.build(VCons::new(vec![left, right], ProductLayout { arity: 2 }));
        let left = fixture.build(Hole);
        let right = fixture.build(Hole);
        let binder = fixture.build(VCons::new(vec![left, right], ProductLayout { arity: 2 }));
        let tail = fixture.external(BuiltinValueRole::WriteLine);
        let root = fixture.build(Let { binder, bindee, tail });
        let program = fixture.normalize(root);
        let arena = program.arena();
        let mut node = program.root();
        for expected in ["left", "right"] {
            let Computation::Join(LetJoin::Value(Let { bindee, tail, .. })) =
                arena.inner.compus[&node]
            else {
                panic!("each trapping value must keep its evaluation")
            };
            assert!(
                matches!(&arena.inner.values[&bindee], Value::Primitive(Primitive { operands, .. })
                    if matches!(&arena.inner.values[&operands[0]], Value::Literal(Literal::Integer(value))
                        if value.value() == if expected == "left" { 1 } else { 2 }))
            );
            node = tail;
        }
        assert!(matches!(arena.inner.compus[&node], Computation::ExternCall(_)));
        assert!(!arena.inner.values.iter().any(|(_, value)| matches!(value, Value::VCons(_))));
    }

    #[test]
    fn general_closure_bodies_move_only_through_single_use_bindings() {
        for shared in [false, true] {
            let mut fixture = Fixture::default();
            let first = fixture.def("first");
            let second = fixture.def("second");
            let thunk = fixture.returning_thunk();
            let first_value = fixture.build(first);
            let (binder, tail) = if shared {
                // Each alias has one use, but they share the same closure body.
                let second_value = fixture.build(second);
                let body = fixture.force(second_value);
                let hole = fixture.build(Hole);
                let stack = fixture.build(Kont { binder: hole, body });
                let tail = fixture.build(SForce { thunk: first_value, stack });
                let first = fixture.build(first);
                let second = fixture.build(second);
                let binder = fixture.build(Alias(ConsN::from_vec(vec![first, second]).unwrap()));
                (binder, tail)
            } else {
                let binder = fixture.build(first);
                (binder, fixture.force(first_value))
            };
            let root = fixture.build(Let { binder, bindee: thunk, tail });
            let program = fixture.normalize(root);
            assert_eq!(
                program
                    .arena()
                    .inner
                    .values
                    .iter()
                    .filter(|(_, value)| matches!(value, Value::Closure(_)))
                    .count(),
                usize::from(shared)
            );
            assert_eq!(
                program
                    .arena()
                    .inner
                    .compus
                    .iter()
                    .filter(|(_, node)| matches!(node, Computation::Force(_)))
                    .count(),
                if shared { 2 } else { 0 }
            );
        }
    }

    #[test]
    fn stack_substitution_keeps_one_continuation_at_an_unknown_branch() {
        let mut fixture = Fixture::default();
        let condition = fixture.def("condition");
        let scrut = fixture.build(condition);
        let arms = (0..2)
            .map(|index| {
                let payload = fixture.build(Hole);
                let binder = fixture.build(Ctor(Fixture::ctor(index), payload));
                let unit = fixture.build(Triv);
                let tail = fixture.ret(unit);
                Matcher { binder, tail }
            })
            .collect();
        let branch = fixture.build(SCoprodMatch { scrut, arms });
        let ambient = fixture.build(Bullet);
        let body = fixture.build(Let { binder: Bullet, bindee: ambient, tail: branch });
        let thunk = fixture.build(Closure { stack: Bullet, body });
        let after = fixture.external(BuiltinValueRole::WriteLine);
        let hole = fixture.build(Hole);
        let stack = fixture.build(Kont { binder: hole, body: after });
        let tail = fixture.build(SForce { thunk, stack });
        let ambient = fixture.build(Bullet);
        let binder = fixture.build(condition);
        let root = fixture.build(Let { binder: Cons(binder, Bullet), bindee: ambient, tail });
        let program = fixture.normalize(root);
        assert_eq!(
            program
                .arena()
                .inner
                .stacks
                .iter()
                .filter(|(_, stack)| matches!(stack, Stack::Kont(_)))
                .count(),
            1
        );
        assert_eq!(
            program
                .arena()
                .inner
                .compus
                .iter()
                .filter(|(_, node)| matches!(node, Computation::ExternCall(_)))
                .count(),
            1
        );
        assert!(
            !program
                .arena()
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, Value::Closure(_)))
        );
    }

    #[test]
    fn known_constructor_selection_preserves_payload_traps() {
        let mut fixture = Fixture::default();
        let payload = fixture.trap("payload");
        let scrut = fixture.build(Ctor(Fixture::ctor(1), payload));
        let arms = (0..2)
            .map(|index| {
                let payload = fixture.build(Hole);
                let binder = fixture.build(Ctor(Fixture::ctor(index), payload));
                let tail = fixture.external(if index == 1 {
                    BuiltinValueRole::WriteStr
                } else {
                    BuiltinValueRole::WriteLine
                });
                Matcher { binder, tail }
            })
            .collect();
        let tail = fixture.build(SCoprodMatch { scrut, arms });
        let bindee = fixture.build(Bullet);
        let root = fixture.build(Let { binder: Bullet, bindee, tail });
        let program = fixture.normalize(root);
        assert!(
            program
                .arena()
                .inner
                .values
                .iter()
                .any(|(_, value)| matches!(value, Value::Primitive(_)))
        );
        assert!(!program.arena().inner.compus.iter().any(|(_, node)| matches!(
            node,
            Computation::CoprodMatch(_) | Computation::Join(LetJoin::Stack(_))
        )));
        let calls = program
            .arena()
            .inner
            .compus
            .iter()
            .filter_map(|(_, node)| match node {
                | Computation::ExternCall(ExternCall {
                    function: ExternalFunction::Host(name),
                    ..
                }) => Some(*name),
                | _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(calls, [BuiltinValueRole::WriteStr]);
    }

    #[test]
    fn tag_beta_consumes_the_selected_tag_and_argument_frames() {
        for known in [false, true] {
            let mut fixture = Fixture::default();
            let tag = DtorIdx { idx: 0, name: DtorName("selected".into()) };
            let argument = fixture.def("argument");
            let returned = fixture.build(argument);
            let tail = fixture.ret(returned);
            let binder = fixture.build(argument);
            let ambient = fixture.build(Bullet);
            let body = fixture.build(Let { binder: Cons(binder, Bullet), bindee: ambient, tail });
            let arms = vec![CoMatcher { dtor: Cons(tag.clone(), Bullet), tail: body }];
            let ambient = fixture.build(Bullet);
            let scrut = if known {
                let value: ValueId = fixture.build(Triv);
                let rest: StackId = fixture.build(Cons(value, ambient));
                fixture.build(Cons(tag, rest))
            } else {
                ambient
            };
            let root = fixture.build(SCoMatch { scrut, arms });
            let program = fixture.normalize(root);
            assert_eq!(
                matches!(program.arena().inner.compus[&program.root()], Computation::Ret(_)),
                known
            );
            assert_eq!(
                matches!(program.arena().inner.compus[&program.root()], Computation::CoCase(_)),
                !known
            );
        }
    }

    #[test]
    fn forwarding_thunks_and_continuations_reduce_without_entering_unknown_code() {
        for returned in [false, true] {
            let mut fixture = Fixture::default();
            let function = fixture.def("function");
            let value = fixture.build(function);
            let body = fixture.force(value);
            let thunk = fixture.build(Closure { stack: Bullet, body });
            let tail = if returned {
                fixture.ret(thunk)
            } else {
                let result = fixture.def("result");
                let value = fixture.build(result);
                let body = fixture.ret(value);
                let binder = fixture.build(result);
                let stack = fixture.build(Kont { binder, body });
                fixture.build(SForce { thunk, stack })
            };
            let binder = fixture.build(function);
            let ambient = fixture.build(Bullet);
            let root = fixture.build(Let { binder: Cons(binder, Bullet), bindee: ambient, tail });
            let program = fixture.normalize(root);
            assert!(
                !program
                    .arena()
                    .inner
                    .values
                    .iter()
                    .any(|(_, value)| matches!(value, Value::Closure(_)))
            );
            assert!(
                !program
                    .arena()
                    .inner
                    .stacks
                    .iter()
                    .any(|(_, stack)| matches!(stack, Stack::Kont(_)))
            );
            assert_eq!(
                program
                    .arena()
                    .inner
                    .compus
                    .iter()
                    .filter(|(_, node)| matches!(node, Computation::Force(_)))
                    .count(),
                usize::from(!returned)
            );
        }
    }

    #[test]
    fn stack_reductions_do_not_delay_traps_past_effects() {
        enum Consumer {
            Closure,
            Argument,
            Destructor,
            Branch,
        }
        for consumer in
            [Consumer::Closure, Consumer::Argument, Consumer::Destructor, Consumer::Branch]
        {
            let mut fixture = Fixture::default();
            let value = fixture.trap("argument");
            let ambient = fixture.build(Bullet);
            let rest: StackId = fixture.build(Cons(value, ambient));
            let body = fixture.effect_before_popping_argument();
            let root = match consumer {
                | Consumer::Closure => {
                    let thunk = fixture.build(Closure { stack: Bullet, body });
                    fixture.build(SForce { thunk, stack: rest })
                }
                | Consumer::Argument => {
                    let value: ValueId = fixture.build(Triv);
                    let bindee = fixture.build(Cons(value, rest));
                    let binder = fixture.build(Hole);
                    fixture.build(Let { binder: Cons(binder, Bullet), bindee, tail: body })
                }
                | Consumer::Destructor => {
                    let tag = DtorIdx { idx: 0, name: DtorName("run".into()) };
                    let scrut = fixture.build(Cons(tag.clone(), rest));
                    let arms = vec![CoMatcher { dtor: Cons(tag, Bullet), tail: body }];
                    fixture.build(SCoMatch { scrut, arms })
                }
                | Consumer::Branch => {
                    let value = fixture.build(Triv);
                    let scrut = fixture.build(Ctor(Fixture::ctor(0), value));
                    let hole = fixture.build(Hole);
                    let binder = fixture.build(Ctor(Fixture::ctor(0), hole));
                    let tail = fixture
                        .build(SCoprodMatch { scrut, arms: vec![Matcher { binder, tail: body }] });
                    fixture.build(Let { binder: Bullet, bindee: rest, tail })
                }
            };
            let program = fixture.normalize(root);
            let node = &program.arena().inner.compus[&program.root()];
            assert!(match consumer {
                | Consumer::Closure => matches!(node, Computation::Force(_)),
                | Consumer::Argument => matches!(node, Computation::LetArg(_)),
                | Consumer::Destructor => matches!(node, Computation::CoCase(_)),
                | Consumer::Branch => matches!(node, Computation::Join(LetJoin::Stack(_))),
            });
            assert_eq!(
                program
                    .arena()
                    .inner
                    .values
                    .iter()
                    .filter(|(_, value)| matches!(value, Value::Primitive(_)))
                    .count(),
                1
            );
        }
    }
}
