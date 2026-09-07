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
use std::{collections::HashMap, rc::Rc};
use zydeco_statics::syntax as ss;

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

/// A delayed stack retains the value and ambient-stack scopes of its producer.
#[derive(Clone)]
struct ScopedStack {
    node: StackId,
    scope: Scope,
}

#[derive(Clone)]
struct Scope {
    values: EnvId,
    stack: Option<Rc<ScopedStack>>,
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
pub struct Normalizer {
    source: StackirArena,
    #[as_ref]
    #[as_mut]
    arena: StackirArena,
    root: CompuId,
    envs: Vec<Environment>,
    /// Conservative syntactic occurrence counts, including currently dead uses.
    occurrences: HashMap<DefId, usize>,
}

impl Normalizer {
    pub fn new(program: BranchJoinProgram) -> Self {
        let StackirRebuild { source, target: arena, root } = program.into_program().into_rebuild();
        let occurrences =
            source.inner.values.iter().fold(HashMap::new(), |mut counts, (_, value)| {
                if let Value::Var(def) = value {
                    *counts.entry(*def).or_default() += 1;
                }
                counts
            });
        let envs = vec![Environment { parent: None, bindings: HashMap::new() }];
        Self { source, arena, root, envs, occurrences }
    }

    pub fn run(mut self) -> BranchJoinProgram {
        let root = self.compu(self.root, Scope { values: EnvId(0), stack: None }).node;
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
            | Value::Hole(_) | Value::Complex(_) => Rc::default(),
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
        match self.components(binder, value) {
            | Some(components) => components
                .into_iter()
                .flat_map(|(binder, value)| self.binding_facts(binder, value))
                .collect(),
            | None => self.pattern_facts(binder, self.binding_fact(binder, value)),
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

    /// Substitution may delay constructing frames until their consumer runs.
    /// Only total value construction can move across an intervening computation.
    /// Continuation bodies are suspended; ambient substitutions satisfy this same
    /// invariant at every point where a delayed stack enters the scope.
    fn movable_stack(&self, id: StackId) -> bool {
        match &self.source.inner.stacks[&id] {
            | Stack::Var(_) | Stack::Kont(_) => true,
            | Stack::Arg(Cons(value, rest)) => {
                self.discardable(*value) && self.movable_stack(*rest)
            }
            | Stack::Tag(Cons(_, rest)) => self.movable_stack(*rest),
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

    fn value(&mut self, id: ValueId, env: EnvId, demand: Demand) -> Residual<ValueId> {
        let site = self.source.admin.terms.back(&TermId::Value(id)).copied();
        if demand.is_absent() && self.discardable(id) {
            return Residual { node: Triv.build(self, site), demands: Demands::default() };
        }
        let (value, demands): (Value, _) = match self.source.inner.values[&id].clone() {
            | Value::Var(def) => match self.lookup(env, def).as_ref() {
                | KnownValue::Alias(alias) => ((*alias).into(), Demands::singleton(*alias, demand)),
                | KnownValue::Triv => (Triv.into(), Demands::default()),
                | KnownValue::Literal(literal) if self.occurrences.get(&def) == Some(&1) => {
                    (literal.clone().into(), Demands::default())
                }
                | _ => (def.into(), Demands::singleton(def, demand)),
            },
            | Value::Closure(Closure { stack, body }) => {
                if let Computation::Force(SForce { thunk, stack }) = self.source.inner.compus[&body]
                    && matches!(self.source.inner.stacks[&stack], Stack::Var(Bullet))
                    && matches!(self.source.inner.values[&thunk], Value::Var(_))
                {
                    return self.value(thunk, env, demand);
                }
                let body = self.compu(body, Scope { values: env, stack: None });
                (Closure { stack, body: body.node }.into(), body.demands)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = self.value(
                    body,
                    env,
                    if demand.is_absent() { Demand::Absent } else { Demand::Used },
                );
                (Ctor(ctor, body.node).into(), body.demands)
            }
            | Value::VCons(VCons { items, layout }) => {
                // A retained suffix spread still reads a product even when only
                // a trapping field keeps this otherwise dead construction alive.
                let demand =
                    if demand.is_absent() { Demand::Fields(Default::default()) } else { demand };
                let count = items.len();
                let (items, demands): (Vec<_>, Vec<_>) = items
                    .into_iter()
                    .enumerate()
                    .map(|(position, item)| {
                        let value = self.value(item, env, demand.item(position, count, layout));
                        (value.node, value.demands)
                    })
                    .unzip();
                (
                    VCons::new(items, layout).into(),
                    demands.into_iter().fold(Demands::default(), Demands::join),
                )
            }
            | Value::Complex(Complex { operator, operands }) => {
                let (operands, demands): (Vec<_>, Vec<_>) = operands
                    .into_iter()
                    .map(|operand| {
                        let value = self.value(operand, env, Demand::Used);
                        (value.node, value.demands)
                    })
                    .unzip();
                (
                    Complex { operator, operands }.into(),
                    demands.into_iter().fold(Demands::default(), Demands::join),
                )
            }
            | value => (value, Demands::default()),
        };
        Residual { node: value.build(self, site), demands }
    }

    fn resolve_stack(&self, mut stack: ScopedStack) -> ScopedStack {
        while matches!(self.source.inner.stacks[&stack.node], Stack::Var(Bullet)) {
            match stack.scope.stack {
                | Some(ambient) => stack = (*ambient).clone(),
                | None => break,
            }
        }
        stack
    }

    fn stack(&mut self, stack: ScopedStack) -> Residual<StackId> {
        let ScopedStack { node: id, scope } = self.resolve_stack(stack);
        let site = self.source.admin.terms.back(&TermId::Stack(id)).copied();
        let (stack, demands): (Stack, _) = match self.source.inner.stacks[&id].clone() {
            | Stack::Var(bullet) => (bullet.into(), Demands::default()),
            | Stack::Arg(Cons(value, stack)) => {
                let value = self.value(value, scope.values, Demand::Used);
                let stack = self.stack(ScopedStack { node: stack, scope });
                (Cons(value.node, stack.node).into(), value.demands.join(stack.demands))
            }
            | Stack::Tag(Cons(tag, stack)) => {
                let stack = self.stack(ScopedStack { node: stack, scope });
                (Cons(tag, stack.node).into(), stack.demands)
            }
            | Stack::Kont(Kont { binder, body }) => {
                if let ValuePattern::Var(def) = self.source.inner.vpats[&binder]
                    && let Computation::Ret(SReturn { value, stack }) =
                        self.source.inner.compus[&body]
                    && matches!(self.source.inner.values[&value], Value::Var(returned) if returned == def)
                    && matches!(self.source.inner.stacks[&stack], Stack::Var(Bullet))
                {
                    return self.stack(ScopedStack { node: stack, scope });
                }
                let values = self.bind(scope.values, binder, Rc::default());
                let mut body = self.compu(body, Scope { values, ..scope });
                for def in binder.vars(&self.source) {
                    body.demands.remove(&def);
                }
                let binder = self.pattern(binder);
                (Kont { binder, body: body.node }.into(), body.demands)
            }
        };
        Residual { node: stack.build(self, site), demands }
    }

    fn binding(
        &mut self, binder: VPatId, bindee: ScopedValue, tail: CompuId, scope: Scope,
        site: Option<ss::TermId>,
    ) -> Residual<CompuId> {
        let values = self.extend(scope.values, self.binding_facts(binder, bindee));
        let tail = self.compu(tail, Scope { values, ..scope });
        self.residual_binding(binder, bindee, tail, site)
    }

    fn residual_binding(
        &mut self, binder: VPatId, bindee: ScopedValue, mut tail: Residual<CompuId>,
        site: Option<ss::TermId>,
    ) -> Residual<CompuId> {
        let bound = binder.vars(&self.source);
        if !bound.iter().any(|def| tail.demands.contains(def)) && self.discardable(bindee.node) {
            return tail;
        }
        if let Some(components) = self.components(binder, bindee) {
            return components.into_iter().rev().fold(tail, |tail, (binder, value)| {
                self.residual_binding(binder, value, tail, site)
            });
        }
        let demand = tail.demands.pattern(&self.source, binder);
        for def in bound {
            tail.demands.remove(&def);
        }
        let bindee = self.value(bindee.node, bindee.env, demand);
        let binder = self.pattern(binder);
        let demands = tail.demands.join(bindee.demands);
        let node = Let { binder, bindee: bindee.node, tail: tail.node }.build(self, site);
        Residual { node, demands }
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

    fn branch(
        &mut self, bindee: StackId, tail: CompuId, scope: Scope, site: Option<ss::TermId>,
    ) -> Residual<CompuId> {
        let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
            self.source.inner.compus[&tail].clone()
        else {
            unreachable!("branch-join input guards exactly a coproduct match")
        };
        let known = self.shared(scrut, scope.values);
        if self.movable_stack(bindee) {
            for arm in &arms {
                match self.matches(arm.binder, &known) {
                    | Some(true) => {
                        let value = ScopedValue { node: scrut, env: scope.values };
                        let stack =
                            Some(Rc::new(ScopedStack { node: bindee, scope: scope.clone() }));
                        return self.binding(
                            arm.binder,
                            value,
                            arm.tail,
                            Scope { stack, ..scope },
                            site,
                        );
                    }
                    | Some(false) => {}
                    | None => break,
                }
            }
        }
        let (arms, demands): (Vec<_>, Vec<_>) = arms
            .into_iter()
            .map(|Matcher { binder, tail }| {
                let values = self.bind(scope.values, binder, known.clone());
                let mut tail = self.compu(tail, Scope { values, stack: None });
                let demand = tail.demands.pattern(&self.source, binder);
                for def in binder.vars(&self.source) {
                    tail.demands.remove(&def);
                }
                let binder = self.pattern(binder);
                (Matcher { binder, tail: tail.node }, (tail.demands, demand))
            })
            .unzip();
        let (demands, demand) = demands
            .into_iter()
            .fold((Demands::default(), Demand::Absent), |(demands, demand), (more, scrut)| {
                (demands.join(more), demand.join(scrut))
            });
        let scrut =
            self.value(scrut, scope.values, if demand.is_absent() { Demand::Used } else { demand });
        let branch_site = self.source.admin.terms.back(&TermId::Compu(tail)).copied();
        let tail = SCoprodMatch { scrut: scrut.node, arms }.build(self, branch_site);
        let stack = self.stack(ScopedStack { node: bindee, scope });
        let node = Let { binder: Bullet, bindee: stack.node, tail }.build(self, site);
        Residual { node, demands: demands.join(scrut.demands).join(stack.demands) }
    }

    fn compu(&mut self, id: CompuId, scope: Scope) -> Residual<CompuId> {
        let site = self.source.admin.terms.back(&TermId::Compu(id)).copied();
        let (compu, demands): (Computation<LetJoin>, _) = match self.source.inner.compus[&id]
            .clone()
        {
            | Computation::Hole(SHole(stack)) => {
                let stack = self.stack(ScopedStack { node: stack, scope });
                (SHole(stack.node).into(), stack.demands)
            }
            | Computation::Force(SForce { thunk, stack }) => {
                let known = self.known(thunk, scope.values);
                let stack = ScopedStack { node: stack, scope: scope.clone() };
                if let KnownValue::External(function) = known.as_ref() {
                    let stack = self.stack(stack);
                    (
                        ExternCall { function: function.clone(), stack: stack.node }.into(),
                        stack.demands,
                    )
                } else if self.movable_stack(stack.node)
                    && let Value::Closure(Closure { body, .. }) = self.source.inner.values[&thunk]
                {
                    return self
                        .compu(body, Scope { values: scope.values, stack: Some(Rc::new(stack)) });
                } else if self.movable_stack(stack.node)
                    && let KnownValue::Closure { body, env } = known.as_ref()
                {
                    return self.compu(*body, Scope { values: *env, stack: Some(Rc::new(stack)) });
                } else {
                    let stack = self.stack(stack);
                    let thunk = self.value(thunk, scope.values, Demand::Used);
                    (
                        SForce { thunk: thunk.node, stack: stack.node }.into(),
                        thunk.demands.join(stack.demands),
                    )
                }
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let stack = self.resolve_stack(ScopedStack { node: stack, scope: scope.clone() });
                if let Stack::Kont(Kont { binder, body }) = self.source.inner.stacks[&stack.node] {
                    return self.binding(
                        binder,
                        ScopedValue { node: value, env: scope.values },
                        body,
                        stack.scope,
                        site,
                    );
                }
                let stack = self.stack(stack);
                let value = self.value(value, scope.values, Demand::Used);
                (
                    SReturn { stack: stack.node, value: value.node }.into(),
                    stack.demands.join(value.demands),
                )
            }
            | Computation::Fix(SFix { param, stack, body }) => {
                let stack = self.stack(ScopedStack { node: stack, scope: scope.clone() });
                let values = self.extend(scope.values, [(param, Rc::default())]);
                let mut body = self.compu(body, Scope { values, stack: None });
                body.demands.remove(&param);
                (
                    SFix { param, stack: stack.node, body: body.node }.into(),
                    stack.demands.join(body.demands),
                )
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                return self.binding(
                    binder,
                    ScopedValue { node: scrut, env: scope.values },
                    body,
                    scope,
                    site,
                );
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                return self.binding(
                    binder,
                    ScopedValue { node: bindee, env: scope.values },
                    tail,
                    scope,
                    site,
                );
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                return self.branch(bindee, tail, scope, site);
            }
            | Computation::CoprodMatch(_) => {
                unreachable!("coproduct matches are handled with their stack join")
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                let stack = self.resolve_stack(ScopedStack { node: bindee, scope: scope.clone() });
                if let Stack::Arg(Cons(value, rest)) = self.source.inner.stacks[&stack.node]
                    && self.movable_stack(rest)
                {
                    let value = ScopedValue { node: value, env: stack.scope.values };
                    let rest = Some(Rc::new(ScopedStack { node: rest, scope: stack.scope }));
                    return self.binding(binder, value, tail, Scope { stack: rest, ..scope }, site);
                }
                let values = self.bind(scope.values, binder, Rc::default());
                let mut tail = self.compu(tail, Scope { values, stack: None });
                for def in binder.vars(&self.source) {
                    tail.demands.remove(&def);
                }
                let bindee = self.stack(stack);
                let binder = Cons(self.pattern(binder), Bullet);
                (
                    Let { binder, bindee: bindee.node, tail: tail.node }.into(),
                    bindee.demands.join(tail.demands),
                )
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let stack = self.resolve_stack(ScopedStack { node: scrut, scope: scope.clone() });
                if let Stack::Tag(Cons(tag, rest)) = &self.source.inner.stacks[&stack.node]
                    && self.movable_stack(*rest)
                    && let Some(CoMatcher { tail, .. }) = arms.iter().find(|arm| arm.dtor.0 == *tag)
                {
                    let rest = Some(Rc::new(ScopedStack { node: *rest, scope: stack.scope }));
                    return self.compu(*tail, Scope { stack: rest, ..scope });
                }
                let scrut = self.stack(stack);
                let (arms, demands): (Vec<_>, Vec<_>) = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = self.compu(tail, Scope { values: scope.values, stack: None });
                        (CoMatcher { dtor, tail: tail.node }, tail.demands)
                    })
                    .unzip();
                (
                    SCoMatch { scrut: scrut.node, arms }.into(),
                    demands.into_iter().fold(scrut.demands, Demands::join),
                )
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let stack = self.stack(ScopedStack { node: stack, scope });
                (ExternCall { function, stack: stack.node }.into(), stack.demands)
            }
        };
        Residual { node: compu.build(self, site), demands }
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

        fn returning_thunk(&mut self) -> ValueId {
            let value = self.build(Triv);
            let body = self.ret(value);
            self.build(Closure { stack: Bullet, body })
        }

        fn external(&mut self, name: &str) -> CompuId {
            let stack = self.build(Bullet);
            self.build(ExternCall { function: ExternalFunction::Host(name.into()), stack })
        }

        fn trap(&mut self, operator: &str) -> ValueId {
            self.build(Complex { operator: operator.into(), operands: Vec::new() })
        }

        fn effect_before_popping_argument(&mut self) -> CompuId {
            let tail = self.external("after");
            let binder = self.build(Hole);
            let ambient = self.build(Bullet);
            let body = self.build(Let { binder: Cons(binder, Bullet), bindee: ambient, tail });
            let binder = self.build(Hole);
            let stack = self.build(Kont { binder, body });
            self.build(ExternCall { function: ExternalFunction::Host("before".into()), stack })
        }

        fn ctor(index: usize) -> CtorIdx {
            CtorIdx { idx: index, name: CtorName(format!("C{index}")) }
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
    fn closure_beta_preserves_changes_to_the_incoming_stack() {
        let mut fixture = Fixture::default();
        let value: ValueId = fixture.build(Triv);
        let ambient = fixture.build(Bullet);
        let stack = fixture.build(Cons(value, ambient));
        let body = fixture
            .build(ExternCall { function: ExternalFunction::Host("int64_add".into()), stack });
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
        let tail = fixture.external("after");
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
                matches!(&arena.inner.values[&bindee], Value::Complex(Complex { operator, .. }) if operator == expected)
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
        let after = fixture.external("after");
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
                let tail = fixture.external(if index == 1 { "selected" } else { "unreachable" });
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
                .any(|(_, value)| matches!(value, Value::Complex(_)))
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
                }) => Some(name.as_str()),
                | _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(calls, ["selected"]);
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
                    .filter(|(_, value)| matches!(value, Value::Complex(_)))
                    .count(),
                1
            );
        }
    }
}
