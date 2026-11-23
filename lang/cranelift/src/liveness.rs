use crate::plan::{Case, ModulePlan};
use std::collections::{HashMap, HashSet};
use zydeco_stackir::low::syntax::{
    self as sps, CompuId, Computation, DefId, LabelKind, SpsLowArena, Stack, StackExt, StackId,
    VPatId, Value, ValueExt, ValueId, ValuePattern,
};

/// Intra-case backward `DefId` liveness used to populate GC safepoint root spills.
pub(crate) struct CaseLiveness {
    defs: Vec<DefId>,
    live_in: HashMap<CompuId, Vec<u64>>,
}

impl CaseLiveness {
    pub(crate) fn analyze(arena: &SpsLowArena, plan: &ModulePlan, case: Case) -> Self {
        let mut builder = LivenessBuilder {
            arena,
            plan,
            defs: Vec::new(),
            indices: HashMap::new(),
            local_labels: HashSet::new(),
            nodes: HashMap::new(),
            order: Vec::new(),
            visited: HashSet::new(),
        };
        if let Some(entry) = case.entry {
            for (_, pat) in entry.words() {
                builder.intern_pattern(pat);
            }
        }
        builder.visit_compu(case.body);
        builder.finish()
    }

    pub(crate) fn live_defs(&self, compu: CompuId) -> impl Iterator<Item = DefId> + '_ {
        let bits = self.live_in.get(&compu);
        (0..self.defs.len()).filter_map(move |idx| {
            let word = bits?[idx / 64];
            ((word & (1_u64 << (idx % 64))) != 0).then_some(self.defs[idx])
        })
    }
}

struct NodeInfo {
    uses: Vec<usize>,
    edges: Vec<(CompuId, Vec<usize>)>,
}

struct LivenessBuilder<'a> {
    arena: &'a SpsLowArena,
    plan: &'a ModulePlan,
    defs: Vec<DefId>,
    indices: HashMap<DefId, usize>,
    local_labels: HashSet<DefId>,
    nodes: HashMap<CompuId, NodeInfo>,
    order: Vec<CompuId>,
    visited: HashSet<CompuId>,
}

impl LivenessBuilder<'_> {
    fn intern_def(&mut self, def: DefId) -> Option<usize> {
        if self.plan.is_code_label(def) || self.local_labels.contains(&def) {
            return None;
        }
        if let Some(&idx) = self.indices.get(&def) {
            return Some(idx);
        }
        let idx = self.defs.len();
        self.defs.push(def);
        self.indices.insert(def, idx);
        Some(idx)
    }

    fn intern_pattern(&mut self, id: VPatId) -> Vec<usize> {
        let mut out = Vec::new();
        let mut stack = vec![id];
        while let Some(pat) = stack.pop() {
            match &self.arena.inner.vpats[&pat] {
                | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Var(def) => {
                    if let Some(idx) = self.intern_def(*def) {
                        out.push(idx);
                    }
                }
                | ValuePattern::Ctor(sps::Ctor(_, body)) => stack.push(*body),
                | ValuePattern::Alias(sps::Alias(pats)) => stack.extend(pats.iter().copied()),
                | ValuePattern::VCons(sps::VCons { items, .. }) => {
                    stack.extend(items.iter().copied());
                }
            }
        }
        out
    }

    fn collect_value_uses(&mut self, id: ValueId, uses: &mut Vec<usize>) {
        let mut stack = vec![id];
        while let Some(val) = stack.pop() {
            if matches!(self.arena.inner.values[&val], Value::Primitive(_)) {
                let call = self.plan.scalars.call(val);
                stack.extend(call.inputs.iter().copied());
                continue;
            }
            match &self.arena.inner.values[&val] {
                | Value::Hole(_) | Value::Triv(_) | Value::Literal(_) => {}
                | Value::Var(def) => {
                    if let Some(idx) = self.intern_def(*def) {
                        uses.push(idx);
                    }
                }
                | Value::Ctor(sps::Ctor(_, body)) => stack.push(*body),
                | Value::VCons(sps::VCons { items, .. }) => stack.extend(items.iter().copied()),
                | Value::AddrOffset(sps::AddrOffset { base, displacement }) => {
                    stack.push(*base);
                    stack.push(*displacement);
                }
                | Value::Primitive(sps::Primitive { operands, .. }) => {
                    stack.extend(operands.iter().copied());
                }
                | Value::Ext(ValueExt::Block(_)) => {}
                | Value::Ext(ValueExt::ClosurePackage(sps::ClosurePackage {
                    environment,
                    code,
                })) => {
                    stack.push(*environment);
                    stack.push(*code);
                }
            }
        }
    }

    fn collect_stack_uses(&mut self, id: StackId, uses: &mut Vec<usize>) {
        let mut cur = id;
        loop {
            match &self.arena.inner.stacks[&cur] {
                | Stack::Var(_) => break,
                | Stack::Arg(sps::Cons(val, rest)) => {
                    self.collect_value_uses(*val, uses);
                    cur = *rest;
                }
                | Stack::Tag(sps::Cons(_, rest)) => cur = *rest,
                | Stack::Ext(StackExt::ContinuationPackage(sps::ContinuationPackage {
                    code,
                    residual,
                })) => {
                    self.collect_value_uses(*code, uses);
                    cur = *residual;
                }
            }
        }
    }

    fn visit_compu(&mut self, root: CompuId) {
        let mut work = vec![root];
        let mut local_bodies: HashMap<DefId, CompuId> = HashMap::new();

        while let Some(id) = work.pop() {
            if !self.visited.insert(id) {
                continue;
            }
            self.visit_compu_node(id, &mut local_bodies, &mut work);
        }

        // Resolve any JumpLabel whose enclosing LetLabel was pushed onto work before it in another order.
        for (&id, info) in &mut self.nodes {
            if let Computation::Ext(sps::CompuExt::JumpLabel(sps::JumpLabel { label, .. })) =
                &self.arena.inner.compus[&id]
                && let Some(&body) = local_bodies.get(label)
                && info.edges.is_empty()
            {
                info.edges.push((body, Vec::new()));
            }
        }
    }

    fn visit_compu_node(
        &mut self, id: CompuId, local_bodies: &mut HashMap<DefId, CompuId>, work: &mut Vec<CompuId>,
    ) {
        if let Some(call) = self.plan.scalars.memory(id).cloned() {
            self.visit_memory_call(id, &call, work);
            return;
        }
        let mut uses = Vec::new();
        let mut edges = Vec::new();

        match &self.arena.inner.compus[&id] {
            | Computation::Hole(sps::SHole(stack)) => {
                self.collect_stack_uses(*stack, &mut uses);
            }
            | Computation::ProductMatch(sps::SProductMatch { scrut, binder, body }) => {
                self.collect_value_uses(*scrut, &mut uses);
                let kills = self.intern_pattern(*binder);
                edges.push((*body, kills));
                work.push(*body);
            }
            | Computation::CoprodMatch(sps::SCoprodMatch { scrut, arms }) => {
                self.collect_value_uses(*scrut, &mut uses);
                for arm in arms {
                    let kills = self.intern_pattern(arm.binder);
                    edges.push((arm.tail, kills));
                    work.push(arm.tail);
                }
            }
            | Computation::Compare(sps::CompareBranch {
                operands, when_true, when_false, ..
            }) => {
                self.collect_value_uses(operands[0], &mut uses);
                self.collect_value_uses(operands[1], &mut uses);
                edges.push((*when_true, Vec::new()));
                edges.push((*when_false, Vec::new()));
                work.push(*when_true);
                work.push(*when_false);
            }
            | Computation::LetValue(sps::LetValue { binder, bindee, tail }) => {
                if self.plan.scalars.elided(id) {
                    edges.push((*tail, Vec::new()));
                } else {
                    self.collect_value_uses(*bindee, &mut uses);
                    let kills = self.intern_pattern(*binder);
                    edges.push((*tail, kills));
                }
                work.push(*tail);
            }
            | Computation::LetStack(sps::LetStack { bindee, tail, .. }) => {
                self.collect_stack_uses(*bindee, &mut uses);
                edges.push((*tail, Vec::new()));
                work.push(*tail);
            }
            | Computation::LetArg(sps::LetArg { binder: sps::Cons(binder, _), bindee, tail }) => {
                self.collect_stack_uses(*bindee, &mut uses);
                let kills = self.intern_pattern(*binder);
                edges.push((*tail, kills));
                work.push(*tail);
            }
            | Computation::CoCase(sps::SCoMatch { scrut, arms }) => {
                self.collect_stack_uses(*scrut, &mut uses);
                for arm in arms {
                    edges.push((arm.tail, Vec::new()));
                    work.push(arm.tail);
                }
            }
            | Computation::ExternCall(sps::ExternCall { stack, .. }) => {
                self.collect_stack_uses(*stack, &mut uses);
            }
            | Computation::Memory(sps::MemoryStep::Load { address, result, next, .. }) => {
                self.collect_value_uses(*address, &mut uses);
                let kills = self.intern_pattern(*result);
                edges.push((*next, kills));
                work.push(*next);
            }
            | Computation::Memory(sps::MemoryStep::Store { address, value, next, .. }) => {
                self.collect_value_uses(*value, &mut uses);
                self.collect_value_uses(*address, &mut uses);
                edges.push((*next, Vec::new()));
                work.push(*next);
            }
            | Computation::Ext(sps::CompuExt::Jump(sps::Jump { target, argument, stack })) => {
                self.collect_value_uses(*target, &mut uses);
                self.collect_value_uses(argument.word().1, &mut uses);
                self.collect_stack_uses(*stack, &mut uses);
            }
            | Computation::Ext(sps::CompuExt::LetLabel(sps::LetLabel {
                label,
                kind,
                body,
                tail,
                ..
            })) => {
                if *kind == LabelKind::Local {
                    self.local_labels.insert(*label);
                    local_bodies.insert(*label, *body);
                    work.push(*body);
                }
                edges.push((*tail, Vec::new()));
                work.push(*tail);
            }
            | Computation::Ext(sps::CompuExt::JumpLabel(sps::JumpLabel { label, stack })) => {
                self.collect_stack_uses(*stack, &mut uses);
                if let Some(&body) = local_bodies.get(label) {
                    edges.push((body, Vec::new()));
                }
            }
            | Computation::Ext(sps::CompuExt::OpenClosure(sps::OpenClosure {
                package,
                environment,
                code,
                body,
            })) => {
                self.collect_value_uses(*package, &mut uses);
                let mut kills = self.intern_pattern(*environment);
                kills.extend(self.intern_pattern(*code));
                edges.push((*body, kills));
                work.push(*body);
            }
            | Computation::Ext(sps::CompuExt::OpenContinuation(sps::OpenContinuation {
                package,
                code,
                body,
            })) => {
                self.collect_stack_uses(*package, &mut uses);
                let kills = self.intern_pattern(*code);
                edges.push((*body, kills));
                work.push(*body);
            }
        }

        self.nodes.insert(id, NodeInfo { uses, edges });
        self.order.push(id);
    }

    fn visit_memory_call(
        &mut self, id: CompuId, call: &zydeco_stackir::low::scalar::MemoryCall,
        work: &mut Vec<CompuId>,
    ) {
        let mut uses = Vec::new();
        for binding in &call.address_bindings {
            self.collect_value_uses(binding.value, &mut uses);
        }
        for &input in &call.inputs {
            self.collect_value_uses(input, &mut uses);
        }
        self.collect_value_uses(call.store_address, &mut uses);
        self.collect_value_uses(call.load_address, &mut uses);
        let kills =
            call.address_bindings.iter().flat_map(|b| self.intern_pattern(b.binder)).collect();
        self.nodes.insert(id, NodeInfo { uses, edges: vec![(call.next, kills)] });
        self.order.push(id);
        work.push(call.next);
    }

    fn finish(self) -> CaseLiveness {
        let words = self.defs.len().div_ceil(64);
        let mut live_in: HashMap<CompuId, Vec<u64>> =
            self.nodes.keys().map(|&id| (id, vec![0_u64; words])).collect();

        if words != 0 {
            let mut changed = true;
            while changed {
                changed = false;
                for &id in self.order.iter().rev() {
                    let Some(info) = self.nodes.get(&id) else { continue };
                    let mut next_bits = vec![0_u64; words];
                    for &u in &info.uses {
                        next_bits[u / 64] |= 1_u64 << (u % 64);
                    }
                    for (succ, kills) in &info.edges {
                        if let Some(succ_bits) = live_in.get(succ) {
                            let mut tmp = succ_bits.clone();
                            for &k in kills {
                                tmp[k / 64] &= !(1_u64 << (k % 64));
                            }
                            for (dst, src) in next_bits.iter_mut().zip(tmp) {
                                *dst |= src;
                            }
                        }
                    }
                    let current = live_in.get_mut(&id).unwrap();
                    if *current != next_bits {
                        *current = next_bits;
                        changed = true;
                    }
                }
            }
        }

        CaseLiveness { defs: self.defs, live_in }
    }
}
