//! Bounded scalar regions shared by the native and Wasm lowering paths.
//!
//! A region can contain a primitive tree or adjacent single-use primitive bindings.
//! Calls, branches, captures, shared bindings, and other value constructions end it.

use super::SpsLowProgram;
use super::syntax::*;
use zydeco_syntax::scalar::{
    ScalarBoxing, ScalarId, ScalarProgram, ScalarRegion, ScalarStep, ScalarType,
};
use zydeco_syntax::word::ScalarRepresentation;

mod memory;
pub use memory::MemoryCall;

const MAX_OPERATIONS: usize = 32;

#[derive(Clone, Debug)]
pub struct ScalarCall {
    pub inputs: Vec<ValueId>,
    pub program: ScalarProgram,
}

#[derive(Default)]
pub struct ScalarPlans {
    calls: SecondaryMap<ValueId, ScalarCall>,
    elided: IdBitSet<CompuId>,
    memories: SecondaryMap<CompuId, MemoryCall>,
}

impl ScalarPlans {
    /// Build representation-checked primitive regions; source typing is supplied by the caller.
    pub fn unoptimized(arena: &SpsLowInnerArena) -> Self {
        let mut calls = SecondaryMap::new();
        for (id, value) in arena.values.iter() {
            if let Value::Primitive(Primitive { operation, operands }) = value {
                calls.insert_new(
                    id,
                    ScalarCall {
                        inputs: operands.to_vec(),
                        program: ScalarProgram::primitive(*operation),
                    },
                );
            }
        }
        Self { calls, ..Self::default() }
    }

    pub fn new(program: &SpsLowProgram, boxing: ScalarBoxing) -> Self {
        let arena = &program.arena().inner;
        let mut plans = Self::unoptimized(arena);
        // The keep policy retains individual primitive boundaries for an execution baseline.
        if boxing == ScalarBoxing::Keep {
            return plans;
        }
        for (id, value) in arena.values.iter() {
            if matches!(value, Value::Primitive(_)) {
                let mut expressions = Expressions::default();
                if let Some((result, _)) = expressions.value(arena, id, None, &SecondaryMap::new())
                {
                    plans.calls.upsert(id, expressions.finish(result));
                }
            }
        }
        let mut uses = SecondaryMap::<DefId, usize>::new();
        for (_, value) in arena.values.iter() {
            if let Value::Var(def) = value {
                let count = uses.get(*def).copied().unwrap_or(0) + 1;
                uses.upsert(*def, count);
            }
        }
        // Native continuation evidence also retains its bindings across suspension.
        for (_, entry) in arena.continuations.iter() {
            for capture in &entry.captures {
                let s_count = uses.get(capture.source).copied().unwrap_or(0) + 1;
                uses.upsert(capture.source, s_count);
                let b_count = uses.get(capture.binding).copied().unwrap_or(0) + 1;
                uses.upsert(capture.binding, b_count);
            }
        }
        for (id, _) in arena.compus.iter() {
            if let Some(call) = memory::MemoryCall::at(arena, id, &uses) {
                plans.memories.insert_new(id, call);
            }
        }
        let mut tails = IdBitSet::<CompuId>::new();
        for (_, compu) in arena.compus.iter() {
            if let Computation::LetValue(LetValue { tail, .. }) = compu {
                tails.insert(*tail);
            }
        }
        for (start, compu) in arena.compus.iter() {
            if !matches!(compu, Computation::LetValue(_)) || tails.contains(start) {
                continue;
            }
            let mut cursor = start;
            while let Computation::LetValue(binding) = &arena.compus[&cursor] {
                let mut expressions = Expressions::default();
                let mut bindings = SecondaryMap::new();
                let Some((mut result, mut result_ty)) =
                    expressions.value(arena, binding.bindee, None, &bindings)
                else {
                    cursor = binding.tail;
                    continue;
                };
                let mut current_compu = cursor;
                let mut current_binding = binding.clone();
                let mut committed_expr = expressions.clone();
                let mut committed_result = result;
                let mut committed_bindee = current_binding.bindee;
                let mut committed_next = current_binding.tail;
                let mut committed_elisions = Vec::new();
                let mut pending_elisions = Vec::new();
                while let ValuePattern::Var(def) = arena.vpats[&current_binding.binder] {
                    if uses.get(def).copied().unwrap_or(0) == 0 {
                        break;
                    }
                    let Computation::LetValue(next) = &arena.compus[&current_binding.tail] else {
                        break;
                    };
                    bindings.upsert(def, (result, result_ty));
                    let mut candidate = expressions.clone();
                    let Some((next_result, next_ty)) =
                        candidate.value(arena, next.bindee, None, &bindings)
                    else {
                        break;
                    };
                    pending_elisions.push((current_compu, def));
                    current_compu = current_binding.tail;
                    current_binding = next.clone();
                    expressions = candidate;
                    result = next_result;
                    result_ty = next_ty;
                    if pending_elisions
                        .iter()
                        .all(|&(_, bound)| expressions.substituted_count(bound) == uses[bound])
                    {
                        committed_elisions.extend(pending_elisions.drain(..).map(|(id, _)| id));
                        committed_expr = expressions.clone();
                        committed_result = result;
                        committed_bindee = current_binding.bindee;
                        committed_next = current_binding.tail;
                    }
                }
                for elided in committed_elisions {
                    plans.elided.insert(elided);
                }
                plans.calls.upsert(committed_bindee, committed_expr.finish(committed_result));
                cursor = committed_next;
            }
        }
        plans
    }

    pub fn call(&self, value: ValueId) -> &ScalarCall {
        &self.calls[value]
    }
    pub fn memory(&self, compu: CompuId) -> Option<&MemoryCall> {
        self.memories.get(compu)
    }
    pub fn elided(&self, compu: CompuId) -> bool {
        self.elided.contains(compu)
    }
    pub fn max_definitions(&self) -> usize {
        self.calls
            .iter()
            .map(|(_, call)| call.program.representations().len())
            .chain(self.memories.iter().map(|(_, call)| call.kernel.definitions()))
            .max()
            .unwrap_or(0)
    }
}

#[derive(Clone)]
enum Expression {
    Loaded,
    Input(ValueId, ScalarType),
    Arithmetic(PrimitiveOp, [usize; 2]),
}

#[derive(Clone, Default)]
struct Expressions {
    nodes: Vec<Expression>,
    operations: usize,
    substituted: SecondaryMap<DefId, usize>,
}

impl Expressions {
    fn substituted_count(&self, def: DefId) -> usize {
        self.substituted.get(def).copied().unwrap_or(0)
    }

    /// Recursive descent is bounded by MAX_OPERATIONS, independently of source depth.
    fn value(
        &mut self, arena: &SpsLowInnerArena, id: ValueId, expected: Option<ScalarType>,
        bindings: &SecondaryMap<DefId, (usize, ScalarType)>,
    ) -> Option<(usize, ScalarType)> {
        let (node, node_ty) = match &arena.values[&id] {
            | Value::Primitive(Primitive { operation, operands }) => {
                let ty = operation.scalar_type();
                if ty.representation() != ScalarRepresentation::OpaqueBox
                    || expected.is_some_and(|expected| expected != ty)
                    || self.operations == MAX_OPERATIONS
                {
                    return None;
                }
                self.operations += 1;
                let (second, _) = self.value(arena, operands[1], Some(ty), bindings)?;
                let (first, _) = self.value(arena, operands[0], Some(ty), bindings)?;
                (Expression::Arithmetic(*operation, [first, second]), ty)
            }
            | Value::Var(def) => {
                let ty = expected?;
                if let Some(&(value, bound_ty)) = bindings.get(*def) {
                    if bound_ty != ty {
                        return None;
                    }
                    let count = self.substituted_count(*def) + 1;
                    self.substituted.upsert(*def, count);
                    return Some((value, ty));
                }
                (Expression::Input(id, ty), ty)
            }
            | Value::Literal(literal) => {
                let ty = expected?;
                if ScalarType::of_literal(literal) != Some(ty) {
                    return None;
                }
                (Expression::Input(id, ty), ty)
            }
            | _ => return None,
        };
        let index = self.nodes.len();
        self.nodes.push(node);
        Some((index, node_ty))
    }

    fn finish(self, result: usize) -> ScalarCall {
        let Expression::Arithmetic(operation, _) = self.nodes[result] else { unreachable!() };
        // Calls push operands right to left. Reverse the leaves so their execution
        // order agrees with the original expression and binding order.
        let (inputs, types): (Vec<_>, Vec<_>) = self
            .nodes
            .iter()
            .rev()
            .filter_map(|node| match node {
                | Expression::Input(id, ty) => Some((*id, *ty)),
                | _ => None,
            })
            .unzip();
        let mut next_input = inputs.len();
        let mut steps = Vec::new();
        let mut definitions = Vec::<ScalarId>::new();
        for node in self.nodes {
            let definition = match node {
                | Expression::Loaded => {
                    unreachable!("ordinary scalar regions have no memory input")
                }
                | Expression::Input(_, _) => {
                    next_input -= 1;
                    ScalarId(next_input)
                }
                | Expression::Arithmetic(operation, operands) => {
                    let ty = operation.scalar_type();
                    let first = ScalarId(inputs.len() + steps.len());
                    steps.push(ScalarStep::Decode { ty, value: definitions[operands[0]] });
                    let second = ScalarId(inputs.len() + steps.len());
                    steps.push(ScalarStep::Decode { ty, value: definitions[operands[1]] });
                    let raw = ScalarId(inputs.len() + steps.len());
                    steps.push(ScalarStep::Arithmetic { operation, operands: [first, second] });
                    let encoded = ScalarId(inputs.len() + steps.len());
                    steps.push(ScalarStep::Encode { ty, raw });
                    encoded
                }
            };
            definitions.push(definition);
        }
        let program = ScalarRegion {
            inputs: types,
            output: operation.scalar_type(),
            steps,
            result: definitions[result],
        }
        .verify()
        .expect("scalar lowering preserves checked primitive types");
        let program = program
            .eliminate_boxes()
            .expect("scalar box elimination preserves representation contracts");
        ScalarCall { inputs, program }
    }
}

#[cfg(test)]
mod tests;
