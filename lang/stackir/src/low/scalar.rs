//! Bounded scalar regions shared by native and both Wasm lowering paths.
//!
//! A region can contain a primitive tree or adjacent single-use primitive bindings.
//! Calls, branches, captures, shared bindings, and other value constructions end it.

use super::SpsLowProgram;
use super::syntax::*;
use std::collections::{HashMap, HashSet};
use zydeco_syntax::scalar::{
    ScalarBoxing, ScalarId, ScalarProgram, ScalarRegion, ScalarStep, ScalarType,
};
use zydeco_syntax::word::ScalarRepresentation;

const MAX_OPERATIONS: usize = 32;

#[derive(Clone, Debug)]
pub struct ScalarCall {
    pub inputs: Vec<ValueId>,
    pub program: ScalarProgram,
}

#[derive(Default)]
pub struct ScalarPlans {
    calls: HashMap<ValueId, ScalarCall>,
    elided: HashSet<CompuId>,
}

impl ScalarPlans {
    /// Build representation-checked primitive regions; source typing is supplied by the caller.
    pub fn unoptimized(arena: &SpsLowInnerArena) -> Self {
        let calls = arena
            .values
            .iter()
            .filter_map(|(&id, value)| match value {
                | Value::Primitive(Primitive { operation, operands }) => Some((
                    id,
                    ScalarCall {
                        inputs: operands.to_vec(),
                        program: ScalarProgram::primitive(*operation),
                    },
                )),
                | _ => None,
            })
            .collect();
        Self { calls, elided: HashSet::new() }
    }

    pub fn new(program: &SpsLowProgram, boxing: ScalarBoxing) -> Self {
        let arena = &program.arena().inner;
        let mut plans = Self::unoptimized(arena);
        // The keep policy retains individual primitive boundaries for an execution baseline.
        if boxing == ScalarBoxing::Keep {
            return plans;
        }
        for (&id, value) in &arena.values {
            if matches!(value, Value::Primitive(_)) {
                let mut expressions = Expressions::default();
                if let Some(result) = expressions.value(arena, id, None, &HashMap::new()) {
                    plans.calls.insert(id, expressions.finish(result));
                }
            }
        }
        let mut uses = HashMap::<DefId, usize>::new();
        for (_, value) in &arena.values {
            if let Value::Var(def) = value {
                *uses.entry(*def).or_default() += 1;
            }
        }
        // Native continuation evidence also retains its bindings across suspension.
        for (_, entry) in &arena.continuations {
            for capture in &entry.captures {
                *uses.entry(capture.source).or_default() += 1;
                *uses.entry(capture.binding).or_default() += 1;
            }
        }
        let tails = arena
            .compus
            .iter()
            .filter_map(|(_, compu)| match compu {
                | Computation::LetValue(LetValue { tail, .. }) => Some(*tail),
                | _ => None,
            })
            .collect::<HashSet<_>>();
        for (&start, compu) in &arena.compus {
            if !matches!(compu, Computation::LetValue(_)) || tails.contains(&start) {
                continue;
            }
            let mut cursor = start;
            while let Computation::LetValue(binding) = &arena.compus[&cursor] {
                let mut expressions = Expressions::default();
                let mut bindings = HashMap::new();
                let Some(mut result) = expressions.value(arena, binding.bindee, None, &bindings)
                else {
                    cursor = binding.tail;
                    continue;
                };
                let mut last = cursor;
                let mut binding = binding.clone();
                while let ValuePattern::Var(def) = arena.vpats[&binding.binder] {
                    if uses.get(&def) != Some(&1) {
                        break;
                    }
                    let Computation::LetValue(next) = &arena.compus[&binding.tail] else {
                        break;
                    };
                    let mut candidate = expressions.clone();
                    bindings.insert(def, result);
                    candidate.substituted.clear();
                    let Some(next_result) = candidate.value(arena, next.bindee, None, &bindings)
                    else {
                        break;
                    };
                    if !candidate.substituted.contains(&def) {
                        break;
                    }
                    plans.elided.insert(last);
                    last = binding.tail;
                    binding = next.clone();
                    expressions = candidate;
                    result = next_result;
                }
                plans.calls.insert(binding.bindee, expressions.finish(result));
                cursor = binding.tail;
            }
        }
        plans
    }

    pub fn call(&self, value: ValueId) -> &ScalarCall {
        &self.calls[&value]
    }
    pub fn elided(&self, compu: CompuId) -> bool {
        self.elided.contains(&compu)
    }
    pub fn max_definitions(&self) -> usize {
        self.calls.values().map(|call| call.program.representations().len()).max().unwrap_or(0)
    }
}

#[derive(Clone)]
enum Expression {
    Input(ValueId, ScalarType),
    Arithmetic(PrimitiveOp, [usize; 2]),
}

#[derive(Clone, Default)]
struct Expressions {
    nodes: Vec<Expression>,
    operations: usize,
    substituted: HashSet<DefId>,
}

impl Expressions {
    /// Recursive descent is bounded by MAX_OPERATIONS, independently of source depth.
    fn value(
        &mut self, arena: &SpsLowInnerArena, id: ValueId, expected: Option<ScalarType>,
        bindings: &HashMap<DefId, usize>,
    ) -> Option<usize> {
        let node = match &arena.values[&id] {
            | Value::Primitive(Primitive { operation, operands }) => {
                let ty = operation.scalar_type();
                if ty.representation() != ScalarRepresentation::OpaqueBox
                    || expected.is_some_and(|expected| expected != ty)
                    || self.operations == MAX_OPERATIONS
                {
                    return None;
                }
                self.operations += 1;
                let second = self.value(arena, operands[1], Some(ty), bindings)?;
                let first = self.value(arena, operands[0], Some(ty), bindings)?;
                Expression::Arithmetic(*operation, [first, second])
            }
            | Value::Var(def) => {
                let ty = expected?;
                if let Some(&value) = bindings.get(def) {
                    self.substituted.insert(*def);
                    return Some(value);
                }
                Expression::Input(id, ty)
            }
            | Value::Literal(literal) => {
                let ty = expected?;
                if ScalarType::of_literal(literal) != Some(ty) {
                    return None;
                }
                Expression::Input(id, ty)
            }
            | _ => return None,
        };
        let index = self.nodes.len();
        self.nodes.push(node);
        Some(index)
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
