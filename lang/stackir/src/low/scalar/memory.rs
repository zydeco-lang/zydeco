//! Close a local, single-use scalar chain at a store without publishing raw source values.

use super::*;
use zydeco_syntax::scalar::{KernelRegion, KernelStep, ScalarKernel};

/// A total address calculation moved before the raw load. Its binding remains
/// available to the kernel and successor with the original address protocol.
#[derive(Clone, Debug)]
pub struct AddressBinding {
    pub binder: VPatId,
    pub value: ValueId,
}

#[derive(Clone, Debug)]
pub struct MemoryCall {
    pub address_bindings: Vec<AddressBinding>,
    pub load_address: ValueId,
    pub store_address: ValueId,
    pub inputs: Vec<ValueId>,
    pub kernel: ScalarKernel,
    pub next: CompuId,
}

impl MemoryCall {
    pub(super) fn at(
        arena: &SpsLowInnerArena, start: CompuId, uses: &HashMap<DefId, usize>,
    ) -> Option<Self> {
        let Computation::Memory(MemoryStep::Load { scalar, address, result, next }) =
            arena.compus[&start]
        else {
            return None;
        };
        let ty = scalar.value_type()?;
        if ty.representation() != ScalarRepresentation::OpaqueBox {
            return None;
        }
        let ValuePattern::Var(loaded) = arena.vpats[&result] else {
            return None;
        };
        if uses.get(&loaded) != Some(&1) {
            return None;
        }
        let mut expressions =
            Expressions { nodes: vec![Expression::Loaded], ..Expressions::default() };
        let mut bindings = HashMap::from([(loaded, 0)]);
        let mut address_bindings = Vec::new();
        let mut cursor = next;
        loop {
            if bindings.len() + address_bindings.len() > MAX_OPERATIONS + 1 {
                return None;
            }
            match &arena.compus[&cursor] {
                | Computation::LetValue(binding) => {
                    let ValuePattern::Var(def) = arena.vpats[&binding.binder] else {
                        return None;
                    };
                    if matches!(arena.values[&binding.bindee], Value::AddrOffset(_))
                        && Self::independent_address(arena, binding.bindee, &bindings)
                    {
                        address_bindings
                            .push(AddressBinding { binder: binding.binder, value: binding.bindee });
                        cursor = binding.tail;
                        continue;
                    }
                    if uses.get(&def) != Some(&1) {
                        return None;
                    }
                    let result = expressions.value(arena, binding.bindee, Some(ty), &bindings)?;
                    bindings.insert(def, result);
                    cursor = binding.tail;
                }
                | Computation::Memory(MemoryStep::Store {
                    scalar: stored,
                    address: destination,
                    value,
                    next,
                }) if *stored == scalar => {
                    // Only total, independent address calculations may move before the load.
                    if !Self::independent_address(arena, *destination, &bindings) {
                        return None;
                    }
                    let result = expressions.value(arena, *value, Some(ty), &bindings)?;
                    if bindings.keys().any(|def| !expressions.substituted.contains(def)) {
                        return None;
                    }
                    let (inputs, kernel) = expressions.finish_kernel(arena, ty, result)?;
                    return Some(Self {
                        address_bindings,
                        load_address: address,
                        store_address: *destination,
                        inputs,
                        kernel,
                        next: *next,
                    });
                }
                | _ => return None,
            }
        }
    }

    fn independent_address(
        arena: &SpsLowInnerArena, address: ValueId, bindings: &HashMap<DefId, usize>,
    ) -> bool {
        let mut address = address;
        for _ in 0..MAX_OPERATIONS {
            match &arena.values[&address] {
                | Value::Var(def) => return !bindings.contains_key(def),
                | Value::AddrOffset(AddrOffset { base, displacement }) => {
                    let total = match &arena.values[displacement] {
                        | Value::Var(def) => !bindings.contains_key(def),
                        | Value::Literal(Literal::Integer(IntegerLiteral::Int(_))) => true,
                        | _ => false,
                    };
                    if !total {
                        return false;
                    }
                    address = *base;
                }
                | _ => return false,
            }
        }
        false
    }
}

impl Expressions {
    fn finish_kernel(
        self, arena: &SpsLowInnerArena, output: ScalarType, result: usize,
    ) -> Option<(Vec<ValueId>, ScalarKernel)> {
        let (inputs, types): (Vec<_>, Vec<_>) = self
            .nodes
            .iter()
            .filter_map(|node| match node {
                | Expression::Input(id, ty) if matches!(arena.values[id], Value::Var(_)) => {
                    Some((*id, *ty))
                }
                | _ => None,
            })
            .unzip();
        let mut region = KernelRegion {
            inputs: std::iter::once(output).chain(types).collect(),
            output,
            steps: Vec::new(),
            result: ScalarId(0),
        };
        let mut next_input = 1;
        let mut definitions = Vec::<ScalarId>::new();
        for node in self.nodes {
            let step = match node {
                | Expression::Loaded => {
                    definitions.push(ScalarId(0));
                    continue;
                }
                | Expression::Input(id, _) => match &arena.values[&id] {
                    | Value::Var(_) => {
                        definitions.push(ScalarId(next_input));
                        next_input += 1;
                        continue;
                    }
                    | Value::Literal(literal) => KernelStep::Literal(literal.clone()),
                    | _ => return None,
                },
                | Expression::Arithmetic(operation, operands) => KernelStep::Arithmetic {
                    operation,
                    operands: operands.map(|id| definitions[id]),
                },
            };
            definitions.push(ScalarId(region.inputs.len() + region.steps.len()));
            region.steps.push(step);
        }
        region.result = definitions[result];
        Some((inputs, region.verify().ok()?))
    }
}
