//! Check known source protocol components while leaving unknown stack extent opaque.

use super::syntax::*;
use crate::protocol::{ProtocolGraphError, StackProtocol, ValueProtocol};
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ProtocolError {
    #[error(transparent)]
    Graph(#[from] ProtocolGraphError),
    #[error("entry {block:?} declares {found:?} protocol evidence for a {expected:?} entry")]
    EntryKind { block: ValueId, expected: EntryKind, found: EntryKind },
    #[error("computation {compu:?} requires stack protocol {expected}, found {found}")]
    Stack { compu: CompuId, expected: StackProtocol, found: StackProtocol },
    #[error("value {value:?} requires protocol {expected}, found {found}")]
    Value { value: ValueId, expected: ValueProtocol, found: ValueProtocol },
    #[error("parameter {pattern:?} requires protocol {expected}, found {found}")]
    Parameter { pattern: VPatId, expected: ValueProtocol, found: ValueProtocol },
    #[error("computation {compu:?} requires a codata protocol, found {found}")]
    ExpectedCodata { compu: CompuId, found: StackProtocol },
    #[error("computation {compu:?} expects observations {expected:?}, found {found:?}")]
    Observations { compu: CompuId, expected: Vec<DtorIdx>, found: Vec<DtorIdx> },
    #[error("computation {compu:?} has no branch for observation {tag:?}")]
    MissingObservation { compu: CompuId, tag: DtorIdx },
}

#[derive(Clone, Default)]
struct ValueFact {
    protocol: ValueProtocol,
    code: Option<EntryProtocol>,
    fields: Vec<ValueFact>,
}

impl ValueFact {
    fn with_protocol(protocol: ValueProtocol) -> Self {
        let fields = match &protocol {
            | ValueProtocol::Product(fields) => {
                fields.iter().cloned().map(Self::with_protocol).collect()
            }
            | _ => Vec::new(),
        };
        Self { protocol, fields, code: None }
    }

    fn product(fields: Vec<Self>) -> Self {
        Self {
            protocol: ValueProtocol::Product(
                fields.iter().map(|field| field.protocol.clone()).collect(),
            ),
            fields,
            code: None,
        }
    }

    fn with_code(code: EntryProtocol) -> Self {
        Self { code: Some(code), ..Self::default() }
    }
}

#[derive(Clone, Default)]
struct Context {
    values: HashMap<DefId, ValueFact>,
    stack: StackProtocol,
}

pub(super) struct ProtocolValidator<'a> {
    arena: &'a SpsLowInnerArena,
}

impl<'a> ProtocolValidator<'a> {
    pub(super) fn validate(
        arena: &'a SpsLowInnerArena, root: CompuId,
    ) -> Result<(), ProtocolError> {
        arena.protocols.validate()?;
        for value in arena
            .value_protocols
            .iter()
            .map(|(_, value)| value)
            .chain(arena.pattern_protocols.iter().map(|(_, value)| value))
        {
            arena.protocols.validate_value(value)?;
        }
        for (_, entry) in arena.entry_protocols.iter() {
            match entry {
                | EntryProtocol::Closure(stack) => arena.protocols.validate_stack(stack)?,
                | EntryProtocol::Continuation(value) => arena.protocols.validate_value(value)?,
            }
        }
        for (_, stack) in arena.case_protocols.iter() {
            arena.protocols.validate_stack(stack)?;
        }
        Self { arena }.compu(root, Context::default())
    }

    fn check_stack(
        &self, compu: CompuId, expected: &StackProtocol, found: &StackProtocol,
    ) -> Result<(), ProtocolError> {
        if self.arena.protocols.stacks_agree(expected, found) {
            Ok(())
        } else {
            Err(ProtocolError::Stack { compu, expected: expected.clone(), found: found.clone() })
        }
    }

    fn check_value(
        &self, value: ValueId, expected: &ValueProtocol, found: &ValueProtocol,
    ) -> Result<(), ProtocolError> {
        if self.arena.protocols.values_agree(expected, found) {
            Ok(())
        } else {
            Err(ProtocolError::Value { value, expected: expected.clone(), found: found.clone() })
        }
    }

    fn entry(
        &self, id: ValueId, parameters: EntryParameters,
    ) -> Result<EntryProtocol, ProtocolError> {
        let entry = self
            .arena
            .entry_protocols
            .get(&id)
            .cloned()
            .unwrap_or_else(|| EntryProtocol::unknown(parameters.kind()));
        if entry.kind() != parameters.kind() {
            return Err(ProtocolError::EntryKind {
                block: id,
                expected: parameters.kind(),
                found: entry.kind(),
            });
        }
        Ok(entry)
    }

    fn parameter(&self, pattern: VPatId, found: &ValueProtocol) -> Result<(), ProtocolError> {
        if let Some(expected) = self.arena.pattern_protocols.get(&pattern)
            && !self.arena.protocols.values_agree(expected, found)
        {
            return Err(ProtocolError::Parameter {
                pattern,
                expected: expected.clone(),
                found: found.clone(),
            });
        }
        Ok(())
    }

    fn bind(&self, id: VPatId, mut fact: ValueFact, context: &mut Context) {
        if let Some(protocol) = self.arena.pattern_protocols.get(&id) {
            fact.protocol = fact.protocol.with_evidence(protocol);
            if fact.fields.is_empty() {
                fact.fields = ValueFact::with_protocol(fact.protocol.clone()).fields;
            }
        }
        match &self.arena.vpats[&id] {
            | ValuePattern::Var(def) => {
                context.values.insert(*def, fact);
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.bind(*pattern, fact.clone(), context);
                }
            }
            | ValuePattern::VCons(product) => {
                for (index, pattern) in product.items.iter().enumerate() {
                    let field = if fact.fields.len() == product.layout.arity {
                        if index + 1 == product.items.len()
                            && product.items.len() < product.layout.arity
                        {
                            ValueFact::product(fact.fields[index..].to_vec())
                        } else {
                            fact.fields[index].clone()
                        }
                    } else {
                        ValueFact::default()
                    };
                    self.bind(*pattern, field, context);
                }
            }
            | ValuePattern::Ctor(Ctor(_, pattern)) => {
                self.bind(*pattern, ValueFact::default(), context)
            }
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
        }
    }

    fn value(&self, id: ValueId, context: &Context) -> Result<ValueFact, ProtocolError> {
        let mut fact = match &self.arena.values[&id] {
            | Value::Var(def) => context.values.get(def).cloned().unwrap_or_default(),
            | Value::Block(Block { label, entry, body }) => {
                let protocol = self.entry(id, *entry)?;
                let mut local = Context::default();
                local.values.insert(*label, ValueFact::with_code(protocol.clone()));
                self.bind(entry.environment(), ValueFact::default(), &mut local);
                match &protocol {
                    | EntryProtocol::Closure(stack) => local.stack = stack.clone(),
                    | EntryProtocol::Continuation(value) => {
                        let EntryParameters::Continuation { result, .. } = entry else {
                            unreachable!()
                        };
                        self.parameter(*result, value)?;
                        self.bind(*result, ValueFact::with_protocol(value.clone()), &mut local);
                    }
                }
                self.compu(*body, local)?;
                ValueFact::with_code(protocol)
            }
            | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                self.value(*environment, context)?;
                let code = self.value(*code, context)?;
                let stack = match code.code {
                    | Some(EntryProtocol::Closure(stack)) => stack,
                    | _ => StackProtocol::Unknown,
                };
                ValueFact::with_protocol(ValueProtocol::Thunk(Box::new(stack)))
            }
            | Value::VCons(product) => {
                let mut fields = product
                    .items
                    .iter()
                    .map(|value| self.value(*value, context))
                    .collect::<Result<Vec<_>, _>>()?;
                if fields.len() < product.layout.arity {
                    let suffix = fields.pop().unwrap_or_default();
                    let remaining = product.layout.arity - fields.len();
                    fields.extend(if suffix.fields.len() == remaining {
                        suffix.fields
                    } else {
                        vec![ValueFact::default(); remaining]
                    });
                }
                ValueFact::product(fields)
            }
            | Value::Triv(_) => ValueFact::with_protocol(ValueProtocol::Unit),
            | Value::Literal(literal) => ValueFact::with_protocol(match literal {
                | Literal::Integer(value) => value
                    .integer_type()
                    .map(|ty| ValueProtocol::Primitive(PrimitiveType::Integer(ty)))
                    .unwrap_or_default(),
                | Literal::Float(value) => {
                    ValueProtocol::Primitive(PrimitiveType::Float(value.float_type()))
                }
                | Literal::String(_) => ValueProtocol::Primitive(PrimitiveType::String),
                | Literal::Char(_) => ValueProtocol::Primitive(PrimitiveType::Char),
            }),
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                for (value, expected) in [
                    (*base, ValueProtocol::Address),
                    (
                        *displacement,
                        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int)),
                    ),
                ] {
                    let fact = self.value(value, context)?;
                    self.check_value(value, &expected, &fact.protocol)?;
                }
                ValueFact::with_protocol(ValueProtocol::Address)
            }
            | Value::Primitive(Primitive { operation, operands }) => {
                for value in operands {
                    self.value(*value, context)?;
                }
                ValueFact::with_protocol(ValueProtocol::Primitive(match operation {
                    | PrimitiveOp::Integer(ty, _) => PrimitiveType::Integer(*ty),
                    | PrimitiveOp::Float(ty, _) => PrimitiveType::Float(*ty),
                }))
            }
            | Value::Ctor(Ctor(_, body)) => {
                self.value(*body, context)?;
                ValueFact::default()
            }
            | Value::Hole(_) => ValueFact::default(),
        };
        if let Some(protocol) = self.arena.value_protocols.get(&id) {
            if matches!(self.arena.values[&id], Value::ClosurePackage(_)) {
                self.check_value(id, protocol, &fact.protocol)?;
            }
            fact.protocol = fact.protocol.with_evidence(protocol);
            if fact.fields.is_empty() {
                fact.fields = ValueFact::with_protocol(fact.protocol.clone()).fields;
            }
        }
        Ok(fact)
    }

    fn stack(&self, id: StackId, context: &Context) -> Result<StackProtocol, ProtocolError> {
        Ok(match &self.arena.stacks[&id] {
            | Stack::Var(_) => context.stack.runtime_head().clone(),
            | Stack::Arg(Cons(value, rest)) => StackProtocol::Argument(
                Box::new(self.value(*value, context)?.protocol),
                Box::new(self.stack(*rest, context)?),
            ),
            | Stack::Tag(Cons(tag, rest)) => {
                StackProtocol::Tag(tag.clone(), Box::new(self.stack(*rest, context)?))
            }
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                let code = self.value(*code, context)?;
                self.stack(*residual, context)?;
                let value = match code.code {
                    | Some(EntryProtocol::Continuation(value)) => value,
                    | _ => ValueProtocol::Unknown,
                };
                StackProtocol::Continuation(Box::new(value))
            }
        })
    }

    fn case(&self, id: CompuId, case: &SCoMatch, context: Context) -> Result<(), ProtocolError> {
        let SCoMatch { scrut, arms } = case;
        let supplied = self.stack(*scrut, &context)?;
        let expected = match self.arena.case_protocols.get(&id) {
            | Some(expected) => {
                self.check_stack(id, expected, &supplied)?;
                expected
            }
            | None => &supplied,
        };
        let definition = match expected.runtime_head() {
            | StackProtocol::Codata(protocol) => {
                let definition = self
                    .arena
                    .protocols
                    .get(*protocol)
                    .expect("protocol references were validated");
                let expected =
                    definition.observations.iter().map(|(tag, _)| tag.clone()).collect::<Vec<_>>();
                let found = arms.iter().map(|arm| arm.dtor.0.clone()).collect::<Vec<_>>();
                if expected.len() != found.len() || expected.iter().any(|tag| !found.contains(tag))
                {
                    return Err(ProtocolError::Observations { compu: id, expected, found });
                }
                Some(definition)
            }
            | StackProtocol::Tag(tag, _) => {
                if !arms.iter().any(|arm| &arm.dtor.0 == tag) {
                    return Err(ProtocolError::MissingObservation { compu: id, tag: tag.clone() });
                }
                None
            }
            | StackProtocol::Unknown | StackProtocol::Parameter(_) => None,
            | found => {
                return Err(ProtocolError::ExpectedCodata { compu: id, found: found.clone() });
            }
        };
        for CoMatcher { dtor: Cons(tag, _), tail } in arms {
            let mut branch = context.clone();
            let declared = definition
                .and_then(|definition| definition.observation(tag))
                .cloned()
                .unwrap_or_default();
            branch.stack = match &supplied {
                | StackProtocol::Tag(selected, rest) if selected == tag => {
                    rest.clone().with_evidence(&declared)
                }
                | _ => declared,
            };
            self.compu(*tail, branch)?;
        }
        Ok(())
    }

    fn compu(&self, mut id: CompuId, mut context: Context) -> Result<(), ProtocolError> {
        loop {
            match &self.arena.compus[&id] {
                | Computation::Memory(MemoryStep::Load { scalar, address, result, next }) => {
                    let address_fact = self.value(*address, &context)?;
                    self.check_value(*address, &ValueProtocol::Address, &address_fact.protocol)?;
                    let protocol = ValueProtocol::from(*scalar);
                    self.parameter(*result, &protocol)?;
                    self.bind(*result, ValueFact::with_protocol(protocol), &mut context);
                    id = *next;
                }
                | Computation::Memory(MemoryStep::Store { scalar, address, value, next }) => {
                    let address_fact = self.value(*address, &context)?;
                    self.check_value(*address, &ValueProtocol::Address, &address_fact.protocol)?;
                    let value_fact = self.value(*value, &context)?;
                    self.check_value(*value, &ValueProtocol::from(*scalar), &value_fact.protocol)?;
                    id = *next;
                }
                | Computation::Hole(SHole(stack))
                | Computation::ExternCall(ExternCall { stack, .. }) => {
                    self.stack(*stack, &context)?;
                    return Ok(());
                }
                | Computation::Jump(Jump { target, argument, stack }) => {
                    let code = self
                        .value(*target, &context)?
                        .code
                        .unwrap_or_else(|| EntryProtocol::unknown(argument.kind()));
                    let value = self.value(argument.word().1, &context)?;
                    let stack = self.stack(*stack, &context)?;
                    match code {
                        | EntryProtocol::Closure(expected) => {
                            self.check_stack(id, &expected, &stack)?
                        }
                        | EntryProtocol::Continuation(expected) => {
                            self.check_value(argument.word().1, &expected, &value.protocol)?
                        }
                    }
                    return Ok(());
                }
                | Computation::ProductMatch(SProductMatch { scrut, binder, body })
                | Computation::LetValue(LetValue { bindee: scrut, binder, tail: body }) => {
                    let value = self.value(*scrut, &context)?;
                    self.bind(*binder, value, &mut context);
                    id = *body;
                }
                | Computation::Compare(CompareBranch {
                    operation,
                    operands,
                    when_true,
                    when_false,
                }) => {
                    for operand in operands {
                        let fact = self.value(*operand, &context)?;
                        self.check_value(
                            *operand,
                            &ValueProtocol::Primitive(operation.operand_type()),
                            &fact.protocol,
                        )?;
                    }
                    self.compu(*when_true, context.clone())?;
                    id = *when_false;
                }
                | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                    self.value(*scrut, &context)?;
                    for Matcher { binder, tail } in arms {
                        let mut branch = context.clone();
                        self.bind(*binder, ValueFact::default(), &mut branch);
                        self.compu(*tail, branch)?;
                    }
                    return Ok(());
                }
                | Computation::LetStack(LetStack { bindee, tail, .. }) => {
                    context.stack = self.stack(*bindee, &context)?;
                    id = *tail;
                }
                | Computation::LetArg(LetArg { binder: Cons(binder, _), bindee, tail }) => {
                    let stack = self.stack(*bindee, &context)?;
                    self.check_stack(
                        id,
                        &StackProtocol::Argument(Box::default(), Box::default()),
                        &stack,
                    )?;
                    let (value, rest) = match stack {
                        | StackProtocol::Argument(value, rest) => (*value, *rest),
                        | _ => (ValueProtocol::Unknown, StackProtocol::Unknown),
                    };
                    self.parameter(*binder, &value)?;
                    self.bind(*binder, ValueFact::with_protocol(value), &mut context);
                    context.stack = rest;
                    id = *tail;
                }
                | Computation::CoCase(case) => return self.case(id, case, context),
                | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                    let value = self.value(*package, &context)?.protocol;
                    self.check_value(*package, &ValueProtocol::Thunk(Box::default()), &value)?;
                    let protocol = match value {
                        | ValueProtocol::Thunk(stack) => *stack,
                        | _ => StackProtocol::Unknown,
                    };
                    self.bind(*environment, ValueFact::default(), &mut context);
                    self.bind(
                        *code,
                        ValueFact::with_code(EntryProtocol::Closure(protocol)),
                        &mut context,
                    );
                    id = *body;
                }
                | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                    let stack = self.stack(*package, &context)?;
                    self.check_stack(id, &StackProtocol::Continuation(Box::default()), &stack)?;
                    let value = match stack {
                        | StackProtocol::Continuation(value) => *value,
                        | _ => ValueProtocol::Unknown,
                    };
                    self.bind(
                        *code,
                        ValueFact::with_code(EntryProtocol::Continuation(value)),
                        &mut context,
                    );
                    // Opening hides the saved extent. Only the provenance checker relates it
                    // to the eventual jump; there is no physical end-of-frame descriptor.
                    context.stack = StackProtocol::Unknown;
                    id = *body;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comparisons_check_both_operands_and_both_successor_protocols() {
        // Vary each input independently; a true first arm must not hide a bad second arm.
        for rejected in [None, Some(0), Some(1), Some(2), Some(3)] {
            let mut arena = SpsLowArena::default();
            let operands = [0, 1].map(|index| {
                let value = if rejected == Some(index) {
                    IntegerLiteral::UInt64(1)
                } else {
                    IntegerLiteral::Int64(1)
                };
                Literal::Integer(value).build(&mut arena, None)
            });
            let successors = [2, 3].map(|index| {
                let stack = Bullet.build(&mut arena, None);
                let tail = SHole(stack).build(&mut arena, None);
                let binder = Hole.build(&mut arena, None);
                let protocol = if rejected == Some(index) {
                    ValueProtocol::Unit
                } else {
                    ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))
                };
                arena.inner.pattern_protocols.insert_new(binder, protocol);
                let bindee = Bullet.build(&mut arena, None);
                (
                    LetArg { binder: Cons(binder, Bullet), bindee, tail }.build(&mut arena, None),
                    binder,
                )
            });
            let root = CompareBranch {
                operation: ComparisonOp::Integer(IntegerType::Int64, ComparisonPredicate::Eq),
                operands,
                when_true: successors[0].0,
                when_false: successors[1].0,
            }
            .build(&mut arena, None);
            let context = Context {
                stack: StackProtocol::Argument(
                    Box::new(ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))),
                    Box::default(),
                ),
                ..Context::default()
            };
            let checked = ProtocolValidator { arena: &arena.inner }.compu(root, context);
            match rejected {
                | None => checked.unwrap(),
                | Some(index @ (0 | 1)) => assert!(
                    matches!(checked, Err(ProtocolError::Value { value, .. }) if value == operands[index])
                ),
                | Some(index) => assert!(
                    matches!(checked, Err(ProtocolError::Parameter { pattern, .. }) if pattern == successors[index - 2].1)
                ),
            }
        }
    }

    #[test]
    fn memory_steps_check_load_results_and_store_operands() {
        use memory::MemoryScalar;
        for store_type in [IntegerType::Int64, IntegerType::UInt64] {
            let mut arena = SpsLowArena::default();
            let base = arena.admin.fresh_def();
            let loaded = arena.admin.fresh_def();
            let address = base.build(&mut arena, None);
            let store_address = base.build(&mut arena, None);
            let value = loaded.build(&mut arena, None);
            let result = loaded.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            let next = SHole(stack).build(&mut arena, None);
            let next = MemoryStep::Store {
                scalar: MemoryScalar::Integer(store_type),
                address: store_address,
                value,
                next,
            }
            .build(&mut arena, None);
            let root = MemoryStep::Load {
                scalar: MemoryScalar::Integer(IntegerType::Int64),
                address,
                result,
                next,
            }
            .build(&mut arena, None);
            let context = Context {
                values: HashMap::from([(base, ValueFact::with_protocol(ValueProtocol::Address))]),
                ..Context::default()
            };
            let checked = ProtocolValidator { arena: &arena.inner }.compu(root, context);
            if store_type == IntegerType::Int64 {
                checked.unwrap();
            } else {
                assert!(
                    matches!(checked, Err(ProtocolError::Value { value: found, .. }) if found == value)
                );
            }
        }
    }

    #[test]
    fn address_offsets_check_operands_and_preserve_address_results() {
        for (base_protocol, displacement, valid) in [
            (ValueProtocol::Address, IntegerLiteral::Int(-7), true),
            (
                ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::UInt64)),
                IntegerLiteral::Int(0),
                false,
            ),
            (ValueProtocol::Address, IntegerLiteral::Int64(0), false),
        ] {
            let mut arena = SpsLowArena::default();
            let definition = arena.admin.fresh_def();
            let base = definition.build(&mut arena, None);
            let delta = Literal::Integer(displacement).build(&mut arena, None);
            let value = AddrOffset { base, displacement: delta }.build(&mut arena, None);
            let context = Context {
                values: HashMap::from([(definition, ValueFact::with_protocol(base_protocol))]),
                ..Context::default()
            };
            let result = ProtocolValidator { arena: &arena.inner }.value(value, &context);
            if valid {
                assert_eq!(result.unwrap().protocol, ValueProtocol::Address);
            } else {
                let expected_value = if displacement.integer_type() == Some(IntegerType::Int) {
                    base
                } else {
                    delta
                };
                assert!(
                    matches!(result, Err(ProtocolError::Value { value, .. }) if value == expected_value)
                );
            }
        }
    }
}
