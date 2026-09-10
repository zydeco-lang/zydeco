//! Check known source protocol components while leaving unknown stack extent opaque.

use super::syntax::*;
use crate::protocol::{StackProtocol, ValueProtocol};
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ProtocolError {
    #[error("entry {block:?} declares {found:?} protocol evidence for a {expected:?} entry")]
    EntryKind { block: ValueId, expected: EntryKind, found: EntryKind },
    #[error("computation {compu:?} requires stack protocol {expected}, found {found}")]
    Stack { compu: CompuId, expected: StackProtocol, found: StackProtocol },
    #[error("value {value:?} requires protocol {expected}, found {found}")]
    Value { value: ValueId, expected: ValueProtocol, found: ValueProtocol },
    #[error("parameter {pattern:?} requires protocol {expected}, found {found}")]
    Parameter { pattern: VPatId, expected: ValueProtocol, found: ValueProtocol },
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
        Self { arena }.compu(root, Context::default())
    }

    fn check_stack(
        compu: CompuId, expected: &StackProtocol, found: &StackProtocol,
    ) -> Result<(), ProtocolError> {
        if expected.agrees(found) {
            Ok(())
        } else {
            Err(ProtocolError::Stack { compu, expected: expected.clone(), found: found.clone() })
        }
    }

    fn check_value(
        value: ValueId, expected: &ValueProtocol, found: &ValueProtocol,
    ) -> Result<(), ProtocolError> {
        if expected.agrees(found) {
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
            && !expected.agrees(found)
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
                Self::check_value(id, protocol, &fact.protocol)?;
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
            | Stack::Var(_) => context.stack.clone(),
            | Stack::Arg(Cons(value, rest)) => StackProtocol::Argument(
                Box::new(self.value(*value, context)?.protocol),
                Box::new(self.stack(*rest, context)?),
            ),
            | Stack::Tag(Cons(_, rest)) => {
                self.stack(*rest, context)?;
                StackProtocol::Unknown
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

    fn compu(&self, mut id: CompuId, mut context: Context) -> Result<(), ProtocolError> {
        loop {
            match &self.arena.compus[&id] {
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
                            Self::check_stack(id, &expected, &stack)?
                        }
                        | EntryProtocol::Continuation(expected) => {
                            Self::check_value(argument.word().1, &expected, &value.protocol)?
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
                    Self::check_stack(
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
                | Computation::CoCase(SCoMatch { scrut, arms }) => {
                    self.stack(*scrut, &context)?;
                    context.stack = StackProtocol::Unknown;
                    for CoMatcher { tail, .. } in arms {
                        self.compu(*tail, context.clone())?;
                    }
                    return Ok(());
                }
                | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                    let value = self.value(*package, &context)?.protocol;
                    Self::check_value(*package, &ValueProtocol::Thunk(Box::default()), &value)?;
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
                    Self::check_stack(id, &StackProtocol::Continuation(Box::default()), &stack)?;
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
