//! Code provenance and administrative word agreement at the SPSLow boundary.
//!
//! Source typing supplies the logical value and stack classifiers. This check retains
//! local code/environment associations and known outer product arities; unknown value
//! shapes do not justify a different entry kind or a different physical transport.

use super::syntax::*;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntrySite {
    Closure(ValueId),
    Continuation(StackId),
    Jump(CompuId),
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum EntryContractError {
    #[error("value {code:?} has no code-entry evidence")]
    UnknownCode { code: ValueId },
    #[error("{site:?} requires a {expected:?} entry, found {found:?}")]
    KindMismatch { site: EntrySite, expected: EntryKind, found: EntryKind },
    #[error("{site:?} does not use the environment from closure opening {opening:?}")]
    ClosureEnvironment { site: EntrySite, opening: CompuId },
    #[error("{site:?} does not use the residual stack from continuation opening {opening:?}")]
    ContinuationResidual { site: EntrySite, opening: CompuId },
    #[error("{site:?} has no captured environment word in stack {stack:?}")]
    MissingEnvironment { site: EntrySite, stack: StackId },
    #[error(
        "{site:?} supplies a known environment with {found} fields to a {expected}-field entry"
    )]
    EnvironmentLayout { site: EntrySite, expected: usize, found: usize },
}

#[derive(Clone, Copy)]
enum CodeEvidence {
    Block(EntryParameters),
    Opening { id: CompuId, kind: EntryKind },
}

impl CodeEvidence {
    fn kind(self) -> EntryKind {
        match self {
            | Self::Block(entry) => entry.kind(),
            | Self::Opening { kind, .. } => kind,
        }
    }
}

#[derive(Clone, Default)]
enum ValueEvidence {
    #[default]
    Unknown,
    Code(CodeEvidence),
    Environment(CompuId),
    Unit,
    Product {
        arity: usize,
        fields: Vec<ValueEvidence>,
    },
}

#[derive(Clone, Default)]
enum StackEvidence {
    #[default]
    Unknown,
    Argument(ValueEvidence, Box<StackEvidence>),
    Tag(Box<StackEvidence>),
    Opened(CompuId),
}

#[derive(Clone, Default)]
struct Context {
    values: HashMap<DefId, ValueEvidence>,
    stack: StackEvidence,
}

pub(super) struct EntryValidator<'a> {
    arena: &'a SpsLowInnerArena,
}

impl<'a> EntryValidator<'a> {
    pub(super) fn validate(
        arena: &'a SpsLowInnerArena, root: CompuId,
    ) -> Result<(), EntryContractError> {
        Self { arena }.compu(root, Context::default())
    }

    fn code(
        &self, value: ValueId, evidence: ValueEvidence, expected: EntryKind, site: EntrySite,
    ) -> Result<CodeEvidence, EntryContractError> {
        let ValueEvidence::Code(code) = evidence else {
            return Err(EntryContractError::UnknownCode { code: value });
        };
        if code.kind() != expected {
            return Err(EntryContractError::KindMismatch { site, expected, found: code.kind() });
        }
        Ok(code)
    }

    fn environment(
        &self, code: CodeEvidence, value: &ValueEvidence, site: EntrySite,
    ) -> Result<(), EntryContractError> {
        match code {
            | CodeEvidence::Opening { id, .. } => {
                if !matches!(value, ValueEvidence::Environment(found) if *found == id) {
                    return Err(EntryContractError::ClosureEnvironment { site, opening: id });
                }
            }
            | CodeEvidence::Block(entry) => {
                let expected = match &self.arena.vpats[&entry.environment()] {
                    | ValuePattern::Triv(_) => Some(0),
                    | ValuePattern::VCons(product) => Some(product.layout.arity),
                    | _ => None,
                };
                let found = match value {
                    | ValueEvidence::Unit => Some(0),
                    | ValueEvidence::Product { arity, .. } => Some(*arity),
                    | _ => None,
                };
                if let (Some(expected), Some(found)) = (expected, found)
                    && expected != found
                {
                    return Err(EntryContractError::EnvironmentLayout { site, expected, found });
                }
            }
        }
        Ok(())
    }

    fn bind(&self, pattern: VPatId, value: ValueEvidence, context: &mut Context) {
        match &self.arena.vpats[&pattern] {
            | ValuePattern::Var(def) => {
                context.values.insert(*def, value);
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.bind(*pattern, value.clone(), context);
                }
            }
            | ValuePattern::Ctor(Ctor(_, pattern)) => {
                self.bind(*pattern, ValueEvidence::Unknown, context)
            }
            | ValuePattern::VCons(product) => {
                let fields = match value {
                    | ValueEvidence::Product { arity, fields }
                        if arity == product.layout.arity && fields.len() == arity =>
                    {
                        Some(fields)
                    }
                    | _ => None,
                };
                for (index, pattern) in product.items.iter().enumerate() {
                    let evidence = fields.as_ref().map_or(ValueEvidence::Unknown, |fields| {
                        if index + 1 == product.items.len() && product.items.len() < fields.len() {
                            ValueEvidence::Product {
                                arity: fields.len() - index,
                                fields: fields[index..].to_vec(),
                            }
                        } else {
                            fields[index].clone()
                        }
                    });
                    self.bind(*pattern, evidence, context);
                }
            }
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
        }
    }

    fn continuation_residual(
        &self, code: CodeEvidence, stack: StackId, residual: StackEvidence, site: EntrySite,
    ) -> Result<(), EntryContractError> {
        match (code, residual) {
            | (CodeEvidence::Opening { id, .. }, StackEvidence::Opened(found)) if id == found => {
                Ok(())
            }
            | (CodeEvidence::Opening { id, .. }, _) => {
                Err(EntryContractError::ContinuationResidual { site, opening: id })
            }
            | (CodeEvidence::Block(_), StackEvidence::Argument(env, _)) => {
                self.environment(code, &env, site)
            }
            | (CodeEvidence::Block(_), _) => {
                Err(EntryContractError::MissingEnvironment { site, stack })
            }
        }
    }

    fn value(&self, id: ValueId, context: &Context) -> Result<ValueEvidence, EntryContractError> {
        Ok(match &self.arena.values[&id] {
            | Value::Var(def) => context.values.get(def).cloned().unwrap_or_default(),
            | Value::Block(Block { label, entry, body }) => {
                let evidence = ValueEvidence::Code(CodeEvidence::Block(*entry));
                let mut local = Context::default();
                local.values.insert(*label, evidence.clone());
                for (_, pattern) in entry.words() {
                    self.bind(pattern, ValueEvidence::Unknown, &mut local);
                }
                self.compu(*body, local)?;
                evidence
            }
            | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                let env = self.value(*environment, context)?;
                let evidence = self.value(*code, context)?;
                let site = EntrySite::Closure(id);
                let code = self.code(*code, evidence, EntryKind::Closure, site)?;
                self.environment(code, &env, site)?;
                ValueEvidence::Unknown
            }
            | Value::VCons(product) => ValueEvidence::Product {
                arity: product.layout.arity,
                fields: product
                    .items
                    .iter()
                    .map(|value| self.value(*value, context))
                    .collect::<Result<_, _>>()?,
            },
            | Value::Ctor(Ctor(_, value)) => {
                self.value(*value, context)?;
                ValueEvidence::Unknown
            }
            | Value::Primitive(Primitive { operands, .. }) => {
                for value in operands {
                    self.value(*value, context)?;
                }
                ValueEvidence::Unknown
            }
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                self.value(*base, context)?;
                self.value(*displacement, context)?;
                ValueEvidence::Unknown
            }
            | Value::Triv(_) => ValueEvidence::Unit,
            | Value::Hole(_) | Value::Literal(_) => ValueEvidence::Unknown,
        })
    }

    fn stack(&self, id: StackId, context: &Context) -> Result<StackEvidence, EntryContractError> {
        Ok(match &self.arena.stacks[&id] {
            | Stack::Var(_) => context.stack.clone(),
            | Stack::Arg(Cons(value, rest)) => StackEvidence::Argument(
                self.value(*value, context)?,
                Box::new(self.stack(*rest, context)?),
            ),
            | Stack::Tag(Cons(_, rest)) => {
                StackEvidence::Tag(Box::new(self.stack(*rest, context)?))
            }
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                let evidence = self.value(*code, context)?;
                let site = EntrySite::Continuation(id);
                let code = self.code(*code, evidence, EntryKind::Continuation, site)?;
                let rest = self.stack(*residual, context)?;
                self.continuation_residual(code, *residual, rest, site)?;
                StackEvidence::Unknown
            }
        })
    }

    fn compu(&self, mut id: CompuId, mut context: Context) -> Result<(), EntryContractError> {
        loop {
            match &self.arena.compus[&id] {
                | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                    self.value(*address, &context)?;
                    self.bind(*result, ValueEvidence::Unknown, &mut context);
                    id = *next;
                }
                | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                    self.value(*address, &context)?;
                    self.value(*value, &context)?;
                    id = *next;
                }
                | Computation::Hole(SHole(stack))
                | Computation::ExternCall(ExternCall { stack, .. }) => {
                    self.stack(*stack, &context)?;
                    return Ok(());
                }
                | Computation::Jump(Jump { target, argument, stack }) => {
                    let evidence = self.value(*target, &context)?;
                    let site = EntrySite::Jump(id);
                    let code = self.code(*target, evidence, argument.kind(), site)?;
                    let value = self.value(argument.word().1, &context)?;
                    let rest = self.stack(*stack, &context)?;
                    match argument {
                        | EntryArgument::Closure { .. } => self.environment(code, &value, site)?,
                        | EntryArgument::Continuation { .. } => {
                            self.continuation_residual(code, *stack, rest, site)?;
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
                        self.bind(*binder, ValueEvidence::Unknown, &mut branch);
                        self.compu(*tail, branch)?;
                    }
                    return Ok(());
                }
                | Computation::LetStack(LetStack { bindee, tail, .. }) => {
                    context.stack = self.stack(*bindee, &context)?;
                    id = *tail;
                }
                | Computation::LetArg(LetArg { binder: Cons(binder, _), bindee, tail }) => {
                    let (value, rest) = match self.stack(*bindee, &context)? {
                        | StackEvidence::Argument(value, rest) => (value, *rest),
                        | _ => (ValueEvidence::Unknown, StackEvidence::Unknown),
                    };
                    self.bind(*binder, value, &mut context);
                    context.stack = rest;
                    id = *tail;
                }
                | Computation::CoCase(SCoMatch { scrut, arms }) => {
                    context.stack = match self.stack(*scrut, &context)? {
                        | StackEvidence::Tag(rest) => *rest,
                        | _ => StackEvidence::Unknown,
                    };
                    for CoMatcher { tail, .. } in arms {
                        self.compu(*tail, context.clone())?;
                    }
                    return Ok(());
                }
                | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                    self.value(*package, &context)?;
                    self.bind(*environment, ValueEvidence::Environment(id), &mut context);
                    self.bind(
                        *code,
                        ValueEvidence::Code(CodeEvidence::Opening { id, kind: EntryKind::Closure }),
                        &mut context,
                    );
                    id = *body;
                }
                | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                    self.stack(*package, &context)?;
                    self.bind(
                        *code,
                        ValueEvidence::Code(CodeEvidence::Opening {
                            id,
                            kind: EntryKind::Continuation,
                        }),
                        &mut context,
                    );
                    context.stack = StackEvidence::Opened(id);
                    id = *body;
                }
            }
        }
    }
}
