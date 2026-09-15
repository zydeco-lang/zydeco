use super::syntax::*;
use thiserror::Error;
use zydeco_utils::pass::CompilerPass;

/// Execute an assembly program with a fresh runtime for each invocation.
pub struct Interpret;

struct Interpreter {
    arena: AssemblyArena,
    root: ProgId,
    pub runtime: Runtime,
}

impl Interpreter {
    pub fn new(program: AssemblyProgram) -> Self {
        let (arena, root) = program.into_parts();
        Self { arena, root, runtime: Runtime::default() }
    }
}

pub enum Output {
    Exit,
    Panic,
}

#[derive(Clone, Debug)]
pub enum Value {
    Atom(Atom),
    Address(zydeco_machine::memory::Address),
    /// A pointer to a value in the heap.
    Pointer(usize),
    Tag(Tag),
}

#[derive(Default)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
    pub context: rpds::HashTrieMapSync<VarId, Value>,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("pattern match failed")]
    PatternMatch,
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Type error: {0}")]
    TypeError(String),
    #[error(transparent)]
    Primitive(#[from] PrimitiveError),
}

impl CompilerPass<AssemblyProgram> for Interpret {
    type Output = Output;
    type Error = Error;
    fn run(&mut self, program: AssemblyProgram) -> Result<Self::Output, Self::Error> {
        let mut interpreter = Interpreter::new(program);
        interpreter.root.eval(&mut interpreter)
    }
}

trait Eval {
    type Output;
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error>;
}

impl Eval for ProgId {
    type Output = Output;
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error> {
        interp.arena.programs[&self].clone().eval(interp)
    }
}

impl Eval for Program {
    type Output = Output;
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error> {
        match self {
            | Program::Terminator(terminator) => terminator.eval(interp),
            | Program::Instruction(instr, next) => {
                let () = instr.eval(interp)?;
                next.eval(interp)
            }
        }
    }
}

impl Eval for Terminator {
    type Output = Output;
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error> {
        match self {
            | Terminator::Jump(Jump(prog)) => prog.eval(interp),
            | Terminator::PopJump(PopJump) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Atom(Atom::Sym(sym)) = value else {
                    Err(Error::TypeError(format!("expected symbol, got {:?}", value)))?
                };
                let symbol = interp.arena.symbols[&sym].clone();
                let Symbol::Prog(prog) = symbol.inner else {
                    Err(Error::TypeError(format!("expected program, got {:?}", symbol.inner)))?
                };
                prog.eval(interp)
            }
            | Terminator::PopBranch(PopBranch(arms)) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Tag(tag) = value else {
                    Err(Error::TypeError(format!("expected tag, got {:?}", value)))?
                };
                let arm = arms.iter().find(|(t, _)| t.idx == tag.idx).unwrap();
                arm.1.eval(interp)
            }
            | Terminator::Extern(external) => {
                let _ = external;
                todo!()
            }
            | Terminator::Abort(Abort) => Err(Error::PatternMatch),
        }
    }
}

impl Eval for Instruction {
    type Output = ();
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error> {
        match self {
            | Instruction::PackProduct(Pack(layout)) => {
                let pointer = interp.runtime.heap.len();
                for index in 0..layout.elements {
                    let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                    if index + 1 == layout.elements && layout.elements < layout.arity {
                        let Value::Pointer(suffix) = value else {
                            Err(Error::TypeError(format!(
                                "expected product suffix, got {:?}",
                                value
                            )))?
                        };
                        let suffix_arity = layout.arity - index;
                        let suffix = interp
                            .runtime
                            .heap
                            .get(suffix..suffix + suffix_arity)
                            .ok_or_else(|| {
                                Error::TypeError(format!(
                                    "product suffix at {} has fewer than {} fields",
                                    suffix, suffix_arity
                                ))
                            })?
                            .to_vec();
                        interp.runtime.heap.extend(suffix);
                    } else {
                        interp.runtime.heap.push(value);
                    }
                }
                debug_assert_eq!(interp.runtime.heap.len(), pointer + layout.arity);
                interp.runtime.stack.push(Value::Pointer(pointer));
                Ok(())
            }
            | Instruction::UnpackProduct(Unpack(layout)) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Pointer(pointer) = value else {
                    Err(Error::TypeError(format!("expected pointer, got {:?}", value)))?
                };
                if pointer + layout.arity > interp.runtime.heap.len() {
                    Err(Error::TypeError(format!(
                        "product at {} has fewer than {} fields",
                        pointer, layout.arity
                    )))?
                }

                let last = layout.elements - 1;
                if layout.elements < layout.arity {
                    interp.runtime.stack.push(Value::Pointer(pointer + last));
                } else {
                    interp.runtime.stack.push(interp.runtime.heap[pointer + last].clone());
                }
                for index in (0..last).rev() {
                    interp.runtime.stack.push(interp.runtime.heap[pointer + index].clone());
                }
                Ok(())
            }
            | Instruction::AllocContext(Alloc(ContextMarker)) => {
                todo!()
            }
            | Instruction::PushArg(Push(atom)) => {
                interp.runtime.stack.push(Value::Atom(atom));
                Ok(())
            }
            | Instruction::PopArg(Pop(var)) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                interp.runtime.context.insert_mut(var, value);
                Ok(())
            }
            | Instruction::PushTag(Push(tag)) => {
                interp.runtime.stack.push(Value::Tag(tag));
                Ok(())
            }
            | Instruction::AddrOffset => {
                let base = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let displacement = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let (Value::Address(base), Value::Atom(Atom::Imm(Imm::Integer(displacement)))) =
                    (base, displacement)
                else {
                    return Err(Error::TypeError("addr.offset expects Addr and Int".into()));
                };
                if displacement.integer_type() != Some(IntegerType::Int) {
                    return Err(Error::TypeError("addr.offset expects an Int displacement".into()));
                }
                interp.runtime.stack.push(Value::Address(base.offset(displacement.value() as i64)));
                Ok(())
            }
            | Instruction::Memory(access) => {
                use memory::{AccessKind, MemoryScalar};
                let address = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Address(address) = address else {
                    return Err(Error::TypeError("memory access expects Addr".into()));
                };
                // SAFETY: raw memory source operations require caller-established validity.
                match access.kind {
                    | AccessKind::Load => {
                        let value = if access.scalar == MemoryScalar::Address {
                            Value::Address(unsafe { address.load_address() })
                        } else {
                            let mut carrier = [0; 8];
                            let bytes = access.scalar.bytes() as usize;
                            carrier[..bytes].copy_from_slice(unsafe { address.bytes(bytes) });
                            match access.scalar.literal(u64::from_le_bytes(carrier)).ok_or_else(
                                || {
                                    Error::TypeError(
                                        "integer exceeds the tagged payload range".into(),
                                    )
                                },
                            )? {
                                | Literal::Integer(value) => {
                                    Value::Atom(Atom::Imm(Imm::Integer(value)))
                                }
                                | Literal::Float(value) => {
                                    Value::Atom(Atom::Imm(Imm::Float(value)))
                                }
                                | _ => unreachable!(),
                            }
                        };
                        interp.runtime.stack.push(value);
                    }
                    | AccessKind::Store => {
                        let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                        match value {
                            | Value::Address(value) if access.scalar == MemoryScalar::Address => unsafe {
                                address.store_address(value)
                            },
                            | Value::Atom(Atom::Imm(value)) => {
                                let literal = match value {
                                    | Imm::Integer(value) => Literal::Integer(value),
                                    | Imm::Float(value) => Literal::Float(value),
                                    | _ => {
                                        return Err(Error::TypeError(
                                            "memory store expects a scalar".into(),
                                        ));
                                    }
                                };
                                let bits = access.scalar.bits(&literal).ok_or_else(|| {
                                    Error::TypeError("memory store scalar mismatch".into())
                                })?;
                                unsafe {
                                    address.write(
                                        &bits.to_le_bytes()[..access.scalar.bytes() as usize],
                                    )
                                };
                            }
                            | _ => {
                                return Err(Error::TypeError(
                                    "memory store scalar mismatch".into(),
                                ));
                            }
                        }
                    }
                }
                Ok(())
            }
            | Instruction::Scalar(region) => {
                let mut operand =
                    || match interp.runtime.stack.pop().ok_or(Error::StackUnderflow)? {
                        | Value::Atom(Atom::Imm(Imm::Integer(value))) => {
                            Ok(Literal::Integer(value))
                        }
                        | Value::Atom(Atom::Imm(Imm::Float(value))) => Ok(Literal::Float(value)),
                        | _ => Err(Error::Primitive(PrimitiveError::OperandType)),
                    };
                let operands = (0..region.region().inputs.len())
                    .map(|_| operand())
                    .collect::<Result<Vec<_>, _>>()?;
                let result = region.evaluate(&operands)?;
                let result = match result {
                    | Literal::Integer(value) => Imm::Integer(value),
                    | Literal::Float(value) => Imm::Float(value),
                    | _ => unreachable!("arithmetic returns a scalar"),
                };
                interp.runtime.stack.push(Value::Atom(Atom::Imm(result)));
                Ok(())
            }
            | Instruction::Clear(context) => {
                for var in context {
                    interp.runtime.context.remove_mut(&var);
                }
                Ok(())
            }
            | Instruction::RetainFrame(_) => {
                unreachable!("native frames require the AMD64 backend")
            }
        }
    }
}
