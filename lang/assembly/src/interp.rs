use super::syntax::*;
use derive_more::{AsMut, AsRef};
use thiserror::Error;
use zydeco_utils::pass::CompilerPass;

#[derive(AsRef, AsMut)]
pub struct Interpreter {
    #[as_ref]
    #[as_mut]
    pub arena: AssemblyArena,
    pub root: ProgId,
    pub runtime: Runtime,
}

impl Interpreter {
    pub fn new(program: AssemblyProgram) -> Self {
        let AssemblyProgram { arena, root } = program;
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
    /// A pointer to a value in the heap.
    Pointer(usize),
    Tag(Tag),
}

#[derive(Default)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
    pub context: im::HashMap<VarId, Value>,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Type error: {0}")]
    TypeError(String),
}

impl CompilerPass for Interpreter {
    type Arena = AssemblyArena;
    type Out = Output;
    type Error = Error;
    fn run(mut self) -> Result<Self::Out, Self::Error> {
        self.root.eval(&mut self)
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
            | Terminator::LeapJump(LeapJump) => {
                let kept = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let address = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                interp.runtime.stack.push(kept);
                let Value::Atom(Atom::Sym(sym)) = address else {
                    Err(Error::TypeError(format!("expected symbol, got {:?}", address)))?
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
            | Terminator::Extern(Extern { name, arity, mode }) => {
                let _ = (name, arity, mode);
                todo!()
            }
            | Terminator::Abort(Abort) => todo!(),
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
            | Instruction::PushContext(Push(ContextMarker)) => {
                todo!()
            }
            | Instruction::PopContext(Pop(ContextMarker)) => {
                todo!()
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
                interp.runtime.context.insert(var, value);
                Ok(())
            }
            | Instruction::PushTag(Push(tag)) => {
                interp.runtime.stack.push(Value::Tag(tag));
                Ok(())
            }
            | Instruction::Intrinsic(Intrinsic { name, arity }) => {
                let _ = name;
                let _ = arity;
                todo!()
            }
            | Instruction::Swap(Swap) => {
                let a = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let b = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                interp.runtime.stack.push(b);
                interp.runtime.stack.push(a);
                Ok(())
            }
            | Instruction::Clear(context) => {
                for var in context {
                    interp.runtime.context.remove(&var);
                }
                Ok(())
            }
        }
    }
}
