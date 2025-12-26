use super::syntax::*;
use thiserror::Error;
use zydeco_utils::pass::CompilerPass;

pub struct Interpreter {
    pub arena: AssemblyArena,
    pub runtime: Runtime,
}

impl Interpreter {
    pub fn new(arena: AssemblyArena) -> Self {
        Self { arena, runtime: Runtime::new() }
    }
}
impl AsRef<AssemblyArena> for Interpreter {
    fn as_ref(&self) -> &AssemblyArena {
        &self.arena
    }
}
impl AsMut<AssemblyArena> for Interpreter {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        &mut self.arena
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

pub struct Runtime {
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
    pub context: im::HashMap<VarId, Value>,
}

impl Runtime {
    pub fn new() -> Self {
        Self { stack: Vec::new(), heap: Vec::new(), context: im::HashMap::new() }
    }
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
        let entry = self.arena.entry.iter().map(|(prog, _)| *prog).next().expect("no entry point");
        entry.eval(&mut self)
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
            | Program::Instruction(instr, next) => {
                let () = instr.eval(interp)?;
                next.eval(interp)
            }
            | Program::Jump(Jump(prog)) => prog.eval(interp),
            | Program::EqJump(EqJump(prog)) => {
                let a = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Tag(a) = a else {
                    Err(Error::TypeError(format!("expected tag, got {:?}", a)))?
                };
                let b = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Tag(b) = b else {
                    Err(Error::TypeError(format!("expected tag, got {:?}", b)))?
                };
                if a.idx == b.idx { prog.eval(interp) } else { todo!() }
            }
            | Program::PopJump(PopJump) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Atom(Atom::Sym(sym)) = value else {
                    Err(Error::TypeError(format!("expected symbol, got {:?}", value)))?
                };
                let symbol = interp.arena.symbols[&sym].clone();
                let SymbolInner::Prog(prog) = symbol.inner else {
                    Err(Error::TypeError(format!("expected program, got {:?}", symbol.inner)))?
                };
                prog.eval(interp)
            }
            | Program::LeapJump(LeapJump) => {
                let kept = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let address = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                interp.runtime.stack.push(kept);
                let Value::Atom(Atom::Sym(sym)) = address else {
                    Err(Error::TypeError(format!("expected symbol, got {:?}", address)))?
                };
                let symbol = interp.arena.symbols[&sym].clone();
                let SymbolInner::Prog(prog) = symbol.inner else {
                    Err(Error::TypeError(format!("expected program, got {:?}", symbol.inner)))?
                };
                prog.eval(interp)
            }
            | Program::PopBranch(PopBranch(arms)) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Tag(tag) = value else {
                    Err(Error::TypeError(format!("expected tag, got {:?}", value)))?
                };
                let arm = arms.iter().find(|(t, _)| t.idx == tag.idx).unwrap();
                arm.1.eval(interp)
            }
            | Program::Panic(Panic) => todo!(),
        }
    }
}

impl Eval for Instruction {
    type Output = ();
    fn eval(self, interp: &mut Interpreter) -> Result<Self::Output, Error> {
        match self {
            | Instruction::PackProduct(Pack(ProductMarker)) => {
                let pointer = interp.runtime.heap.len();
                let a = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let b = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                interp.runtime.heap.push(a);
                interp.runtime.heap.push(b);
                interp.runtime.stack.push(Value::Pointer(pointer));
                Ok(())
            }
            | Instruction::UnpackProduct(Unpack(ProductMarker)) => {
                let value = interp.runtime.stack.pop().ok_or(Error::StackUnderflow)?;
                let Value::Pointer(pointer) = value else {
                    Err(Error::TypeError(format!("expected pointer, got {:?}", value)))?
                };
                let b = interp.runtime.heap[pointer + 1].clone();
                let a = interp.runtime.heap[pointer].clone();
                interp.runtime.stack.push(b);
                interp.runtime.stack.push(a);
                Ok(())
            }
            | Instruction::PushContext(Push(ContextMarker)) => todo!(),
            | Instruction::PopContext(Pop(ContextMarker)) => todo!(),
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
