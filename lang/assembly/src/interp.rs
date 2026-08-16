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
        let AssemblyProgram { arena, root, .. } = program;
        Self { arena, root, runtime: Runtime::default() }
    }
}

pub enum Output {
    Exit,
    Panic,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Atom(Atom),
    /// A pointer to a value in the heap.
    Pointer(usize),
    Tag(Tag),
}

/// Default trigger for the toy collector, in heap words.
const DEFAULT_GC_THRESHOLD_WORDS: usize = 4096;

/// One live product allocation in the toy heap.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct HeapObject {
    start: usize,
    size: usize,
    marked: bool,
}

/// A run of dead words available for reuse.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct FreeRange {
    start: usize,
    size: usize,
}

pub struct Runtime {
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
    pub context: im::HashMap<VarId, Value>,
    /// Live object extents, sorted by `start`.
    objects: Vec<HeapObject>,
    /// Reusable runs of dead words, sorted by `start` and coalesced.
    free: Vec<FreeRange>,
    /// Words appended since the previous collection.
    growth_since_gc: usize,
    /// Growth that triggers a collection before appending further words.
    gc_threshold_words: usize,
    collections: usize,
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            heap: Vec::new(),
            context: im::HashMap::new(),
            objects: Vec::new(),
            free: Vec::new(),
            growth_since_gc: 0,
            gc_threshold_words: DEFAULT_GC_THRESHOLD_WORDS,
            collections: 0,
        }
    }
}

impl Runtime {
    /// Runtime for tests that want to observe collections without allocating
    /// thousands of words.
    pub fn with_gc_threshold(words: usize) -> Self {
        Self { gc_threshold_words: words, ..Self::default() }
    }

    /// Allocate `size` words for one product.
    ///
    /// Dead words are reused first. When growth since the previous collection
    /// reaches the threshold, a mark-sweep collection runs before the heap is
    /// allowed to grow. The words are uninitialized; `PackProduct` fills every
    /// word before the returned pointer is pushed onto the stack.
    pub fn alloc_product(&mut self, size: usize) -> usize {
        assert!(size > 0, "product arity must be positive");
        if let Some(start) = self.take_free(size) {
            return start;
        }
        if self.growth_since_gc >= self.gc_threshold_words {
            self.collect();
            if let Some(start) = self.take_free(size) {
                return start;
            }
        }
        let start = self.heap.len();
        self.heap.resize(start + size, Value::Tag(Tag { idx: usize::MAX, name: None }));
        self.insert_object(HeapObject { start, size, marked: false });
        self.growth_since_gc += size;
        start
    }

    /// Mark every product reachable from the control stack and context, then
    /// sweep unmarked objects into the free list.
    pub fn collect(&mut self) {
        self.collections += 1;
        let mut worklist: Vec<usize> = self
            .stack
            .iter()
            .chain(self.context.values())
            .filter_map(|value| match value {
                | Value::Pointer(pointer) => Some(*pointer),
                | Value::Atom(_) | Value::Tag(_) => None,
            })
            .collect();
        while let Some(pointer) = worklist.pop() {
            let Some(index) = self.object_index(pointer) else { continue };
            if self.objects[index].marked {
                continue;
            }
            let object = self.objects[index];
            self.objects[index].marked = true;
            let mut children = Vec::new();
            for word in &self.heap[object.start..object.start + object.size] {
                if let Value::Pointer(child) = *word {
                    children.push(child);
                }
            }
            worklist.extend(children);
        }

        let mut kept = Vec::with_capacity(self.objects.len());
        let mut freed: Vec<FreeRange> = std::mem::take(&mut self.free);
        for object in self.objects.drain(..) {
            if object.marked {
                kept.push(HeapObject { start: object.start, size: object.size, marked: false });
            } else {
                freed.push(FreeRange { start: object.start, size: object.size });
            }
        }
        self.objects = kept;
        self.coalesce_free(freed);
        self.growth_since_gc = 0;
    }

    /// Number of currently live objects, for tests and diagnostics.
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// Number of collections performed, for tests and diagnostics.
    pub fn collection_count(&self) -> usize {
        self.collections
    }

    /// Number of reusable dead words, for tests and diagnostics.
    pub fn free_words(&self) -> usize {
        self.free.iter().map(|range| range.size).sum()
    }

    fn take_free(&mut self, size: usize) -> Option<usize> {
        let index = self.free.iter().position(|range| range.size >= size)?;
        let range = self.free[index];
        let start = range.start;
        if range.size == size {
            self.free.remove(index);
        } else {
            self.free[index] = FreeRange { start: range.start + size, size: range.size - size };
        }
        self.insert_object(HeapObject { start, size, marked: false });
        Some(start)
    }

    fn insert_object(&mut self, object: HeapObject) {
        let index = self.objects.partition_point(|candidate| candidate.start < object.start);
        self.objects.insert(index, object);
    }

    fn object_index(&self, pointer: usize) -> Option<usize> {
        let index = self.objects.partition_point(|object| object.start <= pointer);
        let index = index.checked_sub(1)?;
        let object = self.objects[index];
        (pointer < object.start + object.size).then_some(index)
    }

    fn coalesce_free(&mut self, mut ranges: Vec<FreeRange>) {
        ranges.sort_unstable_by_key(|range| range.start);
        let mut merged: Vec<FreeRange> = Vec::with_capacity(ranges.len());
        for range in ranges {
            match merged.last_mut() {
                | Some(previous) if previous.start + previous.size == range.start => {
                    previous.size += range.size;
                }
                | _ => merged.push(range),
            }
        }
        self.free = merged;
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
                // Allocate before popping fields: the control stack is a GC root
                // set, so a collection triggered here cannot drop the values that
                // are about to fill the new object.
                let pointer = interp.runtime.alloc_product(layout.arity);
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
                        let tail = pointer + index;
                        interp.runtime.heap[tail..tail + suffix_arity].clone_from_slice(&suffix);
                    } else {
                        interp.runtime.heap[pointer + index] = value;
                    }
                }
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
            | Instruction::Clear(context) => {
                for var in context {
                    interp.runtime.context.remove(&var);
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn integer(word: i64) -> Value {
        Value::Atom(Atom::Imm(Imm::Integer(word.into())))
    }

    #[test]
    fn collection_keeps_objects_reachable_from_the_control_stack() {
        let mut runtime = Runtime::with_gc_threshold(1024);
        let live = runtime.alloc_product(3);
        runtime.heap[live] = integer(1);
        runtime.heap[live + 1] = integer(2);
        runtime.heap[live + 2] = integer(3);
        let dead = runtime.alloc_product(2);
        runtime.heap[dead] = integer(4);

        runtime.stack.push(Value::Pointer(live));
        runtime.collect();

        assert_eq!(runtime.object_count(), 1);
        assert_eq!(runtime.free_words(), 2);
        assert_eq!(runtime.heap[live], integer(1));
    }

    #[test]
    fn collection_keeps_children_reachable_through_pointer_fields() {
        let mut runtime = Runtime::with_gc_threshold(1024);
        let child = runtime.alloc_product(1);
        runtime.heap[child] = integer(7);
        let parent = runtime.alloc_product(2);
        runtime.heap[parent] = Value::Pointer(child);
        runtime.heap[parent + 1] = integer(0);

        runtime.stack.push(Value::Pointer(parent));
        runtime.collect();

        assert_eq!(runtime.object_count(), 2);
        assert_eq!(runtime.heap[child], integer(7));
    }

    #[test]
    fn collection_resolves_interior_pointer_roots_to_the_containing_object() {
        let mut runtime = Runtime::with_gc_threshold(1024);
        let live = runtime.alloc_product(3);
        runtime.heap[live] = integer(1);
        let dead = runtime.alloc_product(1);
        runtime.heap[dead] = integer(2);

        runtime.stack.push(Value::Pointer(live + 1));
        runtime.collect();

        assert_eq!(runtime.object_count(), 1);
        assert_eq!(runtime.heap[live], integer(1));
    }

    #[test]
    fn growth_threshold_triggers_a_collection_and_reuses_dead_words() {
        let mut runtime = Runtime::with_gc_threshold(4);
        let dead = runtime.alloc_product(4);
        runtime.heap[dead] = integer(0);

        let reused = runtime.alloc_product(1);

        assert_eq!(runtime.collection_count(), 1);
        assert_eq!(reused, dead);
        runtime.heap[reused] = integer(1);
        assert_eq!(runtime.heap[dead], integer(1));
    }

    #[test]
    fn free_list_splits_ranges_and_coalesces_neighbors() {
        let mut runtime = Runtime::with_gc_threshold(1024);
        let first = runtime.alloc_product(2);
        let second = runtime.alloc_product(3);
        let _ = (first, second);
        runtime.collect();

        assert_eq!(runtime.free_words(), 5);

        let head = runtime.alloc_product(2);
        assert_eq!(head, 0);
        assert_eq!(runtime.free_words(), 3);

        let tail = runtime.alloc_product(3);
        assert_eq!(tail, 2);
        assert_eq!(runtime.free_words(), 0);
    }
}
