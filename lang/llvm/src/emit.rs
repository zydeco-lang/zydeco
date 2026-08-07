#![allow(dead_code)]

use super::syntax::*;
use derive_more::{AsMut, AsRef, Display};
use std::collections::HashSet;
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike},
    syntax::{self as sa, Atom, Instruction, Intrinsic, ProgId, Program, Symbol, Terminator},
};
use zydeco_stackir::StackirArena;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;

/// Emit trait - similar to amd64's Emit trait
pub trait Emit<'a> {
    type Env;
    fn emit(&self, env: Self::Env, em: &mut Emitter<'a>);
}

struct LlvmStringLiteral<'a>(&'a Utf8String);

impl<'a> LlvmStringLiteral<'a> {
    fn new(string: &'a Utf8String) -> Self {
        Self(string)
    }

    fn encoded_len(&self) -> usize {
        self.0.byte_len() + 1
    }

    fn escaped_bytes(&self) -> String {
        self.0
            .as_bytes()
            .iter()
            .copied()
            .chain(std::iter::once(0))
            .map(|byte| format!("\\{byte:02X}"))
            .collect::<Vec<_>>()
            .concat()
    }
}

/// LLVM IR module representation
#[derive(Clone, Default, Display)]
#[display("{ir}")]
pub struct LlvmModule {
    pub ir: String,
    declarations: Vec<String>,
    definitions: Vec<String>,
}

impl LlvmModule {
    fn new() -> Self {
        let mut m = Self::default();
        m.ir.push_str("; ModuleID = 'zydeco'\n");
        m.ir.push_str("source_filename = \"zydeco\"\n");
        m.ir.push_str("target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"\n");
        m.ir.push_str("target triple = \"x86_64-unknown-linux-gnu\"\n\n");
        m
    }

    fn add_declaration(&mut self, decl: String) {
        self.declarations.push(decl);
    }

    fn add_definition(&mut self, def: String) {
        self.definitions.push(def);
    }

    fn finalize(&mut self) {
        for decl in &self.declarations {
            self.ir.push_str(decl);
            self.ir.push('\n');
        }
        if !self.declarations.is_empty() {
            self.ir.push('\n');
        }
        for def in &self.definitions {
            self.ir.push_str(def);
            self.ir.push('\n');
        }
    }
}

/// Value stack for tracking values on the stack
#[derive(Clone, Default)]
struct ValueStack {
    values: Vec<String>,
}

impl ValueStack {
    fn push(&mut self, value: String) {
        self.values.push(value);
    }

    fn pop(&mut self) -> String {
        self.values.pop().expect("stack underflow")
    }
}

/// Entry in the control stack
#[derive(Clone, Default)]
struct StackEntry {
    locals: Vec<String>,
    value_stack: ValueStack,
}

#[derive(AsRef, AsMut)]
pub struct Emitter<'e> {
    spans: &'e SpanArena,
    scoped: &'e ScopedArena,
    statics: &'e StaticsArena,
    stackir: &'e StackirArena,
    assembly: &'e AssemblyArena,

    target_triple: TargetTriple,
    #[as_ref]
    #[as_mut]
    module: LlvmModule,

    visited: HashSet<ProgId>,
    current_prog: Option<ProgId>,
    current_function: Option<String>,
    stack: StackEntry,
    local_counter: usize,
    block_counter: usize,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        stackir: &'e StackirArena, assembly: &'e AssemblyArena, target_triple: TargetTriple,
    ) -> Self {
        Self {
            spans,
            scoped,
            statics,
            stackir,
            assembly,
            target_triple,
            module: LlvmModule::new(),
            visited: HashSet::new(),
            current_prog: None,
            current_function: None,
            stack: StackEntry::default(),
            local_counter: 0,
            block_counter: 0,
        }
    }

    fn new_local(&mut self) -> String {
        let name = format!("%local{}", self.local_counter);
        self.local_counter += 1;
        name
    }

    fn new_block(&mut self) -> String {
        let name = format!("%block{}", self.block_counter);
        self.block_counter += 1;
        name
    }

    fn alloc_local(&mut self) -> String {
        let local = self.new_local();
        self.stack.locals.push(local.clone());
        local
    }

    fn emit_push(&mut self, value: String) {
        self.stack.value_stack.push(value);
    }

    fn emit_pop(&mut self) -> String {
        self.stack.value_stack.pop()
    }

    fn get_or_create_entry_function(&mut self) -> String {
        "entry".to_string()
    }

    fn add_declaration(&mut self, decl: String) {
        self.module.add_declaration(decl);
    }

    fn add_definition(&mut self, def: String) {
        self.module.add_definition(def);
    }

    pub fn run(mut self) -> Result<LlvmModule, std::convert::Infallible> {
        // Declare extern runtime functions
        self.add_declaration("declare void @zydeco_abort()".to_string());
        self.add_declaration("declare i64 @zydeco_alloc(i64)".to_string());

        // Declare user externs
        for sa::Extern { name, arity, .. } in self.assembly.externs.iter() {
            let label = format!("zydeco_{}", name);
            let args = (1..=*arity).map(|_| "i64").collect::<Vec<_>>().join(", ");
            let decl = format!("declare i64 @{}({})", label, args);
            self.add_declaration(decl);
        }

        // Create entry function
        let entry_name = self.get_or_create_entry_function();
        let entry_prog_id = self.assembly.entry.iter().next().unwrap().0;
        let context_size = self.assembly.contexts[entry_prog_id].iter().len();

        // Build entry function
        let mut entry_ir = format!("define i64 @{}(i64 %env) {{\n", entry_name);
        entry_ir.push_str("entry:\n");

        // Allocate local slots
        for _ in 0..context_size.max(1) {
            let local = self.alloc_local();
            entry_ir.push_str(&format!("  {} = alloca i64, align 8\n", local));
        }

        // Store env to first slot
        entry_ir.push_str(&format!("  store i64 %env, i64* {}, align 8\n", self.stack.locals[0]));

        // Emit entry point
        self.current_function = Some(entry_name.clone());
        entry_prog_id.emit((), &mut self);

        // Simple ret
        entry_ir.push_str("  ret i64 0\n");
        entry_ir.push('}');

        self.add_definition(entry_ir);

        // Create named block functions
        for (prog_id, _) in &self.assembly.programs {
            if let Some(label) = self.assembly.prog_label(prog_id) {
                let context_size = self.assembly.contexts[prog_id].iter().len();

                let mut block_ir = format!("define internal i64 @{}(i64 %env) {{\n", label);
                block_ir.push_str(&format!("{}:\n", label));

                // Allocate local slots
                self.stack = StackEntry::default();
                for _ in 0..context_size.max(1) {
                    let local = self.alloc_local();
                    block_ir.push_str(&format!("  {} = alloca i64, align 8\n", local));
                }

                // Store env
                block_ir.push_str(&format!(
                    "  store i64 %env, i64* {}, align 8\n",
                    self.stack.locals[0]
                ));

                self.current_function = Some(label.clone());
                self.current_prog = Some(*prog_id);

                prog_id.emit((), &mut self);

                block_ir.push_str("  ret i64 0\n");
                block_ir.push('}');

                self.add_definition(block_ir);
            }
        }

        self.module.finalize();
        Ok(self.module)
    }
}

/// Emit implementation for ProgId
impl<'a> Emit<'a> for ProgId {
    type Env = ();
    fn emit(&self, (): Self::Env, em: &mut Emitter) {
        if em.visited.contains(self) {
            return;
        }
        em.visited.insert(*self);

        match &em.assembly.programs[self] {
            | Program::Terminator(terminator) => terminator.emit(*self, em),
            | Program::Instruction(instr, next) => {
                instr.emit(*self, em);
                next.emit((), em);
            }
        }
    }
}

/// Emit implementation for Terminator
impl<'a> Emit<'a> for Terminator {
    type Env = ProgId;
    fn emit(&self, _id: Self::Env, em: &mut Emitter) {
        match self {
            | Terminator::Jump(sa::Jump(target)) => {
                if em.assembly.prog_label(target).is_some() {
                    // Direct jump to labeled block
                } else {
                    target.emit((), em);
                }
            }
            | Terminator::PopJump(sa::PopJump) => {
                let _addr = em.emit_pop();
            }
            | Terminator::LeapJump(sa::LeapJump) => {
                let _value = em.emit_pop();
                let _addr = em.emit_pop();
            }
            | Terminator::PopBranch(sa::PopBranch(arms)) => {
                let _tag = em.emit_pop();
                let _ = arms;
            }
            | Terminator::Extern(sa::Extern { name, arity, .. }) => {
                let _func_name = format!("zydeco_{}", name);
                let mut args = Vec::new();
                for _ in 0..*arity {
                    args.push(em.emit_pop());
                }
                args.reverse();
                let _args_str = args.join(", ");
            }
            | Terminator::Abort(sa::Abort) => {}
        }
    }
}

/// Emit implementation for Instruction
impl<'a> Emit<'a> for Instruction {
    type Env = ProgId;
    fn emit(&self, _id: Self::Env, em: &mut Emitter) {
        match self {
            | Instruction::PackProduct(sa::Pack(layout)) => {
                let ptr = em.new_local();
                let _call_ir =
                    format!("  {} = call i64 @zydeco_alloc(i64 {})\n", ptr, layout.arity);
                let values = (0..layout.elements).map(|_| em.emit_pop()).collect::<Vec<String>>();
                let _ = values;
                em.emit_push(ptr);
            }
            | Instruction::UnpackProduct(sa::Unpack(layout)) => {
                let _ptr = em.emit_pop();
                let values = (0..layout.elements).map(|_| em.new_local()).collect::<Vec<String>>();
                for value in values.into_iter().rev() {
                    em.emit_push(value);
                }
            }
            | Instruction::PushContext(sa::Push(sa::ContextMarker)) => {
                em.emit_push("%env".to_string());
            }
            | Instruction::PopContext(sa::Pop(sa::ContextMarker)) => {
                let _env = em.emit_pop();
            }
            | Instruction::AllocContext(sa::Alloc(sa::ContextMarker)) => {}
            | Instruction::PushArg(sa::Push(atom)) => {
                atom.emit(_id, em);
            }
            | Instruction::PopArg(sa::Pop(var_id)) => {
                let value = em.emit_pop();
                let idx = em.assembly.contexts[&_id]
                    .iter()
                    .position(|var| var == var_id)
                    .expect("variable not found");
                let _local = em.stack.locals.get(idx).expect("local not found");
                let _ = value;
            }
            | Instruction::PushTag(sa::Push(tag)) => {
                let tag_val = format!("{}", tag.idx);
                em.emit_push(tag_val);
            }
            | Instruction::Intrinsic(intrinsic) => {
                intrinsic.emit((), em);
            }
            | Instruction::Swap(sa::Swap) => {
                let a = em.emit_pop();
                let b = em.emit_pop();
                em.emit_push(a);
                em.emit_push(b);
            }
            | Instruction::Clear(_) => {
                while !em.stack.value_stack.values.is_empty() {
                    em.emit_pop();
                }
            }
        }
    }
}

/// Emit implementation for Atom
impl<'a> Emit<'a> for Atom {
    type Env = ProgId;
    fn emit(&self, id: Self::Env, em: &mut Emitter) {
        match self {
            | Atom::Var(var_id) => {
                let idx = em.assembly.contexts[&id]
                    .iter()
                    .position(|var| var == var_id)
                    .expect("variable not found");
                let _local = em.stack.locals.get(idx).expect("local not found");
                let load = em.new_local();
                let _ = load;
                em.emit_push(load);
            }
            | Atom::Sym(sym_id) => {
                let symbol = &em.assembly.symbols[sym_id];
                match symbol.inner.clone() {
                    | Symbol::Prog(prog_id) => {
                        if let Some(label) = em.assembly.prog_label(&prog_id) {
                            em.emit_push(format!("@{}", label));
                        } else {
                            em.emit_push("%undef".to_string());
                        }
                    }
                    | Symbol::Undefined(sa::Undefined) => {
                        unreachable!("undefined symbol should never be emitted")
                    }
                    | Symbol::StringLiteral(s) => {
                        let literal = LlvmStringLiteral::new(&s);
                        let global_name = format!("@.str{}", sym_id.concise_inner());
                        let global_ir = format!(
                            "{} = private constant [{} x i8] c\"{}\"",
                            global_name,
                            literal.encoded_len(),
                            literal.escaped_bytes()
                        );
                        em.add_declaration(global_ir);
                        em.emit_push(global_name);
                    }
                }
            }
            | Atom::Imm(imm) => match imm.clone() {
                | sa::Imm::Triv(Triv) => {
                    em.emit_push("0".to_string());
                }
                | sa::Imm::Int(i) => {
                    em.emit_push(format!("{}", i));
                }
                | sa::Imm::Float(value) => {
                    em.emit_push(format!("{}", value.to_bits()));
                }
                | sa::Imm::Char(c) => {
                    em.emit_push(format!("{}", c as u64));
                }
            },
        }
    }
}

/// Emit implementation for Intrinsic
impl<'a> Emit<'a> for Intrinsic {
    type Env = ();
    fn emit(&self, (): Self::Env, em: &mut Emitter) {
        let Intrinsic { name, arity } = self;
        match (*name, arity) {
            | (_, 2) => {
                let rhs = em.emit_pop();
                let lhs = em.emit_pop();

                let _result = em.new_local();
                let _op = match *name {
                    | "add" => "add",
                    | "sub" => "sub",
                    | "mul" => "mul",
                    | "and" => "and",
                    | "or" => "or",
                    | "xor" => "xor",
                    | "int_eq" => "icmp eq",
                    | "int_lt" => "icmp slt",
                    | "int_gt" => "icmp sgt",
                    | _ => "add",
                };

                let _ = (lhs, rhs);
                em.emit_push(_result);
            }
            | _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LlvmStringLiteral;
    use zydeco_syntax::Utf8String;

    #[test]
    fn string_literals_escape_encoded_bytes() {
        let string = Utf8String::from("é\"\\");
        let literal = LlvmStringLiteral::new(&string);

        assert_eq!(literal.encoded_len(), 5);
        assert_eq!(literal.escaped_bytes(), "\\C3\\A9\\22\\5C\\00");
    }
}
