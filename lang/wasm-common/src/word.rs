//! The instruction sequences over the shared tagged runtime-word representation.
//!
//! The word encoding itself — the immediate boundary, the integer and float
//! classification — lives in [`zydeco_syntax::word`], which every backend and the
//! external boundary pins share. This module adds the WebAssembly function bodies
//! that decode, encode, box, and divide tagged words.

use wasm_encoder::{Function, Instruction as WasmInstruction};

use zydeco_syntax::SpareBox;

pub use zydeco_syntax::word::{EncodedScalar, RuntimeWord, WordError};

use crate::{Limits, WORD_BYTES, WORD_MEMORY, WasmEmitError};

/// A pointer temporary's local index and the word representation it holds.
///
/// Backends whose locals store raw `i32` addresses bind allocations into an `I32` local;
/// backends that keep tagged `i64` words in locals bind into an `I64` local. The emitted
/// sequences differ only in the conversions around the local.
#[derive(Clone, Copy)]
pub enum PointerLocal {
    /// A local holding the raw `i32` address.
    I32(u32),
    /// A local holding the tagged `i64` pointer word.
    I64(u32),
}

impl PointerLocal {
    /// Bind the pointer of a freshly allocated box and expose its store address.
    fn bind_allocation(&self, function: &mut Function) {
        match self {
            | PointerLocal::I32(local) => {
                function.instruction(&WasmInstruction::LocalTee(*local));
            }
            | PointerLocal::I64(local) => {
                function.instruction(&WasmInstruction::I64ExtendI32U);
                function.instruction(&WasmInstruction::LocalTee(*local));
                function.instruction(&WasmInstruction::I32WrapI64);
            }
        }
    }

    /// Push the pointer as a tagged word.
    fn push_word(&self, function: &mut Function) {
        match self {
            | PointerLocal::I32(local) => {
                function.instruction(&WasmInstruction::LocalGet(*local));
                function.instruction(&WasmInstruction::I64ExtendI32U);
            }
            | PointerLocal::I64(local) => {
                function.instruction(&WasmInstruction::LocalGet(*local));
            }
        }
    }
}

/// Shared instruction sequences over a function under construction.
///
/// Each sequence assumes the backend's allocator helper is installed at `alloc_function`
/// and leaves its result on the operand stack unless a local is named.
pub struct WordEmitter<'f> {
    function: &'f mut Function,
    alloc_function: u32,
}

impl<'f> WordEmitter<'f> {
    pub fn new(function: &'f mut Function, alloc_function: u32) -> Self {
        Self { function, alloc_function }
    }

    /// Replace a tagged local with its signed payload, unboxing when tagged as a pointer.
    pub fn decode_signed_local(&mut self, input: u32, output: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64And);
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Result(
            wasm_encoder::ValType::I64,
        )));
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64ShrS);
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalSet(output));
    }

    /// Replace a signed local with its tagged encoding, boxing when outside the
    /// immediate range, and leave the word on the operand stack.
    pub fn encode_signed_local(&mut self, input: u32, pointer: PointerLocal) {
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::SIGNED_MIN));
        self.function.instruction(&WasmInstruction::I64GeS);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::SIGNED_MAX));
        self.function.instruction(&WasmInstruction::I64LeS);
        self.function.instruction(&WasmInstruction::I32And);
        self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Result(
            wasm_encoder::ValType::I64,
        )));
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Shl);
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Or);
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.alloc_function));
        pointer.bind_allocation(self.function);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        pointer.push_word(self.function);
        self.function.instruction(&WasmInstruction::End);
    }

    /// Store `bits` in a fresh one-word box and leave the tagged pointer word on the
    /// operand stack.
    pub fn boxed(&mut self, bits: u64, pointer: PointerLocal) {
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.alloc_function));
        pointer.bind_allocation(self.function);
        self.function.instruction(&WasmInstruction::I64Const(bits as i64));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        pointer.push_word(self.function);
    }

    /// Compute a signed division or remainder of the decoded locals into `result`,
    /// trapping on a zero divisor and wrapping `i64::MIN / -1`.
    pub fn wrapping_division(&mut self, first: u32, second: u32, result: u32, remainder: bool) {
        self.function.instruction(&WasmInstruction::LocalGet(second));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalGet(first));
        self.function.instruction(&WasmInstruction::I64Const(i64::MIN));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::LocalGet(second));
        self.function.instruction(&WasmInstruction::I64Const(-1));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::I32And);
        self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Result(
            wasm_encoder::ValType::I64,
        )));
        self.function.instruction(&WasmInstruction::I64Const(if remainder { 0 } else { i64::MIN }));
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::LocalGet(first));
        self.function.instruction(&WasmInstruction::LocalGet(second));
        self.function.instruction(&if remainder {
            WasmInstruction::I64RemS
        } else {
            WasmInstruction::I64DivS
        });
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalSet(result));
    }

    /// Emit the trailing spare-box argument of a host call that expects one.
    pub fn spare_box(&mut self, spare: Option<SpareBox>) {
        if let Some(spare) = spare {
            match spare {
                | SpareBox::Opaque => {
                    self.function.instruction(&WasmInstruction::I32Const(1));
                    self.function.instruction(&WasmInstruction::Call(self.alloc_function));
                }
                | SpareBox::Unused => {
                    self.function.instruction(&WasmInstruction::I32Const(0));
                }
            }
        }
    }
}

/// Field addressing within boxed products.
pub struct ProductFields;

impl ProductFields {
    /// The byte offset of one field.
    pub fn byte_offset(index: usize) -> Result<u32, WasmEmitError> {
        Limits::u32(index, "product field index")?
            .checked_mul(WORD_BYTES)
            .ok_or(WasmEmitError::Limit { what: "product field offset", value: index })
    }

    /// Checked memory access to one field.
    pub fn word_at(index: usize) -> Result<wasm_encoder::MemArg, WasmEmitError> {
        Ok(wasm_encoder::MemArg { offset: u64::from(Self::byte_offset(index)?), ..WORD_MEMORY })
    }

    /// Memory access to one compile-time-constant field.
    pub fn word_at_const(index: u32) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg { offset: u64::from(index * WORD_BYTES), ..WORD_MEMORY }
    }
}

/// The intrinsic instruction tables shared by the WebAssembly backends.
///
/// The intrinsic names originate in the stack-IR builtin table; both backends must map
/// them to the same wasm operations, so the mapping lives here once.
pub struct Intrinsics;

impl Intrinsics {
    /// The comparison operation of an intrinsic name, if it is one.
    pub fn comparison(name: &str) -> Option<WasmInstruction<'_>> {
        match name {
            | "int_eq" => Some(WasmInstruction::I64Eq),
            | "int_lt" => Some(WasmInstruction::I64LtS),
            | "int_gt" => Some(WasmInstruction::I64GtS),
            | _ => None,
        }
    }

    /// The arithmetic operation of an intrinsic name, if it is one.
    pub fn arithmetic(name: &str) -> Option<WasmInstruction<'_>> {
        match name {
            | "add" => Some(WasmInstruction::I64Add),
            | "sub" => Some(WasmInstruction::I64Sub),
            | "mul" => Some(WasmInstruction::I64Mul),
            | "and" => Some(WasmInstruction::I64And),
            | "or" => Some(WasmInstruction::I64Or),
            | "xor" => Some(WasmInstruction::I64Xor),
            | _ => None,
        }
    }

    /// Whether the intrinsic is a division; the flag selects remainder over quotient.
    pub fn division(name: &str) -> Option<bool> {
        match name {
            | "div" => Some(false),
            | "mod" => Some(true),
            | _ => None,
        }
    }
}
