//! The instruction sequences over the shared tagged runtime-word representation.
//!
//! The word encoding itself — the immediate boundary, the integer and float
//! classification — lives in [`zydeco_syntax::word`], which every backend and the
//! external boundary pins share. This module adds the WebAssembly function bodies
//! that decode, encode, box, and divide tagged words.

use wasm_encoder::{Function, Instruction as WasmInstruction};
use zydeco_syntax::scalar::{ScalarProgram, ScalarStep, ScalarType};

use zydeco_syntax::{
    FloatArithmetic, FloatType, IntegerArithmetic, IntegerType, PrimitiveOp, SpareBox,
};

pub use zydeco_syntax::word::{EncodedScalar, RuntimeWord, ScalarRepresentation, WordError};

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

    /// Decode an ordinary store operand into raw bits in its occurrence-local home.
    pub fn memory_decode(&mut self, scalar: zydeco_syntax::memory::MemoryScalar, local: u32) {
        use zydeco_syntax::memory::MemoryScalar;
        match scalar {
            | MemoryScalar::Integer(ty) => self.decode_integer(local, ty),
            | MemoryScalar::Float(ty) => {
                self.function.instruction(&WasmInstruction::LocalGet(local));
                match ty {
                    | FloatType::Float32 => {
                        self.function.instruction(&WasmInstruction::I64Const(1));
                        self.function.instruction(&WasmInstruction::I64ShrU);
                    }
                    | FloatType::Float64 => {
                        self.function.instruction(&WasmInstruction::I32WrapI64);
                        self.function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
                    }
                }
                self.function.instruction(&WasmInstruction::LocalSet(local));
            }
            | MemoryScalar::Address => {}
        }
    }

    /// Validate a loaded carrier, then leave its ordinary value on the operand stack.
    pub fn memory_encode(
        &mut self, scalar: zydeco_syntax::memory::MemoryScalar, local: u32, pointer: PointerLocal,
    ) {
        use zydeco_syntax::memory::MemoryScalar;
        if let MemoryScalar::Integer(ty) = scalar {
            if matches!(ty, IntegerType::Int | IntegerType::UInt) {
                self.function.instruction(&WasmInstruction::LocalGet(local));
                self.function.instruction(&WasmInstruction::LocalGet(local));
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&WasmInstruction::I64Shl);
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&if ty.is_signed() {
                    WasmInstruction::I64ShrS
                } else {
                    WasmInstruction::I64ShrU
                });
                self.function.instruction(&WasmInstruction::I64Ne);
                self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Empty));
                crate::RuntimeFailure::IntegerRange.emit(self.function);
                self.function.instruction(&WasmInstruction::End);
            }
            if ty.is_signed() && ty.storage_bits() < 64 {
                let shift = (64 - ty.storage_bits()) as i64;
                self.function.instruction(&WasmInstruction::LocalGet(local));
                self.function.instruction(&WasmInstruction::I64Const(shift));
                self.function.instruction(&WasmInstruction::I64Shl);
                self.function.instruction(&WasmInstruction::I64Const(shift));
                self.function.instruction(&WasmInstruction::I64ShrS);
                self.function.instruction(&WasmInstruction::LocalSet(local));
            }
        }
        match scalar.value_type().map(|ty| ty.representation()) {
            | Some(ScalarRepresentation::Immediate) => self.tag_local(local),
            | Some(ScalarRepresentation::OpaqueBox) => self.box_local(local, pointer),
            | None => {
                self.function.instruction(&WasmInstruction::LocalGet(local));
            }
        }
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
    /// trapping on a zero divisor. The caller wraps the result to the source width.
    fn signed_division(
        &mut self, ty: IntegerType, first: u32, second: u32, result: u32, remainder: bool,
    ) {
        self.function.instruction(&WasmInstruction::LocalGet(second));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(wasm_encoder::BlockType::Empty));
        if remainder {
            crate::RuntimeFailure::IntegerRemainderByZero.emit(self.function);
        } else {
            crate::RuntimeFailure::IntegerDivisionByZero.emit(self.function);
        }
        self.function.instruction(&WasmInstruction::End);
        if ty == IntegerType::Int64 {
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
            self.function.instruction(&WasmInstruction::I64Const(if remainder {
                0
            } else {
                i64::MIN
            }));
            self.function.instruction(&WasmInstruction::Else);
        }
        self.function.instruction(&WasmInstruction::LocalGet(first));
        self.function.instruction(&WasmInstruction::LocalGet(second));
        self.function.instruction(&if remainder {
            WasmInstruction::I64RemS
        } else {
            WasmInstruction::I64DivS
        });
        if ty == IntegerType::Int64 {
            self.function.instruction(&WasmInstruction::End);
        }
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

    /// Each definition has a distinct i64 local; raw bits never enter value memory.
    /// Inputs have been placed in the first locals. Leave the ordinary result on the stack.
    pub fn scalar_region(&mut self, program: &ScalarProgram, base: u32, pointer: PointerLocal) {
        for (index, step) in program.region().steps.iter().enumerate() {
            let target = base + (program.region().inputs.len() + index) as u32;
            match *step {
                | ScalarStep::Decode { ty, value } => {
                    let source = base + value.0 as u32;
                    self.function.instruction(&WasmInstruction::LocalGet(source));
                    self.function.instruction(&WasmInstruction::LocalSet(target));
                    match ty {
                        | ScalarType::Integer(ty) => self.decode_integer(target, ty),
                        | ScalarType::Float(ty) => {
                            self.function.instruction(&WasmInstruction::LocalGet(target));
                            match ty {
                                | FloatType::Float32 => {
                                    self.function.instruction(&WasmInstruction::I64Const(1));
                                    self.function.instruction(&WasmInstruction::I64ShrU);
                                }
                                | FloatType::Float64 => {
                                    self.function.instruction(&WasmInstruction::I32WrapI64);
                                    self.function
                                        .instruction(&WasmInstruction::I64Load(WORD_MEMORY));
                                }
                            }
                            self.function.instruction(&WasmInstruction::LocalSet(target));
                        }
                    }
                }
                | ScalarStep::Encode { ty, raw } => {
                    let source = base + raw.0 as u32;
                    match ty.representation() {
                        | ScalarRepresentation::Immediate => self.tag_local(source),
                        | ScalarRepresentation::OpaqueBox => self.box_local(source, pointer),
                    }
                    self.function.instruction(&WasmInstruction::LocalSet(target));
                }
                | ScalarStep::Arithmetic { operation, operands } => {
                    self.raw_arithmetic(
                        operation,
                        base + operands[0].0 as u32,
                        base + operands[1].0 as u32,
                        target,
                    );
                }
            }
        }
        self.function
            .instruction(&WasmInstruction::LocalGet(base + program.region().result.0 as u32));
    }

    fn raw_arithmetic(&mut self, operation: PrimitiveOp, first: u32, second: u32, result: u32) {
        match operation {
            | PrimitiveOp::Integer(ty, operation) => {
                match operation {
                    | IntegerArithmetic::Div | IntegerArithmetic::Mod if ty.is_signed() => {
                        self.signed_division(
                            ty,
                            first,
                            second,
                            result,
                            operation == IntegerArithmetic::Mod,
                        );
                    }
                    | operation => {
                        let instruction = match operation {
                            | IntegerArithmetic::Add => WasmInstruction::I64Add,
                            | IntegerArithmetic::Sub => WasmInstruction::I64Sub,
                            | IntegerArithmetic::Mul => WasmInstruction::I64Mul,
                            | IntegerArithmetic::Div | IntegerArithmetic::Mod => {
                                self.function.instruction(&WasmInstruction::LocalGet(second));
                                self.function.instruction(&WasmInstruction::I64Eqz);
                                self.function.instruction(&WasmInstruction::If(
                                    wasm_encoder::BlockType::Empty,
                                ));
                                if operation == IntegerArithmetic::Mod {
                                    crate::RuntimeFailure::IntegerRemainderByZero
                                        .emit(self.function);
                                } else {
                                    crate::RuntimeFailure::IntegerDivisionByZero
                                        .emit(self.function);
                                }
                                self.function.instruction(&WasmInstruction::End);
                                if operation == IntegerArithmetic::Mod {
                                    WasmInstruction::I64RemU
                                } else {
                                    WasmInstruction::I64DivU
                                }
                            }
                        };
                        self.function.instruction(&WasmInstruction::LocalGet(first));
                        self.function.instruction(&WasmInstruction::LocalGet(second));
                        self.function.instruction(&instruction);
                        self.function.instruction(&WasmInstruction::LocalSet(result));
                    }
                }
                let by = i64::from(64 - ty.bits());
                self.function.instruction(&WasmInstruction::LocalGet(result));
                self.function.instruction(&WasmInstruction::I64Const(by));
                self.function.instruction(&WasmInstruction::I64Shl);
                self.function.instruction(&WasmInstruction::I64Const(by));
                self.function.instruction(&if ty.is_signed() {
                    WasmInstruction::I64ShrS
                } else {
                    WasmInstruction::I64ShrU
                });
                self.function.instruction(&WasmInstruction::LocalSet(result));
            }
            | PrimitiveOp::Float(ty, operation) => {
                for local in [first, second] {
                    self.function.instruction(&WasmInstruction::LocalGet(local));
                    match ty {
                        | FloatType::Float32 => {
                            self.function.instruction(&WasmInstruction::I32WrapI64);
                            self.function.instruction(&WasmInstruction::F32ReinterpretI32);
                        }
                        | FloatType::Float64 => {
                            self.function.instruction(&WasmInstruction::F64ReinterpretI64);
                        }
                    }
                }
                self.function.instruction(&match (ty, operation) {
                    | (FloatType::Float32, FloatArithmetic::Add) => WasmInstruction::F32Add,
                    | (FloatType::Float32, FloatArithmetic::Sub) => WasmInstruction::F32Sub,
                    | (FloatType::Float32, FloatArithmetic::Mul) => WasmInstruction::F32Mul,
                    | (FloatType::Float32, FloatArithmetic::Div) => WasmInstruction::F32Div,
                    | (FloatType::Float64, FloatArithmetic::Add) => WasmInstruction::F64Add,
                    | (FloatType::Float64, FloatArithmetic::Sub) => WasmInstruction::F64Sub,
                    | (FloatType::Float64, FloatArithmetic::Mul) => WasmInstruction::F64Mul,
                    | (FloatType::Float64, FloatArithmetic::Div) => WasmInstruction::F64Div,
                });
                match ty {
                    | FloatType::Float32 => {
                        self.function.instruction(&WasmInstruction::I32ReinterpretF32);
                        self.function.instruction(&WasmInstruction::I64ExtendI32U);
                    }
                    | FloatType::Float64 => {
                        self.function.instruction(&WasmInstruction::I64ReinterpretF64);
                    }
                }
                self.function.instruction(&WasmInstruction::LocalSet(result));
            }
        }
    }

    fn decode_integer(&mut self, local: u32, ty: IntegerType) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        match ty.representation() {
            | ScalarRepresentation::OpaqueBox => {
                self.function.instruction(&WasmInstruction::I32WrapI64);
                self.function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
            }
            | ScalarRepresentation::Immediate => {
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&if ty.is_signed() {
                    WasmInstruction::I64ShrS
                } else {
                    WasmInstruction::I64ShrU
                });
            }
        }
        self.function.instruction(&WasmInstruction::LocalSet(local));
    }

    fn tag_local(&mut self, local: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Shl);
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Or);
    }

    fn box_local(&mut self, local: u32, pointer: PointerLocal) {
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.alloc_function));
        pointer.bind_allocation(self.function);
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        pointer.push_word(self.function);
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
