//! Compiler-side garbage-collection metadata.
//!
//! The bytes produced here are consumed by the native runtime: a root map
//! enumerates the pointer-bearing words of one safepoint, and a descriptor
//! enumerates the pointer-bearing fields of one product allocation site.

use super::{analyze::StackAnalysisScope, arena::AssemblyArena, syntax::FieldClass};
use crate::{
    analyze::{Layout, Slot, SlotId},
    syntax::Symbol,
};
use zydeco_utils::arena::ArenaSparse;

pub const CLASS_SCALAR: u8 = 0;
pub const CLASS_HEAP_POINTER: u8 = 1;
pub const CLASS_INTERIOR_POINTER: u8 = 2;
pub const CLASS_MAYBE_POINTER: u8 = 3;

/// GC treatment of one root word at a safepoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotClass {
    Scalar,
    HeapPointer,
    InteriorPointer { offset_words: u32 },
    MaybePointer,
}

impl SlotClass {
    pub fn code(self) -> u8 {
        match self {
            | Self::Scalar => CLASS_SCALAR,
            | Self::HeapPointer => CLASS_HEAP_POINTER,
            | Self::InteriorPointer { .. } => CLASS_INTERIOR_POINTER,
            | Self::MaybePointer => CLASS_MAYBE_POINTER,
        }
    }

    pub fn interior_offset_words(self) -> u32 {
        match self {
            | Self::InteriorPointer { offset_words } => offset_words,
            | Self::Scalar | Self::HeapPointer | Self::MaybePointer => 0,
        }
    }
}

/// One entry of a root map. Control entries are measured in words from the
/// captured caller `rsp`; context entries index into the frame at `rbp`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GcSlot {
    pub offset_words: u32,
    pub class: SlotClass,
}

/// Roots live at one allocation safepoint.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GcRootMap {
    pub control: Vec<GcSlot>,
    pub context: Vec<GcSlot>,
}

impl Slot {
    pub fn gc_class(&self, arena: &AssemblyArena) -> SlotClass {
        match self {
            | Slot::Sym(sym) => match &arena.symbols[sym].inner {
                | Symbol::StringLiteral(_) => SlotClass::HeapPointer,
                | Symbol::Undefined(_) | Symbol::Prog(_) => SlotClass::Scalar,
            },
            | Slot::Imm(_) | Slot::Tag => SlotClass::Scalar,
            | Slot::Product(_) => SlotClass::HeapPointer,
            | Slot::StackProduct(_) => SlotClass::Scalar,
            | Slot::ProductSuffix { offset, .. } => SlotClass::InteriorPointer {
                offset_words: u32::try_from(*offset).expect("interior pointer offset overflow"),
            },
            | Slot::Unknown => SlotClass::MaybePointer,
        }
    }
}

pub fn root_map(
    arena: &AssemblyArena, layout: &Layout, slots: &ArenaSparse<StackAnalysisScope, SlotId>,
) -> GcRootMap {
    let depth = layout.control.len();
    let control = layout
        .control
        .iter()
        .enumerate()
        .map(|(index, slot)| {
            let offset_words = u32::try_from(depth - 1 - index).expect("control depth overflow");
            GcSlot { offset_words, class: slots[slot].gc_class(arena) }
        })
        .collect();
    let context = layout
        .context
        .iter()
        .enumerate()
        .map(|(index, (_, slot))| {
            let offset_words = u32::try_from(index).expect("context index overflow");
            GcSlot { offset_words, class: slot.gc_class(arena) }
        })
        .collect();
    GcRootMap { control, context }
}

impl GcRootMap {
    /// Encode in the byte format expected by the native runtime.
    pub fn encode(&self) -> Vec<u8> {
        let mut bytes =
            Vec::with_capacity(8 + 12 * self.control.len().saturating_add(self.context.len()));
        bytes.extend_from_slice(
            &u32::try_from(self.control.len()).expect("control root count overflow").to_le_bytes(),
        );
        bytes.extend_from_slice(
            &u32::try_from(self.context.len()).expect("context root count overflow").to_le_bytes(),
        );
        for slot in self.control.iter().chain(self.context.iter()) {
            bytes.extend_from_slice(&slot.offset_words.to_le_bytes());
            bytes.push(slot.class.code());
            bytes.extend_from_slice(&[0; 3]);
            bytes.extend_from_slice(&slot.class.interior_offset_words().to_le_bytes());
        }
        bytes
    }
}

/// Encode a product descriptor in the byte format expected by the native runtime.
pub fn descriptor_bytes(fields: &[FieldClass]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(4 + 8 * fields.len());
    bytes.extend_from_slice(
        &u32::try_from(fields.len()).expect("product arity overflow").to_le_bytes(),
    );
    for field in fields {
        let (class, offset_words) = match field {
            | FieldClass::Scalar => (CLASS_SCALAR, 0),
            | FieldClass::HeapPointer => (CLASS_HEAP_POINTER, 0),
            | FieldClass::InteriorPointer { offset_words } => {
                (CLASS_INTERIOR_POINTER, *offset_words)
            }
            | FieldClass::MaybePointer => (CLASS_MAYBE_POINTER, 0),
        };
        bytes.push(class);
        bytes.extend_from_slice(&[0; 3]);
        bytes.extend_from_slice(&offset_words.to_le_bytes());
    }
    bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn descriptor_encoding_round_trips_its_layout() {
        let fields = [
            FieldClass::Scalar,
            FieldClass::HeapPointer,
            FieldClass::InteriorPointer { offset_words: 7 },
            FieldClass::MaybePointer,
        ];
        let bytes = descriptor_bytes(&fields);
        assert_eq!(&bytes[0..4], &4u32.to_le_bytes());
        assert_eq!(bytes[4], CLASS_SCALAR);
        assert_eq!(bytes[12], CLASS_HEAP_POINTER);
        assert_eq!(&bytes[24..28], &7u32.to_le_bytes());
        assert_eq!(bytes[28], CLASS_MAYBE_POINTER);
    }

    #[test]
    fn root_map_encoding_places_counts_before_entries() {
        let map = GcRootMap {
            control: vec![GcSlot { offset_words: 2, class: SlotClass::HeapPointer }],
            context: vec![GcSlot {
                offset_words: 1,
                class: SlotClass::InteriorPointer { offset_words: 3 },
            }],
        };
        let bytes = map.encode();
        assert_eq!(&bytes[0..4], &1u32.to_le_bytes());
        assert_eq!(&bytes[4..8], &1u32.to_le_bytes());
        assert_eq!(&bytes[8..12], &2u32.to_le_bytes());
        assert_eq!(bytes[12], CLASS_HEAP_POINTER);
        assert_eq!(&bytes[20..24], &1u32.to_le_bytes());
        assert_eq!(bytes[24], CLASS_INTERIOR_POINTER);
        assert_eq!(&bytes[28..32], &3u32.to_le_bytes());
    }
}
