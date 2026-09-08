//! Experimental root publication for environments in a moving managed heap.
//!
//! Heap frames are opaque cells. Their handles and live values are roots; dead
//! fields are never traced. Values are lifted out before collection and written
//! back through relocated handles afterwards. This is a different capability
//! from `Storage`: any managed allocation can relocate the active environment.

use super::{FrameError, Layout};
use crate::native::{AllocationKind, Word};
use alloc::vec::Vec;

/// One live heap frame and the union of its active and suspended slot maps.
/// Each frame must occur once; several continuations may contribute to its map.
pub struct LiveFrame<'a> {
    pub layout: Layout,
    pub handle: &'a mut Word,
    pub slots: &'a [Word],
}

/// The collector copies frame bytes without following stale or dead fields.
/// Root publication supplies their precise live contents separately.
pub const ALLOCATION_KIND: AllocationKind = AllocationKind::Opaque;

pub struct MovingRoots;

impl MovingRoots {
    /// Publish stable handle/value addresses, then restore relocated live fields.
    /// The callback's result may be an error; restoration still occurs on return.
    /// The active machine base must then be reloaded from its updated handle.
    ///
    /// # Safety
    /// Every handle must address a writable opaque cell with `layout.words`
    /// payload words, and each published slot must be initialized. Handles are
    /// distinct frames and no frame words may be accessed during `trace` except
    /// by the collector. The callback must update all roots if it moves objects;
    /// it cannot retain the temporary root addresses beyond the call.
    pub unsafe fn with_roots<T>(
        mut frames: Vec<LiveFrame<'_>>, trace: impl FnOnce(&mut [*mut Word]) -> T,
    ) -> Result<T, FrameError> {
        // Validate every map before reading any payload, including later frames.
        for frame in &frames {
            if let Some(&slot) = frame.slots.iter().find(|&&slot| slot >= frame.layout.words) {
                return Err(FrameError::InvalidSlot { slot, words: frame.layout.words });
            }
        }
        let mut values = frames
            .iter()
            .flat_map(|frame| {
                frame
                    .slots
                    .iter()
                    .map(|&slot| unsafe { (*frame.handle as *const Word).add(slot).read() })
            })
            .collect::<Vec<_>>();
        let mut roots = frames
            .iter_mut()
            .map(|frame| &mut *frame.handle as *mut Word)
            .chain(values.iter_mut().map(|value| value as *mut Word))
            .collect::<Vec<_>>();
        let result = trace(&mut roots);
        let mut values = values.into_iter();
        for frame in frames {
            for &slot in frame.slots {
                unsafe { (*frame.handle as *mut Word).add(slot).write(values.next().unwrap()) };
            }
        }
        Ok(result)
    }
}
