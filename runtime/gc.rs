//! A deliberately small two-space copying heap.
//!
//! The layout follows the useful part of OCaml's runtime representation: every
//! allocated block has a header, and the header says whether its payload contains
//! values that the collector should scan.  Zydeco product blocks are scanned;
//! opaque blocks are available for future boxed scalars or byte payloads.
//!
//! Collection is Cheney's breadth-first algorithm.  Roots are copied into the
//! inactive semispace, then `scan` walks that space until it catches the allocation
//! cursor.
//! Each semispace has a block-start index; locating a payload's header reads one
//! index region and one header, including for pointers into large blocks.
//!
//! Values use the same one-bit convention as OCaml: odd words are immediate values,
//! while managed pointers are aligned and therefore even. Full-width scalars that
//! cannot surrender a tag bit live in opaque one-word blocks. The collector can
//! consequently recognize managed pointers precisely without stack maps.

use std::{mem::size_of, ptr};

pub(crate) type Word = usize;

const WORD_BYTES: usize = size_of::<Word>();
const FORWARDED_BIT: Word = 1;
const TAG_MASK: Word = 0xff;
pub(crate) const IMMEDIATE_TAG: Word = 1;

/// Whether the words in a block are themselves runtime values.
///
/// Tags are even so the low bit remains available for a forwarding pointer while
/// a collection is in progress.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub(crate) enum BlockTag {
    Scanned = 0,
    Opaque = 1 << 1,
}

/// The header immediately before every payload pointer returned to generated code.
///
/// `metadata` normally contains a [`BlockTag`].  After a block has moved, its old
/// header contains the new payload pointer with [`FORWARDED_BIT`] set.  Keeping the
/// size in a separate word makes interior-pointer lookup possible even after a
/// preceding block has been forwarded.
#[derive(Clone, Copy)]
#[repr(C)]
struct BlockHeader {
    size_words: Word,
    metadata: Word,
}

const HEADER_BYTES: usize = size_of::<BlockHeader>();
const INDEX_REGION_WORDS: usize = Word::BITS as usize;
pub(crate) const INDEX_REGION_BYTES: usize = INDEX_REGION_WORDS * WORD_BYTES;

const _: () = {
    assert!(WORD_BYTES == 8, "the native runtime currently requires 64-bit words");
    assert!(HEADER_BYTES == 2 * WORD_BYTES);
    assert!(size_of::<RegionStarts>() == 2 * WORD_BYTES);
};

impl BlockHeader {
    const fn new(size_words: usize, tag: BlockTag) -> Self {
        Self { size_words, metadata: tag as Word }
    }

    fn tag(self) -> BlockTag {
        debug_assert_eq!(self.metadata & FORWARDED_BIT, 0);
        match self.metadata & TAG_MASK {
            | value if value == BlockTag::Scanned as Word => BlockTag::Scanned,
            | value if value == BlockTag::Opaque as Word => BlockTag::Opaque,
            | value => panic!("invalid runtime block tag {value}"),
        }
    }

    fn forwarded_to(self) -> Option<*mut u8> {
        (self.metadata & FORWARDED_BIT != 0).then_some((self.metadata & !FORWARDED_BIT) as *mut u8)
    }

    fn set_forwarded_to(&mut self, payload: *mut u8) {
        debug_assert_eq!(payload as Word & FORWARDED_BIT, 0);
        self.metadata = payload as Word | FORWARDED_BIT;
    }
}

/// Block headers starting in one 512-byte region, plus the header of a block
/// crossing into it. The predecessor is unused when a preceding start bit exists.
#[derive(Clone, Copy)]
struct RegionStarts {
    headers: Word,
    preceding_header: usize,
}

impl RegionStarts {
    const EMPTY: Self = Self { headers: 0, preceding_header: 0 };
}

struct LocatedBlock {
    header: *mut BlockHeader,
    contents: BlockHeader,
    interior_offset: usize,
}

#[cfg(test)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct LookupWork {
    regions: usize,
    headers: usize,
}

/// An append-only index of block boundaries in one semispace.
///
/// Recording a block initializes each region it first touches. Starting again at
/// offset zero therefore replaces stale metadata as the semispace is reused,
/// without clearing its whole index or visiting dead blocks. Lookups must stay
/// within the current allocation cursor; entries beyond it may be stale.
struct BlockStartIndex<const REGIONS: usize> {
    regions: [RegionStarts; REGIONS],
    #[cfg(test)]
    work: std::cell::Cell<LookupWork>,
}

impl<const REGIONS: usize> BlockStartIndex<REGIONS> {
    const fn new() -> Self {
        Self {
            regions: [RegionStarts::EMPTY; REGIONS],
            #[cfg(test)]
            work: std::cell::Cell::new(LookupWork { regions: 0, headers: 0 }),
        }
    }

    /// Record the next contiguous block, including regions crossed by its header.
    fn record(&mut self, header_offset: usize, cell_bytes: usize) {
        debug_assert_eq!(header_offset % WORD_BYTES, 0);
        debug_assert!(cell_bytes >= HEADER_BYTES);
        let first = header_offset / INDEX_REGION_BYTES;
        let last = (header_offset + cell_bytes - 1) / INDEX_REGION_BYTES;
        let word = (header_offset % INDEX_REGION_BYTES) / WORD_BYTES;
        if word == 0 {
            self.regions[first] = RegionStarts::EMPTY;
        }
        self.regions[first].headers |= 1 << word;
        self.regions[first + 1..last + 1]
            .fill(RegionStarts { headers: 0, preceding_header: header_offset });
    }

    fn region(&self, index: usize) -> RegionStarts {
        #[cfg(test)]
        self.work.update(|work| LookupWork { regions: work.regions + 1, ..work });
        self.regions[index]
    }

    unsafe fn read_header(&self, header: *mut BlockHeader) -> BlockHeader {
        #[cfg(test)]
        self.work.update(|work| LookupWork { headers: work.headers + 1, ..work });
        unsafe { header.read() }
    }

    /// Find the owning payload with one index-region read and one header read.
    ///
    /// The prefix mask includes the pointer's word. A pointer to either word of
    /// a header therefore selects that header and fails the payload bounds check,
    /// even when the header straddles an index-region boundary.
    unsafe fn containing_block(
        &self, value: Word, from_base: *mut u8, from_used: usize,
    ) -> Option<LocatedBlock> {
        if value & IMMEDIATE_TAG != 0 || value & (WORD_BYTES - 1) != 0 {
            return None;
        }
        let offset = value.checked_sub(from_base as Word)?;
        if offset < HEADER_BYTES || offset >= from_used {
            return None;
        }

        let region_index = offset / INDEX_REGION_BYTES;
        let region = self.region(region_index);
        let word = (offset % INDEX_REGION_BYTES) / WORD_BYTES;
        let preceding = region.headers & (Word::MAX >> (INDEX_REGION_WORDS - 1 - word));
        let header_offset = if preceding == 0 {
            region.preceding_header
        } else {
            let header_word = INDEX_REGION_WORDS - 1 - preceding.leading_zeros() as usize;
            region_index * INDEX_REGION_BYTES + header_word * WORD_BYTES
        };
        let header = unsafe { from_base.add(header_offset).cast::<BlockHeader>() };
        let contents = unsafe { self.read_header(header) };
        let interior_offset = offset.checked_sub(header_offset + HEADER_BYTES)?;
        if interior_offset / WORD_BYTES >= contents.size_words {
            return None;
        }
        Some(LocatedBlock { header, contents, interior_offset })
    }
}

/// One statically sized and word-aligned semispace.
#[repr(align(8))]
struct Space<const BYTES: usize>([u8; BYTES]);

impl<const BYTES: usize> Space<BYTES> {
    const fn new() -> Self {
        Self([0; BYTES])
    }

    fn base(&mut self) -> *mut u8 {
        self.0.as_mut_ptr()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct OutOfMemory {
    pub requested_words: usize,
    pub live_bytes: usize,
    pub capacity_bytes: usize,
}

#[derive(Clone, Copy)]
pub(crate) struct RootRange {
    pub start: *mut Word,
    pub end: *mut Word,
}

pub(crate) struct Roots<'a> {
    pub stack: RootRange,
    pub environment: RootRange,
    pub host: &'a mut [*mut Word],
}

/// Two fixed semispaces and the cursor in the currently active one.
///
/// `BYTES` includes block headers. Each space additionally reserves two index
/// words per 512 bytes (rounded up), or 3.125% for a whole number of regions.
/// `REGIONS` is explicit because stable const generics cannot derive an array
/// length from `BYTES`; construction checks their relationship.
pub(crate) struct CheneyHeap<const BYTES: usize, const REGIONS: usize> {
    spaces: [Space<BYTES>; 2],
    starts: [BlockStartIndex<REGIONS>; 2],
    active: usize,
    used: usize,
    collections: usize,
}

impl<const BYTES: usize, const REGIONS: usize> CheneyHeap<BYTES, REGIONS> {
    pub const fn new() -> Self {
        assert!(REGIONS == BYTES.div_ceil(INDEX_REGION_BYTES), "incorrect block index capacity");
        Self {
            spaces: [Space::new(), Space::new()],
            starts: [BlockStartIndex::new(), BlockStartIndex::new()],
            active: 0,
            used: 0,
            collections: 0,
        }
    }

    /// Allocate a block, collecting first when the active semispace is full.
    ///
    /// `roots.stack` and `roots.environment` are mutable root slots. `roots.host`
    /// contains addresses of the few roots kept by long-lived Rust-side runtime
    /// objects. All ranges are updated in place.
    ///
    /// # Safety
    ///
    /// Every nonempty root range must be valid, word-aligned, writable, and belong
    /// exclusively to the current runtime thread for the duration of this call.
    pub unsafe fn allocate(
        &mut self, size_words: usize, tag: BlockTag, roots: Roots<'_>,
    ) -> Result<*mut u8, OutOfMemory> {
        let cell_bytes = Self::cell_bytes(size_words).ok_or_else(|| self.oom(size_words))?;
        if cell_bytes > BYTES {
            return Err(self.oom(size_words));
        }

        if self.used + cell_bytes > BYTES {
            unsafe { self.collect(roots) };
        }
        if self.used + cell_bytes > BYTES {
            return Err(self.oom(size_words));
        }

        let base = unsafe { self.active_base().add(self.used) };
        unsafe { base.cast::<BlockHeader>().write(BlockHeader::new(size_words, tag)) };
        let payload = unsafe { base.add(HEADER_BYTES) };
        self.starts[self.active].record(self.used, cell_bytes);
        self.used += cell_bytes;
        Ok(payload)
    }

    fn cell_bytes(size_words: usize) -> Option<usize> {
        HEADER_BYTES.checked_add(size_words.checked_mul(WORD_BYTES)?)
    }

    fn oom(&self, requested_words: usize) -> OutOfMemory {
        OutOfMemory { requested_words, live_bytes: self.used, capacity_bytes: BYTES }
    }

    fn active_base(&mut self) -> *mut u8 {
        self.spaces[self.active].base()
    }

    /// Swap semispaces and copy the transitive closure of the roots.
    unsafe fn collect(&mut self, roots: Roots<'_>) {
        let Roots { stack, environment, host } = roots;
        let from_index = self.active;
        let to_index = 1 - from_index;
        let from_base = self.spaces[from_index].base();
        let to_base = self.spaces[to_index].base();
        let from_used = self.used;
        let mut to_used = 0;

        unsafe {
            self.forward_range(stack.start, stack.end, from_base, from_used, to_base, &mut to_used)
        };
        if environment.start != environment.end {
            unsafe {
                self.forward_range(
                    environment.start,
                    environment.end,
                    from_base,
                    from_used,
                    to_base,
                    &mut to_used,
                )
            };
        }
        for root in host.iter_mut().filter(|root| !root.is_null()) {
            let value = unsafe { **root };
            unsafe { **root = self.forward(value, from_base, from_used, to_base, &mut to_used) };
        }

        // Cheney's queue is the copied part of to-space itself. Newly discovered
        // blocks extend `to_used`; the loop ends exactly when every copied block has
        // been scanned.
        let mut scan = 0;
        while scan < to_used {
            let header_pointer = unsafe { to_base.add(scan).cast::<BlockHeader>() };
            let header = unsafe { header_pointer.read() };
            debug_assert!(header.forwarded_to().is_none());
            let payload = unsafe { header_pointer.cast::<u8>().add(HEADER_BYTES).cast::<Word>() };

            if header.tag() == BlockTag::Scanned {
                for index in 0..header.size_words {
                    let slot = unsafe { payload.add(index) };
                    let value = unsafe { slot.read() };
                    let forwarded =
                        unsafe { self.forward(value, from_base, from_used, to_base, &mut to_used) };
                    unsafe { slot.write(forwarded) };
                }
            }
            scan += Self::cell_bytes(header.size_words).expect("valid block size overflowed");
        }
        debug_assert_eq!(scan, to_used);

        self.active = to_index;
        self.used = to_used;
        self.collections += 1;
    }

    unsafe fn forward_range(
        &mut self, start: *mut Word, end: *mut Word, from_base: *mut u8, from_used: usize,
        to_base: *mut u8, to_used: &mut usize,
    ) {
        debug_assert!(start <= end);
        let mut slot = start;
        while slot < end {
            let value = unsafe { slot.read() };
            let forwarded = unsafe { self.forward(value, from_base, from_used, to_base, to_used) };
            unsafe { slot.write(forwarded) };
            slot = unsafe { slot.add(1) };
        }
    }

    /// Copy the block containing `value`, preserving an interior pointer's offset.
    unsafe fn forward(
        &mut self, value: Word, from_base: *mut u8, from_used: usize, to_base: *mut u8,
        to_used: &mut usize,
    ) -> Word {
        let Some(LocatedBlock { header: old_header, contents: old_header_value, interior_offset }) =
            (unsafe { self.starts[self.active].containing_block(value, from_base, from_used) })
        else {
            return value;
        };
        if let Some(new_payload) = old_header_value.forwarded_to() {
            return unsafe { new_payload.add(interior_offset) } as Word;
        }

        let cell_bytes = Self::cell_bytes(old_header_value.size_words)
            .expect("a heap block has an invalid size");
        // The live graph was allocated in from-space and therefore fits in one
        // semispace. This assertion documents the Cheney invariant directly.
        assert!(*to_used + cell_bytes <= BYTES, "live data does not fit in to-space");
        let new_header = unsafe { to_base.add(*to_used).cast::<BlockHeader>() };
        unsafe { ptr::copy_nonoverlapping(old_header.cast::<u8>(), new_header.cast(), cell_bytes) };
        let new_payload = unsafe { new_header.cast::<u8>().add(HEADER_BYTES) };
        unsafe { (*old_header).set_forwarded_to(new_payload) };
        // A copied header must carry its ordinary tag, not a forwarding address.
        unsafe { (*new_header).metadata = old_header_value.metadata };
        self.starts[1 - self.active].record(*to_used, cell_bytes);
        *to_used += cell_bytes;
        (unsafe { new_payload.add(interior_offset) }) as Word
    }

    #[cfg(test)]
    fn used_bytes(&self) -> usize {
        self.used
    }

    #[cfg(test)]
    fn collections(&self) -> usize {
        self.collections
    }
}

/// Generated Tests
#[cfg(test)]
mod tests {
    use super::*;

    struct TestRoots<const N: usize> {
        words: [Word; N],
    }

    impl<const N: usize> TestRoots<N> {
        fn new(words: [Word; N]) -> Self {
            Self { words }
        }

        fn roots(&mut self) -> Roots<'_> {
            let start = self.words.as_mut_ptr();
            Roots {
                stack: RootRange { start, end: unsafe { start.add(N) } },
                environment: RootRange { start: ptr::null_mut(), end: ptr::null_mut() },
                host: &mut [],
            }
        }

        unsafe fn allocate<const BYTES: usize, const REGIONS: usize>(
            &mut self, heap: &mut CheneyHeap<BYTES, REGIONS>, words: usize, tag: BlockTag,
        ) -> Result<*mut u8, OutOfMemory> {
            unsafe { heap.allocate(words, tag, self.roots()) }
        }

        unsafe fn collect<const BYTES: usize, const REGIONS: usize>(
            &mut self, heap: &mut CheneyHeap<BYTES, REGIONS>,
        ) -> LookupWork {
            let from_index = heap.active;
            heap.starts[from_index].work.set(LookupWork::default());
            unsafe { heap.collect(self.roots()) };
            heap.starts[from_index].work.get()
        }
    }

    unsafe fn write_words(payload: *mut u8, words: &[Word]) {
        for (index, word) in words.iter().copied().enumerate() {
            unsafe { payload.cast::<Word>().add(index).write(word) };
        }
    }

    fn immediate(value: Word) -> Word {
        (value << 1) | IMMEDIATE_TAG
    }

    #[test]
    fn index_accepts_payloads_and_rejects_headers_and_unused_space() {
        let mut heap = CheneyHeap::<4096, 8>::new();
        let mut roots = TestRoots::new([]);
        let base = heap.active_base();
        // The third header starts at word 63 and crosses into the next region;
        // the seventh starts exactly at a region boundary. Empty blocks, large
        // blocks, and partial regions exercise payload bounds.
        let blocks = [1, 58, 1, 0, 1, 119, 3, 0, 190, 64, 1]
            .into_iter()
            .map(|words| {
                let payload =
                    unsafe { roots.allocate(&mut heap, words, BlockTag::Scanned) }.unwrap();
                unsafe { write_words(payload, &vec![IMMEDIATE_TAG; words]) };
                (payload as Word, words)
            })
            .collect::<Vec<_>>();
        let original_used = heap.used_bytes();
        let index = &heap.starts[heap.active];

        for offset in (0..4096).step_by(WORD_BYTES) {
            let value = base as Word + offset;
            let expected = blocks.iter().find_map(|&(payload, words)| {
                (value >= payload && value < payload + words * WORD_BYTES)
                    .then(|| (payload - HEADER_BYTES, value - payload))
            });
            let found = unsafe { index.containing_block(value, base, original_used) }
                .map(|block| (block.header as Word, block.interior_offset));
            assert_eq!(found, expected, "heap word at offset {offset}");
            for invalid in [value | IMMEDIATE_TAG, value + 2] {
                assert!(unsafe { index.containing_block(invalid, base, original_used) }.is_none());
            }
        }

        let foreign = IMMEDIATE_TAG;
        let inactive = heap.spaces[1 - heap.active].base() as Word + HEADER_BYTES;
        for invalid in [0, ptr::from_ref(&foreign) as Word, inactive, base as Word + 4096] {
            assert!(unsafe { index.containing_block(invalid, base, original_used) }.is_none());
        }
        assert_eq!(heap.used_bytes(), original_used);
        assert_eq!(heap.collections(), 0);
    }

    #[test]
    fn large_product_aliases_survive_semispace_reuse() {
        let mut heap = CheneyHeap::<4096, 8>::new();
        let mut roots = TestRoots::new([IMMEDIATE_TAG; 4]);
        // These old starts will lie inside the large product when this space is
        // reused. Its index must replace them, including the final partial region.
        for _ in 0..32 {
            let garbage = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
            unsafe { write_words(garbage, &[IMMEDIATE_TAG]) };
        }
        let leaf = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
        unsafe { write_words(leaf, &[immediate(77)]) };
        roots.words[0] = leaf as Word;
        let product = unsafe { roots.allocate(&mut heap, 200, BlockTag::Scanned) }.unwrap();
        let mut fields = (0..200).map(immediate).collect::<Vec<_>>();
        fields[199] = roots.words[0];
        unsafe { write_words(product, &fields) };
        roots.words = [
            unsafe { product.add(150 * WORD_BYTES) } as Word,
            product as Word,
            unsafe { product.add(61 * WORD_BYTES) } as Word,
            roots.words[0],
        ];

        for _ in 0..6 {
            let previous = roots.words[1];
            let work = unsafe { roots.collect(&mut heap) };
            assert_eq!(work, LookupWork { regions: 5, headers: 5 });
            assert_ne!(roots.words[1], previous);
            assert_eq!(roots.words[0], roots.words[1] + 150 * WORD_BYTES);
            assert_eq!(roots.words[2], roots.words[1] + 61 * WORD_BYTES);
            assert_eq!(heap.used_bytes(), 2 * HEADER_BYTES + 201 * WORD_BYTES);
            let product = roots.words[1] as *const Word;
            for field in 0..199 {
                assert_eq!(unsafe { product.add(field).read() }, immediate(field));
            }
            assert_eq!(unsafe { product.add(199).read() }, roots.words[3]);
            assert_eq!(unsafe { (roots.words[3] as *const Word).read() }, immediate(77));

            while heap.used_bytes() + HEADER_BYTES + WORD_BYTES <= 4096 {
                let garbage = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
                unsafe { write_words(garbage, &[IMMEDIATE_TAG]) };
            }
        }

        // Shrink the live set, then append a block over old index entries. Lookup
        // must ignore stale entries both inside its new payload and past `used`.
        let leaf = roots.words[3];
        roots.words.fill(leaf);
        unsafe { roots.collect(&mut heap) };
        assert_eq!(heap.used_bytes(), HEADER_BYTES + WORD_BYTES);
        let product = unsafe { roots.allocate(&mut heap, 200, BlockTag::Scanned) }.unwrap();
        unsafe { write_words(product, &vec![IMMEDIATE_TAG; 200]) };
        let base = heap.active_base();
        let index = &heap.starts[heap.active];
        for word in 0..200 {
            let value = product as Word + word * WORD_BYTES;
            let block = unsafe { index.containing_block(value, base, heap.used_bytes()) }.unwrap();
            assert_eq!(block.header as Word + HEADER_BYTES, product as Word);
            assert_eq!(block.interior_offset, word * WORD_BYTES);
        }
        for offset in (heap.used_bytes()..4096).step_by(WORD_BYTES) {
            assert!(
                unsafe { index.containing_block(base as Word + offset, base, heap.used_bytes()) }
                    .is_none()
            );
        }
    }

    #[test]
    fn chain_lookup_work_grows_linearly_with_live_edges() {
        for blocks in [1024, 2048, 4096, 8192, 16384] {
            let mut heap = CheneyHeap::<{ 512 * 1024 }, 1024>::new();
            let mut roots = TestRoots::new([IMMEDIATE_TAG]);
            for _ in 0..blocks {
                let previous = roots.words[0];
                let payload = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
                unsafe { write_words(payload, &[previous]) };
                roots.words[0] = payload as Word;
            }
            let work = unsafe { roots.collect(&mut heap) };
            assert_eq!(work, LookupWork { regions: blocks, headers: blocks });
            assert_eq!(heap.used_bytes(), blocks * (HEADER_BYTES + WORD_BYTES));
            let mut node = roots.words[0];
            for _ in 0..blocks {
                assert_ne!(node, IMMEDIATE_TAG);
                node = unsafe { (node as *const Word).read() };
            }
            assert_eq!(node, IMMEDIATE_TAG);
        }
    }

    #[test]
    fn alias_lookup_work_is_independent_of_dead_block_count() {
        for blocks in [1024, 4096, 16384] {
            let mut heap = CheneyHeap::<{ 512 * 1024 }, 1024>::new();
            let mut roots = TestRoots::new([IMMEDIATE_TAG]);
            for _ in 0..blocks {
                let payload = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
                unsafe { write_words(payload, &[immediate(42)]) };
                roots.words[0] = payload as Word;
            }
            let mut aliases = TestRoots::new([roots.words[0]; 8192]);
            let work = unsafe { aliases.collect(&mut heap) };
            assert_eq!(work, LookupWork { regions: 8192, headers: 8192 });
            assert_eq!(heap.used_bytes(), HEADER_BYTES + WORD_BYTES);
            assert!(aliases.words.iter().all(|&word| word == aliases.words[0]));
            assert_eq!(unsafe { (aliases.words[0] as *const Word).read() }, immediate(42));
        }
    }

    #[test]
    fn interior_lookup_work_is_independent_of_block_size() {
        for words in [64, 1024, 16384] {
            let mut heap = CheneyHeap::<{ 512 * 1024 }, 1024>::new();
            let mut roots = TestRoots::new([IMMEDIATE_TAG]);
            let payload = unsafe { roots.allocate(&mut heap, words, BlockTag::Opaque) }.unwrap();
            unsafe { write_words(payload, &vec![immediate(17); words]) };
            roots.words[0] = unsafe { payload.add((words - 1) * WORD_BYTES) } as Word;
            let work = unsafe { roots.collect(&mut heap) };
            assert_eq!(work, LookupWork { regions: 1, headers: 1 });
            assert_eq!(heap.used_bytes(), HEADER_BYTES + words * WORD_BYTES);
            assert_eq!(unsafe { (roots.words[0] as *const Word).read() }, immediate(17));
        }
    }

    #[test]
    fn copies_reachable_blocks_and_rewrites_edges() {
        let mut heap = CheneyHeap::<128, 1>::new();
        let mut roots = TestRoots::new([0]);
        let leaf = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(leaf, &[immediate(41)]) };
        let parent = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(parent, &[leaf as Word]) };
        roots.words[0] = parent as Word;

        let garbage = unsafe { roots.allocate(&mut heap, 6, BlockTag::Scanned).unwrap() };
        unsafe {
            write_words(
                garbage,
                &[
                    immediate(1),
                    immediate(2),
                    immediate(3),
                    immediate(4),
                    immediate(5),
                    immediate(6),
                ],
            )
        };
        let old_parent = roots.words[0];
        let fresh = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(fresh, &[immediate(99)]) };

        assert_eq!(heap.collections(), 1);
        assert_ne!(roots.words[0], old_parent);
        let moved_parent = roots.words[0] as *mut Word;
        let moved_leaf = unsafe { moved_parent.read() } as *mut Word;
        assert_eq!(unsafe { moved_leaf.read() }, immediate(41));
        assert_eq!(heap.used_bytes(), 3 * (HEADER_BYTES + WORD_BYTES));
    }

    #[test]
    fn tagged_immediate_cannot_be_mistaken_for_a_heap_pointer() {
        let mut heap = CheneyHeap::<80, 1>::new();
        let mut roots = TestRoots::new([0]);
        let target = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        let pointer_shaped_payload = target as Word | IMMEDIATE_TAG;
        roots.words[0] = pointer_shaped_payload;

        let garbage = unsafe { roots.allocate(&mut heap, 2, BlockTag::Scanned).unwrap() };
        unsafe { write_words(garbage, &[IMMEDIATE_TAG, IMMEDIATE_TAG]) };
        let fresh = unsafe { roots.allocate(&mut heap, 2, BlockTag::Scanned).unwrap() };
        unsafe { write_words(fresh, &[IMMEDIATE_TAG, IMMEDIATE_TAG]) };

        assert_eq!(heap.collections(), 1);
        assert_eq!(roots.words[0], pointer_shaped_payload);
        assert_eq!(heap.used_bytes(), HEADER_BYTES + 2 * WORD_BYTES);
    }

    #[test]
    fn preserves_interior_product_pointers() {
        let mut heap = CheneyHeap::<104, 1>::new();
        let mut roots = TestRoots::new([0]);
        let product = unsafe { roots.allocate(&mut heap, 3, BlockTag::Scanned).unwrap() };
        unsafe { write_words(product, &[immediate(10), immediate(20), immediate(30)]) };
        roots.words[0] = unsafe { product.add(WORD_BYTES) } as Word;
        let old_suffix = roots.words[0];

        let garbage = unsafe { roots.allocate(&mut heap, 4, BlockTag::Scanned).unwrap() };
        unsafe { write_words(garbage, &[immediate(1), immediate(2), immediate(3), immediate(4)]) };
        let fresh = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(fresh, &[immediate(40)]) };

        assert_eq!(heap.collections(), 1);
        assert_ne!(roots.words[0], old_suffix);
        let suffix = roots.words[0] as *mut Word;
        assert_eq!(unsafe { suffix.read() }, immediate(20));
        assert_eq!(unsafe { suffix.add(1).read() }, immediate(30));
    }

    #[test]
    fn forwarding_breaks_cycles_without_a_mark_stack() {
        let mut heap = CheneyHeap::<96, 1>::new();
        let mut roots = TestRoots::new([0]);
        let first = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        let second = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe {
            write_words(first, &[second as Word]);
            write_words(second, &[first as Word]);
        }
        roots.words[0] = first as Word;
        let garbage = unsafe { roots.allocate(&mut heap, 2, BlockTag::Scanned).unwrap() };
        unsafe { write_words(garbage, &[immediate(7), immediate(8)]) };
        let fresh = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(fresh, &[immediate(9)]) };

        let moved_first = roots.words[0] as *mut Word;
        let moved_second = unsafe { moved_first.read() } as *mut Word;
        assert_eq!(unsafe { moved_second.read() }, moved_first as Word);
        assert_eq!(heap.collections(), 1);
    }

    #[test]
    fn opaque_blocks_do_not_trace_payload_words() {
        let mut heap = CheneyHeap::<96, 1>::new();
        let mut roots = TestRoots::new([0]);
        let target = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(target, &[immediate(77)]) };
        let opaque = unsafe { roots.allocate(&mut heap, 1, BlockTag::Opaque).unwrap() };
        unsafe { write_words(opaque, &[target as Word]) };
        roots.words[0] = opaque as Word;
        let garbage = unsafe { roots.allocate(&mut heap, 3, BlockTag::Scanned).unwrap() };
        unsafe { write_words(garbage, &[immediate(1), immediate(2), immediate(3)]) };
        let fresh = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(fresh, &[immediate(4)]) };

        assert_eq!(heap.collections(), 1);
        assert_eq!(heap.used_bytes(), 2 * (HEADER_BYTES + WORD_BYTES));
    }

    #[test]
    fn rewrites_registered_host_roots() {
        let mut heap = CheneyHeap::<80, 1>::new();
        let mut roots = TestRoots::new([]);
        let target = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(target, &[immediate(55)]) };
        let mut host_value = target as Word;

        let garbage = unsafe { roots.allocate(&mut heap, 2, BlockTag::Scanned).unwrap() };
        unsafe { write_words(garbage, &[immediate(1), immediate(2)]) };
        let mut host_slots = [std::ptr::addr_of_mut!(host_value)];
        let empty = ptr::null_mut();
        let root_set = Roots {
            stack: RootRange { start: empty, end: empty },
            environment: RootRange { start: empty, end: empty },
            host: &mut host_slots,
        };
        let fresh =
            unsafe { heap.allocate(2, BlockTag::Scanned, root_set) }.expect("collection succeeds");
        unsafe { write_words(fresh, &[immediate(3), immediate(4)]) };

        assert_eq!(heap.collections(), 1);
        assert_ne!(host_value, target as Word);
        assert_eq!(unsafe { (host_value as *mut Word).read() }, immediate(55));
    }

    #[test]
    fn reports_oom_when_the_live_set_leaves_no_room() {
        let mut heap = CheneyHeap::<64, 1>::new();
        let mut roots = TestRoots::new([0, 0]);
        let first = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(first, &[immediate(0)]) };
        roots.words[0] = first as Word;
        let second = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned).unwrap() };
        unsafe { write_words(second, &[immediate(0)]) };
        roots.words[1] = second as Word;

        let error = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap_err();
        assert_eq!(error.requested_words, 1);
        assert_eq!(error.live_bytes, 2 * (HEADER_BYTES + WORD_BYTES));
        assert_eq!(error.capacity_bytes, 64);
        // Failed allocation still leaves a valid copied graph and index. Dropping
        // one root makes room for the next allocation through another collection.
        assert!(
            roots.words.iter().all(|&root| unsafe { (root as *const Word).read() == immediate(0) })
        );
        roots.words[1] = IMMEDIATE_TAG;
        let fresh = unsafe { roots.allocate(&mut heap, 1, BlockTag::Scanned) }.unwrap();
        unsafe { write_words(fresh, &[immediate(1)]) };
        assert_eq!(heap.collections(), 2);
        assert_eq!(heap.used_bytes(), 2 * (HEADER_BYTES + WORD_BYTES));
        assert_eq!(unsafe { (roots.words[0] as *const Word).read() }, immediate(0));
    }
}
