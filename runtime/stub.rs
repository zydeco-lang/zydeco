use std::{
    cell::{RefCell, UnsafeCell},
    collections::{BTreeMap, BTreeSet, HashMap},
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, Read, Write},
};

type Word = usize;

/// Every GC-managed pointer is a payload pointer: the object header sits at a
/// fixed negative offset and the raw-word ABI is unchanged.
const GC_HEADER_BYTES: usize = 16;

const KIND_PRODUCT: u8 = 0;
const KIND_STRING: u8 = 1;
const KIND_BYTES: u8 = 2;
const KIND_TRANSFER: u8 = 3;
const KIND_CLOSURE: u8 = 4;
const KIND_FREE: u8 = 5;

const FLAG_MARKED: u8 = 0b0000_0001;

#[repr(C)]
#[derive(Clone, Copy)]
struct GcHeader {
    kind: u8,
    flags: u8,
    _pad: [u8; 2],
    size_words: u32,
    /// Descriptor for products, null for host cells.
    descriptor: *const u8,
}

const _: () = {
    assert!(std::mem::size_of::<GcHeader>() == GC_HEADER_BYTES);
    assert!(std::mem::align_of::<GcHeader>() == 8);
};

impl GcHeader {
    fn new(kind: u8, size_words: u32, descriptor: *const u8) -> Self {
        Self { kind, flags: 0, _pad: [0; 2], size_words, descriptor }
    }
}

/// A host-owned value placed under the same header discipline as products.
/// `payload` stays at offset `GC_HEADER_BYTES`, so existing `&String`-style
/// dereferences keep working.
#[repr(C)]
struct HostCell<T> {
    header: GcHeader,
    payload: T,
}

impl<T> HostCell<T> {
    fn leak(kind: u8, payload: T) -> *mut T {
        let cell = Box::into_raw(Box::new(Self {
            header: GcHeader::new(kind, 0, std::ptr::null()),
            payload,
        }));
        let payload = unsafe { std::ptr::addr_of_mut!((*cell).payload) };
        HEAP.with(|heap| heap.borrow_mut().register_host(payload.cast::<u8>()));
        payload
    }
}

const HEAP_SEGMENT_BYTES: usize = 1024 * 1024;
const GC_THRESHOLD_BYTES: usize = 64 * 1024;

const CLASS_SCALAR: u8 = 0;
const CLASS_HEAP_POINTER: u8 = 1;
const CLASS_INTERIOR_POINTER: u8 = 2;
const CLASS_MAYBE_POINTER: u8 = 3;

/// Byte-format mirrors of the metadata emitted by `zydeco-assembly::gc`.
#[repr(C)]
struct GcDescriptor {
    arity: u32,
    // GcField entries follow immediately.
}

#[repr(C)]
#[derive(Clone, Copy)]
struct GcField {
    class: u8,
    _pad: [u8; 3],
    offset_words: u32,
}

#[repr(C)]
struct GcMap {
    control_count: u32,
    context_count: u32,
    // GcMapEntry entries follow immediately.
}

#[repr(C)]
#[derive(Clone, Copy)]
struct GcMapEntry {
    offset_words: u32,
    class: u8,
    _pad: [u8; 3],
    interior_offset_words: u32,
}

const _: () = {
    assert!(std::mem::size_of::<GcField>() == 8);
    assert!(std::mem::size_of::<GcMapEntry>() == 12);
};

/// One contiguous growable span of the Zydeco heap.
struct Segment {
    base: *mut u8,
    used: usize,
    capacity: usize,
}

/// Mutable runtime heap state. Single-threaded by construction.
struct HeapState {
    segments: Vec<Segment>,
    /// Free product cells keyed by exact payload size in words. Rebuilt by
    /// every sweep; free cells keep their headers so segments stay walkable.
    free: BTreeMap<u32, Vec<*mut u8>>,
    /// Cells freed by the most recent sweep, ineligible for reuse until the
    /// next collection so that stale untracked register references die first.
    free_deferred: BTreeMap<u32, Vec<*mut u8>>,
    /// Payload pointers of host cells, in allocation order.
    host_cells: Vec<*mut u8>,
    /// Address set used by the conservative `MaybePointer` fallback.
    host_addresses: BTreeSet<usize>,
    /// Sorted cell base addresses, rebuilt before each collection so that
    /// interior pointers can be resolved to their containing cell.
    cell_bases: Vec<usize>,
    allocated_since_gc: usize,
    collections: usize,
}

impl HeapState {
    const fn new() -> Self {
        Self {
            segments: Vec::new(),
            free: BTreeMap::new(),
            free_deferred: BTreeMap::new(),
            host_cells: Vec::new(),
            host_addresses: BTreeSet::new(),
            cell_bases: Vec::new(),
            allocated_since_gc: 0,
            collections: 0,
        }
    }

    fn register_host(&mut self, payload: *mut u8) {
        self.host_cells.push(payload);
        self.host_addresses.insert(payload as usize);
    }

    fn unregister_host(&mut self, payload: *mut u8) {
        self.host_addresses.remove(&(payload as usize));
        if let Some(index) = self.host_cells.iter().position(|candidate| *candidate == payload) {
            self.host_cells.swap_remove(index);
        }
    }

    fn bump(&mut self, bytes: usize) -> *mut u8 {
        for segment in self.segments.iter_mut().rev() {
            if segment.capacity - segment.used >= bytes {
                let pointer = unsafe { segment.base.add(segment.used) };
                segment.used += bytes;
                return pointer;
            }
        }
        let capacity = HEAP_SEGMENT_BYTES.max(bytes);
        let layout = std::alloc::Layout::from_size_align(capacity, 8).unwrap();
        let base = unsafe { std::alloc::alloc(layout) };
        self.segments.push(Segment { base, used: bytes, capacity });
        base
    }

    fn write_product_header(payload: *mut u8, size_words: u32, descriptor: *const u8) {
        let base = unsafe { payload.byte_sub(GC_HEADER_BYTES) };
        unsafe {
            base.cast::<GcHeader>().write(GcHeader::new(KIND_PRODUCT, size_words, descriptor))
        };
    }

    /// Reuse a dead cell of the exact requested size, or return `None`.
    fn take_free(&mut self, size_words: u32, descriptor: *const u8) -> Option<*mut u8> {
        let list = self.free.get_mut(&size_words)?;
        let payload = list.pop()?;
        if list.is_empty() {
            self.free.remove(&size_words);
        }
        Self::write_product_header(payload, size_words, descriptor);
        Some(payload)
    }

    /// Bump a fresh product cell, updating the collection trigger counter.
    fn bump_product(&mut self, size_words: u32, descriptor: *const u8) -> *mut u8 {
        let cell_bytes = GC_HEADER_BYTES + size_words as usize * 8;
        let base = self.bump(cell_bytes);
        unsafe {
            base.cast::<GcHeader>().write(GcHeader::new(KIND_PRODUCT, size_words, descriptor))
        };
        self.allocated_since_gc += cell_bytes;
        unsafe { base.add(GC_HEADER_BYTES) }
    }

    /// Legacy ABI allocation: grow without collecting.
    fn alloc_product(&mut self, size_words: usize, descriptor: *const u8) -> *mut u8 {
        assert!(size_words > 0, "product arity must be positive");
        let size_words = u32::try_from(size_words).expect("product too large for one cell");
        self.bump_product(size_words, descriptor)
    }

    fn gc_threshold_bytes() -> usize {
        std::env::var("ZYDECO_GC_THRESHOLD")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(GC_THRESHOLD_BYTES)
    }

    /// GC-aware allocation used by the amd64 backend.
    fn gc_alloc(
        &mut self, size_words: usize, descriptor: *const u8, map: *const u8, rsp: usize, rbp: usize,
    ) -> *mut u8 {
        assert!(size_words > 0, "product arity must be positive");
        let size_words = u32::try_from(size_words).expect("product too large for one cell");
        if let Some(payload) = self.take_free(size_words, descriptor) {
            return payload;
        }
        if self.allocated_since_gc >= Self::gc_threshold_bytes() {
            self.collect(map, rsp, rbp);
            if let Some(payload) = self.take_free(size_words, descriptor) {
                return payload;
            }
        }
        self.bump_product(size_words, descriptor)
    }

    fn rebuild_cell_index(&mut self) {
        self.cell_bases.clear();
        for segment in &self.segments {
            let start = segment.base as usize;
            let end = start + segment.used;
            let mut cursor = start;
            while cursor < end {
                let header = unsafe { &*(cursor as *const GcHeader) };
                let size_words = header.size_words as usize;
                self.cell_bases.push(cursor);
                cursor += GC_HEADER_BYTES + size_words * 8;
            }
        }
        self.cell_bases.sort_unstable();
    }

    /// Map an arbitrary word to the payload of the cell that contains it.
    fn resolve_segment_payload(&self, word: usize) -> Option<usize> {
        let index = self.cell_bases.partition_point(|base| *base <= word);
        let index = index.checked_sub(1)?;
        let base = self.cell_bases[index];
        let header = unsafe { &*(base as *const GcHeader) };
        let payload = base + GC_HEADER_BYTES;
        let end = payload + header.size_words as usize * 8;
        (word >= payload && word < end).then_some(payload)
    }

    fn collect(&mut self, map: *const u8, rsp: usize, rbp: usize) {
        self.free = std::mem::take(&mut self.free_deferred);
        self.rebuild_cell_index();
        let mut worklist = Vec::new();
        unsafe { self.collect_roots(map, rsp, rbp, &mut worklist) };
        // Conservative backstop until control-stack entry layouts propagate
        // through every dynamic continuation: scan the raw Zydeco control and
        // environment ranges for words that look like managed pointers.
        unsafe { self.scan_raw_root_ranges(rsp, rbp, &mut worklist) };
        while let Some(payload) = worklist.pop() {
            self.trace_known(payload, &mut worklist);
        }
        self.sweep_segments();
        self.sweep_host_cells();
        self.allocated_since_gc = 0;
        self.collections += 1;
    }

    unsafe fn scan_raw_root_ranges(&mut self, rsp: usize, _rbp: usize, worklist: &mut Vec<usize>) {
        let stack_limit = STACK_LIMIT.with(|limit| unsafe { *limit.get() });
        let mut cursor = rsp & !7;
        while cursor < stack_limit {
            let word = unsafe { *(cursor as *const usize) };
            self.trace_payload(word, worklist);
            cursor += 8;
        }
        let env_base = ENV.with(|env| unsafe { *env.get() }) as usize;
        let env_end = env_base + BUFFER_SIZE;
        let mut cursor = env_base;
        while cursor < env_end {
            let word = unsafe { *(cursor as *const usize) };
            self.trace_payload(word, worklist);
            cursor += 8;
        }
    }

    unsafe fn collect_roots(
        &mut self, map: *const u8, rsp: usize, rbp: usize, worklist: &mut Vec<usize>,
    ) {
        let Some(map) = (unsafe { map.cast::<GcMap>().as_ref() }) else { return };
        let entry = map as *const GcMap as *const u8;
        let mut entry = unsafe { entry.add(8).cast::<GcMapEntry>() };
        for _ in 0..map.control_count {
            let entry_value = unsafe { *entry };
            let word = unsafe { *((rsp + entry_value.offset_words as usize * 8) as *const usize) };
            self.scan_root(entry_value, word, worklist);
            entry = unsafe { entry.add(1) };
        }
        for _ in 0..map.context_count {
            let entry_value = unsafe { *entry };
            let word = unsafe { *((rbp + entry_value.offset_words as usize * 8) as *const usize) };
            self.scan_root(entry_value, word, worklist);
            entry = unsafe { entry.add(1) };
        }
    }

    fn scan_root(&mut self, entry: GcMapEntry, word: usize, worklist: &mut Vec<usize>) {
        match entry.class {
            | CLASS_SCALAR => {}
            | CLASS_HEAP_POINTER => self.trace_payload(word, worklist),
            | CLASS_INTERIOR_POINTER => {
                let offset = entry.interior_offset_words as usize * 8;
                self.trace_payload(word.wrapping_sub(offset), worklist);
            }
            | CLASS_MAYBE_POINTER => self.trace_payload(word, worklist),
            | _ => {}
        }
    }

    /// Enqueue `word` if it points at a managed product or host cell.
    fn trace_payload(&mut self, word: usize, worklist: &mut Vec<usize>) {
        if let Some(payload) = self.resolve_segment_payload(word) {
            worklist.push(payload);
        } else if self.host_addresses.contains(&word) {
            worklist.push(word);
        }
    }

    fn trace_known(&mut self, payload: usize, worklist: &mut Vec<usize>) {
        if self.resolve_segment_payload(payload) == Some(payload) {
            self.trace_product(payload, worklist);
        } else if self.host_addresses.contains(&payload) {
            self.trace_host(payload, worklist);
        }
    }

    fn trace_product(&mut self, payload: usize, worklist: &mut Vec<usize>) {
        let header = unsafe { &mut *((payload - GC_HEADER_BYTES) as *mut GcHeader) };
        if header.flags & FLAG_MARKED != 0 {
            return;
        }
        header.flags |= FLAG_MARKED;
        let size_words = header.size_words as usize;
        let descriptor = header.descriptor;
        let payload = payload as *const u8;
        if descriptor.is_null() {
            // Legacy allocations carry no descriptor: scan every word
            // conservatively rather than guessing at layout.
            for index in 0..size_words {
                let word = unsafe { *(payload.add(index * 8) as *const usize) };
                self.trace_payload(word, worklist);
            }
            return;
        }
        let arity = unsafe { (*descriptor.cast::<GcDescriptor>()).arity as usize };
        let fields = unsafe { descriptor.add(4).cast::<GcField>() };
        for index in 0..arity {
            let field = unsafe { *fields.add(index) };
            let word = unsafe { *(payload.add(index * 8) as *const usize) };
            match field.class {
                | CLASS_SCALAR => {}
                | CLASS_HEAP_POINTER => worklist.push(word),
                | CLASS_INTERIOR_POINTER => {
                    let offset = field.offset_words as usize * 8;
                    worklist.push(word.wrapping_sub(offset));
                }
                | CLASS_MAYBE_POINTER => self.trace_payload(word, worklist),
                | _ => {}
            }
        }
    }

    fn trace_host(&mut self, payload: usize, worklist: &mut Vec<usize>) {
        let header = unsafe { &mut *((payload - GC_HEADER_BYTES) as *mut GcHeader) };
        if header.flags & FLAG_MARKED != 0 {
            return;
        }
        header.flags |= FLAG_MARKED;
        if header.kind == KIND_TRANSFER {
            let transfer = unsafe { &*(payload as *const ControlTransfer) };
            for word in [transfer.closure, transfer.first, transfer.second] {
                self.trace_payload(word, worklist);
            }
        }
    }

    fn sweep_segments(&mut self) {
        let spans: Vec<(usize, usize)> =
            self.segments.iter().map(|segment| (segment.base as usize, segment.used)).collect();
        for (base, used) in spans {
            let end = base + used;
            let mut cursor = base;
            while cursor < end {
                let header = unsafe { &mut *(cursor as *mut GcHeader) };
                let size_words = header.size_words as usize;
                if header.kind == KIND_FREE {
                    // Already an eligible cell carried over by `free`; do not
                    // re-enqueue it in the deferred generation.
                } else if header.flags & FLAG_MARKED != 0 {
                    header.flags &= !FLAG_MARKED;
                } else if size_words == 1 {
                    // Single-word cells are pinned for now: a live reference to
                    // them can escape the compiler-emitted root maps, so
                    // reclaiming them provokes use-after-free. Fix stack-layout
                    // propagation for dynamically entered blocks first.
                    header.flags = 0;
                } else {
                    header.kind = KIND_FREE;
                    header.flags = 0;
                    header.descriptor = std::ptr::null();
                    self.free_deferred
                        .entry(header.size_words)
                        .or_default()
                        .push((cursor + GC_HEADER_BYTES) as *mut u8);
                }
                cursor += GC_HEADER_BYTES + size_words * 8;
            }
        }
    }

    fn sweep_host_cells(&mut self) {
        let mut dead = Vec::new();
        for payload in &self.host_cells {
            let header = unsafe { &mut *((*payload as usize - GC_HEADER_BYTES) as *mut GcHeader) };
            if header.flags & FLAG_MARKED != 0 {
                header.flags &= !FLAG_MARKED;
            } else {
                dead.push(*payload);
            }
        }
        for payload in dead {
            self.unregister_host(payload);
            unsafe { self.finalize_host(payload) };
        }
    }

    /// Drop the Rust box behind a dead host cell. Host closures own their
    /// argument-fold environment until resumed; dropping an unresumed closure
    /// leaks that environment, which is the pre-GC behavior.
    unsafe fn finalize_host(&self, payload: *mut u8) {
        let header = unsafe { &*(payload.byte_sub(GC_HEADER_BYTES).cast::<GcHeader>()) };
        match header.kind {
            | KIND_STRING => unsafe {
                drop(Box::from_raw(payload.byte_sub(GC_HEADER_BYTES).cast::<HostCell<String>>()));
            },
            | KIND_BYTES => unsafe {
                drop(Box::from_raw(payload.byte_sub(GC_HEADER_BYTES).cast::<HostCell<Vec<u8>>>()));
            },
            | KIND_TRANSFER => unsafe {
                drop(Box::from_raw(
                    payload.byte_sub(GC_HEADER_BYTES).cast::<HostCell<ControlTransfer>>(),
                ));
            },
            | KIND_CLOSURE => unsafe {
                drop(Box::from_raw(
                    payload.byte_sub(GC_HEADER_BYTES).cast::<HostCell<ZydecoClosure>>(),
                ));
            },
            | KIND_PRODUCT | KIND_FREE | _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

#[repr(C)]
struct ZydecoClosure {
    environment: *mut u8,
    code: *mut u8,
}

#[repr(C)]
struct ControlTransfer {
    resume: *mut u8,
    closure: Word,
    first: Word,
    second: Word,
}

impl ControlTransfer {
    fn without_arguments(closure: Word) -> Word {
        Self::leak(rust_resume_zydeco_0, closure, 0, 0)
    }

    fn with_one_argument(closure: Word, argument: Word) -> Word {
        Self::leak(rust_resume_zydeco_1, closure, argument, 0)
    }

    fn with_two_arguments(closure: Word, first: Word, second: Word) -> Word {
        Self::leak(rust_resume_zydeco_2, closure, first, second)
    }

    fn leak(resume: unsafe extern "sysv64" fn(), closure: Word, first: Word, second: Word) -> Word {
        let resume = resume as *const () as *mut u8;
        HostCell::<Self>::leak(KIND_TRANSFER, Self { resume, closure, first, second }) as Word
    }
}

struct HostString;

impl HostString {
    fn leak(string: String) -> Word {
        HostCell::<String>::leak(KIND_STRING, string) as Word
    }

    unsafe fn borrow<'a>(raw: Word) -> &'a str {
        unsafe { &*(raw as *const String) }.as_str()
    }
}

struct HostFloat64;

impl HostFloat64 {
    fn decode(word: Word) -> f64 {
        f64::from_bits(word as u64)
    }

    fn encode(value: f64) -> Word {
        value.to_bits() as Word
    }
}

struct HostFloat32;

impl HostFloat32 {
    fn decode(word: Word) -> f32 {
        f32::from_bits(word as u32)
    }

    fn encode(value: f32) -> Word {
        value.to_bits() as Word
    }
}

struct HostBytes;

impl HostBytes {
    fn leak(bytes: Vec<u8>) -> Word {
        HostCell::<Vec<u8>>::leak(KIND_BYTES, bytes) as Word
    }

    unsafe fn borrow<'a>(raw: Word) -> &'a [u8] {
        unsafe { &*(raw as *const Vec<u8>) }.as_slice()
    }
}

const STDIN_HANDLE: Word = 0;
const STDOUT_HANDLE: Word = 0;
const STDERR_HANDLE: Word = 1;

struct HostIoRuntime {
    next_reader: Word,
    next_writer: Word,
    readers: HashMap<Word, BufReader<File>>,
    writers: HashMap<Word, File>,
}

impl HostIoRuntime {
    fn new() -> Self {
        Self { next_reader: 1, next_writer: 2, readers: HashMap::new(), writers: HashMap::new() }
    }

    fn open_reader(&mut self, path: &str) -> io::Result<Word> {
        let reader = BufReader::new(File::open(path)?);
        let handle = self.next_reader;
        self.next_reader += 1;
        self.readers.insert(handle, reader);
        Ok(handle)
    }

    fn create_writer(&mut self, path: &str) -> io::Result<Word> {
        self.open_writer(path, false)
    }

    fn append_writer(&mut self, path: &str) -> io::Result<Word> {
        self.open_writer(path, true)
    }

    fn open_writer(&mut self, path: &str, append: bool) -> io::Result<Word> {
        let writer = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(!append)
            .append(append)
            .open(path)?;
        let handle = self.next_writer;
        self.next_writer += 1;
        self.writers.insert(handle, writer);
        Ok(handle)
    }

    fn read<T>(
        &mut self, handle: Word, operation: impl FnOnce(&mut dyn BufRead) -> io::Result<T>,
    ) -> io::Result<T> {
        if handle == STDIN_HANDLE {
            operation(&mut std::io::stdin().lock())
        } else {
            let reader = self.readers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
            operation(reader)
        }
    }

    fn write<T>(
        &mut self, handle: Word, operation: impl FnOnce(&mut dyn Write) -> io::Result<T>,
    ) -> io::Result<T> {
        match handle {
            | STDOUT_HANDLE => operation(&mut std::io::stdout().lock()),
            | STDERR_HANDLE => operation(&mut std::io::stderr().lock()),
            | handle => {
                let writer = self.writers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
                operation(writer)
            }
        }
    }

    fn close_reader(&mut self, handle: Word) -> io::Result<()> {
        if handle == STDIN_HANDLE || self.readers.remove(&handle).is_some() {
            Ok(())
        } else {
            Err(HostIoError::closed())
        }
    }

    fn close_writer(&mut self, handle: Word) -> io::Result<()> {
        if matches!(handle, STDOUT_HANDLE | STDERR_HANDLE) {
            self.write(handle, |writer| writer.flush())
        } else if let Some(mut writer) = self.writers.remove(&handle) {
            writer.flush()
        } else {
            Err(HostIoError::closed())
        }
    }
}

#[derive(Clone, Copy)]
#[repr(i64)]
enum HostIoErrorKind {
    NotFound = 0,
    PermissionDenied = 1,
    AlreadyExists = 2,
    InvalidInput = 3,
    InvalidData = 4,
    BrokenPipe = 5,
    Closed = 6,
    Other = 7,
}

impl HostIoErrorKind {
    fn from_error(error: &io::Error) -> Self {
        match error.kind() {
            | io::ErrorKind::NotFound => Self::NotFound,
            | io::ErrorKind::PermissionDenied => Self::PermissionDenied,
            | io::ErrorKind::AlreadyExists => Self::AlreadyExists,
            | io::ErrorKind::InvalidInput => Self::InvalidInput,
            | io::ErrorKind::InvalidData => Self::InvalidData,
            | io::ErrorKind::BrokenPipe => Self::BrokenPipe,
            | io::ErrorKind::NotConnected => Self::Closed,
            | _ => Self::Other,
        }
    }
}

struct HostIoError;

impl HostIoError {
    fn closed() -> io::Error {
        io::Error::new(io::ErrorKind::NotConnected, "I/O capability is closed")
    }
}

struct IoBranch;

impl IoBranch {
    fn error(continuation: Word, error: io::Error) -> Word {
        ControlTransfer::with_two_arguments(
            continuation,
            HostIoErrorKind::from_error(&error) as Word,
            HostString::leak(error.to_string()),
        )
    }

    fn unit(result: io::Result<()>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(()) => ControlTransfer::without_arguments(when_success),
            | Err(error) => Self::error(when_error, error),
        }
    }

    fn value(result: io::Result<Word>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(value) => ControlTransfer::with_one_argument(when_success, value),
            | Err(error) => Self::error(when_error, error),
        }
    }
}

struct Input;

impl Input {
    fn line() -> String {
        let mut line = HOST_IO
            .with(|runtime| {
                runtime.borrow_mut().read(STDIN_HANDLE, |reader| {
                    let mut line = String::new();
                    reader.read_line(&mut line)?;
                    Ok(line)
                })
            })
            .expect("legacy standard-input read failed");
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        line
    }

    fn remaining() -> String {
        HOST_IO
            .with(|runtime| {
                runtime.borrow_mut().read(STDIN_HANDLE, |reader| {
                    let mut input = String::new();
                    reader.read_to_string(&mut input)?;
                    Ok(input)
                })
            })
            .expect("legacy standard-input read failed")
    }
}

struct Branch;

impl Branch {
    fn select(condition: bool, when_true: Word, when_false: Word) -> Word {
        ControlTransfer::without_arguments(if condition { when_true } else { when_false })
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(pair: Option<(String, String)>, when_none: Word, when_some: Word) -> Word {
        match pair {
            | None => ControlTransfer::without_arguments(when_none),
            | Some((first, second)) => ControlTransfer::with_two_arguments(
                when_some,
                HostString::leak(first),
                HostString::leak(second),
            ),
        }
    }

    fn split_at(string: &str, index: i64) -> Option<(String, String)> {
        let index = usize::try_from(index).ok()?;
        let byte = match string.char_indices().nth(index) {
            | Some((byte, _)) => byte,
            | None if index == string.chars().count() => string.len(),
            | None => return None,
        };
        let (first, second) = string.split_at(byte);
        Some((first.to_string(), second.to_string()))
    }
}

struct ArgumentFold {
    arguments: std::vec::IntoIter<String>,
    when_empty: Word,
    when_item: Word,
}

impl ArgumentFold {
    fn from_process(when_empty: Word, when_item: Word) -> Self {
        let arguments = std::env::args().skip(1).collect::<Vec<_>>().into_iter();
        Self { arguments, when_empty, when_item }
    }

    fn into_thunk(self) -> Word {
        let environment = Box::into_raw(Box::new(self)).cast::<u8>();
        let code = rust_arg_fold_tail as *const () as *mut u8;
        HostCell::<ZydecoClosure>::leak(KIND_CLOSURE, ZydecoClosure { environment, code }) as Word
    }

    unsafe fn from_environment(environment: *mut u8) -> Box<Self> {
        unsafe { Box::from_raw(environment.cast::<Self>()) }
    }

    fn resume(mut self) -> Word {
        match self.arguments.next() {
            | None => ControlTransfer::without_arguments(self.when_empty),
            | Some(argument) => {
                let when_item = self.when_item;
                let tail = self.into_thunk();
                ControlTransfer::with_two_arguments(when_item, HostString::leak(argument), tail)
            }
        }
    }
}

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() -> ! {
    std::process::abort()
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| heap.borrow_mut().alloc_product(size, std::ptr::null()))
}

/// GC-aware product allocation for backends that emit stack maps.
///
/// Arguments are, in SysV order: payload size in words, object descriptor,
/// root map, the caller `rsp` captured before alignment fixups, and `rbp`.
#[unsafe(export_name = "\x01zydeco_gc_alloc")]
extern "sysv64" fn zydeco_gc_alloc(
    size: usize, descriptor: *const u8, map: *const u8, rsp: usize, rbp: usize,
) -> *mut u8 {
    HEAP.with(|heap| heap.borrow_mut().gc_alloc(size, descriptor, map, rsp, rbp))
}

unsafe extern "sysv64" {
    #[link_name = "\x01rust_arg_fold_tail"]
    fn rust_arg_fold_tail();
    #[link_name = "\x01rust_resume_zydeco_0"]
    fn rust_resume_zydeco_0();
    #[link_name = "\x01rust_resume_zydeco_1"]
    fn rust_resume_zydeco_1();
    #[link_name = "\x01rust_resume_zydeco_2"]
    fn rust_resume_zydeco_2();
}

#[unsafe(export_name = "\x01zydeco_exit")]
extern "sysv64" fn zydeco_exit(code: i64) -> ! {
    std::process::exit(code as i32);
}

/* ---------------------------------- Pure ---------------------------------- */

#[unsafe(export_name = "\x01zydeco_str_scalar_length")]
extern "sysv64" fn zydeco_str_scalar_length(string: Word) -> i64 {
    unsafe { HostString::borrow(string) }.chars().count() as i64
}

#[unsafe(export_name = "\x01zydeco_str_byte_length")]
extern "sysv64" fn zydeco_str_byte_length(string: Word) -> i64 {
    unsafe { HostString::borrow(string) }.len() as i64
}

#[unsafe(export_name = "\x01zydeco_string_literal")]
extern "sysv64" fn zydeco_string_literal(bytes: *const u8, length: usize) -> Word {
    let bytes = unsafe { std::slice::from_raw_parts(bytes, length) };
    let string = std::str::from_utf8(bytes).expect("invalid UTF-8 string literal");
    HostString::leak(string.to_string())
}

#[unsafe(export_name = "\x01zydeco_str_append")]
extern "sysv64" fn zydeco_str_append(first: Word, second: Word) -> Word {
    let first = unsafe { HostString::borrow(first) };
    let second = unsafe { HostString::borrow(second) };
    HostString::leak([first, second].concat())
}

#[unsafe(export_name = "\x01zydeco_str_get_branch")]
extern "sysv64" fn zydeco_str_get_branch(
    string: Word, index: i64, when_none: Word, when_some: Word,
) -> Word {
    let character = usize::try_from(index)
        .ok()
        .and_then(|index| unsafe { HostString::borrow(string) }.chars().nth(index));
    match character {
        | None => ControlTransfer::without_arguments(when_none),
        | Some(character) => ControlTransfer::with_one_argument(when_some, character as Word),
    }
}

macro_rules! integer_runtime {
    (
        $type:ty,
        $add:ident => $add_symbol:literal,
        $sub:ident => $sub_symbol:literal,
        $mul:ident => $mul_symbol:literal,
        $div:ident => $div_symbol:literal,
        $modulo:ident => $modulo_symbol:literal,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $add_symbol)]
        extern "sysv64" fn $add(first: Word, second: Word) -> Word {
            (first as $type).wrapping_add(second as $type) as Word
        }

        #[unsafe(export_name = $sub_symbol)]
        extern "sysv64" fn $sub(first: Word, second: Word) -> Word {
            (first as $type).wrapping_sub(second as $type) as Word
        }

        #[unsafe(export_name = $mul_symbol)]
        extern "sysv64" fn $mul(first: Word, second: Word) -> Word {
            (first as $type).wrapping_mul(second as $type) as Word
        }

        #[unsafe(export_name = $div_symbol)]
        extern "sysv64" fn $div(first: Word, second: Word) -> Word {
            (first as $type).wrapping_div(second as $type) as Word
        }

        #[unsafe(export_name = $modulo_symbol)]
        extern "sysv64" fn $modulo(first: Word, second: Word) -> Word {
            (first as $type).wrapping_rem(second as $type) as Word
        }

        #[unsafe(export_name = $eq_symbol)]
        extern "sysv64" fn $eq(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(first as $type == second as $type, when_true, when_false)
        }

        #[unsafe(export_name = $lt_symbol)]
        extern "sysv64" fn $lt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(first as $type < second as $type, when_true, when_false)
        }

        #[unsafe(export_name = $gt_symbol)]
        extern "sysv64" fn $gt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(first as $type > second as $type, when_true, when_false)
        }

        #[unsafe(export_name = $to_string_symbol)]
        extern "sysv64" fn $to_string(value: Word) -> Word {
            HostString::leak((value as $type).to_string())
        }
    };
}

integer_runtime!(
    i8,
    zydeco_int8_add => "\x01zydeco_int8_add",
    zydeco_int8_sub => "\x01zydeco_int8_sub",
    zydeco_int8_mul => "\x01zydeco_int8_mul",
    zydeco_int8_div => "\x01zydeco_int8_div",
    zydeco_int8_mod => "\x01zydeco_int8_mod",
    zydeco_int8_eq_branch => "\x01zydeco_int8_eq_branch",
    zydeco_int8_lt_branch => "\x01zydeco_int8_lt_branch",
    zydeco_int8_gt_branch => "\x01zydeco_int8_gt_branch",
    zydeco_int8_to_string => "\x01zydeco_int8_to_string"
);
integer_runtime!(
    i16,
    zydeco_int16_add => "\x01zydeco_int16_add",
    zydeco_int16_sub => "\x01zydeco_int16_sub",
    zydeco_int16_mul => "\x01zydeco_int16_mul",
    zydeco_int16_div => "\x01zydeco_int16_div",
    zydeco_int16_mod => "\x01zydeco_int16_mod",
    zydeco_int16_eq_branch => "\x01zydeco_int16_eq_branch",
    zydeco_int16_lt_branch => "\x01zydeco_int16_lt_branch",
    zydeco_int16_gt_branch => "\x01zydeco_int16_gt_branch",
    zydeco_int16_to_string => "\x01zydeco_int16_to_string"
);
integer_runtime!(
    i32,
    zydeco_int32_add => "\x01zydeco_int32_add",
    zydeco_int32_sub => "\x01zydeco_int32_sub",
    zydeco_int32_mul => "\x01zydeco_int32_mul",
    zydeco_int32_div => "\x01zydeco_int32_div",
    zydeco_int32_mod => "\x01zydeco_int32_mod",
    zydeco_int32_eq_branch => "\x01zydeco_int32_eq_branch",
    zydeco_int32_lt_branch => "\x01zydeco_int32_lt_branch",
    zydeco_int32_gt_branch => "\x01zydeco_int32_gt_branch",
    zydeco_int32_to_string => "\x01zydeco_int32_to_string"
);
integer_runtime!(
    i64,
    zydeco_int64_add => "\x01zydeco_int64_add",
    zydeco_int64_sub => "\x01zydeco_int64_sub",
    zydeco_int64_mul => "\x01zydeco_int64_mul",
    zydeco_int64_div => "\x01zydeco_int64_div",
    zydeco_int64_mod => "\x01zydeco_int64_mod",
    zydeco_int64_eq_branch => "\x01zydeco_int64_eq_branch",
    zydeco_int64_lt_branch => "\x01zydeco_int64_lt_branch",
    zydeco_int64_gt_branch => "\x01zydeco_int64_gt_branch",
    zydeco_int64_to_string => "\x01zydeco_int64_to_string"
);
integer_runtime!(
    u8,
    zydeco_uint8_add => "\x01zydeco_uint8_add",
    zydeco_uint8_sub => "\x01zydeco_uint8_sub",
    zydeco_uint8_mul => "\x01zydeco_uint8_mul",
    zydeco_uint8_div => "\x01zydeco_uint8_div",
    zydeco_uint8_mod => "\x01zydeco_uint8_mod",
    zydeco_uint8_eq_branch => "\x01zydeco_uint8_eq_branch",
    zydeco_uint8_lt_branch => "\x01zydeco_uint8_lt_branch",
    zydeco_uint8_gt_branch => "\x01zydeco_uint8_gt_branch",
    zydeco_uint8_to_string => "\x01zydeco_uint8_to_string"
);
integer_runtime!(
    u16,
    zydeco_uint16_add => "\x01zydeco_uint16_add",
    zydeco_uint16_sub => "\x01zydeco_uint16_sub",
    zydeco_uint16_mul => "\x01zydeco_uint16_mul",
    zydeco_uint16_div => "\x01zydeco_uint16_div",
    zydeco_uint16_mod => "\x01zydeco_uint16_mod",
    zydeco_uint16_eq_branch => "\x01zydeco_uint16_eq_branch",
    zydeco_uint16_lt_branch => "\x01zydeco_uint16_lt_branch",
    zydeco_uint16_gt_branch => "\x01zydeco_uint16_gt_branch",
    zydeco_uint16_to_string => "\x01zydeco_uint16_to_string"
);
integer_runtime!(
    u32,
    zydeco_uint32_add => "\x01zydeco_uint32_add",
    zydeco_uint32_sub => "\x01zydeco_uint32_sub",
    zydeco_uint32_mul => "\x01zydeco_uint32_mul",
    zydeco_uint32_div => "\x01zydeco_uint32_div",
    zydeco_uint32_mod => "\x01zydeco_uint32_mod",
    zydeco_uint32_eq_branch => "\x01zydeco_uint32_eq_branch",
    zydeco_uint32_lt_branch => "\x01zydeco_uint32_lt_branch",
    zydeco_uint32_gt_branch => "\x01zydeco_uint32_gt_branch",
    zydeco_uint32_to_string => "\x01zydeco_uint32_to_string"
);
integer_runtime!(
    u64,
    zydeco_uint64_add => "\x01zydeco_uint64_add",
    zydeco_uint64_sub => "\x01zydeco_uint64_sub",
    zydeco_uint64_mul => "\x01zydeco_uint64_mul",
    zydeco_uint64_div => "\x01zydeco_uint64_div",
    zydeco_uint64_mod => "\x01zydeco_uint64_mod",
    zydeco_uint64_eq_branch => "\x01zydeco_uint64_eq_branch",
    zydeco_uint64_lt_branch => "\x01zydeco_uint64_lt_branch",
    zydeco_uint64_gt_branch => "\x01zydeco_uint64_gt_branch",
    zydeco_uint64_to_string => "\x01zydeco_uint64_to_string"
);

macro_rules! float_runtime {
    (
        $type:ty, $codec:ident,
        $add:ident => $add_symbol:literal,
        $sub:ident => $sub_symbol:literal,
        $mul:ident => $mul_symbol:literal,
        $div:ident => $div_symbol:literal,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $add_symbol)]
        extern "sysv64" fn $add(first: Word, second: Word) -> Word {
            $codec::encode($codec::decode(first) + $codec::decode(second))
        }

        #[unsafe(export_name = $sub_symbol)]
        extern "sysv64" fn $sub(first: Word, second: Word) -> Word {
            $codec::encode($codec::decode(first) - $codec::decode(second))
        }

        #[unsafe(export_name = $mul_symbol)]
        extern "sysv64" fn $mul(first: Word, second: Word) -> Word {
            $codec::encode($codec::decode(first) * $codec::decode(second))
        }

        #[unsafe(export_name = $div_symbol)]
        extern "sysv64" fn $div(first: Word, second: Word) -> Word {
            $codec::encode($codec::decode(first) / $codec::decode(second))
        }

        #[unsafe(export_name = $eq_symbol)]
        extern "sysv64" fn $eq(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) == $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $lt_symbol)]
        extern "sysv64" fn $lt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) < $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $gt_symbol)]
        extern "sysv64" fn $gt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) > $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $to_string_symbol)]
        extern "sysv64" fn $to_string(value: Word) -> Word {
            let value: $type = $codec::decode(value);
            HostString::leak(value.to_string())
        }
    };
}

float_runtime!(
    f32, HostFloat32,
    zydeco_float32_add => "\x01zydeco_float32_add",
    zydeco_float32_sub => "\x01zydeco_float32_sub",
    zydeco_float32_mul => "\x01zydeco_float32_mul",
    zydeco_float32_div => "\x01zydeco_float32_div",
    zydeco_float32_eq_branch => "\x01zydeco_float32_eq_branch",
    zydeco_float32_lt_branch => "\x01zydeco_float32_lt_branch",
    zydeco_float32_gt_branch => "\x01zydeco_float32_gt_branch",
    zydeco_float32_to_string => "\x01zydeco_float32_to_string"
);
float_runtime!(
    f64, HostFloat64,
    zydeco_float64_add => "\x01zydeco_float64_add",
    zydeco_float64_sub => "\x01zydeco_float64_sub",
    zydeco_float64_mul => "\x01zydeco_float64_mul",
    zydeco_float64_div => "\x01zydeco_float64_div",
    zydeco_float64_eq_branch => "\x01zydeco_float64_eq_branch",
    zydeco_float64_lt_branch => "\x01zydeco_float64_lt_branch",
    zydeco_float64_gt_branch => "\x01zydeco_float64_gt_branch",
    zydeco_float64_to_string => "\x01zydeco_float64_to_string"
);

#[unsafe(export_name = "\x01zydeco_char_to_str")]
extern "sysv64" fn zydeco_char_to_str(character: Word) -> Word {
    let character = char::from_u32(character as u32).expect("invalid character");
    HostString::leak(character.to_string())
}

#[unsafe(export_name = "\x01zydeco_char_codepoint")]
extern "sysv64" fn zydeco_char_codepoint(character: Word) -> i64 {
    character as i64
}

#[unsafe(export_name = "\x01zydeco_char_from_codepoint_branch")]
extern "sysv64" fn zydeco_char_from_codepoint_branch(
    codepoint: i64, when_none: Word, when_some: Word,
) -> Word {
    match u32::try_from(codepoint).ok().and_then(char::from_u32) {
        | None => ControlTransfer::without_arguments(when_none),
        | Some(character) => ControlTransfer::with_one_argument(when_some, character as Word),
    }
}

#[unsafe(export_name = "\x01zydeco_str_parse_int_branch")]
extern "sysv64" fn zydeco_str_parse_int_branch(
    string: Word, when_none: Word, when_some: Word,
) -> Word {
    match unsafe { HostString::borrow(string) }.parse::<i64>() {
        | Err(_) => ControlTransfer::without_arguments(when_none),
        | Ok(integer) => ControlTransfer::with_one_argument(when_some, integer as Word),
    }
}

#[unsafe(export_name = "\x01zydeco_bytes_empty")]
extern "sysv64" fn zydeco_bytes_empty() -> Word {
    HostBytes::leak(Vec::new())
}

#[unsafe(export_name = "\x01zydeco_bytes_length")]
extern "sysv64" fn zydeco_bytes_length(bytes: Word) -> i64 {
    unsafe { HostBytes::borrow(bytes) }.len() as i64
}

#[unsafe(export_name = "\x01zydeco_bytes_append")]
extern "sysv64" fn zydeco_bytes_append(first: Word, second: Word) -> Word {
    HostBytes::leak(
        [unsafe { HostBytes::borrow(first) }, unsafe { HostBytes::borrow(second) }].concat(),
    )
}

#[unsafe(export_name = "\x01zydeco_bytes_from_str")]
extern "sysv64" fn zydeco_bytes_from_str(string: Word) -> Word {
    HostBytes::leak(unsafe { HostString::borrow(string) }.as_bytes().to_vec())
}

#[unsafe(export_name = "\x01zydeco_bytes_to_str_branch")]
extern "sysv64" fn zydeco_bytes_to_str_branch(
    bytes: Word, when_invalid: Word, when_valid: Word,
) -> Word {
    match std::str::from_utf8(unsafe { HostBytes::borrow(bytes) }) {
        | Err(_) => ControlTransfer::without_arguments(when_invalid),
        | Ok(string) => {
            ControlTransfer::with_one_argument(when_valid, HostString::leak(string.to_string()))
        }
    }
}

/* -------------------------------- Branches -------------------------------- */

#[unsafe(export_name = "\x01zydeco_str_eq_branch")]
extern "sysv64" fn zydeco_str_eq_branch(
    first: Word, second: Word, when_true: Word, when_false: Word,
) -> Word {
    let condition = unsafe { HostString::borrow(first) == HostString::borrow(second) };
    Branch::select(condition, when_true, when_false)
}

#[unsafe(export_name = "\x01zydeco_str_split_once_branch")]
extern "sysv64" fn zydeco_str_split_once_branch(
    string: Word, separator: Word, when_none: Word, when_some: Word,
) -> Word {
    let string = unsafe { HostString::borrow(string) };
    let separator = char::from_u32(separator as u32).expect("invalid separator");
    let pair =
        string.split_once(separator).map(|(first, second)| (first.to_string(), second.to_string()));
    OptionalPairBranch::select(pair, when_none, when_some)
}

#[unsafe(export_name = "\x01zydeco_str_split_at_branch")]
extern "sysv64" fn zydeco_str_split_at_branch(
    string: Word, index: i64, when_none: Word, when_some: Word,
) -> Word {
    let pair = OptionalPairBranch::split_at(unsafe { HostString::borrow(string) }, index);
    OptionalPairBranch::select(pair, when_none, when_some)
}

/* ----------------------------------- IO ----------------------------------- */

#[unsafe(export_name = "\x01zydeco_stdin")]
extern "sysv64" fn zydeco_stdin() -> Word {
    STDIN_HANDLE
}

#[unsafe(export_name = "\x01zydeco_stdout")]
extern "sysv64" fn zydeco_stdout() -> Word {
    STDOUT_HANDLE
}

#[unsafe(export_name = "\x01zydeco_stderr")]
extern "sysv64" fn zydeco_stderr() -> Word {
    STDERR_HANDLE
}

#[unsafe(export_name = "\x01zydeco_io_read")]
extern "sysv64" fn zydeco_io_read(
    reader: Word, count: i64, when_error: Word, when_success: Word,
) -> Word {
    let count = match u64::try_from(count) {
        | Ok(count) => count,
        | Err(_) => {
            return IoBranch::error(
                when_error,
                io::Error::new(io::ErrorKind::InvalidInput, "byte count cannot be negative"),
            );
        }
    };
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.take(count).read_to_end(&mut bytes)?;
            Ok(HostBytes::leak(bytes))
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_read_line")]
extern "sysv64" fn zydeco_io_read_line(
    reader: Word, when_error: Word, when_eof: Word, when_line: Word,
) -> Word {
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            let read = reader.read_until(b'\n', &mut bytes)?;
            if bytes.last() == Some(&b'\n') {
                bytes.pop();
                if bytes.last() == Some(&b'\r') {
                    bytes.pop();
                }
            }
            Ok((read, bytes))
        })
    });
    match result {
        | Ok((0, _)) => ControlTransfer::without_arguments(when_eof),
        | Ok((_, bytes)) => ControlTransfer::with_one_argument(when_line, HostBytes::leak(bytes)),
        | Err(error) => IoBranch::error(when_error, error),
    }
}

#[unsafe(export_name = "\x01zydeco_io_read_all")]
extern "sysv64" fn zydeco_io_read_all(reader: Word, when_error: Word, when_success: Word) -> Word {
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes)?;
            Ok(HostBytes::leak(bytes))
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_write_all")]
extern "sysv64" fn zydeco_io_write_all(
    writer: Word, bytes: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO.with(|runtime| {
        runtime
            .borrow_mut()
            .write(writer, |writer| writer.write_all(unsafe { HostBytes::borrow(bytes) }))
    });
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_flush")]
extern "sysv64" fn zydeco_io_flush(writer: Word, when_error: Word, when_success: Word) -> Word {
    let result =
        HOST_IO.with(|runtime| runtime.borrow_mut().write(writer, |writer| writer.flush()));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_reader")]
extern "sysv64" fn zydeco_io_close_reader(
    reader: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO.with(|runtime| runtime.borrow_mut().close_reader(reader));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_writer")]
extern "sysv64" fn zydeco_io_close_writer(
    writer: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO.with(|runtime| runtime.borrow_mut().close_writer(writer));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_open_reader")]
extern "sysv64" fn zydeco_fs_open_reader(path: Word, when_error: Word, when_success: Word) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().open_reader(unsafe { HostString::borrow(path) }));
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_create_writer")]
extern "sysv64" fn zydeco_fs_create_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().create_writer(unsafe { HostString::borrow(path) }));
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_append_writer")]
extern "sysv64" fn zydeco_fs_append_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().append_writer(unsafe { HostString::borrow(path) }));
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(continuation: Word) -> Word {
    let line = Input::line();
    ControlTransfer::with_one_argument(continuation, HostString::leak(line))
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int_branch")]
extern "sysv64" fn zydeco_read_line_as_int_branch(when_invalid: Word, when_valid: Word) -> Word {
    match Input::line().parse::<i64>() {
        | Ok(integer) => ControlTransfer::with_one_argument(when_valid, integer as Word),
        | Err(_) => ControlTransfer::without_arguments(when_invalid),
    }
}

#[unsafe(export_name = "\x01zydeco_read_till_eof")]
extern "sysv64" fn zydeco_read_till_eof(continuation: Word) -> Word {
    ControlTransfer::with_one_argument(continuation, HostString::leak(Input::remaining()))
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(string: Word, continuation: Word) -> Word {
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                writer.write_all(unsafe { HostString::borrow(string) }.as_bytes())?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    ControlTransfer::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_int")]
extern "sysv64" fn zydeco_write_int(integer: i64, continuation: Word) -> Word {
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                write!(writer, "{integer}")?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    ControlTransfer::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Word, continuation: Word) -> Word {
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                writeln!(writer, "{}", unsafe { HostString::borrow(line) })?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    ControlTransfer::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_arg_fold")]
extern "sysv64" fn zydeco_arg_fold(when_empty: Word, when_item: Word) -> Word {
    ArgumentFold::from_process(when_empty, when_item).resume()
}

#[unsafe(export_name = "\x01zydeco_arg_fold_resume")]
extern "sysv64" fn zydeco_arg_fold_resume(environment: *mut u8) -> Word {
    unsafe { ArgumentFold::from_environment(environment) }.resume()
}

#[unsafe(export_name = "\x01zydeco_random_int")]
extern "sysv64" fn zydeco_random_int(continuation: Word) -> Word {
    use rand::RngExt;
    let integer = rand::rng().random_range(i64::MIN..=i64::MAX);
    ControlTransfer::with_one_argument(continuation, integer as Word)
}

/* ---------------------------------- Entry --------------------------------- */

unsafe extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(environment: *mut u8) -> i64;
}

const BUFFER_SIZE: usize = 1024 * 1024;
const ALIGNMENT: usize = 8;

thread_local! {
    static ENV: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP: RefCell<HeapState> = RefCell::new(HeapState::new());
    static HOST_IO: RefCell<HostIoRuntime> = RefCell::new(HostIoRuntime::new());
    static STACK_LIMIT: UnsafeCell<usize> = const { UnsafeCell::new(0) };
}

fn current_stack_pointer() -> usize {
    #[cfg(target_arch = "x86_64")]
    {
        let pointer: usize;
        unsafe {
            std::arch::asm!("mov {}, rsp", out(reg) pointer, options(nomem, nostack, preserves_flags));
        }
        pointer
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        0
    }
}

fn init_buffer() -> *mut u8 {
    use std::alloc::{Layout, alloc};
    unsafe {
        let layout = Layout::from_size_align(BUFFER_SIZE, ALIGNMENT).unwrap();
        let pointer = alloc(layout);
        pointer.write_bytes(0, BUFFER_SIZE);
        pointer
    }
}

fn main() {
    STACK_LIMIT.with(|limit| unsafe { *limit.get() = current_stack_pointer() });
    ENV.with(|environment| unsafe {
        let environment = *environment.get();
        entry(environment);
    });
}
