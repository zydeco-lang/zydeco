use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufReader},
    rc::Rc,
};

/// Opaque values whose representation belongs to the interpreter runtime.
#[derive(Clone, Debug)]
pub enum HostValue {
    Bytes(SharedBytes),
    Reader(ReaderHandle),
    Writer(WriterHandle),
}

/// Immutable octet sequence backed by one shared allocation.
///
/// `slice` re-windows the same buffer instead of copying, so derived buffers
/// share memory with their source; `as_slice` is always contiguous, which keeps
/// foreign calls a plain pointer-and-length borrow.
#[derive(Clone, Debug)]
pub struct SharedBytes {
    buffer: Rc<[u8]>,
    start: usize,
    len: usize,
}

impl SharedBytes {
    pub fn from_buffer(buffer: Rc<[u8]>) -> Self {
        let len = buffer.len();
        Self { buffer, start: 0, len }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.buffer[self.start..self.start + self.len]
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Checked sub-window `[start, start + len)` of this buffer.
    pub fn slice(&self, start: usize, len: usize) -> Option<Self> {
        let end = start.checked_add(len)?;
        if start <= self.len && end <= self.len {
            Some(Self { buffer: self.buffer.clone(), start: self.start + start, len })
        } else {
            None
        }
    }
}

impl From<Vec<u8>> for SharedBytes {
    fn from(bytes: Vec<u8>) -> Self {
        Self::from_buffer(bytes.into())
    }
}

impl PartialEq for SharedBytes {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialOrd for SharedBytes {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.as_slice().cmp(other.as_slice()))
    }
}

/// Identifier for a readable runtime capability.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ReaderHandle(usize);

impl ReaderHandle {
    pub const STDIN: Self = Self(0);
}

/// Identifier for a writable runtime capability.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct WriterHandle(usize);

impl WriterHandle {
    pub const STDOUT: Self = Self(0);
    pub const STDERR: Self = Self(1);
}

/// File resources owned by one interpreter invocation.
#[derive(Debug)]
pub struct HostRuntime {
    next_reader: usize,
    next_writer: usize,
    readers: HashMap<ReaderHandle, BufReader<File>>,
    writers: HashMap<WriterHandle, File>,
}

impl HostRuntime {
    pub(crate) fn new() -> Self {
        Self { next_reader: 1, next_writer: 2, readers: HashMap::new(), writers: HashMap::new() }
    }

    pub(crate) fn open_reader(&mut self, path: &str) -> io::Result<ReaderHandle> {
        let reader = BufReader::new(File::open(path)?);
        let handle = ReaderHandle(self.next_reader);
        self.next_reader += 1;
        self.readers.insert(handle, reader);
        Ok(handle)
    }

    pub(crate) fn create_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
        self.open_writer(path, false)
    }

    pub(crate) fn append_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
        self.open_writer(path, true)
    }

    fn open_writer(&mut self, path: &str, append: bool) -> io::Result<WriterHandle> {
        let writer = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(!append)
            .append(append)
            .open(path)?;
        let handle = WriterHandle(self.next_writer);
        self.next_writer += 1;
        self.writers.insert(handle, writer);
        Ok(handle)
    }

    pub(crate) fn reader(&mut self, handle: ReaderHandle) -> io::Result<&mut BufReader<File>> {
        self.readers.get_mut(&handle).ok_or_else(HostIoError::closed)
    }

    pub(crate) fn writer(&mut self, handle: WriterHandle) -> io::Result<&mut File> {
        self.writers.get_mut(&handle).ok_or_else(HostIoError::closed)
    }

    pub(crate) fn close_reader(&mut self, handle: ReaderHandle) -> io::Result<()> {
        if handle == ReaderHandle::STDIN || self.readers.remove(&handle).is_some() {
            Ok(())
        } else {
            Err(HostIoError::closed())
        }
    }

    pub(crate) fn close_writer(&mut self, handle: WriterHandle) -> io::Result<()> {
        if matches!(handle, WriterHandle::STDOUT | WriterHandle::STDERR) {
            Ok(())
        } else if let Some(mut writer) = self.writers.remove(&handle) {
            use std::io::Write;
            writer.flush()
        } else {
            Err(HostIoError::closed())
        }
    }
}

/// Stable error categories shared by the interpreter and native runtime ABI.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub(crate) enum HostIoErrorKind {
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
    pub(crate) fn from_error(error: &io::Error) -> Self {
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
