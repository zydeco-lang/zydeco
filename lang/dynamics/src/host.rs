use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufReader},
    rc::Rc,
};

/// Opaque values whose representation belongs to the interpreter runtime.
#[derive(Clone, Debug)]
pub enum HostValue {
    Bytes(Rc<[u8]>),
    Reader(ReaderHandle),
    Writer(WriterHandle),
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
