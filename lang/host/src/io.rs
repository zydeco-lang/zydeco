//! Shared handle table, stream helpers, and line-input primitives for host I/O.

use alloc::{string::String, vec::Vec};
use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, Read, Write},
};
use zydeco_machine::memory::MemoryError;

/// Identifier for a readable host stream capability.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ReaderHandle(pub usize);

impl ReaderHandle {
    pub const STDIN: Self = Self(0);

    pub const fn raw(self) -> usize {
        self.0
    }

    pub const fn from_raw(raw: usize) -> Self {
        Self(raw)
    }
}

/// Identifier for a writable host stream capability.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct WriterHandle(pub usize);

impl WriterHandle {
    pub const STDOUT: Self = Self(0);
    pub const STDERR: Self = Self(1);

    pub const fn raw(self) -> usize {
        self.0
    }

    pub const fn from_raw(raw: usize) -> Self {
        Self(raw)
    }
}

/// Canonical error constructors shared across hosts.
pub struct HostIoError;

impl HostIoError {
    pub fn memory(error: MemoryError) -> io::Error {
        let kind = match error {
            | MemoryError::AllocationFailed => io::ErrorKind::OutOfMemory,
            | _ => io::ErrorKind::InvalidInput,
        };
        io::Error::new(kind, error.message())
    }

    pub fn closed() -> io::Error {
        io::Error::new(io::ErrorKind::NotConnected, "I/O capability is closed")
    }

    pub fn negative_byte_count() -> io::Error {
        io::Error::new(io::ErrorKind::InvalidInput, "byte count cannot be negative")
    }
}

/// File descriptor table owned by one interpreter or compiled runtime instance.
#[derive(Debug)]
pub struct HostIoRuntime {
    next_reader: usize,
    next_writer: usize,
    readers: HashMap<ReaderHandle, BufReader<File>>,
    writers: HashMap<WriterHandle, File>,
}

impl Default for HostIoRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl HostIoRuntime {
    pub fn new() -> Self {
        Self { next_reader: 1, next_writer: 2, readers: HashMap::new(), writers: HashMap::new() }
    }

    /// Close all opened handles and restore handle counters for instance reuse.
    pub fn reset(&mut self) {
        for (_, mut writer) in self.writers.drain() {
            let _ = writer.flush();
        }
        self.readers.clear();
        self.next_reader = 1;
        self.next_writer = 2;
    }

    pub fn open_reader(&mut self, path: &str) -> io::Result<ReaderHandle> {
        let reader = BufReader::new(File::open(path)?);
        let handle = ReaderHandle(self.next_reader);
        self.next_reader += 1;
        self.readers.insert(handle, reader);
        Ok(handle)
    }

    pub fn create_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
        self.open_writer(path, false)
    }

    pub fn append_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
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

    pub fn read_with<T>(
        &mut self, handle: ReaderHandle, stdin: &mut dyn BufRead,
        operation: impl FnOnce(&mut dyn BufRead) -> io::Result<T>,
    ) -> io::Result<T> {
        if handle == ReaderHandle::STDIN {
            operation(stdin)
        } else {
            let reader = self.readers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
            operation(reader)
        }
    }

    pub fn write_with<T>(
        &mut self, handle: WriterHandle, stdout: &mut dyn Write, stderr: &mut dyn Write,
        operation: impl FnOnce(&mut dyn Write) -> io::Result<T>,
    ) -> io::Result<T> {
        match handle {
            | WriterHandle::STDOUT => operation(stdout),
            | WriterHandle::STDERR => operation(stderr),
            | handle => {
                let writer = self.writers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
                operation(writer)
            }
        }
    }

    pub fn close_reader(&mut self, handle: ReaderHandle) -> io::Result<()> {
        if handle == ReaderHandle::STDIN || self.readers.remove(&handle).is_some() {
            Ok(())
        } else {
            Err(HostIoError::closed())
        }
    }

    pub fn close_writer_with(
        &mut self, handle: WriterHandle, stdout: &mut dyn Write, stderr: &mut dyn Write,
    ) -> io::Result<()> {
        match handle {
            | WriterHandle::STDOUT => stdout.flush(),
            | WriterHandle::STDERR => stderr.flush(),
            | handle => {
                if let Some(mut writer) = self.writers.remove(&handle) {
                    writer.flush()
                } else {
                    Err(HostIoError::closed())
                }
            }
        }
    }

    pub fn read_process<T>(
        &mut self, handle: ReaderHandle, operation: impl FnOnce(&mut dyn BufRead) -> io::Result<T>,
    ) -> io::Result<T> {
        if handle == ReaderHandle::STDIN {
            operation(&mut io::stdin().lock())
        } else {
            let reader = self.readers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
            operation(reader)
        }
    }

    pub fn write_process<T>(
        &mut self, handle: WriterHandle, operation: impl FnOnce(&mut dyn Write) -> io::Result<T>,
    ) -> io::Result<T> {
        match handle {
            | WriterHandle::STDOUT => operation(&mut io::stdout().lock()),
            | WriterHandle::STDERR => operation(&mut io::stderr().lock()),
            | handle => {
                let writer = self.writers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
                operation(writer)
            }
        }
    }

    pub fn close_writer_process(&mut self, handle: WriterHandle) -> io::Result<()> {
        match handle {
            | WriterHandle::STDOUT => io::stdout().lock().flush(),
            | WriterHandle::STDERR => io::stderr().lock().flush(),
            | handle => {
                if let Some(mut writer) = self.writers.remove(&handle) {
                    writer.flush()
                } else {
                    Err(HostIoError::closed())
                }
            }
        }
    }
}

/// Shared line- and byte-reading helpers preserving CRLF vs LF semantics.
pub struct LineInput;

impl LineInput {
    pub fn read_line(reader: &mut dyn BufRead) -> io::Result<String> {
        let mut line = String::new();
        reader.read_line(&mut line)?;
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Ok(line)
    }

    pub fn read_line_bytes(reader: &mut dyn BufRead) -> io::Result<(usize, Vec<u8>)> {
        let mut bytes = Vec::new();
        let read = reader.read_until(b'\n', &mut bytes)?;
        if bytes.last() == Some(&b'\n') {
            bytes.pop();
            if bytes.last() == Some(&b'\r') {
                bytes.pop();
            }
        }
        Ok((read, bytes))
    }

    pub fn read_remaining(reader: &mut dyn BufRead) -> io::Result<String> {
        let mut input = String::new();
        reader.read_to_string(&mut input)?;
        Ok(input)
    }

    pub fn read_up_to(reader: &mut dyn BufRead, count: i64) -> io::Result<Vec<u8>> {
        let count = u64::try_from(count).map_err(|_| HostIoError::negative_byte_count())?;
        let mut bytes = Vec::new();
        reader.take(count).read_to_end(&mut bytes)?;
        Ok(bytes)
    }

    pub fn read_all(reader: &mut dyn BufRead) -> io::Result<Vec<u8>> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes)?;
        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn line_input_strips_crlf_and_lf_and_rejects_negative_counts() {
        for (source, expected) in
            [("text\n", "text"), ("text\r\n", "text"), ("text", "text"), ("text\r", "text\r")]
        {
            assert_eq!(LineInput::read_line(&mut Cursor::new(source)).unwrap(), expected);
            let (read, bytes) = LineInput::read_line_bytes(&mut Cursor::new(source)).unwrap();
            assert_eq!(read, source.len());
            assert_eq!(bytes, expected.as_bytes());
        }
        assert_eq!(LineInput::read_line_bytes(&mut Cursor::new("")).unwrap(), (0, Vec::new()));

        let error = LineInput::read_up_to(&mut Cursor::new(b"abc"), -1).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        assert_eq!(LineInput::read_up_to(&mut Cursor::new(b"abc"), 2).unwrap(), b"ab");
        assert_eq!(LineInput::read_all(&mut Cursor::new(b"abc")).unwrap(), b"abc");
    }

    #[test]
    fn runtime_handles_closed_errors_and_reset() {
        let mut io_runtime = HostIoRuntime::new();
        assert_eq!(
            io_runtime.close_reader(ReaderHandle::from_raw(99)).unwrap_err().kind(),
            io::ErrorKind::NotConnected
        );
        assert_eq!(
            io_runtime
                .close_writer_with(WriterHandle::from_raw(99), &mut io::sink(), &mut io::sink())
                .unwrap_err()
                .kind(),
            io::ErrorKind::NotConnected
        );
        io_runtime.reset();
        assert_eq!(io_runtime.next_reader, 1);
        assert_eq!(io_runtime.next_writer, 2);
    }
}
