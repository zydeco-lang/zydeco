use std::io;
pub(crate) use zydeco_abi::HostIoErrorKind;
pub use zydeco_host::io::{HostIoError, HostIoRuntime, ReaderHandle, WriterHandle};
use zydeco_machine::memory::{Address, MemoryError, RetainedMemory};

/// Opaque values whose representation belongs to the interpreter runtime.
#[derive(Clone, Debug)]
pub enum HostValue {
    Address(Address),
    Reader(ReaderHandle),
    Writer(WriterHandle),
}

/// File resources owned by one interpreter invocation.
#[derive(Debug)]
pub struct HostRuntime {
    pub(crate) memory: RetainedMemory,
    pub(crate) io: HostIoRuntime,
}

impl HostRuntime {
    pub(crate) fn new() -> Self {
        Self { memory: RetainedMemory::default(), io: HostIoRuntime::new() }
    }

    pub(crate) fn import_memory(&mut self, bytes: &[u8]) -> io::Result<Address> {
        self.memory.import(bytes).map_err(HostIoError::memory)
    }

    pub(crate) fn write_memory(
        &mut self, writer: WriterHandle, address: Address, length: i64, output: &mut dyn io::Write,
        stderr: &mut dyn io::Write,
    ) -> io::Result<()> {
        let length =
            usize::try_from(length).map_err(|_| HostIoError::memory(MemoryError::InvalidLayout))?;
        let bytes = unsafe { address.bytes(length) };
        self.io.write_with(writer, output, stderr, |stream| stream.write_all(bytes))
    }

    pub(crate) fn open_reader(&mut self, path: &str) -> io::Result<ReaderHandle> {
        self.io.open_reader(path)
    }

    pub(crate) fn create_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
        self.io.create_writer(path)
    }

    pub(crate) fn append_writer(&mut self, path: &str) -> io::Result<WriterHandle> {
        self.io.append_writer(path)
    }

    pub(crate) fn close_reader(&mut self, handle: ReaderHandle) -> io::Result<()> {
        self.io.close_reader(handle)
    }

    pub(crate) fn close_writer(
        &mut self, handle: WriterHandle, output: &mut dyn io::Write, stderr: &mut dyn io::Write,
    ) -> io::Result<()> {
        self.io.close_writer_with(handle, output, stderr)
    }
}
