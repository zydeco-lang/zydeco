//! Encoded WebAssembly object bytes produced by the backend.

/// An encoded core WebAssembly module or relocatable object file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WasmModule {
    bytes: Vec<u8>,
}

impl WasmModule {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}
