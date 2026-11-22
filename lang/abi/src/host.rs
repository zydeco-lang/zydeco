//! Host call conventions, trap codes, and I/O error categories.

/// How a host operation returns to compiled Zydeco code.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum HostCallMode {
    /// Return one machine-word value through the platform ABI.
    Returning,
    /// Select a Zydeco closure or terminate the process.
    Control,
}

/// Hidden spare-box allocation passed after explicit source arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpareBox {
    /// A one-word opaque box pointer populated by the host operation.
    Opaque,
}

/// Fatal language traps shared by interpreter, native runtimes, and WebAssembly hosts.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(i32)]
pub enum RuntimeFailure {
    PatternMatch = 1,
    IntegerDivisionByZero = 2,
    IntegerRemainderByZero = 3,
    IntegerRange = 4,
}

impl RuntimeFailure {
    /// Returns the numeric code exchanged across host boundaries.
    pub const fn code(self) -> i32 {
        self as i32
    }

    /// Decodes a numeric host trap code into a [`RuntimeFailure`].
    pub const fn from_code(code: i32) -> Option<Self> {
        match code {
            | 1 => Some(Self::PatternMatch),
            | 2 => Some(Self::IntegerDivisionByZero),
            | 3 => Some(Self::IntegerRemainderByZero),
            | 4 => Some(Self::IntegerRange),
            | _ => None,
        }
    }

    /// Human-readable message matching Zydeco runtime diagnostics.
    pub const fn message(self) -> &'static str {
        match self {
            | Self::PatternMatch => "pattern match failed",
            | Self::IntegerDivisionByZero => "integer division by zero",
            | Self::IntegerRemainderByZero => "integer remainder by zero",
            | Self::IntegerRange => "integer exceeds the tagged payload range",
        }
    }
}

/// Stable I/O error categories shared by interpreter, native, and WASI hosts.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(i64)]
pub enum HostIoErrorKind {
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
    /// Maps a `std::io::Error` into the fixed Zydeco host I/O error code.
    #[cfg(feature = "std")]
    pub fn from_io_error(error: &std::io::Error) -> Self {
        match error.kind() {
            | std::io::ErrorKind::NotFound => Self::NotFound,
            | std::io::ErrorKind::PermissionDenied => Self::PermissionDenied,
            | std::io::ErrorKind::AlreadyExists => Self::AlreadyExists,
            | std::io::ErrorKind::InvalidInput => Self::InvalidInput,
            | std::io::ErrorKind::InvalidData => Self::InvalidData,
            | std::io::ErrorKind::BrokenPipe => Self::BrokenPipe,
            | std::io::ErrorKind::NotConnected => Self::Closed,
            | _ => Self::Other,
        }
    }
}
