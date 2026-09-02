//! Dynamic loading and invocation of checked foreign imports.

use crate::{host::HostValue, syntax as ds};
use std::{collections::HashMap, ffi::CString, ptr::NonNull, rc::Rc};
use thiserror::Error;
use zydeco_syntax::{
    ForeignAbi, ForeignImport, ForeignLibraryName, ForeignParameter, ForeignResult,
    ForeignSymbolName, ForeignTarget, IntegerLiteral, Literal, Return,
};

type BytesUInt64ToUInt64 = unsafe extern "C" fn(*const u8, usize, u64) -> u64;

/// An environment or invocation failure at a checked foreign boundary.
#[derive(Clone, Debug, Error)]
pub enum ForeignRuntimeError {
    #[error("dynamic foreign libraries are unsupported on this platform")]
    UnsupportedPlatform,
    #[error("cannot load foreign library `{library}`: {message}")]
    OpenLibrary { library: ForeignLibraryName, message: String },
    #[error("cannot find foreign symbol `{symbol}` in `{library}`: {message}")]
    MissingSymbol { library: ForeignLibraryName, symbol: ForeignSymbolName, message: String },
    #[error("foreign symbol `{0}` received values inconsistent with its checked classifier")]
    InvalidArguments(ForeignSymbolName),
    #[error("foreign symbol `{0}` has a marshalling signature unsupported by the interpreter")]
    UnsupportedSignature(ForeignSymbolName),
}

/// Process-local dynamic libraries retained for one interpreter invocation.
pub(crate) struct ForeignRuntime {
    libraries: HashMap<ForeignLibraryName, DynamicLibrary>,
    functions: HashMap<ForeignTarget, BytesUInt64ToUInt64>,
}

impl ForeignRuntime {
    pub(crate) fn new() -> Self {
        Self { libraries: HashMap::new(), functions: HashMap::new() }
    }

    pub(crate) fn invoke(
        &mut self, import: &ForeignImport, arguments: Vec<ds::SemValue>,
    ) -> Result<ds::Computation, ForeignRuntimeError> {
        if import.target.abi != ForeignAbi::C
            || import.signature.parameters.as_slice()
                != [ForeignParameter::BorrowedBytes, ForeignParameter::UInt64]
            || import.signature.result != ForeignResult::UInt64
        {
            return Err(ForeignRuntimeError::UnsupportedSignature(import.target.symbol.clone()));
        }
        let [
            ds::SemValue::Host(HostValue::Bytes(bytes)),
            ds::SemValue::Literal(Literal::Integer(IntegerLiteral::UInt64(seed))),
        ] = arguments.as_slice()
        else {
            return Err(ForeignRuntimeError::InvalidArguments(import.target.symbol.clone()));
        };
        let function = self.function(&import.target)?;
        let result = unsafe { function(bytes.as_ptr(), bytes.len(), *seed) };
        Ok(ds::Computation::Ret(Return(Rc::new(ds::Value::Lit(Literal::Integer(
            IntegerLiteral::UInt64(result),
        ))))))
    }

    fn function(
        &mut self, target: &ForeignTarget,
    ) -> Result<BytesUInt64ToUInt64, ForeignRuntimeError> {
        if let Some(function) = self.functions.get(target).copied() {
            return Ok(function);
        }
        if !self.libraries.contains_key(&target.library) {
            let library = DynamicLibrary::open(&target.library)?;
            self.libraries.insert(target.library.clone(), library);
        }
        let library = self.libraries.get(&target.library).expect("library was inserted above");
        let symbol = library.symbol(&target.library, &target.symbol)?;
        let function =
            unsafe { std::mem::transmute::<*mut std::ffi::c_void, BytesUInt64ToUInt64>(symbol) };
        self.functions.insert(target.clone(), function);
        Ok(function)
    }
}

struct DynamicLibrary(NonNull<std::ffi::c_void>);

#[cfg(unix)]
impl DynamicLibrary {
    fn open(name: &ForeignLibraryName) -> Result<Self, ForeignRuntimeError> {
        let filename = if cfg!(target_os = "macos") {
            format!("lib{name}.dylib")
        } else {
            format!("lib{name}.so")
        };
        let filename = CString::new(filename).expect("validated library names contain no NUL");
        let handle = unsafe { unix::dlopen(filename.as_ptr(), unix::RTLD_NOW) };
        NonNull::new(handle).map(Self).ok_or_else(|| ForeignRuntimeError::OpenLibrary {
            library: name.clone(),
            message: unix::last_error(),
        })
    }

    fn symbol(
        &self, library: &ForeignLibraryName, name: &ForeignSymbolName,
    ) -> Result<*mut std::ffi::c_void, ForeignRuntimeError> {
        let name_c = CString::new(name.as_str()).expect("validated symbol names contain no NUL");
        unsafe { unix::dlerror() };
        let symbol = unsafe { unix::dlsym(self.0.as_ptr(), name_c.as_ptr()) };
        NonNull::new(symbol).map(NonNull::as_ptr).ok_or_else(|| {
            ForeignRuntimeError::MissingSymbol {
                library: library.clone(),
                symbol: name.clone(),
                message: unix::last_error(),
            }
        })
    }
}

#[cfg(not(unix))]
impl DynamicLibrary {
    fn open(_name: &ForeignLibraryName) -> Result<Self, ForeignRuntimeError> {
        Err(ForeignRuntimeError::UnsupportedPlatform)
    }

    fn symbol(
        &self, _library: &ForeignLibraryName, _name: &ForeignSymbolName,
    ) -> Result<*mut std::ffi::c_void, ForeignRuntimeError> {
        Err(ForeignRuntimeError::UnsupportedPlatform)
    }
}

#[cfg(unix)]
impl Drop for DynamicLibrary {
    fn drop(&mut self) {
        unsafe { unix::dlclose(self.0.as_ptr()) };
    }
}

#[cfg(unix)]
mod unix {
    use std::ffi::{CStr, c_char, c_int, c_void};

    pub const RTLD_NOW: c_int = 2;

    #[cfg_attr(target_os = "linux", link(name = "dl"))]
    unsafe extern "C" {
        pub fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
        pub fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
        pub fn dlclose(handle: *mut c_void) -> c_int;
        pub fn dlerror() -> *const c_char;
    }

    pub fn last_error() -> String {
        let error = unsafe { dlerror() };
        if error.is_null() {
            "dynamic loader returned no diagnostic".to_string()
        } else {
            unsafe { CStr::from_ptr(error) }.to_string_lossy().into_owned()
        }
    }
}
