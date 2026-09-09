//! Dynamic loading and invocation of checked foreign imports.

use crate::syntax as ds;
use thiserror::Error;
use zydeco_syntax::{ForeignImport, ForeignLibraryName, ForeignSymbolName};

#[cfg(unix)]
use {
    crate::host::HostValue,
    libffi::middle::{Arg, Cif, CodePtr, Ret, Type, arg},
    std::{collections::HashMap, ffi::CString, ptr::NonNull, rc::Rc},
    zydeco_syntax::{
        ForeignAbi, ForeignComponent, ForeignResult, ForeignSignature, IntegerLiteral, IntegerType,
        Literal, Return, Triv,
    },
};

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
}

/// Process-local dynamic libraries retained for one interpreter invocation.
pub(crate) struct ForeignRuntime {
    #[cfg(unix)]
    libraries: HashMap<ForeignLibraryName, DynamicLibrary>,
    #[cfg(unix)]
    functions: HashMap<ForeignImport, ForeignFunction>,
}

impl ForeignRuntime {
    pub(crate) fn new() -> Self {
        Self {
            #[cfg(unix)]
            libraries: HashMap::new(),
            #[cfg(unix)]
            functions: HashMap::new(),
        }
    }

    #[cfg(unix)]
    pub(crate) fn invoke(
        &mut self, import: &ForeignImport, arguments: Vec<ds::SemValue>,
    ) -> Result<ds::Computation, ForeignRuntimeError> {
        // Validate before loading: malformed runtime values must never reach foreign code.
        let arguments = ForeignArguments::new(&import.signature, &arguments)
            .ok_or_else(|| ForeignRuntimeError::InvalidArguments(import.target.symbol.clone()))?;
        let function = self.function(import)?;
        let result = function.invoke(&arguments);
        Ok(ds::Computation::Ret(Return(Rc::new(result))))
    }

    #[cfg(not(unix))]
    pub(crate) fn invoke(
        &mut self, _import: &ForeignImport, _arguments: Vec<ds::SemValue>,
    ) -> Result<ds::Computation, ForeignRuntimeError> {
        Err(ForeignRuntimeError::UnsupportedPlatform)
    }

    #[cfg(unix)]
    fn function(
        &mut self, import: &ForeignImport,
    ) -> Result<&ForeignFunction, ForeignRuntimeError> {
        if !self.functions.contains_key(import) {
            let target = &import.target;
            if !self.libraries.contains_key(&target.library) {
                let library = DynamicLibrary::open(&target.library)?;
                self.libraries.insert(target.library.clone(), library);
            }
            let library = self.libraries.get(&target.library).expect("library was inserted above");
            let symbol = library.symbol(&target.library, &target.symbol)?;
            self.functions.insert(import.clone(), ForeignFunction::new(import, CodePtr(symbol)));
        }
        Ok(self.functions.get(import).expect("function was inserted above"))
    }
}

#[cfg(unix)]
struct ForeignFunction {
    code: CodePtr,
    interface: Cif,
    result: ForeignResult,
}

#[cfg(unix)]
impl ForeignFunction {
    fn new(import: &ForeignImport, code: CodePtr) -> Self {
        let arguments = import
            .signature
            .arguments()
            .map(|argument| match argument.component {
                | ForeignComponent::BytesPointer => Type::pointer(),
                | ForeignComponent::BytesLength => Type::usize(),
                | ForeignComponent::Integer(integer) => Self::integer_type(integer),
            })
            .collect::<Vec<_>>();
        let result = import.signature.result();
        let result_type = match result {
            | ForeignResult::Integer(integer) => Self::integer_type(integer),
            | ForeignResult::Unit => Type::void(),
        };
        let interface = match import.target.abi {
            | ForeignAbi::C => Cif::new(arguments, result_type),
        };
        Self { code, interface, result }
    }

    fn integer_type(integer: IntegerType) -> Type {
        match integer {
            | IntegerType::Int8 => Type::i8(),
            | IntegerType::Int16 => Type::i16(),
            | IntegerType::Int32 => Type::i32(),
            | IntegerType::Int64 => Type::i64(),
            | IntegerType::UInt8 => Type::u8(),
            | IntegerType::UInt16 => Type::u16(),
            | IntegerType::UInt32 => Type::u32(),
            | IntegerType::UInt64 => Type::u64(),
        }
    }

    fn invoke(&self, arguments: &ForeignArguments<'_>) -> ds::Value {
        let arguments = arguments.scalars.iter().map(ForeignScalar::as_arg).collect::<Vec<_>>();
        // SAFETY: the call interface and scalar storage follow the same checked signature.
        // The declaration author must ensure that the external symbol actually obeys that
        // signature and neither retains/mutates borrowed bytes nor reenters Zydeco.
        unsafe {
            let integer = match self.result {
                | ForeignResult::Unit => {
                    self.interface.call_return_into(self.code, &arguments, Ret::void());
                    return ds::Value::Triv(Triv);
                }
                // libffi's typed call reserves a register-sized result slot for narrow integers.
                | ForeignResult::Integer(integer) => match integer {
                    | IntegerType::Int8 => {
                        IntegerLiteral::Int8(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::Int16 => {
                        IntegerLiteral::Int16(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::Int32 => {
                        IntegerLiteral::Int32(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::Int64 => {
                        IntegerLiteral::Int64(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::UInt8 => {
                        IntegerLiteral::UInt8(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::UInt16 => {
                        IntegerLiteral::UInt16(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::UInt32 => {
                        IntegerLiteral::UInt32(self.interface.call(self.code, &arguments))
                    }
                    | IntegerType::UInt64 => {
                        IntegerLiteral::UInt64(self.interface.call(self.code, &arguments))
                    }
                },
            };
            ds::Value::Lit(Literal::Integer(integer))
        }
    }
}

/// Own scalar argument storage while borrowing the source values that keep byte buffers alive.
#[cfg(unix)]
struct ForeignArguments<'a> {
    _source: &'a [ds::SemValue],
    scalars: Vec<ForeignScalar>,
}

#[cfg(unix)]
impl<'a> ForeignArguments<'a> {
    fn new(signature: &ForeignSignature, source: &'a [ds::SemValue]) -> Option<Self> {
        if signature.parameters().len() != source.len() {
            return None;
        }
        let scalars = signature
            .arguments()
            .map(|argument| ForeignScalar::new(argument.component, &source[argument.parameter]))
            .collect::<Option<Vec<_>>>()?;
        Some(Self { _source: source, scalars })
    }
}

#[cfg(unix)]
enum ForeignScalar {
    Pointer(*const u8),
    Size(usize),
    Integer(IntegerLiteral),
}

#[cfg(unix)]
impl ForeignScalar {
    fn new(component: ForeignComponent, value: &ds::SemValue) -> Option<Self> {
        match (component, value) {
            | (ForeignComponent::BytesPointer, ds::SemValue::Host(HostValue::Bytes(bytes))) => {
                Some(Self::Pointer(bytes.as_slice().as_ptr()))
            }
            | (ForeignComponent::BytesLength, ds::SemValue::Host(HostValue::Bytes(bytes))) => {
                Some(Self::Size(bytes.len()))
            }
            | (
                ForeignComponent::Integer(integer),
                ds::SemValue::Literal(Literal::Integer(value)),
            ) if value.integer_type() == Some(integer) => Some(Self::Integer(*value)),
            | _ => None,
        }
    }

    fn as_arg(&self) -> Arg<'_> {
        match self {
            | Self::Pointer(value) => arg(value),
            | Self::Size(value) => arg(value),
            | Self::Integer(value) => match value {
                | IntegerLiteral::Int8(value) => arg(value),
                | IntegerLiteral::Int16(value) => arg(value),
                | IntegerLiteral::Int32(value) => arg(value),
                | IntegerLiteral::Int64(value) => arg(value),
                | IntegerLiteral::UInt8(value) => arg(value),
                | IntegerLiteral::UInt16(value) => arg(value),
                | IntegerLiteral::UInt32(value) => arg(value),
                | IntegerLiteral::UInt64(value) => arg(value),
                | IntegerLiteral::Unresolved(_) => unreachable!("arguments were validated above"),
            },
        }
    }
}

#[cfg(unix)]
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
        Self::open_filename(name, &filename)
    }

    fn open_filename(
        name: &ForeignLibraryName, filename: &std::ffi::CStr,
    ) -> Result<Self, ForeignRuntimeError> {
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

#[cfg(all(test, unix))]
mod tests;
