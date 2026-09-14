//! Interpreter adapter for manual memory. Source unsafe contracts establish validity.
use crate::{
    host::{HostRuntime, HostValue},
    syntax::*,
};
use std::rc::Rc;
use zydeco_machine::memory::{Address, MemoryError, MemoryLayout};

pub(crate) struct MemoryRuntime;
impl MemoryRuntime {
    pub(crate) fn invoke(
        role: BuiltinValueRole, args: Vec<SemValue>, host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        use BuiltinValueRole as Role;
        let result: Result<Vec<SemValue>, MemoryError> = match role {
            | Role::MemoryNull => {
                return Ok(Return(Rc::new(
                    SemValue::Host(HostValue::Address(Address::NULL)).into(),
                ))
                .into());
            }
            | Role::MemoryOffset => {
                let address = Self::address(&args[0]).offset(Self::integer(&args[1]));
                return Ok(
                    Return(Rc::new(SemValue::Host(HostValue::Address(address)).into())).into()
                );
            }
            | Role::MemoryAllocate => {
                MemoryLayout::for_request(Self::integer(&args[0]), Self::integer(&args[1]))
                    .and_then(MemoryLayout::allocate)
                    .map(|p| vec![HostValue::Address(p).into()])
            }
            | Role::MemoryFree | Role::MemoryRetain => {
                MemoryLayout::for_request(Self::integer(&args[1]), Self::integer(&args[2]))
                    .and_then(|layout| {
                        let address = Self::address(&args[0]);
                        if role == Role::MemoryFree {
                            unsafe { layout.deallocate(address) };
                            Ok(vec![])
                        } else {
                            unsafe { host.memory.retain(address, layout) }.map(|()| vec![])
                        }
                    })
            }
            | Role::MemoryFromString => {
                let SemValue::Literal(Literal::String(string)) = &args[0] else {
                    unreachable!("typed String")
                };
                host.memory.import(string.as_str().as_bytes()).map(|p| {
                    vec![
                        HostValue::Address(p).into(),
                        Self::int_value(string.as_str().len() as i64),
                    ]
                })
            }
            | Role::MemoryToString => {
                let bytes =
                    unsafe { Self::address(&args[0]).bytes(Self::integer(&args[1]) as usize) };
                std::str::from_utf8(bytes)
                    .map(|s| vec![Literal::String(s.into()).into()])
                    .map_err(|_| MemoryError::InvalidEncoding)
            }
            | Role::MemoryLoadAddr => {
                let address = unsafe { Self::address(&args[0]).load_address() };
                return Ok(Self::resume(&args[1], [HostValue::Address(address).into()]));
            }
            | Role::MemoryStoreAddr => {
                unsafe { Self::address(&args[0]).store_address(Self::address(&args[1])) };
                return Ok(Self::resume(&args[2], []));
            }
            | Role::MemoryCopy => {
                let count = Self::integer(&args[2]) as usize;
                if count != 0 {
                    unsafe {
                        std::ptr::copy(
                            Self::address(&args[0]).pointer(),
                            Self::address(&args[1]).pointer(),
                            count,
                        )
                    };
                }
                return Ok(Self::resume(&args[3], []));
            }
            | Role::MemoryFill => {
                let SemValue::Literal(Literal::Integer(IntegerLiteral::UInt8(value))) = args[2]
                else {
                    unreachable!("typed UInt8")
                };
                let count = Self::integer(&args[1]) as usize;
                if count != 0 {
                    unsafe { Self::address(&args[0]).pointer().write_bytes(value, count) };
                }
                return Ok(Self::resume(&args[3], []));
            }
            | _ => unreachable!("memory role"),
        };
        Ok(match result {
            | Ok(values) => Self::resume(&args[args.len() - 1], values),
            | Err(error) => Self::resume(&args[args.len() - 2], [Self::int_value(error as i64)]),
        })
    }
    pub(crate) fn resume(
        success: &SemValue, arguments: impl IntoIterator<Item = SemValue>,
    ) -> Computation {
        arguments.into_iter().fold(Force(Rc::new(success.clone().into())).into(), |body, arg| {
            App(Rc::new(body), Rc::new(arg.into())).into()
        })
    }
    pub(crate) fn int_value(value: i64) -> SemValue {
        Literal::Integer(IntegerLiteral::Int64(value)).into()
    }
    fn integer(value: &SemValue) -> i64 {
        let SemValue::Literal(Literal::Integer(IntegerLiteral::Int64(value))) = value else {
            unreachable!("typed Int64")
        };
        *value
    }
    pub(crate) fn address(value: &SemValue) -> Address {
        let SemValue::Host(HostValue::Address(value)) = value else { unreachable!("typed Addr") };
        *value
    }
}
