//! Interpreter binding for the checked memory provider.

use crate::{
    host::{HostRuntime, HostValue},
    syntax::*,
};
use std::rc::Rc;
use zydeco_machine::{
    buffer::BufferHandle,
    memory::{AccessHandle, AddressHandle, MemoryError, Permission},
};

pub(crate) struct MemoryRuntime;

impl MemoryRuntime {
    pub(crate) fn invoke(
        role: BuiltinValueRole, args: Vec<SemValue>, host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        let arena = &mut host.buffers;
        let result = match role {
            | BuiltinValueRole::MemoryAllocate => arena
                .allocate_uninitialized(Self::integer(&args[0]), Self::integer(&args[1]))
                .map_err(MemoryError::from)
                .map(|handle| Some(HostValue::Buffer(handle).into())),
            | BuiltinValueRole::MemoryClose => {
                arena.close(Self::buffer(&args[0])).map_err(MemoryError::from).map(|()| None)
            }
            | BuiltinValueRole::MemoryFreeze => arena
                .freeze_memory(Self::buffer(&args[0]))
                .map(|handle| Some(HostValue::Access(handle).into())),
            | BuiltinValueRole::MemoryImmutableLength => arena
                .immutable_length(Self::access(&args[0]))
                .map(|value| Some(Literal::Integer(IntegerLiteral::Int64(value)).into())),
            | BuiltinValueRole::MemoryCheckWrite => arena
                .check_write(
                    Self::access(&args[0]),
                    Self::address(&args[1]),
                    Self::integer(&args[2]),
                    Self::integer(&args[3]),
                )
                .map(|()| None),
            | BuiltinValueRole::MemoryFromString => {
                let SemValue::Literal(Literal::String(string)) = &args[0] else {
                    unreachable!("checked string")
                };
                arena
                    .import_memory(string.as_str().as_bytes())
                    .map(|handle| Some(HostValue::Access(handle).into()))
            }
            | BuiltinValueRole::MemoryToString => arena
                .read_memory(
                    Self::access(&args[0]),
                    Self::address(&args[1]),
                    Self::integer(&args[2]),
                )
                .and_then(|bytes| std::str::from_utf8(bytes).map_err(|_| MemoryError::InvalidValue))
                .map(|string| Some(Literal::String(string.into()).into())),
            | BuiltinValueRole::MemoryGrant => Permission::try_from(Self::integer(&args[3]))
                .and_then(|permission| {
                    arena.grant(
                        Self::buffer(&args[0]),
                        Self::integer(&args[1]),
                        Self::integer(&args[2]),
                        permission,
                    )
                })
                .map(|handle| Some(HostValue::Access(handle).into())),
            | BuiltinValueRole::MemoryRevoke => arena.revoke(Self::access(&args[0])).map(|()| None),
            | BuiltinValueRole::MemoryBase => arena
                .base_address(Self::access(&args[0]))
                .map(|handle| Some(HostValue::Address(handle).into())),
            | BuiltinValueRole::MemoryOffset => arena
                .offset_address(
                    Self::access(&args[0]),
                    Self::address(&args[1]),
                    Self::integer(&args[2]),
                )
                .map(|handle| Some(HostValue::Address(handle).into())),
            | BuiltinValueRole::MemoryCheck => arena
                .check_access(
                    Self::access(&args[0]),
                    Self::address(&args[1]),
                    Self::integer(&args[2]),
                    Self::integer(&args[3]),
                )
                .map(|()| None),
            | BuiltinValueRole::MemoryLoadAddr => arena
                .load_address(Self::access(&args[0]), Self::address(&args[1]))
                .map(|handle| Some(HostValue::Address(handle).into())),
            | BuiltinValueRole::MemoryStoreAddr => arena
                .store_address(
                    Self::access(&args[0]),
                    Self::address(&args[1]),
                    Self::address(&args[2]),
                )
                .map(|()| None),
            | _ => unreachable!("memory adapter requires a memory role"),
        };
        Ok(Self::finish(result, &args[args.len() - 2], &args[args.len() - 1]))
    }

    pub(crate) fn finish(
        result: Result<Option<SemValue>, MemoryError>, error: &SemValue, success: &SemValue,
    ) -> Computation {
        let (continuation, argument) = match result {
            | Ok(value) => (success, value),
            | Err(code) => {
                (error, Some(Literal::Integer(IntegerLiteral::Int64(code as i64)).into()))
            }
        };
        let force: Computation = Force(Rc::new(continuation.clone().into())).into();
        match argument {
            | None => force,
            | Some(argument) => App(Rc::new(force), Rc::new(argument.into())).into(),
        }
    }

    fn integer(value: &SemValue) -> i64 {
        let SemValue::Literal(Literal::Integer(IntegerLiteral::Int64(value))) = value else {
            unreachable!("checked memory operation requires Int64")
        };
        *value
    }

    pub(crate) fn access(value: &SemValue) -> AccessHandle {
        let SemValue::Host(HostValue::Access(value)) = value else {
            unreachable!("checked memory operation requires Access")
        };
        *value
    }

    pub(crate) fn address(value: &SemValue) -> AddressHandle {
        let SemValue::Host(HostValue::Address(value)) = value else {
            unreachable!("checked memory operation requires Addr")
        };
        *value
    }

    fn buffer(value: &SemValue) -> BufferHandle {
        let SemValue::Host(HostValue::Buffer(value)) = value else {
            unreachable!("checked grant operation requires Buffer")
        };
        *value
    }
}
