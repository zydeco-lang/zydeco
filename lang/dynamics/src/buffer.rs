//! Interpreter adapter for the mutable storage capability.

use crate::{
    host::{HostRuntime, HostValue},
    syntax::*,
};
use std::rc::Rc;
use zydeco_machine::buffer::{BufferError, BufferHandle};

pub(crate) struct BufferRuntime;

impl BufferRuntime {
    pub(crate) fn invoke(
        role: BuiltinValueRole, args: Vec<SemValue>, host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        let result = match role {
            | BuiltinValueRole::BufferAllocate => host
                .buffers
                .allocate(Self::integer(&args[0]), Self::integer(&args[1]))
                .map(|handle| Some(HostValue::Buffer(handle).into())),
            | BuiltinValueRole::BufferWrite => host
                .buffers
                .write(Self::handle(&args[0]), Self::integer(&args[1]), Self::bytes(&args[2]))
                .map(|()| None),
            | BuiltinValueRole::BufferRead => host
                .buffers
                .read(Self::handle(&args[0]), Self::integer(&args[1]), Self::integer(&args[2]))
                .map(|bytes| Some(HostValue::Bytes(bytes).into())),
            | BuiltinValueRole::BufferFreeze => host
                .buffers
                .freeze(Self::handle(&args[0]))
                .map(|bytes| Some(HostValue::Bytes(bytes).into())),
            | BuiltinValueRole::BufferClose => {
                host.buffers.close(Self::handle(&args[0])).map(|()| None)
            }
            | _ => unreachable!("buffer adapter requires a buffer role"),
        };
        Ok(Self::finish(result, &args[args.len() - 2], &args[args.len() - 1]))
    }

    fn finish(
        result: Result<Option<SemValue>, BufferError>, error: &SemValue, success: &SemValue,
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
            unreachable!("checked buffer operation requires Int64")
        };
        *value
    }

    fn handle(value: &SemValue) -> BufferHandle {
        let SemValue::Host(HostValue::Buffer(handle)) = value else {
            unreachable!("checked buffer operation requires a Buffer")
        };
        *handle
    }

    fn bytes(value: &SemValue) -> &[u8] {
        let SemValue::Host(HostValue::Bytes(bytes)) = value else {
            unreachable!("checked buffer operation requires Bytes")
        };
        bytes.as_slice()
    }
}
