#![allow(unused)]

use crate::sps::syntax::*;
use std::collections::HashMap;
use zydeco_statics::surface_syntax::ScopedArena;
use zydeco_syntax::{BuiltinValueRole, FloatOperation, IntegerOperation};

pub type BuiltinMap = HashMap<String, Builtin>;

#[derive(Clone, Debug, thiserror::Error)]
pub enum BuiltinPackageLowerError {
    #[error(transparent)]
    Plan(#[from] zydeco_statics::BuiltinPackagePlanError),
    #[error("host operation `{role}` has no Stack IR implementation")]
    UnsupportedOperation { role: BuiltinValueRole },
}

#[derive(Clone, Debug, derive_more::Display)]
#[display("{name}/{arity}")]
pub struct Builtin {
    pub role: BuiltinValueRole,
    pub name: String,
    pub arity: usize,
    pub sort: BuiltinSort,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinSort {
    Operator,
    Function(HostCallMode),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostCallMode {
    /// Return one machine-word value through the platform ABI.
    Returning,
    /// Select a Zydeco closure or terminate the process.
    Control,
}

impl Builtin {
    pub fn all() -> BuiltinMap {
        BuiltinValueRole::all().map(Self::for_known_role).map(Self::generate).collect()
    }

    fn for_known_role(role: BuiltinValueRole) -> Self {
        use BuiltinSort::Function;
        use HostCallMode::{Control, Returning};

        let mode = match role {
            | BuiltinValueRole::Integer(_, operation) => {
                if operation.is_branch() {
                    Control
                } else {
                    Returning
                }
            }
            | BuiltinValueRole::Float(_, operation) => {
                if operation.is_branch() {
                    Control
                } else {
                    Returning
                }
            }
            | BuiltinValueRole::StrSplitOnce
            | BuiltinValueRole::StrSplitAt
            | BuiltinValueRole::StrEq
            | BuiltinValueRole::StrGet
            | BuiltinValueRole::CharFromCodepoint
            | BuiltinValueRole::StrParseInt
            | BuiltinValueRole::BytesToStr
            | BuiltinValueRole::IoRead
            | BuiltinValueRole::IoReadLine
            | BuiltinValueRole::IoReadAll
            | BuiltinValueRole::IoWriteAll
            | BuiltinValueRole::IoFlush
            | BuiltinValueRole::IoCloseReader
            | BuiltinValueRole::IoCloseWriter
            | BuiltinValueRole::FsOpenReader
            | BuiltinValueRole::FsCreateWriter
            | BuiltinValueRole::FsAppendWriter
            | BuiltinValueRole::WriteStr
            | BuiltinValueRole::WriteInt
            | BuiltinValueRole::WriteLine
            | BuiltinValueRole::ReadLine
            | BuiltinValueRole::ReadLineAsInt
            | BuiltinValueRole::ReadTillEof
            | BuiltinValueRole::ArgList
            | BuiltinValueRole::RandomInt
            | BuiltinValueRole::Exit => Control,
            | _ => Returning,
        };
        Builtin { role, name: role.host_name(), arity: role.arity(), sort: Function(mode) }
    }

    fn generate(self) -> (String, Self) {
        (self.name.clone(), self)
    }

    pub fn for_role(
        builtins: &BuiltinMap, role: BuiltinValueRole,
    ) -> Result<Self, BuiltinPackageLowerError> {
        builtins
            .get(&role.host_name())
            .cloned()
            .ok_or(BuiltinPackageLowerError::UnsupportedOperation { role })
    }

    /// Turn a builtin operator definition into returning a complex CBPV value,
    /// pop parameters from stack (CBPV function), and finally wrap it with closure.
    pub fn make_operator<Arena>(&self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackirArena>,
    {
        let op = self.name.clone();
        // make fresh variables as operands
        let params: Vec<_> = (0..self.arity)
            .map(|i| {
                let param = VarName::from(format!("param_{}", i));
                let id = AsMut::<StackirArena>::as_mut(arena).admin.fresh();
                AsMut::<StackirArena>::as_mut(arena).admin.insert_def(id, param);
                id
            })
            .collect();
        let operands = params.iter().map(|def| def.build(arena, None)).collect();
        // construct the complex value
        let complex = Complex { operator: op, operands }.build(arena, None);
        // construct the computation of returning the complex value
        let stack = Bullet.build(arena, None);
        let mut tail = SReturn { stack, value: complex }.build(arena, None);
        // construct the let-argument (CBPV function) wrapping the return computation
        for def in params.into_iter().rev() {
            let vpat = def.build(arena, None);
            let binder = Cons(vpat, Bullet);
            let bindee = Bullet.build(arena, None);
            tail = Computation::LetArg(Let { binder, bindee, tail }).build(arena, None);
        }
        // construct the closure wrapping the whole computation
        Closure { stack: Bullet, body: tail }.build(arena, None)
    }

    /// Wrap a builtin function definition with closure.
    pub fn make_function<Arena>(&self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackirArena>,
    {
        let function = self.name.clone();
        let stack = Bullet.build(arena, None);
        let body = ExternCall { function, stack }.build(arena, None);
        Closure { stack: Bullet, body }.build(arena, None)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.sort, BuiltinSort::Function(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_builtin_role_has_a_stack_ir_package_implementation() {
        let builtins = Builtin::all();
        let missing = BuiltinValueRole::all()
            .filter(|role| Builtin::for_role(&builtins, *role).is_err())
            .collect::<Vec<_>>();

        assert!(missing.is_empty(), "missing Stack IR Builtin roles: {missing:?}");
    }
}
