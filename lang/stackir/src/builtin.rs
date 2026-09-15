use crate::high::syntax::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostCallMode {
    /// Return one machine-word value through the platform ABI.
    Returning,
    /// Select a Zydeco closure or terminate the process.
    Control,
}

impl HostCallMode {
    pub fn for_role(role: BuiltinValueRole) -> Self {
        use HostCallMode::{Control, Returning};

        match role {
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
            | BuiltinValueRole::Int64ToInt
            | BuiltinValueRole::UInt64ToUInt
            | BuiltinValueRole::MemoryFree
            | BuiltinValueRole::MemoryRetain
            | BuiltinValueRole::MemoryFromString
            | BuiltinValueRole::MemoryToString
            | BuiltinValueRole::MemoryCopy
            | BuiltinValueRole::MemoryFill
            | BuiltinValueRole::MemoryAllocate
            | BuiltinValueRole::MemoryLoadAddr
            | BuiltinValueRole::MemoryStoreAddr
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
            | BuiltinValueRole::ArgAt
            | BuiltinValueRole::RandomInt
            | BuiltinValueRole::Exit => Control,
            | BuiltinValueRole::Int64FromInt
            | BuiltinValueRole::UInt64FromUInt
            | BuiltinValueRole::StrScalarLength
            | BuiltinValueRole::StrByteLength
            | BuiltinValueRole::StrAppend
            | BuiltinValueRole::CharToStr
            | BuiltinValueRole::CharCodepoint
            | BuiltinValueRole::MemoryNull
            | BuiltinValueRole::MemoryOffset
            | BuiltinValueRole::Stdin
            | BuiltinValueRole::Stdout
            | BuiltinValueRole::Stderr => Returning,
        }
    }
}

impl ExternalFunction {
    /// Wrap an external call in a high Stack IR closure.
    pub fn make_function<Arena>(self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackirArena>,
    {
        let stack = Bullet.build(arena, None);
        let body = ExternCall { function: self, stack }.build(arena, None);
        Closure { stack: Bullet, body }.build(arena, None)
    }
}
