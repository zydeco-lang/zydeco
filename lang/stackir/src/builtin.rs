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
        if matches!(self, ExternalFunction::Host(BuiltinValueRole::MemoryOffset)) {
            return AddrOffset::make_function(arena);
        }
        let stack = Bullet.build(arena, None);
        let body = ExternCall { function: self, stack }.build(arena, None);
        Closure { stack: Bullet, body }.build(arena, None)
    }
}

impl AddrOffset {
    /// Materialize the builtin's pure body even when optional normalization is disabled.
    fn make_function<Arena: AsMut<StackirArena>>(arena: &mut Arena) -> ValueId {
        use crate::protocol::ValueProtocol;

        let [base, displacement] = ["__address_base__", "__byte_displacement__"].map(|name| {
            let admin = &mut arena.as_mut().admin;
            let def = admin.fresh();
            admin.insert_def(def, VarName(name.into()));
            def
        });
        let value = AddrOffset {
            base: base.build(arena, None),
            displacement: displacement.build(arena, None),
        }
        .build(arena, None);
        let stack = Bullet.build(arena, None);
        let body = SReturn { stack, value }.build(arena, None);
        let body = [
            (displacement, ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))),
            (base, ValueProtocol::Address),
        ]
        .into_iter()
        .fold(body, |tail, (def, protocol)| {
            let binder = def.build(arena, None);
            arena.as_mut().inner.pattern_protocols.insert_new(binder, protocol);
            let bindee = Bullet.build(arena, None);
            Let { binder: Cons(binder, Bullet), bindee, tail }.build(arena, None)
        });
        let function = Closure { stack: Bullet, body }.build(arena, None);
        arena.as_mut().inner.builtin_functions.insert_new(function, BuiltinValueRole::MemoryOffset);
        function
    }
}
