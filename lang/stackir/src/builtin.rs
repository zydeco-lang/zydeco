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
        if let ExternalFunction::Host(role) = self
            && let Some(operation) = ComparisonOp::from_builtin(role)
        {
            return CompareBranch::make_function(arena, operation);
        }
        if matches!(self, ExternalFunction::Host(BuiltinValueRole::MemoryOffset)) {
            return AddrOffset::make_function(arena);
        }
        if let ExternalFunction::Host(role) = self
            && let Some(access) = memory::MemoryAccess::from_builtin(role)
        {
            return MemoryStep::make_function(arena, access);
        }
        let stack = Bullet.build(arena, None);
        let body = ExternCall { function: self, stack }.build(arena, None);
        Closure { stack: Bullet, body }.build(arena, None)
    }
}

impl CompareBranch {
    /// Keep primitive selection in the builtin body, including at escaping uses
    /// and when optional normalization is disabled.
    fn make_function<Arena: AsMut<StackirArena>>(
        arena: &mut Arena, operation: ComparisonOp,
    ) -> ValueId {
        use crate::protocol::ValueProtocol;
        let [first, second, when_true, when_false] =
            ["__compare_first__", "__compare_second__", "__compare_true__", "__compare_false__"]
                .map(|name| {
                    let admin = &mut arena.as_mut().admin;
                    let def = admin.fresh();
                    admin.insert_def(def, VarName(name.into()));
                    def
                });
        let [when_true_body, when_false_body] = [when_true, when_false].map(|def| {
            let thunk = def.build(arena, None);
            let stack = Bullet.build(arena, None);
            SForce { thunk, stack }.build(arena, None)
        });
        let operands = [first, second].map(|def| def.build(arena, None));
        let branch = CompareBranch {
            operation,
            operands,
            when_true: when_true_body,
            when_false: when_false_body,
        }
        .build(arena, None);
        let bindee = Bullet.build(arena, None);
        let body = Let { binder: Bullet, bindee, tail: branch }.build(arena, None);
        let scalar = ValueProtocol::Primitive(operation.operand_type());
        let body = [
            (first, scalar.clone()),
            (second, scalar),
            (when_true, ValueProtocol::Thunk(Box::default())),
            (when_false, ValueProtocol::Thunk(Box::default())),
        ]
        .into_iter()
        .rev()
        .fold(body, |tail, (def, protocol)| {
            let binder = def.build(arena, None);
            arena.as_mut().inner.pattern_protocols.insert_new(binder, protocol);
            let bindee = Bullet.build(arena, None);
            Let { binder: Cons(binder, Bullet), bindee, tail }.build(arena, None)
        });
        let function = Closure { stack: Bullet, body }.build(arena, None);
        arena.as_mut().inner.builtin_functions.insert_new(function, operation.builtin());
        function
    }
}

impl MemoryStep {
    fn make_function<Arena: AsMut<StackirArena>>(
        arena: &mut Arena, access: memory::MemoryAccess,
    ) -> ValueId {
        use crate::protocol::ValueProtocol;
        use memory::AccessKind;
        let protocol = ValueProtocol::from(access.scalar);
        let [address, value, success] =
            ["__memory_address__", "__memory_value__", "__memory_success__"].map(|name| {
                let admin = &mut arena.as_mut().admin;
                let def = admin.fresh();
                admin.insert_def(def, VarName(name.into()));
                def
            });
        let thunk = success.build(arena, None);
        let stack = Bullet.build(arena, None);
        let stack = if access.kind == AccessKind::Load {
            let value: ValueId = value.build(arena, None);
            Cons(value, stack).build(arena, None)
        } else {
            stack
        };
        let next = SForce { thunk, stack }.build(arena, None);
        let address_value = address.build(arena, None);
        let body = match access.kind {
            | AccessKind::Load => {
                let result = value.build(arena, None);
                arena.as_mut().inner.pattern_protocols.insert_new(result, protocol.clone());
                MemoryStep::Load { scalar: access.scalar, address: address_value, result, next }
            }
            | AccessKind::Store => MemoryStep::Store {
                scalar: access.scalar,
                address: address_value,
                value: value.build(arena, None),
                next,
            },
        }
        .build(arena, None);
        let operands = [
            Some((address, ValueProtocol::Address)),
            (access.kind == AccessKind::Store).then_some((value, protocol)),
            Some((success, ValueProtocol::Unknown)),
        ];
        let body = operands.into_iter().flatten().rev().fold(body, |tail, (def, protocol)| {
            let binder = def.build(arena, None);
            arena.as_mut().inner.pattern_protocols.insert_new(binder, protocol);
            let bindee = Bullet.build(arena, None);
            Let { binder: Cons(binder, Bullet), bindee, tail }.build(arena, None)
        });
        let function = Closure { stack: Bullet, body }.build(arena, None);
        arena.as_mut().inner.builtin_functions.insert_new(function, access.builtin());
        function
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
