use crate::prelude::*;

pub struct PackageAssembly {
    pub spans: t::SpanArena,
    pub scoped: sc::ScopedArena,
    pub statics: ss::StaticsArena,
    pub stack: sk::StackArena,
    pub assembly: sa::AssemblyArena,
}
