use crate::prelude::*;

pub struct PackageStack {
    pub spans: t::SpanArena,
    pub scoped: sc::ScopedArena,
    pub statics: ss::StaticsArena,
    pub stackir: sk::StackirArena,
}
