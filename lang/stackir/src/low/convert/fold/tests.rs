use super::*;
use crate::high::syntax::*;
use zydeco_utils::fold::Recursive;

pub(in crate::low::convert) struct Drivers;

impl Drivers {
    fn input(source: &high::StackirArena, root: high::CompuId) -> BranchJoinProgram {
        let arena = high::StackirArena {
            admin: high::AdminArena {
                defs: source.admin.defs.clone(),
                pats: source.admin.pats.clone(),
                terms: source.admin.terms.clone(),
                ..Default::default()
            },
            inner: high::StackirInnerArena {
                builtin_functions: source.inner.builtin_functions.clone(),
                protocols: source.inner.protocols.clone(),
                value_protocols: source.inner.value_protocols.clone(),
                pattern_protocols: source.inner.pattern_protocols.clone(),
                compu_protocols: source.inner.compu_protocols.clone(),
                vpats: source.inner.vpats.clone(),
                values: source.inner.values.clone(),
                stacks: source.inner.stacks.clone(),
                compus: source.inner.compus.clone(),
            },
        };
        BranchJoinProgram::try_new(high::StackirProgram::new(arena, root)).unwrap()
    }

    pub(in crate::low::convert) fn convert(
        source: &high::StackirArena, root: high::CompuId, scoped: &ScopedArena,
        statics: &StaticsArena,
    ) -> SpsLowProgram {
        let mut converter = SpsLowConverter { scoped, statics };
        let explicit = converter.run_with_driver::<Explicit>(Self::input(source, root)).unwrap();
        let recursive = converter.run_with_driver::<Recursive>(Self::input(source, root)).unwrap();
        assert_eq!(
            Snapshot::of(&explicit, source, scoped, statics),
            Snapshot::of(&recursive, source, scoped, statics)
        );
        explicit
    }

    fn returning(arena: &mut high::StackirArena) -> high::CompuId {
        let value = Triv.build(arena, None);
        let stack = Bullet.build(arena, None);
        SReturn { stack, value }.build(arena, None)
    }

    fn small_stack(test: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new().stack_size(512 * 1024).spawn(test).unwrap().join().unwrap();
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Definition {
    Source(high::DefId),
    Generated(u32),
}

impl Definition {
    fn of(id: high::DefId, source: &high::StackirArena) -> Self {
        if source.admin.defs.get(&id).is_some() {
            Self::Source(id)
        } else {
            Self::Generated(id.raw().into_u32())
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Continuation {
    stack: u32,
    result: u32,
    body: u32,
    captures: Vec<(Definition, Definition)>,
}

#[derive(Debug, PartialEq, Eq)]
struct Snapshot {
    rendered: String,
    counts: [usize; 4],
    definitions: Vec<(Definition, VarName)>,
    terms: Vec<(u32, Option<ss::TermId>)>,
    patterns: Vec<(u32, Option<ss::PatId>)>,
    value_protocols: Vec<(u32, ValueProtocol)>,
    pattern_protocols: Vec<(u32, ValueProtocol)>,
    entry_protocols: Vec<(u32, low::EntryProtocol)>,
    case_protocols: Vec<(u32, StackProtocol)>,
    continuations: Vec<Continuation>,
}

impl Snapshot {
    fn sorted<Id: ArenaId, T>(items: impl Iterator<Item = (Id, T)>) -> Vec<(u32, T)> {
        let mut items = items.map(|(id, value)| (id.raw().into_u32(), value)).collect::<Vec<_>>();
        items.sort_by_key(|(id, _)| *id);
        items
    }

    fn of(
        program: &SpsLowProgram, source: &high::StackirArena, scoped: &ScopedArena,
        statics: &StaticsArena,
    ) -> Self {
        let arena = program.arena();
        let formatter =
            crate::low::fmt::Formatter::new(&arena.admin, &arena.inner, scoped, statics);
        let mut rendered = program.root().ugly(&formatter);
        let mut definitions = arena
            .admin
            .defs
            .iter()
            .map(|(id, name)| {
                let definition = Definition::of(*id, source);
                if let Definition::Generated(slot) = definition {
                    rendered = rendered.replace(&id.concise(), &format!("[generated#{slot}]"));
                }
                (definition, name.clone())
            })
            .collect::<Vec<_>>();
        definitions.sort_by(|a, b| a.0.cmp(&b.0));
        let mut terms = arena
            .inner
            .values
            .iter()
            .map(|(id, _)| low::TermId::Value(*id))
            .chain(arena.inner.stacks.iter().map(|(id, _)| low::TermId::Stack(*id)))
            .chain(arena.inner.compus.iter().map(|(id, _)| low::TermId::Compu(*id)))
            .map(|id| {
                let slot = match id {
                    | low::TermId::Value(id) => id.raw().into_u32(),
                    | low::TermId::Stack(id) => id.raw().into_u32(),
                    | low::TermId::Compu(id) => id.raw().into_u32(),
                };
                (slot, arena.admin.terms.back(&id).copied())
            })
            .collect::<Vec<_>>();
        terms.sort_by_key(|(slot, _)| *slot);
        let mut continuations = arena
            .inner
            .continuations
            .iter()
            .map(|(stack, entry)| Continuation {
                stack: stack.raw().into_u32(),
                result: entry.result.raw().into_u32(),
                body: entry.body.raw().into_u32(),
                captures: entry
                    .captures
                    .iter()
                    .map(|capture| {
                        (
                            Definition::of(capture.source, source),
                            Definition::of(capture.binding, source),
                        )
                    })
                    .collect(),
            })
            .collect::<Vec<_>>();
        continuations.sort_by_key(|entry| entry.stack);
        Self {
            rendered,
            definitions,
            terms,
            continuations,
            counts: [
                arena.inner.vpats.len(),
                arena.inner.values.len(),
                arena.inner.stacks.len(),
                arena.inner.compus.len(),
            ],
            patterns: Self::sorted(
                arena.inner.vpats.iter().map(|(id, _)| (*id, arena.admin.pats.back(id).copied())),
            ),
            value_protocols: Self::sorted(
                arena.inner.value_protocols.iter().map(|(id, p)| (*id, p.clone())),
            ),
            pattern_protocols: Self::sorted(
                arena.inner.pattern_protocols.iter().map(|(id, p)| (*id, p.clone())),
            ),
            entry_protocols: Self::sorted(
                arena.inner.entry_protocols.iter().map(|(id, p)| (*id, p.clone())),
            ),
            case_protocols: Self::sorted(
                arena.inner.case_protocols.iter().map(|(id, p)| (*id, p.clone())),
            ),
        }
    }
}

#[test]
fn drivers_preserve_empty_and_wide_children() {
    for count in [0, 1, 32] {
        let mut arena = high::StackirArena::default();
        let arms = (0..count)
            .map(|idx| {
                let values = (0..count).map(|_| Triv.build(&mut arena, None)).collect();
                let value = VCons::new(values, ProductLayout { arity: count.max(1) })
                    .build(&mut arena, None);
                let stack = Bullet.build(&mut arena, None);
                let tail = SReturn { stack, value }.build(&mut arena, None);
                let payload: high::VPatId = Hole.build(&mut arena, None);
                let binder = Ctor(CtorIdx { idx, name: CtorName(format!("+Arm{idx}")) }, payload)
                    .build(&mut arena, None);
                Matcher { binder, tail }
            })
            .collect();
        let scrut = Hole.build(&mut arena, None);
        let tail = SCoprodMatch { scrut, arms }.build(&mut arena, None);
        let bindee = Bullet.build(&mut arena, None);
        let branch = Let { binder: Bullet, bindee, tail }.build(&mut arena, None);
        let mut coarms = vec![CoMatcher {
            dtor: Cons(DtorIdx { idx: 0, name: DtorName("branch".into()) }, Bullet),
            tail: branch,
        }];
        coarms.extend((1..count).map(|idx| CoMatcher {
            dtor: Cons(DtorIdx { idx, name: DtorName(format!("method{idx}")) }, Bullet),
            tail: Drivers::returning(&mut arena),
        }));
        let scrut = Bullet.build(&mut arena, None);
        let root = SCoMatch { scrut, arms: coarms }.build(&mut arena, None);
        Drivers::convert(&arena, root, &ScopedArena::default(), &StaticsArena::default());
    }
}

#[test]
fn drivers_preserve_origins_and_partial_protocols() {
    let mut arena = high::StackirArena::default();
    let mut origins = IdAllocator::<zydeco_statics::arena::StaticsScope>::new();
    let value_site = ss::TermId::Value(origins.alloc());
    let body_site = ss::TermId::Compu(origins.alloc());
    let closure_site = ss::TermId::Value(origins.alloc());
    let unit = Triv.build(&mut arena, Some(value_site));
    arena.inner.value_protocols.insert_new(unit, ValueProtocol::Unit);
    let stack = Bullet.build(&mut arena, None);
    let body = SReturn { stack, value: unit }.build(&mut arena, Some(body_site));
    let closure = Closure { stack: Bullet, body }.build(&mut arena, Some(closure_site));
    let protocol = StackProtocol::Continuation(Box::new(ValueProtocol::Unit));
    arena
        .inner
        .value_protocols
        .insert_new(closure, ValueProtocol::Thunk(Box::new(protocol.clone())));
    let stack = Bullet.build(&mut arena, None);
    let root = SReturn { stack, value: closure }.build(&mut arena, None);
    let output = Drivers::convert(&arena, root, &ScopedArena::default(), &StaticsArena::default());
    assert_eq!(output.arena().inner.entry_protocols.len(), 1);
    assert!(
        output
            .arena()
            .inner
            .entry_protocols
            .iter()
            .any(|(_, p)| *p == low::EntryProtocol::Closure(protocol.clone()))
    );
    assert!(
        output.arena().inner.values.iter().any(|(id, _)| output
            .arena()
            .admin
            .terms
            .back(&low::TermId::Value(*id))
            == Some(&closure_site))
    );
}

#[test]
fn deep_conversion_reconstructs_and_drops_without_native_recursion() {
    // Isolate reconstruction: low validation has independent recursive analyses.
    Drivers::small_stack(|| {
        let mut arena = high::StackirArena::default();
        let mut root = Drivers::returning(&mut arena);
        for _ in 0..4096 {
            let binder: high::VPatId = Hole.build(&mut arena, None);
            let bindee = Triv.build(&mut arena, None);
            let body = Let { binder, bindee, tail: root }.build(&mut arena, None);
            let value = Closure { stack: Bullet, body }.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            root = SReturn { stack, value }.build(&mut arena, None);
        }
        let program = BranchJoinProgram::try_new(high::StackirProgram::new(arena, root)).unwrap();
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut conversion = ClosureConversion::new(program, &scoped, &statics);
        ConversionFolder::<Explicit>::new(&mut conversion).run(root);
        assert_eq!(conversion.arena.inner.compus.len(), 4096 * 3 + 2);
        assert_eq!(conversion.arena.inner.values.len(), 4096 * 5 + 2);
    });
    Drivers::small_stack(|| {
        let mut arena = high::StackirArena::default();
        let mut stack = Bullet.build(&mut arena, None);
        for _ in 0..16384 {
            let value: high::ValueId = Triv.build(&mut arena, None);
            stack = Cons(value, stack).build(&mut arena, None);
        }
        let root = SHole(stack).build(&mut arena, None);
        let program = BranchJoinProgram::try_new(high::StackirProgram::new(arena, root)).unwrap();
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut conversion = ClosureConversion::new(program, &scoped, &statics);
        ConversionFolder::<Explicit>::new(&mut conversion).run(root);
        assert_eq!(conversion.arena.inner.stacks.len(), 16385);
        assert_eq!(conversion.arena.inner.values.len(), 16384);
    });
}
