use super::*;
use crate::protocol::{StackProtocol, ValueProtocol};
use zydeco_utils::fold::Recursive;

pub(super) struct Drivers;

impl Drivers {
    // Bounded semantic fixtures can copy immutable inputs. Each run receives a
    // fresh allocator, preserving source IDs without duplicating allocation rights.
    fn input(source: &StackirArena, root: CompuId) -> BranchJoinProgram {
        let arena = StackirArena {
            admin: AdminArena {
                defs: source.admin.defs.clone(),
                pats: source.admin.pats.clone(),
                terms: source.admin.terms.clone(),
                ..Default::default()
            },
            inner: StackirInnerArena {
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
        BranchJoinProgram::try_new(StackirProgram::new(arena, root)).unwrap()
    }

    pub(super) fn normalize(source: &StackirArena, root: CompuId) -> StackirProgram {
        let explicit = Normalizer
            .run_with_driver::<Explicit>(Self::input(source, root))
            .unwrap()
            .into_program();
        let recursive = Normalizer
            .run_with_driver::<Recursive>(Self::input(source, root))
            .unwrap()
            .into_program();
        assert_eq!(Snapshot::of(&explicit), Snapshot::of(&recursive));
        explicit
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Snapshot {
    rendered: String,
    counts: [usize; 4],
    terms: Vec<(u32, Option<ss::TermId>)>,
    patterns: Vec<(u32, Option<ss::PatId>)>,
    value_protocols: Vec<(u32, ValueProtocol)>,
    pattern_protocols: Vec<(u32, ValueProtocol)>,
    compu_protocols: Vec<(u32, StackProtocol)>,
}

impl Snapshot {
    fn sorted<Id: ArenaId, T>(items: impl Iterator<Item = (Id, T)>) -> Vec<(u32, T)> {
        let mut items = items.map(|(id, value)| (id.raw().into_u32(), value)).collect::<Vec<_>>();
        items.sort_by_key(|(slot, _)| *slot);
        items
    }

    fn of(program: &StackirProgram) -> Self {
        let arena = program.arena();
        let scoped = zydeco_surface::scoped::arena::ScopedArena::default();
        let statics = zydeco_statics::arena::StaticsArena::default();
        let formatter =
            crate::high::fmt::Formatter::new(&arena.admin, &arena.inner, &scoped, &statics);
        // Keep all raw allocation slots and source identities, normalizing only
        // the independently allocated output key spaces in generated definitions.
        let rendered = program
            .root()
            .ugly(&formatter)
            .replace(&format!("[{:?}#", program.root().key_space()), "[output#");
        let mut terms = arena
            .inner
            .values
            .iter()
            .map(|(id, _)| TermId::Value(*id))
            .chain(arena.inner.stacks.iter().map(|(id, _)| TermId::Stack(*id)))
            .chain(arena.inner.compus.iter().map(|(id, _)| TermId::Compu(*id)))
            .map(|id| {
                let slot = match id {
                    | TermId::Value(id) => id.raw().into_u32(),
                    | TermId::Stack(id) => id.raw().into_u32(),
                    | TermId::Compu(id) => id.raw().into_u32(),
                };
                (slot, arena.admin.terms.back(&id).copied())
            })
            .collect::<Vec<_>>();
        terms.sort_by_key(|(slot, _)| *slot);
        Self {
            rendered,
            counts: [
                arena.inner.vpats.len(),
                arena.inner.values.len(),
                arena.inner.stacks.len(),
                arena.inner.compus.len(),
            ],
            terms,
            patterns: Self::sorted(
                arena.inner.vpats.iter().map(|(id, _)| (*id, arena.admin.pats.back(id).copied())),
            ),
            value_protocols: Self::sorted(
                arena.inner.value_protocols.iter().map(|(id, protocol)| (*id, protocol.clone())),
            ),
            pattern_protocols: Self::sorted(
                arena.inner.pattern_protocols.iter().map(|(id, protocol)| (*id, protocol.clone())),
            ),
            compu_protocols: Self::sorted(
                arena.inner.compu_protocols.iter().map(|(id, protocol)| (*id, protocol.clone())),
            ),
        }
    }
}

#[test]
fn drivers_preserve_reconstruction_sites_and_protocols() {
    let mut arena = StackirArena::default();
    let mut origins = IdAllocator::<zydeco_statics::arena::StaticsScope>::new();
    let pattern_site = ss::PatId::Value(origins.alloc());
    let value_site = ss::TermId::Value(origins.alloc());
    let computation_site = ss::TermId::Compu(origins.alloc());
    let parameter: DefId = arena.admin.fresh();
    arena.admin.insert_def(parameter, VarName("argument".into()));
    let binder = parameter.build(&mut arena, Some(pattern_site));
    arena.inner.pattern_protocols.insert_new(binder, ValueProtocol::Unit);
    let value = parameter.build(&mut arena, Some(value_site));
    arena.inner.value_protocols.insert_new(value, ValueProtocol::Unit);
    let stack = Bullet.build(&mut arena, None);
    let tail = SReturn { stack, value }.build(&mut arena, None);
    let stack = Bullet.build(&mut arena, None);
    let body = Let { binder: Cons(binder, Bullet), bindee: stack, tail }.build(&mut arena, None);
    let param: DefId = arena.admin.fresh();
    arena.admin.insert_def(param, VarName("recursive".into()));
    let stack = Bullet.build(&mut arena, None);
    let root = SFix { param, stack, body }.build(&mut arena, Some(computation_site));
    let protocol = StackProtocol::Argument(
        Box::new(ValueProtocol::Unit),
        Box::new(StackProtocol::Continuation(Box::new(ValueProtocol::Unit))),
    );
    arena.inner.compu_protocols.insert_new(root, protocol.clone());

    let normalized = Drivers::normalize(&arena, root);
    let snapshot = Snapshot::of(&normalized);
    assert_eq!(snapshot.pattern_protocols.len(), 1);
    assert_eq!(snapshot.value_protocols.len(), 1);
    assert_eq!(snapshot.compu_protocols.len(), 1);
    assert_eq!(normalized.arena().inner.compu_protocols[&normalized.root()], protocol);
    assert_eq!(
        normalized.arena().admin.terms.back(&TermId::Compu(normalized.root())),
        Some(&computation_site)
    );
    assert!(snapshot.patterns.iter().any(|(_, site)| *site == Some(pattern_site)));
    assert!(snapshot.terms.iter().any(|(_, site)| *site == Some(value_site)));
}

#[test]
fn drivers_preserve_empty_and_wide_child_sequences() {
    enum Children {
        Values,
        ComatchArms,
        CoproductArms,
    }
    for count in [0, 1, 64] {
        for children in [Children::Values, Children::ComatchArms, Children::CoproductArms] {
            let mut arena = StackirArena::default();
            let (root, expected) = match children {
                | Children::Values => {
                    let items = (0..count).map(|_| Triv.build(&mut arena, None)).collect();
                    let value = VCons::new(items, ProductLayout { arity: count.max(1) })
                        .build(&mut arena, None);
                    let stack = Bullet.build(&mut arena, None);
                    let root = SReturn { stack, value }.build(&mut arena, None);
                    (root, [0, count + 1, 1, 1])
                }
                | Children::ComatchArms => {
                    let arms = (0..count)
                        .map(|idx| {
                            let value = Triv.build(&mut arena, None);
                            let stack = Bullet.build(&mut arena, None);
                            let tail = SReturn { stack, value }.build(&mut arena, None);
                            let tag = DtorIdx { idx, name: DtorName(format!("arm{idx}")) };
                            CoMatcher { dtor: Cons(tag, Bullet), tail }
                        })
                        .collect();
                    let scrut = Bullet.build(&mut arena, None);
                    let root = SCoMatch { scrut, arms }.build(&mut arena, None);
                    (root, [0, count, count + 1, count + 1])
                }
                | Children::CoproductArms => {
                    let arms = (0..count)
                        .map(|idx| {
                            let value = Triv.build(&mut arena, None);
                            let stack = Bullet.build(&mut arena, None);
                            let tail = SReturn { stack, value }.build(&mut arena, None);
                            let payload: VPatId = Hole.build(&mut arena, None);
                            let tag = CtorIdx { idx, name: CtorName(format!("+Arm{idx}")) };
                            let binder = Ctor(tag, payload).build(&mut arena, None);
                            Matcher { binder, tail }
                        })
                        .collect();
                    let scrut = Hole.build(&mut arena, None);
                    let tail = SCoprodMatch { scrut, arms }.build(&mut arena, None);
                    let bindee = Bullet.build(&mut arena, None);
                    let root = Let { binder: Bullet, bindee, tail }.build(&mut arena, None);
                    (root, [count * 2, count + 1, count + 1, count + 2])
                }
            };
            let normalized = Drivers::normalize(&arena, root);
            assert_eq!(Snapshot::of(&normalized).counts, expected);
        }
    }
}
