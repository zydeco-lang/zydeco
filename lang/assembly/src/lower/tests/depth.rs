use super::*;
use crate::unbox::LocalUnboxing;

struct Deep;

impl Deep {
    const DEPTH: usize = 16_384;

    fn check(build: &AssemblyBuild, programs: usize) {
        assert_eq!(build.arena.programs.len(), programs);
        assert_eq!(build.arena.publication_order.len(), programs);
        assert!(build.arena.programs.get(&build.root).is_some());
        for (id, program) in &build.arena.programs {
            assert!(build.arena.contexts.get(id).is_some());
            let targets = match program {
                | Program::Instruction(_, next) => vec![next],
                | Program::Terminator(Terminator::PopBranch(PopBranch(arms))) => {
                    arms.iter().map(|(_, target)| target).collect()
                }
                | _ => Vec::new(),
            };
            for target in targets {
                assert!(build.arena.programs.get(target).is_some());
            }
        }
    }

    fn arguments_and_tags() {
        let mut fixture = Fixture::default();
        let mut stack = sk::Bullet.build(&mut fixture.arena, None);
        for index in 0..Self::DEPTH {
            let value = fixture.integer(index as i64);
            stack = sk::Cons(value, stack).build(&mut fixture.arena, None);
            let dtor = sk::DtorIdx { idx: index, name: ".observe".into() };
            stack = sk::Cons(dtor, stack).build(&mut fixture.arena, None);
        }
        let root = sk::SHole(stack).build(&mut fixture.arena, None);
        let build = fixture.lower_raw::<Explicit>(root, false, LocalUnboxing::default());
        Self::check(&build, Self::DEPTH * 2 + 1);
        let mut id = build.root;
        for index in 0..Self::DEPTH {
            let Program::Instruction(
                Instruction::PushArg(Push(Atom::Imm(Imm::Integer(IntegerLiteral::Int64(value))))),
                next,
            ) = build.arena.programs[&id]
            else {
                panic!("argument expected")
            };
            assert_eq!(value, index as i64);
            let Program::Instruction(Instruction::PushTag(Push(tag)), next) =
                &build.arena.programs[&next]
            else {
                panic!("tag expected")
            };
            assert_eq!(tag.idx, index);
            id = *next;
        }
        assert!(matches!(build.arena.programs[&id], Program::Terminator(Terminator::Abort(_))));
    }

    fn constructors(abandon: bool) {
        let mut fixture = Fixture::default();
        let mut value: sk::ValueId =
            if abandon { sk::Hole.build(&mut fixture.arena, None) } else { fixture.integer(9) };
        for _ in 0..Self::DEPTH {
            value = sk::Ctor(sk::CtorIdx { idx: 0, name: "+Nested".into() }, value)
                .build(&mut fixture.arena, None);
        }
        let ambient = sk::Bullet.build(&mut fixture.arena, None);
        let stack = sk::Cons(value, ambient).build(&mut fixture.arena, None);
        let root = sk::SHole(stack).build(&mut fixture.arena, None);
        let build = fixture.lower_raw::<Explicit>(root, false, LocalUnboxing::default());
        Self::check(&build, if abandon { 1 } else { Self::DEPTH * 2 + 2 });
    }

    fn patterns() {
        let mut fixture = Fixture::default();
        let mut unboxing = LocalUnboxing::default();
        let mut pattern = sk::Hole.build(&mut fixture.arena, None);
        for _ in 0..Self::DEPTH {
            pattern = sk::VCons::new(vec![pattern], sk::ProductLayout { arity: 1 })
                .build(&mut fixture.arena, None);
            unboxing.patterns.insert(pattern);
        }
        let bindee = fixture.integer(1);
        let tail = fixture.terminal();
        let root = sk::LetValue { binder: pattern, bindee, tail }.build(&mut fixture.arena, None);
        let build = fixture.lower_raw::<Explicit>(root, false, unboxing);
        Self::check(&build, 3);
    }

    fn branches() {
        let mut fixture = Fixture::default();
        let mut root = fixture.terminal();
        for index in 0..Self::DEPTH {
            let scrut = sk::Bullet.build(&mut fixture.arena, None);
            let dtor = sk::Cons(sk::DtorIdx { idx: index, name: ".next".into() }, sk::Bullet);
            let arms = vec![sk::CoMatcher { dtor, tail: root }];
            root = sk::SCoMatch { scrut, arms }.build(&mut fixture.arena, None);
        }
        // Every level waits for a child ID before it can construct its branch table.
        let build = fixture.lower_raw::<Explicit>(root, false, LocalUnboxing::default());
        Self::check(&build, Self::DEPTH + 1);
        assert_eq!(build.arena.symbols.len(), Self::DEPTH);
        assert_eq!(build.arena.labels.len(), Self::DEPTH);
    }

    fn continuations(native: bool) {
        let mut fixture = Fixture::default();
        let mut root = fixture.terminal();
        for _ in 0..Self::DEPTH {
            let label = fixture.definition("resume");
            let result = sk::Hole.build(&mut fixture.arena, None);
            let environment = sk::Triv.build(&mut fixture.arena, None);
            let entry = sk::EntryParameters::Continuation { result, environment };
            let code = sk::Block { label, entry, body: root }.build(&mut fixture.arena, None);
            let value: sk::ValueId = sk::Triv.build(&mut fixture.arena, None);
            let stack = sk::Bullet.build(&mut fixture.arena, None);
            let residual = sk::Cons(value, stack).build(&mut fixture.arena, None);
            let package =
                sk::ContinuationPackage { code, residual }.build(&mut fixture.arena, None);
            fixture.arena.inner.continuations.insert_new(
                package,
                sk::ContinuationEntry { result, body: root, captures: Vec::new() },
            );
            root = sk::SHole(package).build(&mut fixture.arena, None);
        }
        let build = fixture.lower_raw::<Explicit>(root, native, LocalUnboxing::default());
        Self::check(&build, Self::DEPTH * if native { 4 } else { 5 } + 1);
        assert_eq!(build.arena.symbols.len(), Self::DEPTH);
        assert_eq!(build.arena.frame_entries.len(), if native { Self::DEPTH + 1 } else { 0 });
        if native {
            assert!(matches!(build.arena.frame_entries[&build.root], crate::frames::Entry::Fresh));
            assert_eq!(
                build
                    .arena
                    .frame_entries
                    .values()
                    .filter(|entry| matches!(entry, crate::frames::Entry::Resume { .. }))
                    .count(),
                Self::DEPTH
            );
        }
    }
}

#[test]
fn explicit_folder_lowers_and_drops_deep_inputs_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            Deep::arguments_and_tags();
            Deep::constructors(false);
            Deep::constructors(true);
            Deep::patterns();
            Deep::branches();
            Deep::continuations(false);
            Deep::continuations(true);
        })
        .unwrap()
        .join()
        .unwrap();
}
