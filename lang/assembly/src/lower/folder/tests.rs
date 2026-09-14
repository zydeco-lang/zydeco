use super::*;
use zydeco_stackir::arena::Construct as _;
use zydeco_utils::fold::Explicit;

struct Storage;

impl Storage {
    fn sequential_lets(depth: usize) -> usize {
        let mut arena = sk::SpsLowArena::default();
        let stack = sk::Bullet.build(&mut arena, None);
        let mut root = sk::SHole(stack).build(&mut arena, None);
        for _ in 0..depth {
            root = sk::LetStack { binder: sk::Bullet, bindee: stack, tail: root }
                .build(&mut arena, None);
        }
        let mut lo = Lowerer {
            allocator: IdAllocator::new(),
            arena: AssemblyArena::default(),
            spans: &SpanArena::default(),
            scoped: &ScopedArena::default(),
            statics: &StaticsArena::default(),
            sps_low: &arena,
            root,
            unboxing: crate::unbox::LocalUnboxing::default(),
            unboxed_var_slots: HashMap::new(),
            native_frames: false,
        };
        let mut folder = Lowering::new(&mut lo);
        let root = Explicit::run(&mut folder, Work::Start(root));
        assert_eq!(folder.lo.arena.programs.len(), 1);
        assert!(matches!(
            folder.lo.arena.programs[&root],
            Program::Terminator(Terminator::Abort(_))
        ));
        assert!(folder.continuations.is_empty());
        folder.continuations.capacity()
    }
}

#[test]
fn completed_consumers_do_not_accumulate_across_sequential_steps() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            assert_eq!(Storage::sequential_lets(32), Storage::sequential_lets(16_384));
        })
        .unwrap()
        .join()
        .unwrap();
}
