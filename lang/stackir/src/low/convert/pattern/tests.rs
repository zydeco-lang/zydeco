use super::*;
use crate::high::syntax::*;
use zydeco_utils::fold::Explicit;

struct Fixture {
    arena: high::SpsArena,
    origins: IdAllocator<zydeco_statics::arena::StaticsScope>,
}

impl Fixture {
    fn new() -> Self {
        Self { arena: high::SpsArena::default(), origins: IdAllocator::new() }
    }

    fn pattern(&mut self, pattern: impl Into<high::ValuePattern>) -> high::VPatId {
        pattern.into().build(&mut self.arena, Some(ss::PatId::Value(self.origins.alloc())))
    }

    fn program(mut self, binder: high::VPatId) -> FragmentProgram {
        let stack = StackVar.build(&mut self.arena, None);
        let value = Triv.build(&mut self.arena, None);
        let tail = SReturn { stack, value }.build(&mut self.arena, None);
        let bindee = StackVar.build(&mut self.arena, None);
        let root =
            Let { binder: Cons(binder, StackVar), bindee, tail }.build(&mut self.arena, None);
        FragmentProgram::try_new(SpsProgram::new(self.arena, root), SpsScheme::BranchJoin).unwrap()
    }
}

#[test]
fn pattern_translation_preserves_binding_order_origins_layouts_and_protocols() {
    let mut fixture = Fixture::new();
    let def: DefId = fixture.arena.admin.fresh_def();
    fixture.arena.admin.insert_def(def, VarName("argument".into()));
    let variable = fixture.pattern(def);
    let ctor = fixture.pattern(Ctor(CtorIdx { idx: 1, name: CtorName("+Item".into()) }, variable));
    let other: DefId = fixture.arena.admin.fresh_def();
    fixture.arena.admin.insert_def(other, VarName("second".into()));
    let second = fixture.pattern(other);
    let alias = fixture.pattern(Alias(ConsN(vec![ctor], second)));
    let unit = fixture.pattern(Triv);
    fixture.arena.inner.pattern_protocols.insert_new(unit, ValueProtocol::Unit);
    let empty = fixture.pattern(VCons::new(vec![], ProductLayout { arity: 2 }));
    let root = fixture.pattern(VCons::new(vec![alias, unit, empty], ProductLayout { arity: 4 }));
    let scoped = ScopedArena::default();
    let statics = StaticsArena::default();
    let mut conversion = ClosureConversion::new(fixture.program(root), &scoped, &statics);
    let translated = Explicit::run(&mut PatternFolder { conversion: &mut conversion }, root);
    assert_eq!(conversion.arena.inner.vpats.len(), 7);
    assert_eq!(
        translated.bindings.iter().map(|(source, _)| *source).collect::<Vec<_>>(),
        [def, other]
    );
    for (source, fresh) in &translated.bindings {
        assert_ne!(fresh, source);
    }
    let output = translated.pattern;
    assert_eq!(conversion.arena.admin.pats.back(&output), conversion.source.admin.pats.back(&root));
    let low::ValuePattern::VCons(low::VCons { items, layout }) =
        &conversion.arena.inner.vpats[&output]
    else {
        panic!()
    };
    assert_eq!(layout.arity, 4);
    assert_eq!(conversion.arena.inner.pattern_protocols[&items[1]], ValueProtocol::Unit);
    assert_eq!(
        conversion.arena.admin.pats.back(&items[1]),
        conversion.source.admin.pats.back(&unit)
    );
    assert!(
        matches!(&conversion.arena.inner.vpats[&items[2]], low::ValuePattern::VCons(product) if product.items.is_empty() && product.layout.arity == 2)
    );
}

#[test]
fn deep_pattern_translation_and_destruction_use_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut fixture = Fixture::new();
            let def: DefId = fixture.arena.admin.fresh_def();
            fixture.arena.admin.insert_def(def, VarName("argument".into()));
            let mut root = fixture.pattern(def);
            for _ in 0..8192 {
                let hole = fixture.pattern(Hole);
                root = fixture.pattern(Alias(ConsN(vec![root], hole)));
            }
            let scoped = ScopedArena::default();
            let statics = StaticsArena::default();
            let mut conversion = ClosureConversion::new(fixture.program(root), &scoped, &statics);
            let output = Explicit::run(&mut PatternFolder { conversion: &mut conversion }, root);
            assert_eq!(output.bindings.len(), 1);
            assert_eq!(output.bindings[0].0, def);
            assert_eq!(conversion.arena.inner.vpats.len(), 16385);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn invalid_layouts_reject_the_parent_after_independent_children() {
    let mut fixture = Fixture::new();
    let hole = fixture.pattern(Hole);
    let unit = fixture.pattern(Triv);
    let root = fixture.pattern(VCons::new(vec![hole, unit], ProductLayout { arity: 2 }));
    let scoped = ScopedArena::default();
    let statics = StaticsArena::default();
    let mut conversion = ClosureConversion::new(fixture.program(root), &scoped, &statics);
    let high::ValuePattern::VCons(product) = &mut conversion.source.inner.vpats[&root] else {
        panic!()
    };
    product.layout.arity = 1;
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        Explicit::run(&mut PatternFolder { conversion: &mut conversion }, root)
    }));
    let message = result.err().expect("an invalid layout is rejected");
    let message = message
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| message.downcast_ref::<String>().map(String::as_str));
    assert_eq!(message, Some("assertion failed: items.len() <= layout.arity"));
    assert_eq!(conversion.arena.inner.vpats.len(), 2);
}
