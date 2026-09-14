use super::*;
use crate::protocol::ValueProtocol;
use zydeco_statics::arena::StaticsScope;
use zydeco_utils::fold::{Driver, Explicit, Recursive};

struct Fixture {
    source: StackirArena,
    origins: IdAllocator<StaticsScope>,
}

impl Fixture {
    fn pattern(&mut self, pattern: impl Into<ValuePattern>) -> VPatId {
        let origin = ss::PatId::Value(self.origins.alloc());
        pattern.into().build(&mut self.source, Some(origin))
    }

    fn normalization(mut self, binder: VPatId) -> Normalization {
        let value = Triv.build(&mut self.source, None);
        let stack = Bullet.build(&mut self.source, None);
        let tail = SReturn { stack, value }.build(&mut self.source, None);
        let stack = Bullet.build(&mut self.source, None);
        let root =
            Let { binder: Cons(binder, Bullet), bindee: stack, tail }.build(&mut self.source, None);
        let program = BranchJoinProgram::try_new(StackirProgram::new(self.source, root)).unwrap();
        Normalization::new(program)
    }
}

#[test]
fn drivers_preserve_pattern_children_definitions_layouts_and_evidence() {
    let mut fixture = Fixture { source: StackirArena::default(), origins: IdAllocator::new() };
    let definition: DefId = fixture.source.admin.fresh();
    fixture.source.admin.insert_def(definition, VarName("argument".into()));
    let variable = fixture.pattern(definition);
    let constructor =
        fixture.pattern(Ctor(CtorIdx { idx: 2, name: CtorName("+Item".into()) }, variable));
    let hole = fixture.pattern(Hole);
    let unit = fixture.pattern(Triv);
    fixture.source.inner.pattern_protocols.insert_new(unit, ValueProtocol::Unit);
    let alias = fixture.pattern(Alias(ConsN(vec![hole], unit)));
    let empty = fixture.pattern(VCons::new(vec![], ProductLayout { arity: 2 }));
    let root =
        fixture.pattern(VCons::new(vec![constructor, alias, empty], ProductLayout { arity: 4 }));
    let mut norm = fixture.normalization(root);
    let explicit = Explicit::run(&mut PatternFolder { norm: &mut norm }, root);
    let recursive = Recursive::run(&mut PatternFolder { norm: &mut norm }, root);
    assert_ne!(explicit, recursive);
    assert_eq!(norm.arena.inner.vpats.len(), 14);

    let spans = zydeco_surface::scoped::arena::ScopedArena::default();
    let statics = zydeco_statics::arena::StaticsArena::default();
    let formatter =
        crate::high::fmt::Formatter::new(&norm.arena.admin, &norm.arena.inner, &spans, &statics);
    assert_eq!(explicit.ugly(&formatter), recursive.ugly(&formatter));
    for output in [explicit, recursive] {
        let ValuePattern::VCons(VCons { items, layout }) = &norm.arena.inner.vpats[&output] else {
            panic!("product")
        };
        assert_eq!(layout.arity, 4);
        assert_eq!(norm.arena.admin.pats.back(&output), norm.source.admin.pats.back(&root));
        let ValuePattern::Ctor(Ctor(tag, child)) = &norm.arena.inner.vpats[&items[0]] else {
            panic!("constructor")
        };
        assert_eq!(tag.idx, 2);
        assert!(
            matches!(&norm.arena.inner.vpats[child], ValuePattern::Var(actual) if *actual == definition)
        );
        let ValuePattern::Alias(Alias(ConsN(head, tail))) = &norm.arena.inner.vpats[&items[1]]
        else {
            panic!("alias")
        };
        assert!(matches!(norm.arena.inner.vpats[&head[0]], ValuePattern::Hole(_)));
        assert!(matches!(norm.arena.inner.vpats[tail], ValuePattern::Triv(_)));
        assert_eq!(norm.arena.inner.pattern_protocols[tail], ValueProtocol::Unit);
        assert_eq!(norm.arena.admin.pats.back(tail), norm.source.admin.pats.back(&unit));
        assert!(
            matches!(&norm.arena.inner.vpats[&items[2]], ValuePattern::VCons(VCons { items, layout }) if items.is_empty() && layout.arity == 2)
        );
    }
}

#[test]
fn drivers_reject_invalid_product_layouts_before_allocating_the_parent() {
    fn reject<D: Driver>(arity: usize, expected: &str) {
        let mut fixture = Fixture { source: StackirArena::default(), origins: IdAllocator::new() };
        let hole = fixture.pattern(Hole);
        let unit = fixture.pattern(Triv);
        let root = fixture.pattern(VCons::new(vec![hole, unit], ProductLayout { arity: 2 }));
        let mut norm = fixture.normalization(root);
        let ValuePattern::VCons(product) = &mut norm.source.inner.vpats[&root] else {
            unreachable!()
        };
        product.layout.arity = arity;
        let rejected = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            D::run(&mut PatternFolder { norm: &mut norm }, root)
        }));
        let error = rejected.expect_err("an invalid product layout cannot be reconstructed");
        let message = error
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| error.downcast_ref::<&str>().copied());
        assert_eq!(message, Some(expected));
        assert_eq!(norm.arena.inner.vpats.len(), 2, "only independent children were allocated");
        assert!(
            norm.arena
                .inner
                .vpats
                .iter()
                .all(|(_, pattern)| !matches!(pattern, ValuePattern::VCons(_)))
        );
    }
    for (arity, expected) in [
        (0, "assertion failed: layout.arity > 0"),
        (1, "assertion failed: items.len() <= layout.arity"),
    ] {
        reject::<Explicit>(arity, expected);
        reject::<Recursive>(arity, expected);
    }
}
