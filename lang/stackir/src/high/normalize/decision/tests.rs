use super::*;
use zydeco_utils::fold::Recursive;

struct Fixture;

impl Fixture {
    fn decide(arena: &StackirInnerArena, binder: VPatId, known: &KnownValue) -> Option<bool> {
        let input = (binder, KnownView::Value(known));
        let explicit = Explicit::run(&mut DecisionFolder { source: arena }, input);
        let recursive = Recursive::run(&mut DecisionFolder { source: arena }, input);
        assert_eq!(explicit, recursive);
        explicit
    }

    fn tag() -> CtorIdx {
        CtorIdx { idx: 0, name: CtorName("known".into()) }
    }

    fn other() -> CtorIdx {
        CtorIdx { idx: 1, name: CtorName("other".into()) }
    }
}

#[test]
fn decisions_preserve_the_first_uncertain_or_rejected_alias_member() {
    let mut arena = StackirArena::default();
    let hole = Hole.build(&mut arena, None);
    let nested = Ctor(Fixture::tag(), hole).build(&mut arena, None);
    let uncertain = Ctor(Fixture::tag(), nested).build(&mut arena, None);
    let rejected = Ctor(Fixture::other(), hole).build(&mut arena, None);
    let known = KnownValue::Constructor(Fixture::tag(), Rc::default());
    let first_unknown =
        Alias(ConsN::from_vec(vec![uncertain, rejected]).unwrap()).build(&mut arena, None);
    let first_false =
        Alias(ConsN::from_vec(vec![rejected, uncertain]).unwrap()).build(&mut arena, None);
    assert_eq!(Fixture::decide(&arena.inner, first_unknown, &known), None);
    assert_eq!(Fixture::decide(&arena.inner, first_false, &known), Some(false));

    let accepted = Ctor(Fixture::tag(), hole).build(&mut arena, None);
    let alias = Alias(ConsN::from_vec(vec![accepted, hole]).unwrap()).build(&mut arena, None);
    assert_eq!(Fixture::decide(&arena.inner, alias, &known), Some(true));
    assert_eq!(Fixture::decide(&arena.inner, accepted, &KnownValue::Unknown), None);
}

#[test]
fn suffix_decisions_borrow_physical_fields_and_preserve_shape_uncertainty() {
    let mut arena = StackirArena::default();
    let hole = Hole.build(&mut arena, None);
    let first = Ctor(Fixture::tag(), hole).build(&mut arena, None);
    let second = Ctor(Fixture::other(), hole).build(&mut arena, None);
    let suffix = VCons::new(vec![second, hole], ProductLayout { arity: 2 }).build(&mut arena, None);
    let pattern =
        VCons::new(vec![first, suffix], ProductLayout { arity: 3 }).build(&mut arena, None);
    let fields = vec![
        Rc::new(KnownValue::Constructor(Fixture::tag(), Rc::default())),
        Rc::new(KnownValue::Constructor(Fixture::other(), Rc::default())),
        Rc::default(),
    ];
    let known = KnownValue::Product(fields);
    assert_eq!(Fixture::decide(&arena.inner, pattern, &known), Some(true));
    let KnownValue::Product(fields) = &known else { unreachable!() };
    assert!(fields.iter().all(|field| Rc::strong_count(field) == 1));
    let wrong_shape = VCons::new(vec![hole], ProductLayout { arity: 4 }).build(&mut arena, None);
    assert_eq!(Fixture::decide(&arena.inner, wrong_shape, &known), None);
    assert_eq!(Fixture::decide(&arena.inner, pattern, &KnownValue::Triv), None);
    let empty = VCons::new(vec![], ProductLayout { arity: 3 }).build(&mut arena, None);
    assert_eq!(Fixture::decide(&arena.inner, empty, &known), Some(true));
}

#[test]
fn false_and_uncertain_prefixes_do_not_inspect_later_children() {
    let mut arena = StackirArena::default();
    let missing: VPatId = arena.admin.fresh();
    let hole = Hole.build(&mut arena, None);
    let rejected = Ctor(Fixture::other(), hole).build(&mut arena, None);
    let alias = Alias(ConsN::from_vec(vec![rejected, missing]).unwrap()).build(&mut arena, None);
    let known = KnownValue::Constructor(Fixture::tag(), Rc::default());
    assert_eq!(Fixture::decide(&arena.inner, alias, &known), Some(false));
    assert_eq!(Fixture::decide(&arena.inner, alias, &KnownValue::Unknown), None);
}

#[test]
fn wide_alias_decisions_preserve_every_matching_component() {
    let mut arena = StackirArena::default();
    let patterns = (0..256)
        .map(|_| {
            let hole = Hole.build(&mut arena, None);
            Ctor(Fixture::tag(), hole).build(&mut arena, None)
        })
        .collect();
    let binder = Alias(ConsN::from_vec(patterns).unwrap()).build(&mut arena, None);
    let known = KnownValue::Constructor(Fixture::tag(), Rc::default());
    assert_eq!(Fixture::decide(&arena.inner, binder, &known), Some(true));
}

#[test]
fn deep_alias_decisions_run_and_drop_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut arena = StackirArena::default();
            let mut binder = Hole.build(&mut arena, None);
            for _ in 0..16_384 {
                binder = Alias(ConsN::from_vec(vec![binder]).unwrap()).build(&mut arena, None);
            }
            let known = KnownValue::Triv;
            assert_eq!(
                Explicit::run(
                    &mut DecisionFolder { source: &arena.inner },
                    (binder, KnownView::Value(&known))
                ),
                Some(true)
            );
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn deep_product_decisions_borrow_facts_without_owning_their_teardown() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut arena = StackirArena::default();
            let mut binder = Hole.build(&mut arena, None);
            // Retain each level so the fixture can release parents before children.
            // Production KnownValue destruction remains a separate representation concern.
            let mut facts = vec![Rc::new(KnownValue::Triv)];
            for _ in 0..8_192 {
                binder =
                    VCons::new(vec![binder], ProductLayout { arity: 1 }).build(&mut arena, None);
                facts.push(Rc::new(KnownValue::Product(vec![facts.last().unwrap().clone()])));
            }
            assert_eq!(
                Explicit::run(
                    &mut DecisionFolder { source: &arena.inner },
                    (binder, KnownView::Value(facts.last().unwrap()))
                ),
                Some(true)
            );
            while let Some(fact) = facts.pop() {
                drop(fact);
            }
        })
        .unwrap()
        .join()
        .unwrap();
}
