use super::*;
use zydeco_statics::arena::StaticsScope;
use zydeco_syntax::{FieldName, Hole, IntegerType};
use zydeco_utils::prelude::IdAllocator;

struct Fixture {
    allocator: IdAllocator<StaticsScope>,
    statics: StaticsArena,
    kind: ss::KindId,
    integer: ss::TypeId,
    character: ss::TypeId,
    ret: ss::TypeId,
}

impl Fixture {
    fn new() -> Self {
        let mut allocator = IdAllocator::new();
        let (kind, integer, character, ret) =
            (allocator.alloc(), allocator.alloc(), allocator.alloc(), allocator.alloc());
        let mut statics = StaticsArena::default();
        for (id, ty) in [
            (
                integer,
                ss::Type::Primitive(ss::PrimitiveTy(PrimitiveType::Integer(IntegerType::Int))),
            ),
            (character, ss::Type::Primitive(ss::PrimitiveTy(PrimitiveType::Char))),
            (ret, ss::Type::Ret(ss::RetTy)),
        ] {
            statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kind);
        }
        Self { allocator, statics, kind, integer, character, ret }
    }

    fn ty(&mut self, ty: ss::Type) -> ss::TypeId {
        let id = self.allocator.alloc();
        self.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), self.kind);
        id
    }

    fn witness(&mut self) -> (ss::AbstId, ss::TypeId) {
        let witness = self.allocator.alloc();
        (witness, self.ty(ss::Type::Abst(witness)))
    }

    fn binder(&mut self, witness: ss::AbstId) -> ss::TypeBinder {
        let pattern = self.allocator.alloc();
        self.statics.tpats.insert_new(pattern, ss::TypePattern::Hole(Hole));
        ss::TypeBinder { pattern, witness }
    }

    fn function(&mut self, witness: ss::AbstId, body: ss::TypeId) -> ss::TypeId {
        let binder = self.binder(witness);
        self.ty(ss::Type::Abs(ss::TypeAbstraction { binder, body }))
    }

    fn apply(&mut self, function: ss::TypeId, argument: ss::TypeId) -> ss::TypeId {
        self.ty(ss::Type::App(App(function, argument)))
    }

    /// Stream A R = codata .done : R; .item : A -> Stream A R.
    /// The growing variant passes A * A to the next occurrence instead.
    fn stream(&mut self, growing: bool) -> ss::TypeId {
        let (family, sealed) = self.witness();
        let (a, input) = self.witness();
        let (r, result) = self.witness();
        let next_input =
            if growing { self.ty(ss::Type::Prod(Prod(vec![input, input]))) } else { input };
        let next = self.apply(sealed, next_input);
        let next = self.apply(next, result);
        let item = self.ty(ss::Type::Arrow(Arrow(input, next)));
        let codata = self.allocator.alloc();
        self.statics.codatas.insert_new(
            codata,
            ss::CoData::new([(DtorName(".item".into()), item), (DtorName(".done".into()), result)]),
        );
        let body = self.ty(ss::Type::CoData(codata));
        let body = self.function(r, body);
        let body = self.function(a, body);
        self.statics.seals.insert_new(family, body);
        sealed
    }

    fn instantiate(
        &mut self, family: ss::TypeId, input: ss::TypeId, result: ss::TypeId,
    ) -> ss::TypeId {
        let partial = self.apply(family, input);
        self.apply(partial, result)
    }

    fn integer() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))
    }

    fn character() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Char)
    }

    fn check_stream(
        source: &mut SourceProtocols<'_>, ty: ss::TypeId, input: ValueProtocol,
        result: StackProtocol, growing: bool,
    ) -> CodataProtocolId {
        let StackProtocol::Codata(id) = source.stack(ty) else { panic!("expected a stream") };
        assert_eq!(source.stack(ty), StackProtocol::Codata(id));
        source.graph.validate().unwrap();
        let next = if growing { StackProtocol::Unknown } else { StackProtocol::Codata(id) };
        assert_eq!(
            source.graph.get(id).unwrap().observations,
            vec![
                (DtorIdx { idx: 0, name: DtorName(".done".into()) }, result),
                (
                    DtorIdx { idx: 1, name: DtorName(".item".into()) },
                    StackProtocol::Argument(Box::new(input), Box::new(next)),
                ),
            ]
        );
        id
    }
}

#[test]
fn recursive_applications_keep_value_and_computation_arguments_distinct() {
    let mut fixture = Fixture::new();
    let stream = fixture.stream(false);
    let integer_result = fixture.apply(fixture.ret, fixture.integer);
    let char_result = fixture.apply(fixture.ret, fixture.character);
    let integer_stream = fixture.instantiate(stream, fixture.integer, integer_result);
    let char_stream = fixture.instantiate(stream, fixture.character, char_result);
    let mixed_stream = fixture.instantiate(stream, fixture.integer, char_result);
    let mut source = SourceProtocols::new(&fixture.statics);
    let mut ids = HashSet::new();
    for (ty, input, result) in [
        (integer_stream, Fixture::integer(), Fixture::integer()),
        (char_stream, Fixture::character(), Fixture::character()),
        (mixed_stream, Fixture::integer(), Fixture::character()),
    ] {
        let id = Fixture::check_stream(
            &mut source,
            ty,
            input,
            StackProtocol::Continuation(Box::new(result)),
            false,
        );
        for other in &ids {
            assert!(
                !source
                    .graph
                    .stacks_agree(&StackProtocol::Codata(id), &StackProtocol::Codata(*other))
            );
        }
        assert!(ids.insert(id));
    }
    assert_eq!(source.graph.iter().count(), 3);
}

#[test]
fn growing_applications_keep_the_prefix_without_inventing_a_recursive_equation() {
    let mut fixture = Fixture::new();
    let stream = fixture.stream(true);
    let result = fixture.apply(fixture.ret, fixture.integer);
    let integer_stream = fixture.instantiate(stream, fixture.integer, result);
    let char_stream = fixture.instantiate(stream, fixture.character, result);
    let mut source = SourceProtocols::new(&fixture.statics);
    for (ty, input) in [(integer_stream, Fixture::integer()), (char_stream, Fixture::character())] {
        Fixture::check_stream(
            &mut source,
            ty,
            input,
            StackProtocol::Continuation(Box::new(Fixture::integer())),
            true,
        );
    }
    assert_eq!(source.graph.iter().count(), 2);
}

#[test]
fn the_instance_guard_also_bounds_growth_through_returned_thunks() {
    let mut fixture = Fixture::new();
    let (family, sealed) = fixture.witness();
    let (a, input) = fixture.witness();
    let pair = fixture.ty(ss::Type::Prod(Prod(vec![input, input])));
    let next = fixture.apply(sealed, pair);
    let thunk = fixture.ty(ss::Type::Thk(ss::ThkTy));
    let next = fixture.apply(thunk, next);
    let result = fixture.apply(fixture.ret, next);
    let observation = fixture.ty(ss::Type::Arrow(Arrow(input, result)));
    let codata = fixture.allocator.alloc();
    fixture
        .statics
        .codatas
        .insert_new(codata, ss::CoData::new([(DtorName(".resume".into()), observation)]));
    let body = fixture.ty(ss::Type::CoData(codata));
    let body = fixture.function(a, body);
    fixture.statics.seals.insert_new(family, body);
    let applied = fixture.apply(sealed, fixture.integer);
    let mut source = SourceProtocols::new(&fixture.statics);
    let StackProtocol::Codata(id) = source.stack(applied) else { panic!("expected codata") };
    assert_eq!(
        source.graph.get(id).unwrap().observations[0].1,
        StackProtocol::Argument(
            Box::new(Fixture::integer()),
            Box::new(StackProtocol::Continuation(Box::new(ValueProtocol::Thunk(Box::new(
                StackProtocol::Unknown
            ))))),
        ),
    );
    source.graph.validate().unwrap();
    assert_eq!(source.graph.iter().count(), 1);
}

#[test]
fn value_type_applications_preserve_product_and_thunk_components() {
    let mut fixture = Fixture::new();
    let (a, input) = fixture.witness();
    let (r, result) = fixture.witness();
    let thunk = fixture.ty(ss::Type::Thk(ss::ThkTy));
    let worker = fixture.ty(ss::Type::Arrow(Arrow(input, result)));
    let worker = fixture.apply(thunk, worker);
    let pair = fixture.ty(ss::Type::Prod(Prod(vec![input, worker])));
    let family = fixture.function(r, pair);
    let family = fixture.function(a, family);
    let result = fixture.apply(fixture.ret, fixture.character);
    let integer = fixture.instantiate(family, fixture.integer, result);
    let character = fixture.instantiate(family, fixture.character, result);
    let mut source = SourceProtocols::new(&fixture.statics);
    for (ty, input) in [(integer, Fixture::integer()), (character, Fixture::character())] {
        assert_eq!(
            source.value(ty),
            ValueProtocol::Product(vec![
                input.clone(),
                ValueProtocol::Thunk(Box::new(StackProtocol::Argument(
                    Box::new(input),
                    Box::new(StackProtocol::Continuation(Box::new(Fixture::character()))),
                ))),
            ])
        );
    }
    let (integer, character) = (source.value(integer), source.value(character));
    assert!(!source.graph.values_agree(&integer, &character));
}

#[test]
fn opaque_arguments_preserve_the_interface_without_equating_witnesses() {
    let mut fixture = Fixture::new();
    let stream = fixture.stream(false);
    let (first_witness, first) = fixture.witness();
    let (second_witness, second) = fixture.witness();
    let (result_witness, result) = fixture.witness();
    let first = fixture.instantiate(stream, first, result);
    let second = fixture.instantiate(stream, second, result);
    let mut source = SourceProtocols::new(&fixture.statics);
    let first_parameter = source.parameter(first_witness, ProtocolParameterKind::Value);
    let second_parameter = source.parameter(second_witness, ProtocolParameterKind::Value);
    let result_parameter = source.parameter(result_witness, ProtocolParameterKind::Stack);
    let a = Fixture::check_stream(
        &mut source,
        first,
        ValueProtocol::Parameter(first_parameter),
        StackProtocol::Parameter(result_parameter),
        false,
    );
    let b = Fixture::check_stream(
        &mut source,
        second,
        ValueProtocol::Parameter(second_parameter),
        StackProtocol::Parameter(result_parameter),
        false,
    );
    assert_ne!(a, b, "the same partial evidence must not merge different source arguments");
    assert!(source.graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(b)));
}

#[test]
fn partial_application_captures_the_lexical_argument_and_respects_rebinding() {
    let mut fixture = Fixture::new();
    let (a, first) = fixture.witness();
    let (b, _) = fixture.witness();
    let body = fixture.apply(fixture.ret, first);
    let inner = fixture.function(b, body);
    let outer = fixture.function(a, inner);
    let integer = fixture.instantiate(outer, fixture.integer, fixture.character);
    let character = fixture.instantiate(outer, fixture.character, fixture.integer);
    // A universal binder hides an argument with the same witness identity.
    // This also models reentry into the same source binder under a new scope.
    let binder = fixture.binder(a);
    let quantified = fixture.ty(ss::Type::Forall(ss::Forall(binder, body)));
    let outer_quantified = fixture.function(a, quantified);
    let quantified = fixture.apply(outer_quantified, fixture.integer);
    fixture
        .statics
        .kinds_pre
        .insert_new(fixture.kind, ss::Fillable::Done(ss::Kind::VType(ss::VType)));
    fixture.statics.annotations_abst.insert_new(a, fixture.kind);
    let mut source = SourceProtocols::new(&fixture.statics);
    assert_eq!(source.stack(integer), StackProtocol::Continuation(Box::new(Fixture::integer())));
    assert_eq!(
        source.stack(character),
        StackProtocol::Continuation(Box::new(Fixture::character()))
    );
    let StackProtocol::Forall(parameter, body) = source.stack(quantified) else {
        panic!("the source universal binder must survive")
    };
    assert_eq!(*body, StackProtocol::Continuation(Box::new(ValueProtocol::Parameter(parameter))));
    source.graph.validate().unwrap();
}

#[test]
fn named_binders_project_the_payload_and_whole_binders_keep_the_wrapper() {
    let mut fixture = Fixture::new();
    let field = FieldName("item".into());
    let (a, argument) = fixture.witness();
    let payload = fixture.apply(fixture.ret, argument);
    let mut binder = fixture.binder(a);
    let named_pattern = fixture.allocator.alloc();
    fixture
        .statics
        .tpats
        .insert_new(named_pattern, ss::TypePattern::Named(Named(field.clone(), binder.pattern)));
    binder.pattern = named_pattern;
    let named = fixture.ty(ss::Type::Abs(ss::TypeAbstraction { binder, body: payload }));
    let projection = fixture.ty(ss::Type::Proj(Proj(argument, field.clone())));
    let result = fixture.apply(fixture.ret, projection);
    let whole = fixture.function(a, result);
    let argument = fixture.ty(ss::Type::Named(Named(field, fixture.integer)));
    let applied_named = fixture.apply(named, argument);
    let applied_whole = fixture.apply(whole, argument);
    let wrong_argument =
        fixture.ty(ss::Type::Named(Named(FieldName("other".into()), fixture.character)));
    let wrong = fixture.apply(named, wrong_argument);
    let mut source = SourceProtocols::new(&fixture.statics);
    let expected = StackProtocol::Continuation(Box::new(Fixture::integer()));
    assert_eq!(source.stack(applied_named), expected);
    assert_eq!(source.stack(applied_whole), expected);
    assert_eq!(source.stack(wrong), StackProtocol::Unknown);
}

#[test]
fn unguarded_growing_type_functions_stop_without_allocating_graph_nodes() {
    for has_prefix in [false, true] {
        let mut fixture = Fixture::new();
        let (family, sealed) = fixture.witness();
        let (a, input) = fixture.witness();
        let pair = fixture.ty(ss::Type::Prod(Prod(vec![input, input])));
        let next = fixture.apply(sealed, pair);
        let body = if has_prefix { fixture.ty(ss::Type::Arrow(Arrow(input, next))) } else { next };
        let body = fixture.function(a, body);
        fixture.statics.seals.insert_new(family, body);
        let applied = fixture.apply(sealed, fixture.integer);
        let mut source = SourceProtocols::new(&fixture.statics);
        let expected = if has_prefix {
            StackProtocol::Argument(Box::new(Fixture::integer()), Box::new(StackProtocol::Unknown))
        } else {
            StackProtocol::Unknown
        };
        assert_eq!(source.stack(applied), expected);
        assert_eq!(source.graph.iter().count(), 0);
    }
}
