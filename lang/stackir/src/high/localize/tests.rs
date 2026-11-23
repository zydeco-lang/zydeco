use super::*;
use crate::scheme::SpsScheme;

struct Fixture {
    arena: SpsArena,
}

/// Reachable computations by the forms that localization changes.
#[derive(Debug, Default, PartialEq)]
struct Forms {
    procedures: usize,
    recursive_procedures: usize,
    calls: usize,
    forces: usize,
    fixes: usize,
}

impl Visitor<Sps> for Forms {
    fn enter(&mut self, node: Node<'_>, _: Edge, _: Occurrence) {
        match node {
            | Node::Computation(_, Computation::Ext(CompuExt::LetProc(proc_node))) => {
                self.procedures += 1;
                if proc_node.recursive {
                    self.recursive_procedures += 1;
                }
            }
            | Node::Computation(_, Computation::Ext(CompuExt::CallProc(_))) => self.calls += 1,
            | Node::Computation(_, Computation::Ext(CompuExt::Force(_))) => self.forces += 1,
            | Node::Computation(_, Computation::Ext(CompuExt::Fix(_))) => self.fixes += 1,
            | _ => {}
        }
    }
}

impl Fixture {
    fn new() -> Self {
        Self { arena: SpsArena::default() }
    }

    fn def(&mut self) -> DefId {
        self.arena.admin.fresh_def()
    }

    fn ambient(&mut self) -> StackId {
        StackVar.build(&mut self.arena, None)
    }

    fn returns(&mut self, value: ValueId) -> CompuId {
        let stack = self.ambient();
        SReturn { stack, value }.build(&mut self.arena, None)
    }

    fn returns_unit(&mut self) -> CompuId {
        let value = Triv.build(&mut self.arena, None);
        self.returns(value)
    }

    fn closure(&mut self, body: CompuId) -> ValueId {
        Closure { stack: StackVar, body }.build(&mut self.arena, None)
    }

    fn var(&mut self, def: DefId) -> ValueId {
        def.build(&mut self.arena, None)
    }

    /// `let x = value in tail`
    fn bind(&mut self, x: DefId, value: ValueId, tail: CompuId) -> CompuId {
        let binder: VPatId = x.build(&mut self.arena, None);
        Let { binder, bindee: value, tail }.build(&mut self.arena, None)
    }

    /// `x ! stack`
    fn call(&mut self, x: DefId, stack: StackId) -> CompuId {
        let thunk = self.var(x);
        SForce { thunk, stack }.build(&mut self.arena, None)
    }

    fn kont(&mut self, body: CompuId) -> StackId {
        let binder: VPatId = Triv.build(&mut self.arena, None);
        Kont { binder, body }.build(&mut self.arena, None)
    }

    /// `let x = { • -> • @ () } in` the computation that `tail` builds.
    fn unit_procedure(&mut self, tail: impl FnOnce(&mut Self, DefId) -> CompuId) -> CompuId {
        let x = self.def();
        let body = self.returns_unit();
        let closure = self.closure(body);
        let tail = tail(self, x);
        self.bind(x, closure, tail)
    }

    fn localize(self, root: CompuId, scheme: SpsScheme) -> Forms {
        let program = FragmentProgram::try_new(SpsProgram::new(self.arena, root), scheme).unwrap();
        let program = Localizer.run(program).unwrap().into_program();
        let mut forms = Forms::default();
        Traversal { arena: &program.arena().inner }.run(program.root().into(), &mut forms);
        forms
    }
}

#[test]
fn a_closure_that_is_only_called_becomes_a_procedure() {
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let ambient = fixture.ambient();
        let resumed = fixture.call(x, ambient);
        let kont = fixture.kont(resumed);
        fixture.call(x, kont)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 1, recursive_procedures: 0, calls: 2, forces: 0, fixes: 0 }
    );
}

#[test]
fn a_closure_that_escapes_or_is_never_called_stays_bound() {
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let value = fixture.var(x);
        let returned = fixture.returns(value);
        let kont = fixture.kont(returned);
        fixture.call(x, kont)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 0, recursive_procedures: 0, calls: 0, forces: 1, fixes: 0 }
    );

    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, _| fixture.returns_unit());
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(forms, Forms::default());
}

#[test]
fn a_closure_called_under_a_closure_or_escaping_fixed_point_stays_bound() {
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let ambient = fixture.ambient();
        let call = fixture.call(x, ambient);
        let thunk = fixture.closure(call);
        fixture.returns(thunk)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 0, recursive_procedures: 0, calls: 0, forces: 1, fixes: 0 }
    );

    // `param` escapes in the comparison arm, so `SFix` stays a closure and keeps `x` bound.
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let param = fixture.def();
        let ambient = fixture.ambient();
        let call = fixture.call(x, ambient);
        let escaping = fixture.var(param);
        let ret_escape = fixture.returns(escaping);
        let operands = [0, 1]
            .map(|v| Literal::Integer(IntegerLiteral::Int(v)).build(&mut fixture.arena, None));
        let branch = CompareBranch {
            operation: ComparisonOp::Integer(IntegerType::Int, ComparisonPredicate::Lt),
            operands,
            when_true: call,
            when_false: ret_escape,
        }
        .build(&mut fixture.arena, None);
        let stack = fixture.ambient();
        let body =
            Let { binder: StackVar, bindee: stack, tail: branch }.build(&mut fixture.arena, None);
        let stack = fixture.ambient();
        SFix { param, stack, body }.build(&mut fixture.arena, None)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 0, recursive_procedures: 0, calls: 0, forces: 1, fixes: 1 }
    );
}

#[test]
fn non_escaping_fixed_points_contify_into_recursive_procedures_and_unlock_localization() {
    // Non-escaping tail-recursive loop calling an outer localized closure `x`:
    // both `x` and the `SFix` parameter become procedures.
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let param = fixture.def();
        let ambient = fixture.ambient();
        let self_call = fixture.call(param, ambient);
        let ambient = fixture.ambient();
        let call_x = fixture.call(x, ambient);
        let operands = [0, 1]
            .map(|v| Literal::Integer(IntegerLiteral::Int(v)).build(&mut fixture.arena, None));
        let branch = CompareBranch {
            operation: ComparisonOp::Integer(IntegerType::Int, ComparisonPredicate::Lt),
            operands,
            when_true: call_x,
            when_false: self_call,
        }
        .build(&mut fixture.arena, None);
        let stack = fixture.ambient();
        let body =
            Let { binder: StackVar, bindee: stack, tail: branch }.build(&mut fixture.arena, None);
        let stack = fixture.ambient();
        SFix { param, stack, body }.build(&mut fixture.arena, None)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 2, recursive_procedures: 1, calls: 3, forces: 0, fixes: 0 }
    );
}

#[test]
fn localizing_an_inner_closure_lets_the_closure_it_calls_localize() {
    // let x = { • -> • @ () } in let y = { • -> x ! • } in y ! (kont _ -> y ! •)
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let y = fixture.def();
        let ambient = fixture.ambient();
        let inner = fixture.call(x, ambient);
        let closure = fixture.closure(inner);
        let ambient = fixture.ambient();
        let resumed = fixture.call(y, ambient);
        let kont = fixture.kont(resumed);
        let tail = fixture.call(y, kont);
        fixture.bind(y, closure, tail)
    });
    let forms = fixture.localize(root, SpsScheme::BranchJoin);
    assert_eq!(
        forms,
        Forms { procedures: 2, recursive_procedures: 0, calls: 3, forces: 0, fixes: 0 }
    );
}

#[test]
fn localization_preserves_the_full_fragment() {
    // let x = { • -> • @ () } in let • = (kont _ -> x ! •) in x ! •
    let mut fixture = Fixture::new();
    let root = fixture.unit_procedure(|fixture, x| {
        let ambient = fixture.ambient();
        let resumed = fixture.call(x, ambient);
        let bindee = fixture.kont(resumed);
        let ambient = fixture.ambient();
        let tail = fixture.call(x, ambient);
        Let { binder: StackVar, bindee, tail }.build(&mut fixture.arena, None)
    });
    let forms = fixture.localize(root, SpsScheme::Full);
    assert_eq!(
        forms,
        Forms { procedures: 1, recursive_procedures: 0, calls: 2, forces: 0, fixes: 0 }
    );
}
