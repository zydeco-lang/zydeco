use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_statics::{BuiltinPackagePlan, BuiltinPackageValue, arena::StaticsArena, syntax as ss};
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// Lower typed syntax nodes into stack IR.
trait Lower {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

#[derive(Clone)]
struct ValueBinding {
    binder: VPatId,
    bindee: ValueId,
    site: Option<ss::TermId>,
}

#[derive(Clone)]
struct ValueApplication {
    binder: VPatId,
    function: ValueId,
    argument: ValueId,
    site: Option<ss::TermId>,
}

#[derive(Clone)]
enum ValueStep {
    Bind(ValueBinding),
    Apply(ValueApplication),
}

#[derive(Clone)]
struct ValuePlan<T> {
    steps: Vec<ValueStep>,
    value: T,
}

impl<T> ValuePlan<T> {
    fn pure(value: T) -> Self {
        Self { steps: Vec::new(), value }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> ValuePlan<U> {
        let Self { steps, value } = self;
        ValuePlan { steps, value: f(value) }
    }

    fn with_binding<U>(self, binding: ValueBinding, value: U) -> ValuePlan<U> {
        let Self { steps, value: _ } = self;
        ValuePlan { steps: steps.into_iter().chain([ValueStep::Bind(binding)]).collect(), value }
    }

    fn with_application<U>(self, application: ValueApplication, value: U) -> ValuePlan<U> {
        let Self { steps, value: _ } = self;
        ValuePlan {
            steps: steps.into_iter().chain([ValueStep::Apply(application)]).collect(),
            value,
        }
    }

    fn sequence(plans: impl IntoIterator<Item = Self>) -> ValuePlan<Vec<T>> {
        let (steps, values): (Vec<_>, Vec<_>) =
            plans.into_iter().map(|Self { steps, value }| (steps, value)).unzip();
        ValuePlan { steps: steps.into_iter().flatten().collect(), value: values }
    }
}

impl ValuePlan<ValueId> {
    fn scoped(self, binder: VPatId, tail: Self, site: Option<ss::TermId>) -> Self {
        let Self { steps, value: bindee } = self;
        let Self { steps: tail_steps, value } = tail;
        let binding = ValueBinding { binder, bindee, site };
        ValuePlan {
            steps: steps.into_iter().chain([ValueStep::Bind(binding)]).chain(tail_steps).collect(),
            value,
        }
    }

    fn lower_into(
        self, lo: &mut Lowerer, kont: impl FnOnce(ValueId, &mut Lowerer) -> CompuId,
    ) -> CompuId {
        let Self { steps, value } = self;
        let tail = kont(value, lo);
        steps.into_iter().rev().fold(tail, |tail, step| match step {
            | ValueStep::Bind(ValueBinding { binder, bindee, site }) => {
                Let { binder, bindee, tail }.build(lo, site)
            }
            | ValueStep::Apply(ValueApplication { binder, function, argument, site }) => {
                let stack = Kont { binder, body: tail }.build(lo, site);
                let stack = Cons(argument, stack).build(lo, site);
                SForce { thunk: function, stack }.build(lo, site)
            }
        })
    }
}

/// Stateful lowering pass from typed syntax into stack IR.
#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: StackirArena,
    pub spans: &'a SpanArena,
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
    pub statics: &'a StaticsArena,
}

/// Lowering pass for one checked computation root.
#[derive(AsRef, AsMut)]
pub struct RootLowerer<'a> {
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    lowerer: Lowerer<'a>,
    root: ss::CompuId,
}

/// Lowering pass for a package-dependent root applied to the host Builtin package.
#[derive(AsRef, AsMut)]
pub struct BuiltinRootLowerer<'a> {
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    lowerer: Lowerer<'a>,
    root: ss::CompuId,
    signature: ss::PackPi,
}

/// Materializes backend-independent Builtin package plans as Stack IR values.
struct BuiltinPackageLowering;

impl<'a> Lowerer<'a> {
    /// Create a new lowerer with fresh stack arenas.
    pub fn new(
        spans: &'a SpanArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
    ) -> Self {
        let arena = StackirArena::default();
        Self { arena, spans, scoped, statics }
    }

    fn product_arity(&self, ty: ss::TypeId) -> usize {
        match &self.statics.types_normalized[&ty] {
            | ss::Type::Unit(_) => 0,
            | ss::Type::Prod(ss::Prod(_, tail)) => {
                1 + match &self.statics.types_normalized[tail] {
                    | ss::Type::Prod(_) => self.product_arity(*tail),
                    | _ => 1,
                }
            }
            | _ => unreachable!("VCons must have Unit or product type"),
        }
    }

    fn product_layout(&self, ty: ss::TypeId) -> ProductLayout {
        ProductLayout { arity: self.product_arity(ty) }
    }

    fn alloc_projection_def(&mut self) -> DefId {
        let def = self.arena.admin.fresh();
        self.scoped.insert_def(def, VarName("__proj__".to_owned()));
        def
    }

    fn alloc_pure_result(&mut self) -> DefId {
        let def = self.arena.admin.fresh();
        self.scoped.insert_def(def, VarName("__pure_result__".to_owned()));
        def
    }

    fn projection_binding(
        &mut self, head: ValueId, position: usize, layout: ProductLayout, site: Option<ss::TermId>,
    ) -> (ValueBinding, ValueId) {
        assert!(position < layout.arity);
        let selected = self.alloc_projection_def();
        let fields = (0..layout.arity)
            .map(|index| {
                if index == position { selected.build(self, None) } else { Hole.build(self, None) }
            })
            .collect::<Vec<VPatId>>();
        let fields =
            ConsN::from_vec(fields).expect("a projected product must have at least one field");
        let binder = VCons::new(fields, layout).build(self, None);
        let projected = selected.build(self, site);
        (ValueBinding { binder, bindee: head, site }, projected)
    }
}

impl<'a> RootLowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
        root: ss::CompuId,
    ) -> Self {
        Self { lowerer: Lowerer::new(spans, scoped, statics), root }
    }
}

impl<'a> BuiltinRootLowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
        root: ss::CompuId, signature: ss::PackPi,
    ) -> Self {
        Self { lowerer: Lowerer::new(spans, scoped, statics), root, signature }
    }
}

impl BuiltinPackageLowering {
    fn lower(
        value: BuiltinPackageValue, lowerer: &mut Lowerer<'_>,
    ) -> Result<ValueId, BuiltinPackageLowerError> {
        match value {
            | BuiltinPackageValue::Unit => Ok(Triv.build(lowerer, None)),
            | BuiltinPackageValue::Operation(role) => {
                let builtin = Builtin::for_role(&lowerer.arena.admin.builtins, role)?;
                Ok(match builtin.sort {
                    | BuiltinSort::Operator => builtin.make_operator(lowerer),
                    | BuiltinSort::Function(_) => builtin.make_function(lowerer),
                })
            }
            | BuiltinPackageValue::Product(product) => {
                let values = product
                    .into_values()
                    .into_iter()
                    .map(|value| Self::lower(value, lowerer))
                    .collect::<Result<Vec<_>, _>>()?;
                let layout = ProductLayout { arity: values.len() };
                let items = ConsN::from_vec(values).expect("a checked product plan is non-empty");
                Ok(VCons::new(items, layout).build(lowerer, None))
            }
        }
    }
}

impl CompilerPass for RootLowerer<'_> {
    type Arena = StackirArena;
    type Out = StackirArena;
    type Error = std::convert::Infallible;

    fn run(self) -> Result<StackirArena, Self::Error> {
        let Self { mut lowerer, root } = self;
        let root = root.lower(&mut lowerer, ());
        lowerer.arena.inner.entry.insert_new(root, ());
        Ok(lowerer.arena)
    }
}

impl CompilerPass for BuiltinRootLowerer<'_> {
    type Arena = StackirArena;
    type Out = StackirArena;
    type Error = BuiltinPackageLowerError;

    fn run(self) -> Result<StackirArena, Self::Error> {
        let Self { mut lowerer, root, signature } = self;
        let plan = BuiltinPackagePlan::for_executable(lowerer.statics, &signature)?;
        let function = root.lower(&mut lowerer, ());
        let package = BuiltinPackageLowering::lower(plan.value, &mut lowerer)?;
        let stack = Cons(package, Bullet.build(&mut lowerer, None)).build(&mut lowerer, None);
        let root = Let { binder: Bullet, bindee: stack, tail: function }.build(&mut lowerer, None);
        lowerer.arena.inner.entry.insert_new(root, ());
        Ok(lowerer.arena)
    }
}

impl Lower for ss::VPatId {
    type Kont = ();
    type Out = VPatId;

    fn lower(&self, lo: &mut Lowerer, _kont: Self::Kont) -> Self::Out {
        // Get the pattern from statics arena
        let ss_vpat = lo.statics.vpats[self].clone();
        // Map from ss::VPatId to ss::PatId
        let ss_pat_id = ss::PatId::Value(*self);
        // Convert statics ValuePattern to stack ValuePattern
        use super::syntax::ValuePattern as StackVPat;
        use ss::ValuePattern as SSVPat;
        let stack_vpat: StackVPat = match ss_vpat {
            | SSVPat::Hole(hole) => hole.into(),
            | SSVPat::Var(def) => def.into(),
            | SSVPat::Named(Named(_, inner)) => {
                let vpat = inner.lower(lo, ());
                lo.arena.inner.vpats[&vpat].clone()
            }
            | SSVPat::Ctor(ctor) => {
                use zydeco_syntax::Ctor;
                let Ctor(name, tail) = ctor;
                let tail_vpat = tail.lower(lo, ());
                let data_id = lo.statics.data_pat_hints[&self];
                let idx = lo.statics.datas[&data_id]
                    .iter()
                    .position(|(tag_branch, _ty)| tag_branch == &name)
                    .expect("Constructor tag not found");
                let ctor_idx = CtorIdx { idx, name };
                Ctor(ctor_idx, tail_vpat).into()
            }
            | SSVPat::Alias(Alias(patterns)) => {
                let patterns = patterns.into_iter().map(|pattern| pattern.lower(lo, ())).collect();
                Alias(ConsN::from_vec(patterns).unwrap()).into()
            }
            | SSVPat::Triv(Triv) => Triv.into(),
            | SSVPat::VCons(ss::ConsN(items, tail)) => {
                let items = items.into_iter().map(|item| item.lower(lo, ())).collect();
                let tail = tail.lower(lo, ());
                let ty = lo.statics.annotations_vpat[self];
                VCons::new(ConsN(items, tail), lo.product_layout(ty)).into()
            }
            | SSVPat::SCons(ss::ConsN(_, body)) => {
                let vpat = body.lower(lo, ());
                lo.arena.inner.vpats[&vpat].clone()
            }
        };
        // Create new VPatId in stack arena and store the mapping
        stack_vpat.build(lo, Some(ss_pat_id))
    }
}

impl Lower for ss::ValueId {
    type Kont = ();
    type Out = ValuePlan<ValueId>;

    fn lower(&self, lo: &mut Lowerer, (): Self::Kont) -> Self::Out {
        let value = lo.statics.values[self].clone();
        let site = Some(ss::TermId::Value(*self));
        match value {
            | ss::Value::Hole(_) => ValuePlan::pure(Hole.build(lo, site)),
            | ss::Value::Var(def) => ValuePlan::pure(def.build(lo, site)),
            | ss::Value::Named(Named(_, inner)) => inner.lower(lo, ()),
            | ss::Value::Let(Let { binder, bindee, tail }) => {
                let binder = binder.lower(lo, ());
                let bindee = bindee.lower(lo, ());
                let tail = tail.lower(lo, ());
                bindee.scoped(binder, tail, site)
            }
            | ss::Value::VAbs(Abs(param, body)) => {
                let param = param.lower(lo, ());
                let body = body.lower(lo, ());
                let body = body.lower_into(lo, |value, lo| {
                    let stack = Bullet.build(lo, site);
                    SReturn { stack, value }.build(lo, site)
                });
                let stack = Bullet.build(lo, site);
                let body =
                    Let { binder: Cons(param, Bullet), bindee: stack, tail: body }.build(lo, site);
                ValuePlan::pure(Closure { stack: Bullet, body }.build(lo, site))
            }
            | ss::Value::VApp(App(function, argument)) => {
                let function = function.lower(lo, ());
                let argument = argument.lower(lo, ());
                let values = ValuePlan::sequence([function, argument]);
                let [function, argument] = values.value.as_slice() else { unreachable!() };
                let function = *function;
                let argument = *argument;
                let result = lo.alloc_pure_result();
                let binder = result.build(lo, None);
                let value = result.build(lo, site);
                values
                    .with_application(ValueApplication { binder, function, argument, site }, value)
            }
            | ss::Value::TAbs(Abs(_param, body)) => body.lower(lo, ()),
            | ss::Value::TApp(App(body, _arg)) => body.lower(lo, ()),
            | ss::Value::Thunk(Thunk(body)) => {
                let body = body.lower(lo, ());
                ValuePlan::pure(Closure { stack: Bullet, body }.build(lo, site))
            }
            | ss::Value::Ctor(Ctor(name, body)) => {
                let data_id = lo.statics.data_hints[self];
                let idx = lo.statics.datas[&data_id]
                    .iter()
                    .position(|(tag_branch, _ty)| tag_branch == &name)
                    .expect("Constructor tag not found");
                let body = body.lower(lo, ());
                body.map(|body| Ctor(CtorIdx { idx, name }, body).build(lo, site))
            }
            | ss::Value::Triv(Triv) => ValuePlan::pure(Triv.build(lo, site)),
            | ss::Value::VCons(items) => {
                let layout = lo.product_layout(lo.statics.annotations_value[self]);
                let items = items.into_vec().lower(lo, ());
                items.map(|items| {
                    let items =
                        ConsN::from_vec(items).expect("non-empty product value in stack IR");
                    VCons::new(items, layout).build(lo, site)
                })
            }
            | ss::Value::SCons(ss::ConsN(_witnesses, inner)) => {
                // Type cons values are erased.
                inner.lower(lo, ())
            }
            | ss::Value::Proj(Proj(head, field)) => {
                field.target.products.into_iter().fold(head.lower(lo, ()), |head, projection| {
                    let layout = lo.product_layout(projection.product);
                    let (binding, projected) =
                        lo.projection_binding(head.value, projection.position, layout, site);
                    head.with_binding(binding, projected)
                })
            }
            | ss::Value::Lit(lit) => ValuePlan::pure(lit.build(lo, site)),
        }
    }
}

impl Lower for Vec<ss::ValueId> {
    type Kont = ();
    type Out = ValuePlan<Vec<ValueId>>;

    fn lower(&self, lo: &mut Lowerer, (): Self::Kont) -> Self::Out {
        ValuePlan::sequence(self.iter().map(|item| item.lower(lo, ())))
    }
}

impl Lower for ss::CompuId {
    type Kont = ();
    type Out = CompuId;

    fn lower(&self, lo: &mut Lowerer, (): Self::Kont) -> Self::Out {
        let compu = lo.statics.compus[self].clone();
        let site = Some(ss::TermId::Compu(*self));
        use ss::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => {
                let tail = Bullet.build(lo, site);
                SHole(tail).build(lo, site)
            }
            | Compu::VAbs(Abs(param, body)) => {
                let param_vpat = param.lower(lo, ());
                let body_compu = body.lower(lo, ());
                let stack_id = Bullet.build(lo, site);
                Let { binder: Cons(param_vpat, Bullet), bindee: stack_id, tail: body_compu }
                    .build(lo, site)
            }
            | Compu::VApp(App(body, arg)) => {
                let arg = arg.lower(lo, ());
                arg.lower_into(lo, move |arg, lo| {
                    let next_stack = Bullet.build(lo, site);
                    let stack = Cons(arg, next_stack).build(lo, site);
                    let body = body.lower(lo, ());
                    Let { binder: Bullet, bindee: stack, tail: body }.build(lo, site)
                })
            }
            | Compu::TAbs(Abs(_param, body)) => {
                // Type abstractions are erased
                body.lower(lo, ())
            }
            | Compu::TApp(App(body, _arg)) => {
                // Type applications are erased
                body.lower(lo, ())
            }
            | Compu::Fix(Fix(param, body)) => {
                // Extract DefId from binder (should be a Var pattern)
                use ss::ValuePattern as VPat;
                let def_id = match &lo.statics.vpats[&param] {
                    | VPat::Var(def) => *def,
                    | _ => {
                        let fmt = zydeco_statics::fmt::Formatter::new(lo.scoped, lo.statics);
                        let param_str = param.ugly(&fmt);
                        panic!("Fix param must be a variable, found:\n{}", param_str);
                    }
                };
                let body_compu = body.lower(lo, ());
                SFix { param: def_id, body: body_compu }.build(lo, site)
            }
            | Compu::Force(Force(body)) => {
                let body = body.lower(lo, ());
                body.lower_into(lo, move |thunk, lo| {
                    SForce { thunk, stack: Bullet.build(lo, site) }.build(lo, site)
                })
            }
            | Compu::Ret(Return(body)) => {
                let body = body.lower(lo, ());
                body.lower_into(lo, move |value, lo| {
                    let stack_id = Bullet.build(lo, site);
                    SReturn { stack: stack_id, value }.build(lo, site)
                })
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let binder_vpat = binder.lower(lo, ());
                let tail_compu = tail.lower(lo, ());
                let kont_stack_id = Kont { binder: binder_vpat, body: tail_compu }.build(lo, site);
                let bindee_compu = bindee.lower(lo, ());
                Let { binder: Bullet, bindee: kont_stack_id, tail: bindee_compu }.build(lo, site)
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                let binder_vpat = binder.lower(lo, ());
                let bindee = bindee.lower(lo, ());
                bindee.lower_into(lo, move |bindee, lo| {
                    let tail_compu = tail.lower(lo, ());
                    Let { binder: binder_vpat, bindee, tail: tail_compu }.build(lo, site)
                })
            }
            | Compu::Match(Match { scrut, arms }) => {
                // Match: lower the scrutinee, then create a case statement
                let scrut = scrut.lower(lo, ());
                scrut.lower_into(lo, move |scrut, lo| {
                    // Lower all the arms - arms are (VPatId, CompuId) in statics
                    let lowered_arms: Vec<_> = arms
                        .iter()
                        .map(|arm| {
                            let Matcher { binder, tail } = arm;
                            let binder_vpat = binder.lower(lo, ());
                            let body_compu = tail.lower(lo, ());
                            Matcher { binder: binder_vpat, tail: body_compu }
                        })
                        .collect();
                    Match { scrut, arms: lowered_arms }.build(lo, site)
                })
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let CoMatcher { dtor: name, tail } = arm;
                        let codata_id = lo.statics.codata_hints[&self];
                        let idx = lo.statics.codatas[&codata_id]
                            .iter()
                            .position(|(tag_branch, _ty)| tag_branch == &name)
                            .expect("Destructor tag not found");
                        let dtor_idx = DtorIdx { idx, name };
                        let body_compu = tail.lower(lo, ());
                        CoMatcher { dtor: Cons(dtor_idx, Bullet), tail: body_compu }
                    })
                    .collect();
                let scrut = Bullet.build(lo, site);
                SCoMatch { scrut, arms }.build(lo, site)
            }
            | Compu::Dtor(Dtor(body, name)) => {
                // Destructor: push the destructor onto the stack and continue with body
                let next_stack = Bullet.build(lo, Some(ss::TermId::Compu(body)));
                let codata_id = lo.statics.codata_hints[&body];
                let idx = lo.statics.codatas[&codata_id]
                    .iter()
                    .position(|(tag_branch, _ty)| tag_branch == &name)
                    .expect("Destructor tag not found");
                let dtor_idx = DtorIdx { idx, name };
                let tag_stack_id = Cons(dtor_idx, next_stack).build(lo, site);
                let body_compu = body.lower(lo, ());
                // Create LetStack to bind from the stack with the tag to the current stack, then run body
                Let { binder: Bullet, bindee: tag_stack_id, tail: body_compu }.build(lo, site)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_statics::arena::StaticsScope;
    use zydeco_utils::prelude::IdAllocator;

    #[test]
    fn computation_roots_lower_without_declaration_entries() {
        let mut allocator = IdAllocator::<StaticsScope>::new();
        let value = allocator.alloc();
        let root = allocator.alloc();
        let mut statics = StaticsArena::default();
        statics.values.insert_new(value, ss::Triv.into());
        statics.compus.insert_new(root, ss::Return(value).into());
        let spans = SpanArena::default();
        let mut scoped = ScopedArena::default();

        let stackir = RootLowerer::new(&spans, &mut scoped, &statics, root).run().unwrap();

        assert_eq!(stackir.inner.entry.len(), 1);
        super::super::check::check(&stackir, &scoped);
    }
}
