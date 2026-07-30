use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_statics::{tyck::arena::StaticsArena, tyck::syntax as ss};
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
struct ValuePlan<T> {
    bindings: Vec<ValueBinding>,
    value: T,
}

impl<T> ValuePlan<T> {
    fn pure(value: T) -> Self {
        Self { bindings: Vec::new(), value }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> ValuePlan<U> {
        let Self { bindings, value } = self;
        ValuePlan { bindings, value: f(value) }
    }

    fn with_binding<U>(self, binding: ValueBinding, value: U) -> ValuePlan<U> {
        let Self { bindings, value: _ } = self;
        ValuePlan {
            bindings: bindings.into_iter().chain(std::iter::once(binding)).collect(),
            value,
        }
    }

    fn sequence(plans: impl IntoIterator<Item = Self>) -> ValuePlan<Vec<T>> {
        let (bindings, values): (Vec<_>, Vec<_>) =
            plans.into_iter().map(|Self { bindings, value }| (bindings, value)).unzip();
        ValuePlan { bindings: bindings.into_iter().flatten().collect(), value: values }
    }
}

impl ValuePlan<ValueId> {
    fn lower_into(
        self, lo: &mut Lowerer, kont: impl FnOnce(ValueId, &mut Lowerer) -> CompuId,
    ) -> CompuId {
        let Self { bindings, value } = self;
        let tail = kont(value, lo);
        bindings.into_iter().rev().fold(tail, |tail, binding| {
            let ValueBinding { binder, bindee, site } = binding;
            Let { binder, bindee, tail }.build(lo, site)
        })
    }
}

/// Stateful lowering pass from typed syntax into stack IR.
#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: StackirArena,
    /// initialization order of globals (built during lowering, folded into entry lets at end)
    sequence: Vec<DefId>,
    /// global def -> value (built during lowering, folded into entry lets at end)
    globals: ArenaAssoc<DefId, ValuePlan<ValueId>>,
    pub spans: &'a SpanArena,
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
    pub statics: &'a StaticsArena,
}

impl<'a> Lowerer<'a> {
    /// Create a new lowerer with fresh stack arenas.
    pub fn new(
        spans: &'a SpanArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
    ) -> Self {
        let arena = StackirArena::default();
        Self { arena, sequence: Vec::new(), globals: ArenaAssoc::new(), spans, scoped, statics }
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

impl<'a> CompilerPass for Lowerer<'a> {
    type Arena = StackirArena;
    type Out = StackirArena;
    type Error = std::convert::Infallible;
    /// Lower the full program into a stack arena.
    fn run(mut self) -> Result<StackirArena, Self::Error> {
        // Topologically traverse context bindings and translate runtime values.
        for node_id in self.scoped.root.context.topological_order() {
            let node = self.scoped.root.context.nodes[&node_id].clone();
            for decl_id in node.bindings().iter().filter_map(|binding| binding.id.declaration()) {
                let Some(decl) = self.statics.decls.get(&decl_id).cloned() else {
                    continue;
                };
                use ss::Declaration as Decl;
                match decl {
                    | Decl::VAliasBody(valias_body) => valias_body.lower(&mut self, ()),
                    | Decl::VAliasHead(valias_head) => valias_head.lower(&mut self, ()),
                    | Decl::TAliasBody(_) | Decl::Exec(_) => {}
                }
            }
        }

        // Get entry declarations from statics arena; lower each and wrap in global lets
        for decl_id in self.statics.entry.iter().map(|(decl_id, _)| *decl_id) {
            // Get the declaration and extract the computation
            let decl = &self.statics.decls[&decl_id];
            use ss::Declaration as Decl;
            let entry = match decl {
                | Decl::Exec(ss::Exec(compu)) => *compu,
                // Only Exec declarations should be entry points
                | Decl::TAliasBody(_) | Decl::VAliasBody(_) | Decl::VAliasHead(_) => {
                    let fmt = zydeco_statics::tyck::fmt::Formatter::new(self.scoped, self.statics);
                    let decl_str = decl_id.ugly(&fmt);
                    panic!("entry point must be a main declaration, found:\n{}", decl_str);
                }
            };

            // Lower the computation
            let lowered = entry.lower(&mut self, ());

            // Wrap in let bindings for all globals (in sequence order)
            let wrapped = {
                let sequence = self.sequence.clone();
                sequence.into_iter().rev().fold(lowered, |tail, def| {
                    let bindee = self.globals[&def].clone();
                    let binder = ValuePattern::Var(def).build(&mut self.arena, None);
                    bindee.lower_into(&mut self, move |bindee, lo| {
                        Let { binder, bindee, tail }.build(lo, None)
                    })
                })
            };

            // Register as entry point
            self.arena.inner.entry.insert_new(wrapped, ());
        }
        Ok(self.arena)
    }
}

impl Lower for ss::VAliasBody {
    type Kont = ();
    type Out = ();

    fn lower(&self, lo: &mut Lowerer, (): Self::Kont) -> Self::Out {
        let ss::VAliasBody { binder, bindee } = self.clone();
        // Lower the binder (VPatId) - creates new VPatId and stores mapping
        let binder_vpat = binder.lower(lo, ());
        // Extract DefId from binder (should be a Var pattern)
        use ValuePattern as VPat;
        let def_id = match lo.arena.inner.vpats[&binder_vpat] {
            | VPat::Var(def) => def,
            | _ => {
                let fmt = super::fmt::Formatter::new(
                    &lo.arena.admin,
                    &lo.arena.inner,
                    lo.scoped,
                    lo.statics,
                );
                let binder_doc = binder_vpat.pretty(&fmt);
                let mut binder_str = String::new();
                binder_doc.render_fmt(80, &mut binder_str).unwrap();
                panic!("VAliasBody binder must be a variable, found:\n{}", binder_str);
            }
        };
        let value = bindee.lower(lo, ());
        lo.sequence.push(def_id);
        lo.globals.insert_new(def_id, value);
    }
}

impl Lower for ss::VAliasHead {
    type Kont = ();
    type Out = ();

    fn lower(&self, lo: &mut Lowerer, (): Self::Kont) -> Self::Out {
        let ss::VAliasHead { binder, ty: _ } = self.clone();
        // Lower the binder (VPatId) - creates new VPatId and stores mapping
        let binder_vpat = binder.lower(lo, ());
        // Extract DefId from binder (should be a Var pattern)
        use ValuePattern as VPat;
        let def = match &lo.arena.inner.vpats[&binder_vpat] {
            | VPat::Var(def) => *def,
            | _ => {
                let fmt = super::fmt::Formatter::new(
                    &lo.arena.admin,
                    &lo.arena.inner,
                    lo.scoped,
                    lo.statics,
                );
                let binder_doc = binder_vpat.pretty(&fmt);
                let mut binder_str = String::new();
                binder_doc.render_fmt(usize::MAX, &mut binder_str).unwrap();
                panic!("VAliasHead binder must be a variable, found:\n{}", binder_str);
            }
        };
        let name = lo.scoped.defs[&def].plain();
        let Some(builtin) = lo.arena.admin.builtins.get(name.as_str()).cloned() else {
            panic!("Undefined builtin extern:\n{}", name);
        };
        // Create the builtin value and store it in globals
        let value = match builtin.sort {
            | BuiltinSort::Operator => builtin.make_operator(lo),
            | BuiltinSort::Function => builtin.make_function(lo),
        };
        lo.sequence.push(def);
        lo.globals.insert_new(def, ValuePlan::pure(value));
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
            | SSVPat::Triv(Triv) => Triv.into(),
            | SSVPat::VCons(ss::ConsN(items, tail)) => {
                let items = items.into_iter().map(|item| item.lower(lo, ())).collect();
                let tail = tail.lower(lo, ());
                let ty = lo.statics.annotations_vpat[self];
                VCons::new(ConsN(items, tail), lo.product_layout(ty)).into()
            }
            | SSVPat::TCons(ss::ConsN(_, body)) => {
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
            | ss::Value::TCons(ss::ConsN(_witnesses, inner)) => {
                // Type cons values are erased.
                inner.lower(lo, ())
            }
            | ss::Value::Proj(Proj(head, field)) => match field.target {
                | ss::ProjTarget::Direct => head.lower(lo, ()),
                | ss::ProjTarget::Product(position) => {
                    let layout = lo.product_layout(lo.statics.annotations_value[&head]);
                    let head = head.lower(lo, ());
                    let (binding, projected) =
                        lo.projection_binding(head.value, position, layout, site);
                    head.with_binding(binding, projected)
                }
            },
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
                        let fmt = zydeco_statics::tyck::fmt::Formatter::new(lo.scoped, lo.statics);
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
