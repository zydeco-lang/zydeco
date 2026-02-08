use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_statics::{tyck::arena::StaticsArena, tyck::syntax as ss};
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::{arena::ArcGlobalAlloc, context::Context, pass::CompilerPass, phantom::Phantom};

/// Lower typed syntax nodes into stack IR.
pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

/// Stateful lowering pass from typed syntax into stack IR.
#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: StackirArena,
    /// initialization order of globals (built during lowering, folded into entry lets at end)
    pub sequence: Vec<DefId>,
    /// global def -> value (built during lowering, folded into entry lets at end)
    pub globals: ArenaAssoc<DefId, ValueId>,
    pub spans: &'a SpanArena,
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
    pub statics: &'a StaticsArena,
}

impl<'a> Lowerer<'a> {
    /// Create a new lowerer with fresh stack arenas.
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a SpanArena, scoped: &'a mut ScopedArena,
        statics: &'a StaticsArena,
    ) -> Self {
        let arena = StackirArena::new_arc(alloc);
        Self { arena, sequence: Vec::new(), globals: ArenaAssoc::new(), spans, scoped, statics }
    }

    /// Compute the minimal capture list for a closure from a computation body.
    /// Filters out type and kind identifiers, keeping only term-level (value/computation) identifiers.
    /// Excludes the fix parameter from the capture list.
    fn compute_capture(&self, body: ss::CompuId, param: Option<ss::DefId>) -> Context<DefId> {
        // Convert CompuId to statics TermId
        let ss_term_id = ss::TermId::Compu(body);
        // Map from statics TermId to scoped TermId
        let su_term_id = self.statics.terms.back(&ss_term_id).unwrap_or_else(|| {
            let fmt = zydeco_statics::tyck::fmt::Formatter::new(self.scoped, self.statics);
            let body_str = body.ugly(&fmt);
            // Try to get span information if we can map to scoped TermId
            // (this will likely also fail since the mapping already failed)
            let span_str = self
                .statics
                .terms
                .back(&ss_term_id)
                .and_then(|su_term_id| {
                    use zydeco_surface::scoped::syntax::EntityId as SuEntityId;
                    let entity_id: SuEntityId = (*su_term_id).into();
                    self.scoped.textual.back(&entity_id)
                })
                .map(|entity| format!(" ({})", &self.spans[entity]))
                .unwrap_or_default();
            panic!("Failed to map statics CompuId to scoped TermId ({}):\n{}", span_str, body_str);
        });
        // Get cocontext (free variables) from scoped arena
        // This gives us the minimal set of variables that need to be captured
        let coctx = self.scoped.coctxs_term_local[su_term_id].clone();
        // Convert CoContext to Context<DefId>, filtering out type and kind identifiers
        // Only keep term-level (value/computation) identifiers
        coctx
            .iter()
            .cloned()
            .filter(|def_id| {
                // Check if this is a term-level definition (value/computation)
                // by checking if it has a Type annotation (not Kind or Set)
                self.statics
                    .annotations_var
                    .get(def_id)
                    .map(|ann| matches!(ann, ss::AnnId::Type(_)))
                    .unwrap_or(false)
            })
            .filter(|def_id| param.map(|param| *def_id != param).unwrap_or(true))
            .collect()
    }
}

impl<'a> CompilerPass for Lowerer<'a> {
    type Arena = StackirArena;
    type Out = StackirArena;
    type Error = std::convert::Infallible;
    /// Lower the full program into a stack arena.
    fn run(mut self) -> Result<StackirArena, Self::Error> {
        // Topologically traverse declarations and translate VAliasBody
        let mut scc = self.scoped.top.clone();
        loop {
            let groups = scc.top();
            if groups.is_empty() {
                break;
            }
            for group in groups {
                for decl_id in group.iter() {
                    let Some(decl) = self.statics.decls.get(decl_id).cloned() else {
                        continue;
                    };
                    use ss::Declaration as Decl;
                    match decl {
                        | Decl::VAliasBody(valias_body) => valias_body.lower(&mut self, ()),
                        | Decl::VAliasHead(valias_head) => valias_head.lower(&mut self, ()),
                        | Decl::TAliasBody(_) | Decl::Exec(_) => {}
                    }
                }
                scc.release(group);
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
                let mut tail = lowered;
                for &def in self.sequence.iter().rev() {
                    let bindee = self.globals[&def];
                    let binder = ValuePattern::Var(def).build(&mut self.arena, None);
                    tail = Let { binder, bindee, tail }.build(&mut self.arena, None);
                }
                tail
            };

            // Register as entry point
            self.arena.inner.entry.insert(wrapped, ());
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
        let value_id = Phantom::new(bindee).lower(lo, Box::new(move |val_id, _lo| val_id));
        lo.sequence.push(def_id);
        lo.globals.insert(def_id, value_id);
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
        lo.globals.insert(def, value);
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
            | SSVPat::Ctor(ctor) => {
                use zydeco_syntax::Ctor;
                let Ctor(name, tail) = ctor;
                let tail_vpat = tail.lower(lo, ());
                Ctor(name, tail_vpat).into()
            }
            | SSVPat::Triv(triv) => triv.into(),
            | SSVPat::VCons(cons) => {
                use zydeco_syntax::Cons;
                let Cons(a, b) = cons;
                let a_vpat = a.lower(lo, ());
                let b_vpat = b.lower(lo, ());
                Cons(a_vpat, b_vpat).into()
            }
            | SSVPat::TCons(_) => {
                panic!("TCons patterns should not appear in stack IR")
            }
        };
        // Create new VPatId in stack arena and store the mapping
        stack_vpat.build(lo, Some(ss_pat_id))
    }
}

impl<T: 'static> Lower for Phantom<ss::ValueId, T> {
    type Kont = Box<dyn FnOnce(ValueId, &mut Lowerer) -> T>;
    type Out = T;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.statics.values[self.as_ref()].clone();
        let site = Some(ss::TermId::Value(self.clone_inner()));
        match value {
            | ss::Value::Hole(_) => {
                let value_id = Hole.build(lo, site);
                kont(value_id, lo)
            }
            | ss::Value::Var(def) => {
                let value_id = def.build(lo, site);
                kont(value_id, lo)
            }
            | ss::Value::Thunk(Thunk(body)) => {
                let body_compu = body.lower(lo, ());
                // Get minimal capture from cocontext information
                let capture = lo.compute_capture(body, None);
                let value_id = Closure { capture, stack: Bullet, body: body_compu }.build(lo, site);
                kont(value_id, lo)
            }
            | ss::Value::Ctor(Ctor(ctor, body)) => Phantom::new(body).lower(
                lo,
                Box::new(move |body_val, lo| {
                    let value_id = Ctor(ctor, body_val).build(lo, site);
                    kont(value_id, lo)
                }),
            ),
            | ss::Value::Triv(_) => {
                let value_id = Triv.build(lo, site);
                kont(value_id, lo)
            }
            | ss::Value::VCons(Cons(a, b)) => Phantom::new(a).lower(
                lo,
                Box::new(move |a_val, lo| {
                    Phantom::new(b).lower(
                        lo,
                        Box::new(move |b_val, lo| {
                            let value_id = Cons(a_val, b_val).build(lo, site);
                            kont(value_id, lo)
                        }),
                    )
                }),
            ),
            | ss::Value::TCons(Cons(_ty, inner)) => {
                // Type cons values are erased
                Phantom::new(inner).lower(lo, kont)
            }
            | ss::Value::Lit(lit) => {
                let value_id = lit.build(lo, site);
                kont(value_id, lo)
            }
        }
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
            | Compu::Hole(Hole) => Hole.build(lo, site),
            | Compu::VAbs(Abs(param, body)) => {
                let param_vpat = param.lower(lo, ());
                let body_compu = body.lower(lo, ());
                let stack_id = Bullet.build(lo, site);
                Let { binder: Cons(param_vpat, Bullet), bindee: stack_id, tail: body_compu }
                    .build(lo, site)
            }
            | Compu::VApp(App(body, arg)) => Phantom::new(arg).lower(
                lo,
                Box::new(move |arg_val, lo| {
                    let next_stack = Bullet.build(lo, site);
                    let stack_id = Cons(arg_val, next_stack).build(lo, site);
                    let body_compu = body.lower(lo, ());
                    let let_stack = Let { binder: Bullet, bindee: stack_id, tail: body_compu };
                    let_stack.build(lo, site)
                }),
            ),
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
                let capture = lo.compute_capture(body, Some(def_id));
                let body_compu = body.lower(lo, ());
                SFix { capture, param: def_id, body: body_compu }.build(lo, site)
            }
            | Compu::Force(Force(body)) => Phantom::new(body).lower(
                lo,
                Box::new(move |thunk_val, lo| {
                    SForce { thunk: thunk_val, stack: Bullet.build(lo, site) }.build(lo, site)
                }),
            ),
            | Compu::Ret(Return(body)) => Phantom::new(body).lower(
                lo,
                Box::new(move |value, lo| {
                    let stack_id = Bullet.build(lo, site);
                    SReturn { stack: stack_id, value }.build(lo, site)
                }),
            ),
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let binder_vpat = binder.lower(lo, ());
                let tail_compu = tail.lower(lo, ());
                let kont_stack_id = Kont { binder: binder_vpat, body: tail_compu }.build(lo, site);
                let bindee_compu = bindee.lower(lo, ());
                Let { binder: Bullet, bindee: kont_stack_id, tail: bindee_compu }.build(lo, site)
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                let binder_vpat = binder.lower(lo, ());
                Phantom::new(bindee).lower(
                    lo,
                    Box::new(move |bindee_val, lo| {
                        let tail_compu = tail.lower(lo, ());
                        Let { binder: binder_vpat, bindee: bindee_val, tail: tail_compu }
                            .build(lo, site)
                    }),
                )
            }
            | Compu::Match(Match { scrut, arms }) => {
                // Match: lower the scrutinee, then create a case statement
                Phantom::new(scrut).lower(
                    lo,
                    Box::new(move |scrut_val, lo| {
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
                        Match { scrut: scrut_val, arms: lowered_arms }.build(lo, site)
                    }),
                )
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                let lowered_arms = arms
                    .iter()
                    .map(|arm| {
                        let CoMatcher { dtor, tail } = arm;
                        let body_compu = tail.lower(lo, ());
                        CoMatcher { dtor: Cons(dtor.clone(), Bullet), tail: body_compu }
                    })
                    .collect();
                CoMatch { arms: lowered_arms }.build(lo, site)
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                // Destructor: push the destructor onto the stack and continue with body
                let next_stack = Bullet.build(lo, Some(ss::TermId::Compu(body)));
                let tag_stack_id = Cons(dtor.clone(), next_stack).build(lo, site);
                let body_compu = body.lower(lo, ());
                // Create LetStack to bind from the stack with the tag to the current stack, then run body
                Let { binder: Bullet, bindee: tag_stack_id, tail: body_compu }.build(lo, site)
            }
        }
    }
}
