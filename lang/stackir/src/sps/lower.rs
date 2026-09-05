use super::{
    check::BranchJoinProgram,
    demand::{DefinitionDemand, Demand},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use std::collections::HashMap;
use zydeco_statics::{BuiltinPackagePlan, BuiltinPackageValue, arena::StaticsArena, syntax as ss};
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::{pass::CompilerPass, prelude::ArenaAccess};

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

/// A source-pattern decision whose view transformations have not yet been
/// expanded into structural Stack IR bindings and matches.
#[derive(Clone)]
enum MatchPlan {
    Fail,
    Tail(ss::CompuId),
    Pattern {
        scrutinee: DefId,
        pattern: ss::VPatId,
        success: Box<MatchPlan>,
        failure: Box<MatchPlan>,
    },
    Apply {
        binder: DefId,
        function: ss::ValueId,
        argument: DefId,
        tail: Box<MatchPlan>,
    },
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
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    demand: DefinitionDemand,
    /// Definitions of second-class value functions whose binding lowering has
    /// elided, mapped to the right-hand side their applications unfold to.
    /// Entries follow lowering order, so a right-hand side only chases
    /// definitions recorded before it.
    value_functions: HashMap<ss::DefId, ss::ValueId>,
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
    /// Create a new lowerer with fresh stack arenas, analyzing which of the
    /// root's bindings survive lowering.
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena, root: ss::CompuId,
    ) -> Self {
        let arena = StackirArena::default();
        let demand = DefinitionDemand::new(statics, root);
        let value_functions = HashMap::new();
        Self { arena, spans, scoped, statics, demand, value_functions }
    }

    fn product_arity(&self, ty: ss::TypeId) -> usize {
        match self.statics.normalized_at(ty) {
            | Some(ss::Type::Unit(_)) => 0,
            | Some(ss::Type::Prod(ss::Prod(components))) => components.len(),
            | _ => unreachable!("VCons must have Unit or product type"),
        }
    }

    fn product_layout(&self, ty: ss::TypeId) -> ProductLayout {
        ProductLayout { arity: self.product_arity(ty) }
    }

    fn alloc_projection_def(&mut self) -> DefId {
        self.alloc_admin_def("__proj__")
    }

    fn alloc_admin_def(&mut self, role: &str) -> DefId {
        let def = self.arena.admin.fresh();
        self.arena.admin.insert_def(def, VarName(role.to_owned()));
        def
    }

    fn pattern_contains_view(&self, pattern: ss::VPatId) -> bool {
        match &self.statics.vpats[&pattern] {
            | ss::ValuePattern::View(_) => true,
            | ss::ValuePattern::Named(Named(_, pattern))
            | ss::ValuePattern::Ctor(Ctor(_, pattern))
            | ss::ValuePattern::SCons(ss::ConsN(_, pattern)) => {
                self.pattern_contains_view(*pattern)
            }
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().any(|pattern| self.pattern_contains_view(*pattern))
            }
            | ss::ValuePattern::VCons(patterns) => {
                patterns.iter().any(|pattern| self.pattern_contains_view(*pattern))
            }
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {
                false
            }
        }
    }

    /// Expand a view-bearing pattern used by a complex-value `let` into the
    /// ordered structural bindings that implement its value-level cuts.
    fn lower_value_pattern_bindings(
        &mut self, pattern: ss::VPatId, bindee: ValueId, site: Option<ss::TermId>,
    ) -> Vec<ValueStep> {
        if !self.pattern_contains_view(pattern) {
            return vec![ValueStep::Bind(ValueBinding {
                binder: pattern.lower(self, ()),
                bindee,
                site,
            })];
        }

        match self.statics.vpats[&pattern].clone() {
            | ss::ValuePattern::Named(Named(_, inner))
            | ss::ValuePattern::SCons(ss::ConsN(_, inner)) => {
                self.lower_value_pattern_bindings(inner, bindee, site)
            }
            | ss::ValuePattern::View(view) => {
                let ss::ViewPattern { function, pattern } = *view;
                if let Some(unfolded) =
                    self.unfold_value_application(function, ValuePlan::pure(bindee), site)
                {
                    let result = self.alloc_admin_def("__view_result__");
                    let binder = result.build(self, None);
                    let binding =
                        ValueStep::Bind(ValueBinding { binder, bindee: unfolded.value, site });
                    let result = result.build(self, site);
                    let nested = self.lower_value_pattern_bindings(pattern, result, site);
                    return unfolded.steps.into_iter().chain([binding]).chain(nested).collect();
                }
                let ValuePlan { steps, value: function } = function.lower(self, ());
                let result = self.alloc_admin_def("__view_result__");
                let binder = result.build(self, None);
                let application =
                    ValueStep::Apply(ValueApplication { binder, function, argument: bindee, site });
                let result = result.build(self, site);
                let nested = self.lower_value_pattern_bindings(pattern, result, site);
                steps.into_iter().chain([application]).chain(nested).collect()
            }
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                let whole = self.alloc_admin_def("__view_alias__");
                let whole_pattern = whole.build(self, None);
                let binding = ValueStep::Bind(ValueBinding { binder: whole_pattern, bindee, site });
                std::iter::once(binding)
                    .chain(patterns.into_iter().flat_map(|pattern| {
                        let bindee = whole.build(self, site);
                        self.lower_value_pattern_bindings(pattern, bindee, site)
                    }))
                    .collect()
            }
            | ss::ValuePattern::VCons(patterns) => {
                let layout = self.product_layout(self.statics.annotations_vpat[&pattern]);
                let components = patterns
                    .iter()
                    .map(|_| self.alloc_admin_def("__view_component__"))
                    .collect::<Vec<_>>();
                let fields = components.iter().map(|definition| definition.build(self, None));
                let binder = VCons::new(fields.collect(), layout).build(self, None);
                let binding = ValueStep::Bind(ValueBinding { binder, bindee, site });
                std::iter::once(binding)
                    .chain(patterns.into_iter().zip(components).flat_map(
                        |(pattern, definition)| {
                            let bindee = definition.build(self, site);
                            self.lower_value_pattern_bindings(pattern, bindee, site)
                        },
                    ))
                    .collect()
            }
            | ss::ValuePattern::Ctor(Ctor(name, inner)) => {
                let payload = self.alloc_admin_def("__view_payload__");
                let payload_pattern = payload.build(self, None);
                let data = self.statics.data_pat_hints[&pattern];
                let index = self.statics.datas[&data]
                    .iter()
                    .position(|(candidate, _)| candidate == &name)
                    .expect("constructor tag not found");
                let binder = Ctor(CtorIdx { idx: index, name }, payload_pattern).build(self, None);
                let binding = ValueStep::Bind(ValueBinding { binder, bindee, site });
                let bindee = payload.build(self, site);
                std::iter::once(binding)
                    .chain(self.lower_value_pattern_bindings(inner, bindee, site))
                    .collect()
            }
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {
                unreachable!("a view-free pattern is lowered by the structural fast path")
            }
        }
    }

    fn match_plan(&self, scrutinee: DefId, arms: &[Matcher<ss::VPatId, ss::CompuId>]) -> MatchPlan {
        arms.iter().rev().fold(MatchPlan::Fail, |failure, arm| MatchPlan::Pattern {
            scrutinee,
            pattern: arm.binder,
            success: Box::new(MatchPlan::Tail(arm.tail)),
            failure: Box::new(failure),
        })
    }

    fn lower_match_plan(
        &mut self, plan: MatchPlan, stack: StackId, site: Option<ss::TermId>,
    ) -> CompuId {
        match plan {
            | MatchPlan::Fail => SHole(stack).build(self, site),
            | MatchPlan::Tail(tail) => tail.lower(self, stack),
            | MatchPlan::Apply { binder, function, argument, tail } => {
                let argument_value = argument.build(self, site);
                if let Some(unfolded) =
                    self.unfold_value_application(function, ValuePlan::pure(argument_value), site)
                {
                    let result_pattern = binder.build(self, None);
                    let result = binder.build(self, site);
                    let binding = ValueStep::Bind(ValueBinding {
                        binder: result_pattern,
                        bindee: unfolded.value,
                        site,
                    });
                    let plan = ValuePlan {
                        steps: unfolded.steps.into_iter().chain([binding]).collect(),
                        value: result,
                    };
                    return plan.lower_into(self, move |_, lowerer| {
                        lowerer.lower_match_plan(*tail, stack, site)
                    });
                }
                let function = function.lower(self, ());
                let function_value = function.value;
                let argument = argument.build(self, site);
                let result_pattern = binder.build(self, None);
                let result = binder.build(self, site);
                function
                    .with_application(
                        ValueApplication {
                            binder: result_pattern,
                            function: function_value,
                            argument,
                            site,
                        },
                        result,
                    )
                    .lower_into(self, move |_, lowerer| {
                        lowerer.lower_match_plan(*tail, stack, site)
                    })
            }
            | MatchPlan::Pattern { scrutinee, pattern, success, failure } => {
                match self.statics.vpats[&pattern].clone() {
                    | ss::ValuePattern::Hole(_) | ss::ValuePattern::Triv(_) => {
                        self.lower_match_plan(*success, stack, site)
                    }
                    | ss::ValuePattern::Var(definition) => {
                        let bindee: ValueId = scrutinee.build(self, site);
                        let binder: VPatId = definition.build(self, None);
                        let tail = self.lower_match_plan(*success, stack, site);
                        Let { binder, bindee, tail }.build(self, site)
                    }
                    | ss::ValuePattern::Named(Named(_, inner))
                    | ss::ValuePattern::SCons(ss::ConsN(_, inner)) => self.lower_match_plan(
                        MatchPlan::Pattern { scrutinee, pattern: inner, success, failure },
                        stack,
                        site,
                    ),
                    | ss::ValuePattern::Alias(Alias(patterns)) => {
                        let success =
                            patterns.into_iter().rev().fold(*success, |success, pattern| {
                                MatchPlan::Pattern {
                                    scrutinee,
                                    pattern,
                                    success: Box::new(success),
                                    failure: failure.clone(),
                                }
                            });
                        self.lower_match_plan(success, stack, site)
                    }
                    | ss::ValuePattern::VCons(patterns) => {
                        let layout = self.product_layout(self.statics.annotations_vpat[&pattern]);
                        let components = patterns
                            .iter()
                            .map(|_| self.alloc_admin_def("__match_component__"))
                            .collect::<Vec<_>>();
                        let fields =
                            components.iter().map(|definition| definition.build(self, None));
                        let binder = VCons::new(fields.collect(), layout).build(self, None);
                        let body_plan = patterns
                            .into_iter()
                            .zip(components.iter().copied())
                            .rev()
                            .fold(*success, |success, (pattern, component)| MatchPlan::Pattern {
                                scrutinee: component,
                                pattern,
                                success: Box::new(success),
                                failure: failure.clone(),
                            });
                        let body = self.lower_match_plan(body_plan, stack, site);
                        let scrut = scrutinee.build(self, site);
                        SProductMatch { scrut, binder, body }.build(self, site)
                    }
                    | ss::ValuePattern::Ctor(Ctor(name, argument)) => {
                        let data = self.statics.data_pat_hints[&pattern];
                        let constructors = self.statics.datas[&data].clone();
                        let arms = constructors
                            .iter()
                            .enumerate()
                            .map(|(index, (candidate, _))| {
                                let branch_stack = Bullet.build(self, site);
                                if candidate == &name {
                                    let payload = self.alloc_admin_def("__match_payload__");
                                    let payload_pattern = payload.build(self, None);
                                    let binder = Ctor(
                                        CtorIdx { idx: index, name: candidate.clone() },
                                        payload_pattern,
                                    )
                                    .build(self, None);
                                    let tail = self.lower_match_plan(
                                        MatchPlan::Pattern {
                                            scrutinee: payload,
                                            pattern: argument,
                                            success: success.clone(),
                                            failure: failure.clone(),
                                        },
                                        branch_stack,
                                        site,
                                    );
                                    Matcher { binder, tail }
                                } else {
                                    let payload = Hole.build(self, None);
                                    let binder = Ctor(
                                        CtorIdx { idx: index, name: candidate.clone() },
                                        payload,
                                    )
                                    .build(self, None);
                                    let tail = self.lower_match_plan(
                                        (*failure).clone(),
                                        branch_stack,
                                        site,
                                    );
                                    Matcher { binder, tail }
                                }
                            })
                            .collect();
                        let scrut = scrutinee.build(self, site);
                        let body = SCoprodMatch { scrut, arms }.build(self, site);
                        Let { binder: Bullet, bindee: stack, tail: body }.build(self, site)
                    }
                    | ss::ValuePattern::View(view) => {
                        let ss::ViewPattern { function, pattern } = *view;
                        let output = self.alloc_admin_def("__view_result__");
                        let nested = MatchPlan::Pattern {
                            scrutinee: output,
                            pattern,
                            success,
                            failure: failure.clone(),
                        };
                        let transformed = MatchPlan::Apply {
                            binder: output,
                            function,
                            argument: scrutinee,
                            tail: Box::new(nested),
                        };
                        self.lower_match_plan(transformed, stack, site)
                    }
                }
            }
        }
    }

    fn lower_view_match(
        &mut self, scrut: ValueId, arms: &[Matcher<ss::VPatId, ss::CompuId>], stack: StackId,
        site: Option<ss::TermId>,
    ) -> CompuId {
        let scrutinee = self.alloc_admin_def("__view_scrutinee__");
        let binder = scrutinee.build(self, None);
        let plan = self.match_plan(scrutinee, arms);
        let tail = self.lower_match_plan(plan, stack, site);
        Let { binder, bindee: scrut, tail }.build(self, site)
    }

    fn is_coprod_pattern(&self, pattern: ss::VPatId) -> bool {
        match &self.statics.vpats[&pattern] {
            | ss::ValuePattern::Ctor(_) => true,
            | ss::ValuePattern::Named(Named(_, pattern)) => self.is_coprod_pattern(*pattern),
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().any(|pattern| self.is_coprod_pattern(*pattern))
            }
            | ss::ValuePattern::SCons(ss::ConsN(_, pattern)) => self.is_coprod_pattern(*pattern),
            | ss::ValuePattern::View(view) => self.is_coprod_pattern(view.pattern),
            | ss::ValuePattern::Hole(_)
            | ss::ValuePattern::Var(_)
            | ss::ValuePattern::Triv(_)
            | ss::ValuePattern::VCons(_) => false,
        }
    }

    fn is_coprod_match(&self, arms: &[Matcher<ss::VPatId, ss::CompuId>]) -> bool {
        match arms {
            | [Matcher { binder, tail: _ }] => self.is_coprod_pattern(*binder),
            | _ => true,
        }
    }

    fn finish(self, root: CompuId) -> BranchJoinProgram {
        BranchJoinProgram::try_new(StackirProgram::new(self.arena, root))
            .expect("stack-indexed lowering must construct branch-join SPS")
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
        let binder = VCons::new(fields, layout).build(self, None);
        let projected = selected.build(self, site);
        (ValueBinding { binder, bindee: head, site }, projected)
    }

    /// Whether the recorded classifier of one typed value is a `val pi`.
    fn is_value_function_value(&self, value: ss::ValueId) -> bool {
        self.statics
            .annotations_value
            .get(&value)
            .and_then(|ty| self.statics.normalized_at(*ty))
            .is_some_and(|ty| matches!(ty, ss::Type::ValPi(_)))
    }

    /// The next runtime binder of a value function, after chasing elided
    /// bindings, aliases, erased type binders, and erased type arguments.
    fn next_abstraction(&self, node: ss::ValueId) -> Option<(ss::VPatId, ss::ValueId)> {
        match &self.statics.values[&node] {
            | ss::Value::ValAbs(Abs(ss::ValBinder::Value(param), body)) => Some((*param, *body)),
            | ss::Value::ValAbs(Abs(ss::ValBinder::Type(_), body)) => self.next_abstraction(*body),
            | ss::Value::ValApp(App(function, ss::ValArgument::Type(_))) => {
                self.next_abstraction(*function)
            }
            | ss::Value::Var(def) => {
                self.value_functions.get(def).and_then(|rhs| self.next_abstraction(*rhs))
            }
            | _ => None,
        }
    }

    /// Split an application into its runtime arguments and its head, chasing
    /// elided definitions so a bound partial application contributes its own
    /// arguments. Arguments come back in application order.
    fn application_spine(&self, node: ss::ValueId) -> Option<(Vec<ss::ValueId>, ss::ValueId)> {
        let mut arguments = Vec::new();
        let mut head = node;
        loop {
            match &self.statics.values[&head] {
                | ss::Value::ValApp(App(function, ss::ValArgument::Value(argument))) => {
                    arguments.push(*argument);
                    head = *function;
                }
                | ss::Value::Var(def) => match self.value_functions.get(def) {
                    | Some(rhs) => head = *rhs,
                    | None => break,
                },
                | _ => break,
            }
        }
        arguments.reverse();
        Some((arguments, head))
    }

    /// Whether reducing one application spine against the abstractions under
    /// `head` never gets stuck before the arguments are consumed.
    fn spine_reduces(&self, head: ss::ValueId, arguments: &[ss::ValueId]) -> bool {
        let mut cursor = head;
        for _ in arguments {
            let Some((_, body)) = self.next_abstraction(cursor) else { return false };
            cursor = body;
        }
        true
    }

    /// Whether applications of `bindee` can always unfold to lexical bindings.
    fn is_unfoldable_definition(&self, bindee: ss::ValueId) -> bool {
        if self.next_abstraction(bindee).is_some() {
            return true;
        }
        if matches!(
            &self.statics.values[&bindee],
            ss::Value::ValApp(App(_, ss::ValArgument::Value(_)))
        ) {
            let Some((arguments, head)) = self.application_spine(bindee) else { return false };
            return self.spine_reduces(head, &arguments);
        }
        false
    }

    /// The definitions a pattern binds, when every one of them is a plain
    /// variable or wildcard position.
    fn bound_definitions(&self, pattern: ss::VPatId) -> Option<Vec<ss::DefId>> {
        match self.statics.vpats[&pattern].clone() {
            | ss::ValuePattern::Var(def) => Some(vec![def]),
            | ss::ValuePattern::Hole(_) => Some(Vec::new()),
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                let mut definitions = Vec::new();
                for pattern in patterns.iter() {
                    definitions.extend(self.bound_definitions(*pattern)?);
                }
                Some(definitions)
            }
            | _ => None,
        }
    }

    /// Elide the lowering of one value-function definition binding, recording
    /// its right-hand side for unfolding at each application instead.
    /// Returns whether the binding may be skipped.
    fn record_value_function_binding(&mut self, binder: ss::VPatId, bindee: ss::ValueId) -> bool {
        if !self.is_value_function_value(bindee) || !self.is_unfoldable_definition(bindee) {
            return false;
        }
        let Some(definitions) = self.bound_definitions(binder) else { return false };
        definitions.into_iter().for_each(|def| {
            self.value_functions.insert(def, bindee);
        });
        true
    }

    /// Unfold one value application: reduce the application spine ending at
    /// `function` — including `outer_argument`, already lowered — against the
    /// function's abstractions, elaborating each cut as a lexical pattern
    /// binding. Returns `None` when the head is not statically reducible.
    fn unfold_value_application(
        &mut self, function: ss::ValueId, outer_argument: ValuePlan<ValueId>,
        site: Option<ss::TermId>,
    ) -> Option<ValuePlan<ValueId>> {
        let (arguments, head) = self.application_spine(function)?;
        let mut steps: Vec<ValueStep> = Vec::new();
        let mut cursor = head;
        let mut pending: Vec<ValuePlan<ValueId>> =
            arguments.into_iter().map(|argument| argument.lower(self, ())).collect();
        pending.push(outer_argument);
        for argument in pending {
            let (param, body) = self.next_abstraction(cursor)?;
            steps.extend(argument.steps);
            steps.extend(self.lower_value_pattern_bindings(param, argument.value, site));
            cursor = body;
        }
        let residual = cursor.lower(self, ());
        steps.extend(residual.steps);
        Some(ValuePlan { steps, value: residual.value })
    }
}

impl<'a> RootLowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena, root: ss::CompuId,
    ) -> Self {
        Self { lowerer: Lowerer::new(spans, scoped, statics, root), root }
    }
}

impl<'a> BuiltinRootLowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        root: ss::CompuId, signature: ss::PackPi,
    ) -> Self {
        Self { lowerer: Lowerer::new(spans, scoped, statics, root), root, signature }
    }

    /// The demand the analyzed root places on the host package. An executable
    /// root is a package abstraction, so the parameter pattern's demand
    /// describes exactly which package positions must be materialized.
    fn package_demand(&self) -> Demand {
        match &self.lowerer.statics.compus[&self.root] {
            | ss::Computation::VAbs(ss::Abs(param, _)) => {
                self.lowerer.demand.pattern_demand(self.lowerer.statics, param)
            }
            | _ => Demand::Used,
        }
    }
}

impl BuiltinPackageLowering {
    fn lower(
        value: BuiltinPackageValue, lowerer: &mut Lowerer<'_>, demand: &Demand,
    ) -> Result<ValueId, BuiltinPackageLowerError> {
        match value {
            | BuiltinPackageValue::Unit => Ok(Triv.build(lowerer, None)),
            | BuiltinPackageValue::Operation(role) => {
                if demand.is_absent() {
                    // The program never projects this entry; the position must
                    // still hold a value to preserve the product layout.
                    return Ok(Triv.build(lowerer, None));
                }
                let builtin = Builtin::for_role(&lowerer.arena.admin.builtins, role)?;
                Ok(match builtin.sort {
                    | BuiltinSort::Operator => builtin.make_operator(lowerer),
                    | BuiltinSort::Function(_) => builtin.make_function(lowerer),
                })
            }
            | BuiltinPackageValue::Product(product) => {
                let values = match demand {
                    | Demand::Absent => {
                        product.into_iter().map(|_| Triv.build(lowerer, None)).collect::<Vec<_>>()
                    }
                    | Demand::Used => product
                        .into_iter()
                        .map(|value| Self::lower(value, lowerer, &Demand::Used))
                        .collect::<Result<Vec<_>, _>>()?,
                    | Demand::Fields(fields) => product
                        .into_iter()
                        .enumerate()
                        .map(|(position, value)| match fields.get(&position) {
                            | Some(demand) => Self::lower(value, lowerer, demand),
                            | None => Ok(Triv.build(lowerer, None)),
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                };
                let layout = ProductLayout { arity: values.len() };
                Ok(VCons::new(values, layout).build(lowerer, None))
            }
        }
    }
}

impl CompilerPass for RootLowerer<'_> {
    type Out = BranchJoinProgram;
    type Error = std::convert::Infallible;

    fn run(self) -> Result<BranchJoinProgram, Self::Error> {
        let Self { mut lowerer, root } = self;
        let stack = Bullet.build(&mut lowerer, None);
        let root = root.lower(&mut lowerer, stack);
        Ok(lowerer.finish(root))
    }
}

impl CompilerPass for BuiltinRootLowerer<'_> {
    type Out = BranchJoinProgram;
    type Error = BuiltinPackageLowerError;

    fn run(self) -> Result<BranchJoinProgram, Self::Error> {
        let demand = self.package_demand();
        let Self { mut lowerer, root, signature } = self;
        let plan = BuiltinPackagePlan::for_executable(lowerer.statics, &signature)?;
        let package = BuiltinPackageLowering::lower(plan.value, &mut lowerer, &demand)?;
        let stack = Cons(package, Bullet.build(&mut lowerer, None)).build(&mut lowerer, None);
        let root = root.lower(&mut lowerer, stack);
        Ok(lowerer.finish(root))
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
                let data_id = lo.statics.data_pat_hints[self];
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
            | SSVPat::VCons(items) => {
                let items = items.into_iter().map(|item| item.lower(lo, ())).collect();
                let ty = lo.statics.annotations_vpat[self];
                VCons::new(items, lo.product_layout(ty)).into()
            }
            | SSVPat::SCons(ss::ConsN(_, body)) => {
                let vpat = body.lower(lo, ());
                lo.arena.inner.vpats[&vpat].clone()
            }
            | SSVPat::View(_) => {
                unreachable!("view patterns must be expanded before structural Stack IR lowering")
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
        if let Some(import) = lo.statics.foreign_imports.get(self).cloned() {
            let site = Some(ss::TermId::Value(*self));
            let stack = Bullet.build(lo, site);
            let body =
                ExternCall { function: ExternalFunction::Foreign(import), stack }.build(lo, site);
            return ValuePlan::pure(Closure { stack: Bullet, body }.build(lo, site));
        }
        let value = lo.statics.values[self].clone();
        let site = Some(ss::TermId::Value(*self));
        match value {
            | ss::Value::Hole(_) => ValuePlan::pure(Hole.build(lo, site)),
            | ss::Value::Var(def) => ValuePlan::pure(def.build(lo, site)),
            | ss::Value::Named(Named(_, inner)) => inner.lower(lo, ()),
            | ss::Value::Let(Let { binder, bindee, tail }) => {
                if lo.demand.is_absent(lo.statics, &binder) {
                    return tail.lower(lo, ());
                }
                if lo.record_value_function_binding(binder, bindee) {
                    return tail.lower(lo, ());
                }
                let bindee = bindee.lower(lo, ());
                let tail = tail.lower(lo, ());
                let bindings = lo.lower_value_pattern_bindings(binder, bindee.value, site);
                ValuePlan {
                    steps: bindee.steps.into_iter().chain(bindings).chain(tail.steps).collect(),
                    value: tail.value,
                }
            }
            | ss::Value::ValAbs(Abs(ss::ValBinder::Type(_), body)) => body.lower(lo, ()),
            | ss::Value::ValAbs(Abs(ss::ValBinder::Value(param), body)) => {
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
            | ss::Value::ValApp(App(function, ss::ValArgument::Type(_))) => function.lower(lo, ()),
            | ss::Value::ValApp(App(function, ss::ValArgument::Value(argument))) => {
                let argument = argument.lower(lo, ());
                if let Some(unfolded) =
                    lo.unfold_value_application(function, argument.clone(), site)
                {
                    return unfolded;
                }
                let function = function.lower(lo, ());
                let steps = function.steps.into_iter().chain(argument.steps).collect::<Vec<_>>();
                let function = function.value;
                let argument = argument.value;
                let result = lo.alloc_admin_def("__value_result__");
                let binder = result.build(lo, None);
                let value = result.build(lo, site);
                ValuePlan { steps, value }
                    .with_application(ValueApplication { binder, function, argument, site }, value)
            }
            | ss::Value::Thunk(Thunk(body)) => {
                let stack = Bullet.build(lo, site);
                let body = body.lower(lo, stack);
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
                let items = items.lower(lo, ());
                items.map(|items| VCons::new(items, layout).build(lo, site))
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
    type Kont = StackId;
    type Out = CompuId;

    fn lower(&self, lo: &mut Lowerer, stack: Self::Kont) -> Self::Out {
        let compu = lo.statics.compus[self].clone();
        let site = Some(ss::TermId::Compu(*self));
        use ss::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => SHole(stack).build(lo, site),
            | Compu::VAbs(Abs(param, body)) => {
                let body_stack = Bullet.build(lo, site);
                let (param_vpat, body_compu) = if lo.pattern_contains_view(param) {
                    let argument = lo.alloc_admin_def("__view_argument__");
                    let param_vpat = argument.build(lo, None);
                    let plan = MatchPlan::Pattern {
                        scrutinee: argument,
                        pattern: param,
                        success: Box::new(MatchPlan::Tail(body)),
                        failure: Box::new(MatchPlan::Fail),
                    };
                    let body_compu = lo.lower_match_plan(plan, body_stack, site);
                    (param_vpat, body_compu)
                } else {
                    (param.lower(lo, ()), body.lower(lo, body_stack))
                };
                Let { binder: Cons(param_vpat, Bullet), bindee: stack, tail: body_compu }
                    .build(lo, site)
            }
            | Compu::VApp(App(body, arg)) => {
                let arg = arg.lower(lo, ());
                arg.lower_into(lo, move |arg, lo| {
                    let stack = Cons(arg, stack).build(lo, site);
                    body.lower(lo, stack)
                })
            }
            | Compu::TAbs(Abs(_param, body)) => {
                // Type abstractions are erased
                body.lower(lo, stack)
            }
            | Compu::TApp(App(body, _arg)) => {
                // Type applications are erased
                body.lower(lo, stack)
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
                let body_stack = Bullet.build(lo, site);
                let body_compu = body.lower(lo, body_stack);
                SFix { param: def_id, stack, body: body_compu }.build(lo, site)
            }
            | Compu::Force(Force(body)) => {
                let body = body.lower(lo, ());
                body.lower_into(lo, move |thunk, lo| SForce { thunk, stack }.build(lo, site))
            }
            | Compu::Ret(Return(body)) => {
                let body = body.lower(lo, ());
                body.lower_into(lo, move |value, lo| SReturn { stack, value }.build(lo, site))
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let (binder_vpat, tail_compu) = if lo.pattern_contains_view(binder) {
                    let returned = lo.alloc_admin_def("__view_returned__");
                    let binder_vpat = returned.build(lo, None);
                    let plan = MatchPlan::Pattern {
                        scrutinee: returned,
                        pattern: binder,
                        success: Box::new(MatchPlan::Tail(tail)),
                        failure: Box::new(MatchPlan::Fail),
                    };
                    let tail_compu = lo.lower_match_plan(plan, stack, site);
                    (binder_vpat, tail_compu)
                } else {
                    (binder.lower(lo, ()), tail.lower(lo, stack))
                };
                let kont_stack_id = Kont { binder: binder_vpat, body: tail_compu }.build(lo, site);
                bindee.lower(lo, kont_stack_id)
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                if lo.demand.is_absent(lo.statics, &binder) {
                    return tail.lower(lo, stack);
                }
                if lo.record_value_function_binding(binder, bindee) {
                    return tail.lower(lo, stack);
                }
                let bindee = bindee.lower(lo, ());
                bindee.lower_into(lo, move |bindee, lo| {
                    if lo.pattern_contains_view(binder) {
                        let scrutinee = lo.alloc_admin_def("__view_let__");
                        let binder_vpat = scrutinee.build(lo, None);
                        let plan = MatchPlan::Pattern {
                            scrutinee,
                            pattern: binder,
                            success: Box::new(MatchPlan::Tail(tail)),
                            failure: Box::new(MatchPlan::Fail),
                        };
                        let tail_compu = lo.lower_match_plan(plan, stack, site);
                        Let { binder: binder_vpat, bindee, tail: tail_compu }.build(lo, site)
                    } else {
                        let binder_vpat = binder.lower(lo, ());
                        let tail_compu = tail.lower(lo, stack);
                        Let { binder: binder_vpat, bindee, tail: tail_compu }.build(lo, site)
                    }
                })
            }
            | Compu::Match(Match { scrut, arms }) => {
                let has_view = arms.iter().any(|arm| lo.pattern_contains_view(arm.binder));
                let is_coprod = !has_view && lo.is_coprod_match(&arms);
                let scrut = scrut.lower(lo, ());
                scrut.lower_into(lo, move |scrut, lo| {
                    if has_view {
                        lo.lower_view_match(scrut, &arms, stack, site)
                    } else if is_coprod {
                        let lowered_arms = arms
                            .iter()
                            .map(|Matcher { binder, tail }| {
                                let binder = binder.lower(lo, ());
                                let branch_stack = Bullet.build(lo, site);
                                let tail = tail.lower(lo, branch_stack);
                                Matcher { binder, tail }
                            })
                            .collect();
                        let body = SCoprodMatch { scrut, arms: lowered_arms }.build(lo, site);
                        Let { binder: Bullet, bindee: stack, tail: body }.build(lo, site)
                    } else {
                        let [Matcher { binder, tail }] = arms.as_slice() else {
                            unreachable!("an irrefutable match has exactly one arm")
                        };
                        let binder = binder.lower(lo, ());
                        let body = tail.lower(lo, stack);
                        SProductMatch { scrut, binder, body }.build(lo, site)
                    }
                })
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let CoMatcher { dtor: name, tail } = arm;
                        let codata_id = lo.statics.codata_hints[self];
                        let idx = lo.statics.codatas[&codata_id]
                            .iter()
                            .position(|(tag_branch, _ty)| tag_branch == &name)
                            .expect("Destructor tag not found");
                        let dtor_idx = DtorIdx { idx, name };
                        let branch_stack = Bullet.build(lo, site);
                        let body_compu = tail.lower(lo, branch_stack);
                        CoMatcher { dtor: Cons(dtor_idx, Bullet), tail: body_compu }
                    })
                    .collect();
                SCoMatch { scrut: stack, arms }.build(lo, site)
            }
            | Compu::Dtor(Dtor(body, name)) => {
                let codata_id = lo.statics.codata_hints[&body];
                let idx = lo.statics.codatas[&codata_id]
                    .iter()
                    .position(|(tag_branch, _ty)| tag_branch == &name)
                    .expect("Destructor tag not found");
                let dtor_idx = DtorIdx { idx, name };
                let stack = Cons(dtor_idx, stack).build(lo, site);
                body.lower(lo, stack)
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
    fn computation_roots_lower_as_single_program_roots() {
        let mut allocator = IdAllocator::<StaticsScope>::new();
        let value = allocator.alloc();
        let root = allocator.alloc();
        let mut statics = StaticsArena::default();
        statics.values.insert_new(value, ss::Triv.into());
        statics.compus.insert_new(root, ss::Return(value).into());
        let spans = SpanArena::default();
        let scoped = ScopedArena::default();

        let stackir = RootLowerer::new(&spans, &scoped, &statics, root).run().unwrap();
        let stackir = stackir.as_program();

        assert!(stackir.arena().inner.compus.get(&stackir.root()).is_some());
        super::super::check::check(stackir, &scoped, &statics);
    }
}
