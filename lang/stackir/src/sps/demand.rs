use std::collections::{BTreeMap, HashMap};
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_syntax::{Alias, ConsN, Ctor, Named};
use zydeco_utils::prelude::ArenaAccess;

use super::value_functions as definitions;

/// How live code consumes the value bound by one definition.
///
/// The analysis reads the checked program backwards from the root: a binding
/// survives only when its binder is demanded, and a product survives only in
/// the positions that are actually projected. Because values are pure and
/// computation sequencing goes through `Do`, an absent binding or field can
/// be skipped or replaced by a trivial value without observable effect.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Demand {
    /// Nothing references the binding; its bindee never evaluates.
    #[default]
    Absent,
    /// Only the listed product positions are ever projected; other positions
    /// are never observed and may hold trivial values instead. An empty map
    /// still observes the product's shape — a pattern that unpacks the
    /// product needs a real product in that position, with every position
    /// itself trivial.
    Fields(BTreeMap<usize, Demand>),
    /// The value flows into an unknown context and must be kept whole.
    Used,
}

impl Demand {
    pub fn is_absent(&self) -> bool {
        matches!(self, Self::Absent)
    }

    /// Join two demands for the same binding: keep the union of what either
    /// context needs.
    fn join(self, other: Self) -> Self {
        match (self, other) {
            | (Self::Used, _) | (_, Self::Used) => Self::Used,
            | (Self::Absent, rest) | (rest, Self::Absent) => rest,
            | (Self::Fields(mut left), Self::Fields(right)) => {
                for (position, demand) in right {
                    let joined = match left.remove(&position) {
                        | Some(left) => left.join(demand),
                        | None => demand,
                    };
                    left.insert(position, joined);
                }
                Self::Fields(left)
            }
        }
    }

    /// The demand on the head of a resolved projection chain.
    ///
    /// `products` are stored outermost first: folding in reverse nests each
    /// projection around the incoming demand of the projected value.
    fn through_projections(self, products: &[ss::ProductProjection]) -> Self {
        products.iter().rev().fold(self, |demand, projection| {
            let mut fields = BTreeMap::new();
            fields.insert(projection.position, demand);
            Self::Fields(fields)
        })
    }
}

/// Demand analysis over one checked computation root.
///
/// The pass visits every computation that can still run, so any `Let` binder
/// whose demand is absent is provably dead. Recursive `Fix` bindings are
/// always visited and never eliminated: an outer fixpoint body may reference
/// an inner fixpoint's parameter, so a binding-site decision taken before
/// that body is visited could drop a definition that later code reaches.
/// That reference pattern only arises between mutually recursive fixpoints,
/// which plain `Let` bindings cannot express.
pub struct DefinitionDemand {
    demands: HashMap<ss::DefId, Demand>,
    /// Value-function definitions seen so far, used to resolve application
    /// heads so caller demand can reach callee bodies.
    value_functions: definitions::Definitions,
    /// The joined demand each visited value node was analyzed under, so
    /// lowering can skip product positions nothing projects.
    contexts: HashMap<ss::ValueId, Demand>,
}

impl DefinitionDemand {
    /// Analyze one checked root. The root's own result is demanded whole.
    ///
    /// The traversal repeats until its demand tables stop growing: an
    /// application site demands its argument through the parameter pattern of
    /// the callee, whose definitions are only complete once the callee body
    /// has been visited, and joins only grow, so successive rounds converge.
    pub fn new(statics: &StaticsArena, root: ss::CompuId) -> Self {
        let mut analysis = Self {
            demands: HashMap::new(),
            value_functions: HashMap::new(),
            contexts: HashMap::new(),
        };
        loop {
            let previous = (analysis.demands.clone(), analysis.contexts.clone());
            analysis.visit_compu(statics, &root, Demand::Used);
            if analysis.demands == previous.0 && analysis.contexts == previous.1 {
                break;
            }
        }
        analysis
    }

    /// The demand one visited value node was analyzed under. Unvisited nodes
    /// report whole-value demand, matching lowering without trimming.
    pub fn value_demand(&self, node: ss::ValueId) -> Demand {
        self.contexts.get(&node).cloned().unwrap_or(Demand::Used)
    }

    /// Print every definition's final demand, for debugging trimming.
    pub fn trace(
        &self, statics: &StaticsArena, scoped: &zydeco_surface::scoped::arena::ScopedArena,
    ) {
        use std::fmt::Write as _;
        let mut names: Vec<_> = self
            .demands
            .iter()
            .map(|(def, demand)| {
                let name = statics.def_name(scoped, def).0.clone();
                let mut described = String::new();
                match demand {
                    | Demand::Absent => write!(described, "absent"),
                    | Demand::Used => write!(described, "used"),
                    | Demand::Fields(fields) => {
                        write!(described, "fields {:?}", fields.keys().collect::<Vec<_>>())
                    }
                }
                .unwrap();
                (name, described)
            })
            .collect();
        names.sort();
        names.into_iter().for_each(|(name, described)| eprintln!("{name}: {described}"));
    }

    /// Whether none of the definitions bound by `binder` is demanded.
    pub fn is_absent(&self, statics: &StaticsArena, binder: &ss::VPatId) -> bool {
        self.pattern_demand(statics, binder).is_absent()
    }

    /// The demand accumulated for the definitions bound by `pattern`.
    pub fn pattern_demand(&self, statics: &StaticsArena, pattern: &ss::VPatId) -> Demand {
        match statics.vpats[pattern].clone() {
            | ss::ValuePattern::Var(def) => self.demands.get(&def).cloned().unwrap_or_default(),
            | ss::ValuePattern::Named(Named(_, inner)) => self.pattern_demand(statics, &inner),
            // Every member of a pattern alias matches the same scrutinee, so
            // the demanded structure is the join of the members' demands.
            | ss::ValuePattern::Alias(Alias(patterns)) => patterns
                .iter()
                .map(|pattern| self.pattern_demand(statics, pattern))
                .fold(Demand::Absent, Demand::join),
            | ss::ValuePattern::VCons(items) => {
                self.positional_demand(statics, items.iter().copied())
            }
            | ss::ValuePattern::SCons(ConsN(_, tail)) => self.pattern_demand(statics, &tail),
            // Matching a constructor observes the tag, so a constructor
            // pattern demands the scrutinee whole even when its payload
            // binders are all ignored.
            | ss::ValuePattern::Ctor(_) => Demand::Used,
            // The view's function result is matched structurally; keep it whole.
            | ss::ValuePattern::View(_) => Demand::Used,
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Triv(_) => Demand::Absent,
        }
    }

    fn positional_demand(
        &self, statics: &StaticsArena, patterns: impl Iterator<Item = ss::VPatId>,
    ) -> Demand {
        let fields = patterns
            .enumerate()
            .filter_map(|(position, pattern)| {
                let demand = self.pattern_demand(statics, &pattern);
                (!demand.is_absent()).then_some((position, demand))
            })
            .collect::<BTreeMap<_, _>>();
        Demand::Fields(fields)
    }

    fn join_def(&mut self, def: ss::DefId, demand: Demand) {
        let joined = match self.demands.remove(&def) {
            | Some(left) => left.join(demand),
            | None => demand,
        };
        self.demands.insert(def, joined);
    }

    fn visit_compu(&mut self, statics: &StaticsArena, compu: &ss::CompuId, ctx: Demand) {
        match statics.compus[compu].clone() {
            | ss::Computation::Hole(_) => {}
            | ss::Computation::VAbs(ss::Abs(param, body)) => {
                if !ctx.is_absent() {
                    self.visit_pattern(statics, &param);
                    // The function's result is demanded by unknown callers.
                    self.visit_compu(statics, &body, Demand::Used);
                }
            }
            | ss::Computation::VApp(ss::App(head, argument)) => {
                self.visit_compu(statics, &head, ctx);
                self.visit_value(statics, &argument, Demand::Used);
            }
            | ss::Computation::TAbs(ss::Abs(_, body)) => self.visit_compu(statics, &body, ctx),
            | ss::Computation::TApp(ss::App(head, _)) => self.visit_compu(statics, &head, ctx),
            | ss::Computation::Fix(ss::Fix(param, body)) => {
                self.visit_pattern(statics, &param);
                self.visit_compu(statics, &body, Demand::Used);
            }
            // Forcing enters the thunk, so the thunk value is consumed whole;
            // the context demand belongs to the result, which only a literal
            // `Thunk` node could forward to its suspended body.
            | ss::Computation::Force(ss::Force(thunk)) => {
                self.visit_value(statics, &thunk, Demand::Used);
            }
            | ss::Computation::Ret(ss::Return(value)) => self.visit_value(statics, &value, ctx),
            | ss::Computation::Do(ss::Bind { binder, bindee, tail }) => {
                self.visit_compu(statics, &tail, ctx);
                // The sequencing always evaluates; only its result is slimmed.
                self.visit_pattern(statics, &binder);
                let binder_demand = self.pattern_demand(statics, &binder);
                self.visit_compu(statics, &bindee, binder_demand);
            }
            | ss::Computation::Let(ss::Let { binder, bindee, tail }) => {
                definitions::record_binding(statics, &mut self.value_functions, binder, bindee);
                self.visit_compu(statics, &tail, ctx);
                let binder_demand = self.pattern_demand(statics, &binder);
                if !binder_demand.is_absent() {
                    self.visit_pattern(statics, &binder);
                    self.visit_value(statics, &bindee, binder_demand);
                }
            }
            | ss::Computation::Match(ss::Match { scrut, arms }) => {
                let scrut_demand = arms
                    .iter()
                    .map(|arm| {
                        self.visit_compu(statics, &arm.tail, ctx.clone());
                        self.visit_pattern(statics, &arm.binder);
                        self.pattern_demand(statics, &arm.binder)
                    })
                    .fold(Demand::Absent, Demand::join);
                // Matching consumes the scrutinee even when no arm binder
                // reads a position, so an absent demand still requires the
                // value to exist.
                let scrut_demand =
                    if scrut_demand.is_absent() { Demand::Used } else { scrut_demand };
                self.visit_value(statics, &scrut, scrut_demand);
            }
            | ss::Computation::CoMatch(ss::CoMatch { arms }) => {
                for arm in &arms {
                    // Dtor invocation sites decide the result demand.
                    self.visit_compu(statics, &arm.tail, Demand::Used);
                }
            }
            | ss::Computation::Dtor(ss::Dtor(head, _)) => {
                self.visit_compu(statics, &head, Demand::Used);
            }
        }
    }

    fn visit_value(&mut self, statics: &StaticsArena, value: &ss::ValueId, ctx: Demand) {
        if statics.foreign_imports.get(value).is_some() {
            return;
        }
        let joined = match self.contexts.remove(value) {
            | Some(earlier) => earlier.join(ctx.clone()),
            | None => ctx.clone(),
        };
        self.contexts.insert(*value, joined);
        match statics.values[value].clone() {
            | ss::Value::Hole(_) | ss::Value::Triv(_) | ss::Value::Lit(_) => {}
            | ss::Value::Var(def) => self.join_def(def, ctx),
            | ss::Value::Named(Named(_, inner)) => self.visit_value(statics, &inner, ctx),
            | ss::Value::Let(ss::Let { binder, bindee, tail }) => {
                definitions::record_binding(statics, &mut self.value_functions, binder, bindee);
                self.visit_value(statics, &tail, ctx);
                let binder_demand = self.pattern_demand(statics, &binder);
                if !binder_demand.is_absent() {
                    self.visit_pattern(statics, &binder);
                    self.visit_value(statics, &bindee, binder_demand);
                }
            }
            | ss::Value::ValAbs(ss::Abs(param, body)) => {
                if !ctx.is_absent() {
                    if let ss::ValBinder::Value(param) = param {
                        self.visit_pattern(statics, &param);
                    }
                    // The abstraction's result is consumed only at its
                    // applications, so the caller's demand is the demand of
                    // its body.
                    self.visit_value(statics, &body, ctx);
                }
            }
            | ss::Value::ValApp(ss::App(function, argument)) => {
                // Applying a value function unfolds its definition: a runtime
                // argument is demanded like a let bindee, against the
                // parameter pattern its cut binds, and the caller's demand
                // reaches the callee's body through the head's definition
                // binding.
                let reduction = definitions::reduce_application(
                    statics,
                    &self.value_functions,
                    function,
                    &argument,
                );
                match argument {
                    | ss::ValArgument::Type(_) => self.visit_value(statics, &function, ctx),
                    | ss::ValArgument::Value(argument) => {
                        match reduction {
                            | Some((parameters, _)) => {
                                let argument_demand = parameters
                                    .last()
                                    .map(|parameter| self.pattern_demand(statics, parameter))
                                    .unwrap_or(Demand::Used);
                                self.visit_value(statics, &argument, argument_demand);
                                self.visit_value(statics, &function, ctx);
                            }
                            | None => {
                                // Statically unresolved heads do not occur in
                                // checked programs; stay conservative when
                                // they do.
                                self.visit_value(statics, &function, Demand::Used);
                                self.visit_value(statics, &argument, Demand::Used);
                            }
                        }
                    }
                }
            }
            // The thunk's suspended body produces the demanded result.
            | ss::Value::Thunk(ss::Thunk(body)) => self.visit_compu(statics, &body, ctx),
            | ss::Value::Ctor(Ctor(_, payload)) => {
                self.visit_value(statics, &payload, Demand::Used);
            }
            | ss::Value::VCons(items) => match &ctx {
                | Demand::Absent => {}
                | Demand::Used => {
                    for item in items {
                        self.visit_value(statics, &item, Demand::Used);
                    }
                }
                | Demand::Fields(fields) => {
                    for (position, item) in items.into_iter().enumerate() {
                        if let Some(demand) = fields.get(&position) {
                            self.visit_value(statics, &item, demand.clone());
                        }
                    }
                }
            },
            | ss::Value::SCons(ConsN(_, body)) => self.visit_value(statics, &body, ctx),
            | ss::Value::Proj(ss::Proj(head, field)) => {
                let head_demand = ctx.through_projections(&field.target.products);
                if std::env::var_os("ZYDECO_TRACE_TRIMMING").is_some() {
                    let positions: Vec<_> =
                        field.target.products.iter().map(|step| step.position).collect();
                    eprintln!("proj /{} steps {:?}", field.name, positions);
                }
                self.visit_value(statics, &head, head_demand);
            }
        }
    }

    /// Visit the view functions embedded in a live pattern.
    fn visit_pattern(&mut self, statics: &StaticsArena, pattern: &ss::VPatId) {
        match statics.vpats[pattern].clone() {
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {}
            | ss::ValuePattern::Named(Named(_, inner)) => self.visit_pattern(statics, &inner),
            | ss::ValuePattern::Ctor(Ctor(_, payload)) => self.visit_pattern(statics, &payload),
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns.iter() {
                    self.visit_pattern(statics, pattern);
                }
            }
            | ss::ValuePattern::VCons(items) => {
                for item in items {
                    self.visit_pattern(statics, &item);
                }
            }
            | ss::ValuePattern::SCons(ConsN(_, tail)) => self.visit_pattern(statics, &tail),
            | ss::ValuePattern::View(view) => {
                self.visit_value(statics, &view.function, Demand::Used);
                self.visit_pattern(statics, &view.pattern);
            }
        }
    }
}
