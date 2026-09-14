use super::{check::BranchJoinProgram, syntax::*};
use crate::protocol::SourceProtocols;
use ariadne::{Label, Report, ReportKind};
use derive_more::{AsMut, AsRef};
use std::ops::Range;
use thiserror::Error;
use zydeco_statics::{
    BuiltinPackagePlan, BuiltinPackagePlanError, BuiltinPackageValue, arena::StaticsArena,
    syntax as ss,
};
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::{
    fold::{Driver, Explicit},
    pass::CompilerPass,
    prelude::ArenaAccess,
    span::{PathDisplay, Span, internal_ariadne_span},
};

/// An internal invariant failure in a purportedly residual typed program.
/// Source-level static-elimination failures are reported by the shared checker.
#[derive(Clone, Debug, Error)]
pub enum SpsLowerError {
    #[error("internal compiler error: a static value operation survived static elimination")]
    ResidualStaticValue { value: ss::ValueId },
}

impl SpsLowerError {
    /// The typed node blamed for the failure.
    fn value(&self) -> ss::ValueId {
        match self {
            | Self::ResidualStaticValue { value } => *value,
        }
    }

    /// Primary source span of this lowering failure.
    pub fn primary_span(
        &self, spans: &SpanArena, scoped: &ScopedArena, statics: &StaticsArena,
    ) -> Span {
        statics
            .terms
            .source(&ss::TermId::Value(self.value()))
            .map(|term| *zydeco_syntax::span_via_back(spans, scoped, term))
            .unwrap_or_else(Span::dummy)
    }

    /// Create an Ariadne report for this lowering error.
    ///
    /// `spans` resolves the span into a file and byte range, while `scoped`
    /// and `statics` render the blamed term.
    pub fn to_report(
        &self, spans: &SpanArena, scoped: &ScopedArena, statics: &StaticsArena,
    ) -> Report<'static, (PathDisplay, Range<usize>)> {
        let span = self.primary_span(spans, scoped, statics);
        let (file_path, range) = spans
            .source_map()
            .and_then(|map| map.ariadne_range(span))
            .unwrap_or_else(internal_ariadne_span);
        let formatter = zydeco_statics::fmt::Formatter::new(scoped, statics);
        let term = self.value().ugly(&formatter);
        let label = "this value should have been eliminated before lowering";
        Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
            .with_message(self.to_string())
            .with_label(Label::new((file_path, range)).with_message(format!("{label}:\n{term}")))
            .finish()
    }
}

/// Errors of lowering one executable root applied to the host Builtin package.
#[derive(Debug, Error)]
pub enum BuiltinRootLowerError {
    #[error(transparent)]
    Package(#[from] BuiltinPackagePlanError),
    #[error("static elimination left an invalid residual program")]
    Sps(Vec<SpsLowerError>),
}

mod builtin;
mod fold;

use builtin::BuiltinPackageFolder;
use fold::LoweringFolder;

#[derive(Clone)]
struct ValueBinding {
    binder: VPatId,
    bindee: ValueId,
    site: Option<ss::TermId>,
}

#[derive(Clone)]
enum ValueStep {
    Bind(ValueBinding),
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

    fn sequence(plans: impl IntoIterator<Item = Self>) -> ValuePlan<Vec<T>> {
        let (steps, values): (Vec<_>, Vec<_>) =
            plans.into_iter().map(|Self { steps, value }| (steps, value)).unzip();
        ValuePlan { steps: steps.into_iter().flatten().collect(), value: values }
    }
}

impl<T> ValuePlan<T> {
    fn bind(self, lo: &mut Lowerer, tail: CompuId) -> CompuId {
        self.steps.into_iter().rev().fold(tail, |tail, step| match step {
            | ValueStep::Bind(ValueBinding { binder, bindee, site }) => {
                Let { binder, bindee, tail }.build(lo, site)
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
    /// Internal residual invariant failures collected during lowering.
    lower_errors: Vec<SpsLowerError>,
    protocols: SourceProtocols<'a>,
}

/// Lowering pass for one checked computation root.
pub struct RootLowerer<'a> {
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
}

/// Lowering pass for a package-dependent root applied to the host Builtin package.
pub struct BuiltinRootLowerer<'a> {
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub signature: ss::PackPi,
}

impl<'a> Lowerer<'a> {
    /// Create a structural lowerer with fresh stack arenas.
    pub fn new(spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        let arena = StackirArena::default();
        let lower_errors = Vec::new();
        Self {
            arena,
            spans,
            scoped,
            statics,
            lower_errors,
            protocols: SourceProtocols::new(statics),
        }
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

    /// Whether a pattern must be lowered through the match-plan machinery
    /// instead of a structural binder: it contains a refutable literal row,
    /// possibly nested under other patterns.
    fn pattern_needs_match_plan(&self, pattern: ss::VPatId) -> bool {
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match &self.statics.vpats[&pattern] {
                | ss::ValuePattern::Lit(_) => return true,
                | ss::ValuePattern::View(_) => unreachable!("static elaboration eliminates views"),
                | ss::ValuePattern::Named(Named(_, pattern))
                | ss::ValuePattern::Ctor(Ctor(_, pattern))
                | ss::ValuePattern::SCons(ss::ConsN(_, pattern)) => pending.push(*pattern),
                | ss::ValuePattern::Alias(Alias(patterns)) => {
                    pending.extend(patterns.iter().rev().copied());
                }
                | ss::ValuePattern::VCons(patterns) => {
                    pending.extend(patterns.iter().rev().copied());
                }
                | ss::ValuePattern::Hole(_)
                | ss::ValuePattern::Var(_)
                | ss::ValuePattern::Triv(_) => {}
            }
        }
        false
    }

    fn is_coprod_pattern(&self, pattern: ss::VPatId) -> bool {
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match &self.statics.vpats[&pattern] {
                | ss::ValuePattern::Ctor(_) => return true,
                | ss::ValuePattern::Named(Named(_, pattern))
                | ss::ValuePattern::SCons(ss::ConsN(_, pattern)) => pending.push(*pattern),
                | ss::ValuePattern::Alias(Alias(patterns)) => {
                    pending.extend(patterns.iter().rev().copied());
                }
                | ss::ValuePattern::View(_) => unreachable!("static elaboration eliminates views"),
                | ss::ValuePattern::Hole(_)
                | ss::ValuePattern::Var(_)
                | ss::ValuePattern::Lit(_)
                | ss::ValuePattern::Triv(_)
                | ss::ValuePattern::VCons(_) => {}
            }
        }
        false
    }

    fn is_coprod_match(&self, arms: &[Matcher<ss::VPatId, ss::CompuId>]) -> bool {
        match arms {
            | [Matcher { binder, tail: _ }] => self.is_coprod_pattern(*binder),
            | _ => true,
        }
    }

    fn finish(mut self, root: CompuId) -> Result<BranchJoinProgram, Vec<SpsLowerError>> {
        if self.lower_errors.is_empty() {
            self.arena.inner.protocols = std::sync::Arc::new(self.protocols.graph);
            Ok(BranchJoinProgram::try_new(StackirProgram::new(self.arena, root))
                .expect("stack-indexed lowering must construct branch-join SPS"))
        } else {
            Err(self.lower_errors)
        }
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
}

impl CompilerPass<ss::CompuId> for RootLowerer<'_> {
    type Output = BranchJoinProgram;
    type Error = Vec<SpsLowerError>;

    fn run(&mut self, root: ss::CompuId) -> Result<BranchJoinProgram, Self::Error> {
        self.run_with_builtin(root, None)
    }
}

impl RootLowerer<'_> {
    /// Lower either a process or a checked export, supplying its validated host package.
    pub fn run_with_builtin(
        &mut self, root: ss::CompuId, builtin: Option<BuiltinPackagePlan>,
    ) -> Result<BranchJoinProgram, Vec<SpsLowerError>> {
        let mut lowerer = Lowerer::new(self.spans, self.scoped, self.statics);
        let root = self.statics.execution_compu(root);
        let mut stack = Bullet.build(&mut lowerer, None);
        if let Some(plan) = builtin {
            let package =
                Explicit::run(&mut BuiltinPackageFolder { lowerer: &mut lowerer }, plan.value);
            stack = Cons(package, stack).build(&mut lowerer, None);
        }
        let root = LoweringFolder::new(&mut lowerer).lower(root, stack);
        lowerer.finish(root)
    }
}

impl CompilerPass<ss::CompuId> for BuiltinRootLowerer<'_> {
    type Output = BranchJoinProgram;
    type Error = BuiltinRootLowerError;

    fn run(&mut self, root: ss::CompuId) -> Result<BranchJoinProgram, Self::Error> {
        let plan = BuiltinPackagePlan::for_executable(self.statics, &self.signature)?;
        RootLowerer { spans: self.spans, scoped: self.scoped, statics: self.statics }
            .run_with_builtin(root, Some(plan))
            .map_err(BuiltinRootLowerError::Sps)
    }
}

#[cfg(test)]
mod tests;
