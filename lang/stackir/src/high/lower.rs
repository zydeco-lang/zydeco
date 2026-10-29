use super::{check::FragmentProgram, syntax::*};
use crate::protocol::SourceProtocols;
use crate::scheme::SpsScheme;
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
pub enum SpsLoweringError {
    #[error("internal compiler error: a static value operation survived static elimination")]
    ResidualStaticValue { value: ss::ValueId },
}

impl SpsLoweringError {
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
    Sps(Vec<SpsLoweringError>),
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

impl ValueBinding {
    fn bind(self, lo: &mut Lowerer, tail: CompuId) -> CompuId {
        let Self { binder, bindee, site } = self;
        if let ValuePattern::Ctor(Ctor(ctor, _)) = &lo.arena.inner.vpats[&binder] {
            // Static value elimination can leave an irrefutable constructor binder.
            // Its single-constructor match must be explicit even without normalization.
            assert_eq!(ctor.idx, 0, "an irrefutable constructor binder must have tag zero");
            let tail = SCoprodMatch { scrut: bindee, arms: vec![Matcher { binder, tail }] }
                .build(lo, site);
            lo.join(None, tail, site)
        } else {
            Let { binder, bindee, tail }.build(lo, site)
        }
    }
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
            | ValueStep::Bind(binding) => binding.bind(lo, tail),
        })
    }
}

/// How lowering continues after pushing one frame onto the stack it consumes.
enum Pushed {
    /// The frame extends the stack, which the next computation consumes directly.
    Extended(StackId),
    /// A stack let-binding names the frame, and the next computation consumes the fresh `ambient`.
    Named { frame: StackId, ambient: StackId },
}

/// Stateful lowering pass from typed syntax into stack IR.
#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: SpsArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    /// Internal residual invariant failures collected during lowering.
    lower_errors: Vec<SpsLoweringError>,
    protocols: SourceProtocols<'a>,
    /// Where this lowering names the current stack.
    scheme: SpsScheme,
}

/// Lowering pass for one checked computation root.
pub struct RootLowerer<'a> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub scheme: SpsScheme,
}

/// Lowering pass for a package-dependent root applied to the host Builtin package.
pub struct BuiltinRootLowerer<'a> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub signature: ss::PackPi,
    pub scheme: SpsScheme,
}

impl<'a> Lowerer<'a> {
    /// Create a structural lowerer with fresh stack arenas.
    pub fn new(scoped: &'a ScopedArena, statics: &'a StaticsArena, scheme: SpsScheme) -> Self {
        let arena = SpsArena::default();
        let lower_errors = Vec::new();
        Self {
            arena,
            scoped,
            statics,
            lower_errors,
            protocols: SourceProtocols::new(statics),
            scheme,
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
        let def = self.arena.admin.fresh_def();
        self.arena.admin.insert_def(def, VarName(role.to_owned()));
        def
    }

    /// Whether a pattern must be lowered through the match-plan machinery
    /// instead of a structural binder. Constructors need explicit coproduct
    /// elimination even when coverage proves that their tag is irrefutable.
    fn pattern_needs_match_plan(&self, pattern: ss::VPatId) -> bool {
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match &self.statics.vpats[&pattern] {
                | ss::ValuePattern::Lit(_) | ss::ValuePattern::Ctor(_) => return true,
                | ss::ValuePattern::View(_) => unreachable!("static elaboration eliminates views"),
                | ss::ValuePattern::Named(Named(_, pattern))
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

    /// A direct coproduct arm consumes one constructor head itself. Its payload
    /// must be structural; nested decisions and constructor aliases use plans.
    fn match_arm_needs_match_plan(&self, mut pattern: ss::VPatId) -> bool {
        loop {
            match &self.statics.vpats[&pattern] {
                | ss::ValuePattern::Named(Named(_, inner))
                | ss::ValuePattern::SCons(ss::ConsN(_, inner)) => pattern = *inner,
                | ss::ValuePattern::Ctor(Ctor(_, payload)) => {
                    return self.pattern_needs_match_plan(*payload);
                }
                | _ => return self.pattern_needs_match_plan(pattern),
            }
        }
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

    /// The stack one arm of a branch consumes. A branch that names its stack gives every arm a
    /// fresh `•`; otherwise each arm but the last consumes its own copy of the branch's stack,
    /// and the last arm consumes the stack itself.
    fn arm_stack(&mut self, stack: StackId, last: bool, site: Option<ss::TermId>) -> StackId {
        if self.scheme.shares_branch_stack() {
            StackVar.build(self, site)
        } else if last {
            stack
        } else {
            self.arena.copy_stack(stack)
        }
    }

    /// Push a frame built over the consuming stack. The full scheme names it with a stack
    /// let-binding; the other schemes extend the consuming stack with it.
    fn push(&mut self, frame: StackId, site: Option<ss::TermId>) -> Pushed {
        if self.scheme.names_frames() {
            Pushed::Named { frame, ambient: StackVar.build(self, site) }
        } else {
            Pushed::Extended(frame)
        }
    }

    /// Push a frame, collecting it in `named` when the scheme names it, and return the stack
    /// that the consuming computation receives.
    fn push_onto(
        &mut self, frame: StackId, named: &mut Vec<StackId>, site: Option<ss::TermId>,
    ) -> StackId {
        match self.push(frame, site) {
            | Pushed::Extended(stack) => stack,
            | Pushed::Named { frame, ambient } => {
                named.push(frame);
                ambient
            }
        }
    }

    /// Guard a branch with the stack its arms share, when the scheme names it.
    /// `None` names the ambient stack itself.
    fn join(
        &mut self, stack: Option<StackId>, branch: CompuId, site: Option<ss::TermId>,
    ) -> CompuId {
        if self.scheme.shares_branch_stack() {
            let bindee = stack.unwrap_or_else(|| StackVar.build(self, site));
            Let { binder: StackVar, bindee, tail: branch }.build(self, site)
        } else {
            branch
        }
    }

    fn finish(mut self, root: CompuId) -> Result<FragmentProgram, Vec<SpsLoweringError>> {
        if self.lower_errors.is_empty() {
            self.arena.inner.protocols = std::sync::Arc::new(self.protocols.graph);
            Ok(FragmentProgram::try_new(SpsProgram::new(self.arena, root), self.scheme)
                .expect("stack-indexed lowering must construct its scheme's fragment"))
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
    type Output = FragmentProgram;
    type Error = Vec<SpsLoweringError>;

    fn run(&mut self, root: ss::CompuId) -> Result<FragmentProgram, Self::Error> {
        self.run_with_builtin(root, None)
    }
}

impl RootLowerer<'_> {
    /// Lower either a process or a checked export, supplying its validated host package.
    pub fn run_with_builtin(
        &mut self, root: ss::CompuId, builtin: Option<BuiltinPackagePlan>,
    ) -> Result<FragmentProgram, Vec<SpsLoweringError>> {
        self.run_with_driver::<Explicit>(root, builtin)
    }

    /// Select continuation storage for both package materialization and residual reconstruction.
    pub fn run_with_driver<D: Driver>(
        &mut self, root: ss::CompuId, builtin: Option<BuiltinPackagePlan>,
    ) -> Result<FragmentProgram, Vec<SpsLoweringError>> {
        let mut lowerer = Lowerer::new(self.scoped, self.statics, self.scheme);
        let root = self.statics.execution_compu(root);
        let mut stack = StackVar.build(&mut lowerer, None);
        let mut named = Vec::new();
        if let Some(plan) = builtin {
            let package = D::run(&mut BuiltinPackageFolder { lowerer: &mut lowerer }, plan.value);
            let frame = Cons(package, stack).build(&mut lowerer, None);
            stack = lowerer.push_onto(frame, &mut named, None);
        }
        let root = LoweringFolder::new(&mut lowerer).lower::<D>(root, stack);
        let root = lowerer.arena.name_frames(named, root, None);
        lowerer.finish(root)
    }
}

impl CompilerPass<ss::CompuId> for BuiltinRootLowerer<'_> {
    type Output = FragmentProgram;
    type Error = BuiltinRootLowerError;

    fn run(&mut self, root: ss::CompuId) -> Result<FragmentProgram, Self::Error> {
        let plan = BuiltinPackagePlan::for_executable(self.statics, &self.signature)?;
        let Self { scoped, statics, scheme, .. } = *self;
        RootLowerer { scoped, statics, scheme }
            .run_with_builtin(root, Some(plan))
            .map_err(BuiltinRootLowerError::Sps)
    }
}

#[cfg(test)]
mod tests;
