use super::{check::FragmentProgram, syntax::*};
use crate::{ir, scheme::SpsScheme};
use derive_more::{AsMut, AsRef};
use std::convert::Infallible;
use zydeco_foundation::{dense::SecondaryMap, fold::Explicit, pass::CompilerPass};

mod builtin;
mod fold;

use builtin::BuiltinPackageFolder;
use fold::LoweringFolder;

#[derive(Clone)]
struct ValueBinding {
    binder: VPatId,
    bindee: ValueId,
    site: Option<ir::TermId>,
}

impl ValueBinding {
    fn bind(self, lo: &mut Lowerer<'_>, tail: CompuId) -> CompuId {
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
    fn bind(self, lo: &mut Lowerer<'_>, tail: CompuId) -> CompuId {
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

/// Stateful lowering pass from residual [`ir::CoreProgram`] into stack IR.
#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: SpsArena,
    pub core: &'a ir::CoreProgram,
    var_map: SecondaryMap<ir::VarId, DefId>,
    /// Where this lowering names the current stack.
    scheme: SpsScheme,
}

/// Lowering pass for one checked computation root.
pub struct RootLowerer {
    pub scheme: SpsScheme,
}

impl<'a> Lowerer<'a> {
    /// Create a structural lowerer with fresh stack arenas seeded from `core`.
    pub fn new(core: &'a ir::CoreProgram, scheme: SpsScheme) -> Self {
        let mut arena = SpsArena::default();
        let mut var_map = SecondaryMap::with_capacity(core.vars.len());
        for (var, &name) in core.vars.iter() {
            let def = arena.admin.alloc_def(name);
            var_map.insert_new(var, def);
        }
        arena.inner.protocols = core.protocols.clone();
        Self { arena, core, var_map, scheme }
    }

    fn def(&self, var: ir::VarId) -> DefId {
        self.var_map[var]
    }

    fn alloc_projection_def(&mut self) -> DefId {
        self.alloc_admin_def("__proj__")
    }

    fn alloc_admin_def(&mut self, role: &str) -> DefId {
        self.arena.admin.alloc_def(VarName::new(role))
    }

    /// Whether a pattern must be lowered through the match-plan machinery
    /// instead of a structural binder. Constructors need explicit coproduct
    /// elimination even when coverage proves that their tag is irrefutable.
    fn pattern_needs_match_plan(&self, pattern: ir::VPatId) -> bool {
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match &self.core.vpats[pattern] {
                | ir::ValuePattern::Lit(_) | ir::ValuePattern::Ctor { .. } => return true,
                | ir::ValuePattern::Alias(Alias(patterns)) => {
                    pending.extend(patterns.iter().rev().copied());
                }
                | ir::ValuePattern::VCons(VCons { items, .. }) => {
                    pending.extend(items.iter().rev().copied());
                }
                | ir::ValuePattern::Hole(_)
                | ir::ValuePattern::Var(_)
                | ir::ValuePattern::Triv(_) => {}
            }
        }
        false
    }

    /// A direct coproduct arm consumes one constructor head itself. Its payload
    /// must be structural; nested decisions and constructor aliases use plans.
    fn match_arm_needs_match_plan(&self, pattern: ir::VPatId) -> bool {
        match &self.core.vpats[pattern] {
            | ir::ValuePattern::Ctor { payload, .. } => self.pattern_needs_match_plan(*payload),
            | _ => self.pattern_needs_match_plan(pattern),
        }
    }

    fn is_coprod_pattern(&self, pattern: ir::VPatId) -> bool {
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match &self.core.vpats[pattern] {
                | ir::ValuePattern::Ctor { .. } => return true,
                | ir::ValuePattern::Alias(Alias(patterns)) => {
                    pending.extend(patterns.iter().rev().copied());
                }
                | ir::ValuePattern::Hole(_)
                | ir::ValuePattern::Var(_)
                | ir::ValuePattern::Lit(_)
                | ir::ValuePattern::Triv(_)
                | ir::ValuePattern::VCons(_) => {}
            }
        }
        false
    }

    fn is_coprod_match(&self, arms: &[Matcher<ir::VPatId, ir::CompuId>]) -> bool {
        match arms {
            | [Matcher { binder, tail: _ }] => self.is_coprod_pattern(*binder),
            | _ => true,
        }
    }

    /// The stack one arm of a branch consumes. A branch that names its stack gives every arm a
    /// fresh `•`; otherwise each arm but the last consumes its own copy of the branch's stack,
    /// and the last arm consumes the stack itself.
    fn arm_stack(&mut self, stack: StackId, last: bool, site: Option<ir::TermId>) -> StackId {
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
    fn push(&mut self, frame: StackId, site: Option<ir::TermId>) -> Pushed {
        if self.scheme.names_frames() {
            Pushed::Named { frame, ambient: StackVar.build(self, site) }
        } else {
            Pushed::Extended(frame)
        }
    }

    /// Push a frame, collecting it in `named` when the scheme names it, and return the stack
    /// that the consuming computation receives.
    fn push_onto(
        &mut self, frame: StackId, named: &mut Vec<StackId>, site: Option<ir::TermId>,
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
        &mut self, stack: Option<StackId>, branch: CompuId, site: Option<ir::TermId>,
    ) -> CompuId {
        if self.scheme.shares_branch_stack() {
            let bindee = stack.unwrap_or_else(|| StackVar.build(self, site));
            Let { binder: StackVar, bindee, tail: branch }.build(self, site)
        } else {
            branch
        }
    }

    fn finish(self, root: CompuId) -> FragmentProgram {
        FragmentProgram::from_pass(SpsProgram::new(self.arena, root), self.scheme)
    }

    fn projection_binding(
        &mut self, head: ValueId, position: usize, layout: ProductLayout, site: Option<ir::TermId>,
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

impl CompilerPass<&ir::CoreProgram> for RootLowerer {
    type Output = FragmentProgram;
    type Error = Infallible;

    fn run(&mut self, core: &ir::CoreProgram) -> Result<FragmentProgram, Self::Error> {
        Ok(self.run_with_builtin(core, None))
    }
}

impl RootLowerer {
    /// Lower either a process or a checked export, optionally supplying its validated host package.
    pub fn run_with_builtin(
        &mut self, core: &ir::CoreProgram, builtin: Option<&ir::BuiltinPackagePlan>,
    ) -> FragmentProgram {
        let ir::CoreRoot::Compu(root) = core.root() else {
            panic!("stack IR lowering requires a computation root");
        };
        let mut lowerer = Lowerer::new(core, self.scheme);
        let mut stack = StackVar.build(&mut lowerer, None);
        let mut named = Vec::new();
        if let Some(plan) = builtin {
            let package = Explicit::run(
                &mut BuiltinPackageFolder { lowerer: &mut lowerer },
                plan.value.clone(),
            );
            let frame = Cons(package, stack).build(&mut lowerer, None);
            stack = lowerer.push_onto(frame, &mut named, None);
        }
        let root = LoweringFolder::new(&mut lowerer).lower(root, stack);
        let root = lowerer.arena.name_frames(named, root, None);
        lowerer.finish(root)
    }
}

#[cfg(test)]
mod tests;
