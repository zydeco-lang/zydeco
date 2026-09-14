use super::syntax::*;
use super::traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor};
use rustc_hash::FxHashMap;

/// Bottom-up bound and free variable facts, reusable across roots of one immutable SPS arena.
#[derive(Default)]
pub struct Variables {
    patterns: FxHashMap<VPatId, Context<DefId>>,
    values: FxHashMap<ValueId, CoContext<DefId>>,
    stacks: FxHashMap<StackId, CoContext<DefId>>,
    computations: FxHashMap<CompuId, CoContext<DefId>>,
    cyclic: bool,
}

impl Variables {
    pub fn free_variables(&self, term: TermId) -> Option<&CoContext<DefId>> {
        if self.cyclic {
            return None;
        }
        match term {
            | TermId::Value(id) => self.values.get(&id),
            | TermId::Stack(id) => self.stacks.get(&id),
            | TermId::Compu(id) => self.computations.get(&id),
        }
    }

    pub fn bound_variables(&self, pattern: VPatId) -> Option<&Context<DefId>> {
        if self.cyclic { None } else { self.patterns.get(&pattern) }
    }

    fn analyze(arena: &StackirArena, root: EntityId) -> Self {
        let mut analysis = Self::default();
        Traversal { arena: &arena.inner }.run(root, &mut analysis);
        assert!(!analysis.cyclic, "free variable analysis requires acyclic SPS syntax");
        analysis
    }

    fn pattern(&self, vpat: ValuePattern) -> Context<DefId> {
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => Context::new(),
            | VPat::Var(def_id) => Context::singleton(def_id),
            | VPat::Ctor(Ctor(_ctor, body)) => body.summary(self),
            | VPat::Alias(Alias(patterns)) => patterns
                .into_iter()
                .map(|pattern| pattern.summary(self))
                .fold(Context::new(), |vars, pattern| vars + pattern),
            | VPat::Triv(Triv) => Context::new(),
            | VPat::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.summary(self))
                .fold(Context::new(), |vars, item| vars + item),
        }
    }
    fn value(&self, value: Value) -> CoContext<DefId> {
        match value {
            | Value::Var(def_id) => CoContext::singleton(def_id),
            | Value::Closure(Closure { stack: Bullet, body }) => body.summary(self),
            | Value::Ctor(Ctor(_ctor, body)) => body.summary(self),
            | Value::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.summary(self))
                .fold(CoContext::new(), |vars, item| vars + item),
            | Value::Primitive(Primitive { operation: _, operands }) => operands
                .into_iter()
                .map(|operand| operand.summary(self))
                .fold(CoContext::new(), |acc, x| acc + x),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => CoContext::new(),
        }
    }
    fn stack(&self, stack: Stack) -> CoContext<DefId> {
        match stack {
            | Stack::Kont(Kont { binder, body }) => body.summary(self) - binder.summary(self),
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(a, b)) => a.summary(self) + b.summary(self),
            | Stack::Tag(Cons(_dtor, body)) => body.summary(self),
        }
    }
    fn computation(&self, compu: Computation<LetJoin>) -> CoContext<DefId> {
        use Computation as Compu;
        match compu {
            | Compu::Hole(SHole(tail)) => tail.summary(self),
            | Compu::Force(SForce { thunk, stack }) => thunk.summary(self) + stack.summary(self),
            | Compu::Ret(SReturn { stack, value }) => stack.summary(self) + value.summary(self),
            | Compu::Fix(SFix { param, stack, body }) => {
                stack.summary(self) + (body.summary(self) - Context::singleton(param))
            }
            | Compu::ProductMatch(SProductMatch { scrut, binder, body }) => {
                scrut.summary(self) + (body.summary(self) - binder.summary(self))
            }
            | Compu::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                scrut.summary(self)
                    + arms
                        .into_iter()
                        .map(|Matcher { binder, tail }| tail.summary(self) - binder.summary(self))
                        .fold(CoContext::new(), |acc, x| acc + x)
            }
            | Compu::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                tail.summary(self) - binder.summary(self) + bindee.summary(self)
            }
            | Compu::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                bindee.summary(self) + tail.summary(self)
            }
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                tail.summary(self) - param.summary(self) + bindee.summary(self)
            }
            | Compu::CoCase(SCoMatch { scrut, arms }) => {
                scrut.summary(self)
                    + arms
                        .into_iter()
                        .map(|CoMatcher { dtor: _, tail }| tail.summary(self))
                        .fold(CoContext::new(), |acc, x| acc + x)
            }
            | Compu::ExternCall(ExternCall { function: _, stack }) => stack.summary(self),
        }
    }
}

impl Visitor for Variables {
    fn enter(&mut self, _node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        self.cyclic |= occurrence == Occurrence::Cyclic;
    }

    fn exit(&mut self, node: Node<'_>) {
        if self.cyclic {
            return;
        }
        match node {
            | Node::Pattern(id, pattern) => {
                self.patterns.insert(id, self.pattern(pattern.clone()));
            }
            | Node::Value(id, value) => {
                self.values.insert(id, self.value(value.clone()));
            }
            | Node::Stack(id, stack) => {
                self.stacks.insert(id, self.stack(stack.clone()));
            }
            | Node::Computation(id, computation) => {
                self.computations.insert(id, self.computation(computation.clone()));
            }
        }
    }
}

trait Summary {
    type Output;
    fn summary(self, analysis: &Variables) -> Self::Output;
}

macro_rules! Summary {
    ($id:ty, $field:ident, $output:ty) => {
        impl Summary for $id {
            type Output = $output;
            fn summary(self, analysis: &Variables) -> Self::Output {
                analysis.$field[&self].clone()
            }
        }
    };
}
Summary!(VPatId, patterns, Context<DefId>);
Summary!(ValueId, values, CoContext<DefId>);
Summary!(StackId, stacks, CoContext<DefId>);
Summary!(CompuId, computations, CoContext<DefId>);

/// Collect bound variables from patterns.
pub trait Vars {
    fn vars(self, arena: &impl AsRef<StackirArena>) -> Context<DefId>;
}
impl Vars for VPatId {
    fn vars(self, arena: &impl AsRef<StackirArena>) -> Context<DefId> {
        self.summary(&Variables::analyze(arena.as_ref(), self.into()))
    }
}

/// Collect free variables from stack IR nodes.
pub trait FreeVars {
    fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId>;
}
macro_rules! FreeVars {
    ($id:ty) => {
        impl FreeVars for $id {
            fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId> {
                self.summary(&Variables::analyze(arena.as_ref(), self.into()))
            }
        }
    };
}
FreeVars!(ValueId);
FreeVars!(StackId);
FreeVars!(CompuId);
