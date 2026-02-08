use super::syntax::*;
use super::variables::{FreeVars, Vars};
use crate::sps::syntax::*;
use derive_more::From;
use derive_more::{AsMut, AsRef};
use std::{
    cmp::{Ordering, PartialEq, PartialOrd},
    collections::{BTreeSet, VecDeque},
};
use zydeco_statics::surface_syntax::ScopedArena;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_utils::pass::CompilerPass;

/// Assignments of values and stacks.
///
/// The order is from innermost to outermost, namely,
/// - we should apply the substitution to the underlying body from the innermost to the outermost.
/// - or equivalently, we should apply the outermost substitution to both all inner substitutions
///   and the underlying body, and then take one step inner, and so on.
///
/// Consider them as a bunch of reversed let bindings / join points.
#[derive(Clone, Debug)]
pub struct SubstAssignments {
    pub items: VecDeque<AssignItem>,
}
#[derive(From, Clone, Debug)]
pub enum AssignItem {
    Def(AssignDef),
    Pattern(AssignPattern),
    Stack(AssignStack),
}
#[derive(Clone, Debug)]
pub struct AssignDef {
    pub def: DefId,
    pub value: ValueId,
}
#[derive(Clone, Debug)]
pub struct AssignPattern {
    pub pat: VPatId,
    pub value: ValueId,
}
#[derive(Clone, Debug)]
pub struct AssignStack {
    pub stack: StackId,
}

/// Decorate the assign items with the variable introductions and free references.
pub struct IntroRefItem {
    pub intros: Context<DefId>,
    pub mentions: CoContext<DefId>,
    pub item: AssignItem,
}

mod impls {
    use super::*;

    impl SubstAssignments {
        pub fn new() -> Self {
            Self { items: VecDeque::new() }
        }
        pub fn cascade_value(&mut self, pat: VPatId, value: ValueId) {
            self.items.push_back(AssignItem::Pattern(AssignPattern { pat, value }));
        }
        pub fn cascade_stack(&mut self, stack: StackId) {
            self.items.push_back(AssignItem::Stack(AssignStack { stack }));
        }
        pub fn normalize(self, arena: &mut SNormArenaMut) -> Self {
            // Individually normalize the assign items and flatten the result.
            let flattened = self
                .items
                .into_iter()
                .flat_map(|item| match item {
                    | AssignItem::Def(assign_def) => vec![AssignItem::Def(assign_def)],
                    | AssignItem::Pattern(assign_pattern) => assign_pattern.normalize(arena),
                    | AssignItem::Stack(assign_stack) => assign_stack.normalize(arena),
                })
                .collect::<Vec<_>>();

            // Try to move outer (closer to end) stack assignments to inner (closer to begin)
            // A bubble-sort-like algorithm; implemented using the [`Ord`] trait and [`BTreeMap`].
            let ordered: BTreeSet<AssignItem> = flattened.into_iter().rev().collect();
            let items = ordered.into_iter().collect();
            Self { items }
        }

        /// Extract all join points from the assignments,
        /// and perform all inline substitutions available.
        pub fn extract_or_inline<Arena>(mut self, arena: &mut Arena) -> Self
        where
            Arena: AsRef<SNormInnerArena> + AsMut<SNormInnerArena>,
        {
            let mut items = VecDeque::new();
            while let Some(item) = self.items.pop_front() {
                match item {
                    | AssignItem::Def(AssignDef { def, value }) => {
                        match arena.as_ref().users.get(&def).cloned().unwrap_or_default().as_slice()
                        {
                            | [] => {}
                            | [user] => {
                                // directly substitute all users of the variable with the value
                                let value = arena.as_mut().svalues[&value].clone();
                                arena.as_mut().svalues.replace(*user, value);
                            }
                            | _users => {
                                items.push_back(item);
                                // // Note: dangerous explosion
                                // for user in _users {
                                //     // directly substitute all users of the variable with the value
                                //     let value = arena.as_mut().svalues[&value].clone();
                                //     arena.as_mut().svalues.replace(*user, value);
                                // }
                            }
                        }
                    }
                    | AssignItem::Pattern(AssignPattern { .. }) => {
                        items.push_back(item);
                    }
                    | AssignItem::Stack(AssignStack { .. }) => items.push_back(item),
                }
            }
            Self { items }
        }
    }

    impl<T> std::ops::AddAssign<T> for SubstAssignments
    where
        T: Into<AssignItem>,
    {
        fn add_assign(&mut self, rhs: T) {
            self.items.push_back(rhs.into());
        }
    }

    impl<T> std::ops::Add<T> for SubstAssignments
    where
        T: Into<AssignItem>,
    {
        type Output = Self;

        fn add(mut self, rhs: T) -> Self::Output {
            std::ops::AddAssign::add_assign(&mut self, rhs);
            self
        }
    }

    /// No item is equal to another.
    impl PartialEq for AssignItem {
        fn eq(&self, _other: &Self) -> bool {
            false
        }
    }
    impl Eq for AssignItem {}
    /// Delegated to the [`Ord`] implementation.
    impl PartialOrd for AssignItem {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
    /// IMPORTANT: This is to ensure that stack assignments are moved to inner positions,
    /// so that the later reductions with computations can happen more easily.
    ///
    /// The move happens if `other` is smaller, namely returning [`Ordering::Greater`].
    impl Ord for AssignItem {
        fn cmp(&self, other: &Self) -> Ordering {
            match &other {
                // if the RHS is a stack assignment, ...
                | AssignItem::Stack(_) => {
                    match &self {
                        // if LHS is also a stack assignment, hold still
                        | AssignItem::Stack(_) => Ordering::Less,
                        // otherwise, swap
                        | _ => Ordering::Greater,
                    }
                }
                // otherwise, remain still
                | _ => Ordering::Less,
            }
        }
    }

    impl AssignPattern {
        pub fn normalize(self, arena: &mut SNormArenaMut) -> Vec<AssignItem> {
            let pat = arena.inner.svpats[&self.pat].clone();
            let value = arena.inner.svalues[&self.value].clone();
            use Value;
            use ValuePattern as VPat;
            match (pat, value) {
                | (VPat::Hole(Hole), _) => Vec::new(),
                | (VPat::Var(def), _) => {
                    let value = self.value.clone();
                    vec![AssignItem::Def(AssignDef { def, value })]
                }
                | (VPat::Ctor(Ctor(ctor, _)), _) => {
                    unreachable!("ctor patterns ({}) are not expected in assignments", ctor.0)
                }
                | (VPat::Triv(Triv), _) => Vec::new(),
                | (VPat::VCons(_), Value::Hole(_) | Value::Var(_)) => {
                    vec![AssignItem::Pattern(AssignPattern { pat: self.pat, value: self.value })]
                }
                | (VPat::VCons(Cons(pa, pb)), Value::VCons(Cons(va, vb))) => {
                    let a = AssignPattern { pat: pa, value: va }.normalize(arena);
                    let b = AssignPattern { pat: pb, value: vb }.normalize(arena);
                    std::iter::empty().chain(a).chain(b).collect()
                }
                | (
                    VPat::VCons(_),
                    Value::Closure(_)
                    | Value::Ctor(_)
                    | Value::Triv(_)
                    | Value::Literal(_)
                    | Value::Complex(_),
                ) => {
                    unreachable!("these value patterns are not well-typed")
                }
            }
        }
    }

    impl AssignStack {
        pub fn normalize(self, arena: &mut SNormArenaMut) -> Vec<AssignItem> {
            let stack = arena.inner.sstacks[&self.stack].clone();
            match stack {
                | Stack::Kont(_) => vec![self.into()],
                | Stack::Var(Bullet) => Vec::new(),
                | Stack::Arg(Cons(value, stack)) => {
                    let v = AssignStack { stack }.normalize(arena);
                    let bullet = arena.admin.allocator.alloc();
                    arena.inner.sstacks.insert(bullet, Bullet.into());
                    arena.inner.holes.insert(bullet, bullet);
                    let arg = arena.admin.allocator.alloc();
                    arena.inner.sstacks.insert(arg, Stack::Arg(Cons(value, bullet)));
                    arena.inner.holes.insert(arg, bullet);
                    std::iter::empty().chain([AssignStack { stack: arg }.into()]).chain(v).collect()
                }
                | Stack::Tag(Cons(dtor, stack)) => {
                    let v = AssignStack { stack }.normalize(arena);
                    let bullet = arena.admin.allocator.alloc();
                    arena.inner.sstacks.insert(bullet, Bullet.into());
                    arena.inner.holes.insert(bullet, bullet);
                    let tag = arena.admin.allocator.alloc();
                    arena.inner.sstacks.insert(tag, Stack::Tag(Cons(dtor, bullet)));
                    arena.inner.holes.insert(tag, bullet);
                    std::iter::empty().chain([AssignStack { stack: tag }.into()]).chain(v).collect()
                }
            }
        }
    }

    impl IntroRefItem {
        pub fn from_assign_item<Arena>(item: AssignItem, arena: &Arena) -> Self
        where
            Arena: AsRef<SNormArena>,
        {
            match item {
                | AssignItem::Def(AssignDef { def, value }) => IntroRefItem {
                    intros: Context::singleton(def.clone()),
                    mentions: value.free_vars(arena),
                    item: AssignDef { def, value }.into(),
                },
                | AssignItem::Pattern(AssignPattern { pat, value }) => IntroRefItem {
                    intros: pat.vars(arena),
                    mentions: value.free_vars(arena),
                    item: AssignPattern { pat, value }.into(),
                },
                | AssignItem::Stack(AssignStack { stack }) => IntroRefItem {
                    intros: Context::new(),
                    mentions: stack.free_vars(arena),
                    item: AssignStack { stack }.into(),
                },
            }
        }
    }
}

/// Re-construction-style substitution.
///
/// Perform substitution of normalized stack IR nodes into stack IR arena.
/// During the substitution, the [`SNormArena`] is destructed,
/// and the [`StackirArena`] is reconstructed.
pub trait Substitute<Assign> {
    /// The output type of the substitution.
    type Out;
    /// Perform the substitution defined by the assign item.
    ///
    /// The [`DefId`]s in the pattern part of the map are guaranteed to be free.
    fn substitute(self, su: &mut Substitutor, assign: Assign) -> Self::Out;
}

#[derive(AsRef, AsMut)]
pub struct Substitutor<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: StackirArena,
    #[as_ref]
    #[as_mut]
    pub snorm: &'a mut SNormInnerArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
}

impl<'a> Substitutor<'a> {
    pub fn new(
        admin: AdminArena, snorm: &'a mut SNormInnerArena, scoped: &'a ScopedArena,
        statics: &'a StaticsArena,
    ) -> Self {
        Self {
            arena: StackirArena { admin, inner: StackirInnerArena::new() },
            snorm,
            scoped,
            statics,
        }
    }
}

impl<'a> CompilerPass for Substitutor<'a> {
    type Arena = StackirArena;
    type Out = StackirArena;
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<StackirArena, Self::Error> {
        self.arena.inner.entry = self
            .snorm
            .entry
            .clone()
            .into_iter()
            .map(|(compu_id, ())| {
                let new_compu_id = compu_id.substitute(&mut self, ());
                (new_compu_id, ())
            })
            .collect();
        Ok(self.arena)
    }
}

impl Substitute<()> for CompuId {
    type Out = CompuId;

    fn substitute(self, su: &mut Substitutor, (): ()) -> Self::Out {
        let SComputation { compu, assignments } = su.snorm.scompus[&self].clone();
        compu.substitute(su, assignments)
    }
}

impl Substitute<SubstAssignments> for Computation<NonJoin> {
    type Out = CompuId;

    fn substitute(self, su: &mut Substitutor, assignments: SubstAssignments) -> Self::Out {
        // let assignments = {
        //     let mut arena_mut = SNormArenaMut { admin: &mut su.arena.admin, inner: &mut su.snorm };
        //     assignments.normalize(&mut arena_mut)
        // };
        // // Pretty print the assignments.
        // {
        //     use crate::norm::fmt::*;
        //     let fmt = Formatter::new(
        //         &su.arena.admin,
        //         &su.snorm,
        //         &su.arena.inner,
        //         &su.scoped,
        //         &su.statics,
        //     );
        //     let doc = assignments.pretty(&fmt);
        //     let mut buf = String::new();
        //     doc.render_fmt(100, &mut buf).unwrap();
        //     // log::trace!("assignments:\n{}", buf);
        // }
        let mut assignments = assignments.extract_or_inline(&mut su.snorm);
        let frontier = match assignments.items.front() {
            | Some(AssignItem::Stack(AssignStack { stack })) => {
                Some(su.snorm.sstacks[&stack].clone())
            }
            | _ => None,
        };
        use Computation as Compu;
        let mut tail = match self {
            | Compu::Hole(Hole) => Hole.build(su, None),
            | Compu::Force(SForce { thunk, stack }) => match su.snorm.svalues[&thunk].clone() {
                | Value::Closure(Closure { capture: _, stack: Bullet, body }) => {
                    let body = body.substitute(su, ());
                    let stack = stack.substitute(su, ());
                    Let { binder: Bullet, bindee: stack, tail: body }.build(su, None)
                }
                | _ => {
                    let thunk = thunk.substitute(su, ());
                    let stack = stack.substitute(su, ());
                    SForce { thunk, stack }.build(su, None)
                }
            },
            | Compu::Ret(SReturn { stack, value }) => match frontier {
                | Some(Stack::Kont(Kont { binder, body })) => {
                    assert!(matches!(su.snorm.sstacks[&stack], Stack::Var(Bullet)));
                    assignments.items.pop_front();
                    let binder = binder.substitute(su, ());
                    let body = body.substitute(su, ());
                    let value = value.substitute(su, ());
                    Let { binder, bindee: value, tail: body }.build(su, None)
                }
                | _ => {
                    let stack = stack.substitute(su, ());
                    let value = value.substitute(su, ());
                    SReturn { stack, value }.build(su, None)
                }
            },
            | Compu::Fix(SFix { capture, param, body }) => {
                let body = body.substitute(su, ());
                SFix { capture, param, body }.build(su, None)
            }
            | Compu::Case(Match { scrut, arms }) => {
                let scrut = scrut.substitute(su, ());
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.substitute(su, ());
                        let tail = tail.substitute(su, ());
                        Matcher { binder, tail }
                    })
                    .collect();
                Match { scrut, arms }.build(su, None)
            }
            | Compu::Join(join) => match join {},
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => match frontier {
                | Some(Stack::Arg(Cons(arg, next))) => {
                    assignments.items.pop_front();
                    let param = param.substitute(su, ());
                    assert!(matches!(su.snorm.sstacks[&bindee], Stack::Var(Bullet)));
                    let tail = tail.substitute(su, ());
                    let arg = arg.substitute(su, ());
                    let next = next.substitute(su, ());
                    let tail = Let { binder: param, bindee: arg, tail }.build(su, None);
                    Let { binder: Bullet, bindee: next, tail }.build(su, None)
                }
                | _ => {
                    let param = param.substitute(su, ());
                    let bindee = bindee.substitute(su, ());
                    let tail = tail.substitute(su, ());
                    Let { binder: Cons(param, Bullet), bindee, tail }.build(su, None)
                }
            },
            | Compu::CoCase(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.substitute(su, ());
                        CoMatcher { dtor, tail }
                    })
                    .collect();
                CoMatch { arms }.build(su, None)
            }
            | Compu::ExternCall(ExternCall { function, stack: Bullet }) => {
                ExternCall { function, stack: Bullet }.build(su, None)
            }
        };
        // Create join points from the assignments.
        for item in assignments.items {
            match item {
                | AssignItem::Def(AssignDef { def, value }) => {
                    let binder = def.build(su, None);
                    let bindee = value.substitute(su, ());
                    tail = Let { binder, bindee, tail }.build(su, None);
                }
                | AssignItem::Pattern(AssignPattern { pat, value }) => {
                    let pat = pat.substitute(su, ());
                    let value = value.substitute(su, ());
                    tail = Let { binder: pat, bindee: value, tail }.build(su, None);
                }
                | AssignItem::Stack(AssignStack { stack }) => {
                    let bindee = stack.substitute(su, ());
                    tail = Let { binder: Bullet, bindee, tail }.build(su, None);
                }
            }
        }
        tail
    }
}

impl Substitute<()> for StackId {
    type Out = StackId;

    fn substitute(self, su: &mut Substitutor, (): ()) -> Self::Out {
        let stack = su.snorm.sstacks[&self].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let binder = binder.substitute(su, ());
                let body = body.substitute(su, ());
                Kont { binder, body }.build(su, None)
            }
            | Stack::Var(Bullet) => Bullet.build(su, None),
            | Stack::Arg(Cons(value, stack)) => {
                let value = value.substitute(su, ());
                let stack = stack.substitute(su, ());
                Cons(value, stack).build(su, None)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = stack.substitute(su, ());
                Cons(dtor, stack).build(su, None)
            }
        }
    }
}

impl Substitute<()> for ValueId {
    type Out = ValueId;

    fn substitute(self, su: &mut Substitutor, (): ()) -> Self::Out {
        let value = su.snorm.svalues[&self].clone();
        use Value;
        match value {
            | Value::Hole(Hole) => Hole.build(su, None),
            | Value::Var(def_id) => def_id.build(su, None),
            | Value::Closure(Closure { capture, stack, body }) => {
                let body = body.substitute(su, ());
                Closure { capture, stack, body }.build(su, None)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = body.substitute(su, ());
                Ctor(ctor, body).build(su, None)
            }
            | Value::Triv(Triv) => Triv.build(su, None),
            | Value::VCons(Cons(a, b)) => {
                let a = a.substitute(su, ());
                let b = b.substitute(su, ());
                Cons(a, b).build(su, None)
            }
            | Value::Literal(literal) => literal.build(su, None),
            | Value::Complex(Complex { operator, operands }) => {
                let operands = operands.iter().map(|operand| operand.substitute(su, ())).collect();
                Complex { operator, operands }.build(su, None)
            }
        }
    }
}

impl Substitute<()> for VPatId {
    type Out = VPatId;

    fn substitute(self, su: &mut Substitutor, (): ()) -> Self::Out {
        let vpat = su.snorm.svpats[&self].clone();
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => Hole.build(su, None),
            | VPat::Var(def) => def.build(su, None),
            | VPat::Ctor(Ctor(ctor, body)) => {
                let body = body.substitute(su, ());
                Ctor(ctor, body).build(su, None)
            }
            | VPat::Triv(Triv) => Triv.build(su, None),
            | VPat::VCons(Cons(a, b)) => {
                let a = a.substitute(su, ());
                let b = b.substitute(su, ());
                Cons(a, b).build(su, None)
            }
        }
    }
}
