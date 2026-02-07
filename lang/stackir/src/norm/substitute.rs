use super::syntax::*;
use crate::sps::syntax::*;
use derive_more::From;
use im::Vector;

/// Assignments of values and stacks.
///
/// The order is from innermost to outermost, namely,
/// - we should apply the substitution to the underlying body from the innermost to the outermost.
/// - or equivalently, we should apply the outermost substitution to both all inner substitutions
///   and the underlying body, and then take one step inner, and so on.
///
/// Consider them as a bunch of reversed let bindings / join points.
#[derive(Clone, Debug)]
pub struct SubstAssignVec {
    pub items: Vector<AssignItem>,
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
/// Assignment of values to their corresponding value patterns.
/// The order is from innermost to outermost, namely,
/// - we should apply the substitution to the underlying body from the innermost to the outermost.
/// - or equivalently, we should apply the outermost substitution to both all inner substitutions
///   and the underlying body, and then take one step inner, and so on.
///
/// If you feel confused, simply think of it as a bunch of reversed let bindings.
#[derive(Clone, Debug)]
pub struct AssignPattern {
    pub pat: VPatId,
    pub value: ValueId,
}
#[derive(Clone, Debug)]
pub struct AssignStack {
    pub stack: StackId,
}

mod impls {
    use super::*;

    impl SubstAssignVec {
        pub fn new() -> Self {
            Self { items: Vector::new() }
        }
        pub fn cascade_value(&mut self, pat: VPatId, value: ValueId) {
            self.items.push_back(AssignItem::Pattern(AssignPattern { pat, value }));
        }
        pub fn cascade_stack(&mut self, stack: StackId) {
            self.items.push_back(AssignItem::Stack(AssignStack { stack }));
        }
    }

    impl<T> std::ops::AddAssign<T> for SubstAssignVec
    where
        T: Into<AssignItem>,
    {
        fn add_assign(&mut self, rhs: T) {
            self.items.push_back(rhs.into());
        }
    }

    impl<T> std::ops::Add<T> for SubstAssignVec
    where
        T: Into<AssignItem>,
    {
        type Output = Self;

        fn add(mut self, rhs: T) -> Self::Output {
            std::ops::AddAssign::add_assign(&mut self, rhs);
            self
        }
    }

    impl AssignPattern {
        pub fn normalize(self, arena: &mut SNormArena) -> Vec<AssignItem> {
            let arena_mut: &mut SNormArena = arena.as_mut();
            let pat = arena_mut.inner.svpats[&self.pat].clone();
            let value = arena_mut.inner.svalues[&self.value].clone();
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
}

/// In-place substitution for normalized stack IR nodes.
pub trait SubstAssignItemInPlace {
    /// Substitute the free variables in the term in place.
    ///
    /// The [`DefId`]s in the pattern part of the map are guaranteed to be free.
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>;
}

// No need to implement SubstAssignItemInPlace for VPatId,
// because there's no shadowing/capturing of variables in value patterns.

impl SubstAssignItemInPlace for ValueId {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        let arena_mut = arena.as_mut();
        let value = arena_mut.inner.svalues[&self].clone();
        let AssignItem::Def(assign_def) = item.clone() else {
            // If the item is not a definition assignment, do nothing.
            return;
        };
        use Value;
        match value {
            | Value::Hole(Hole) => {}
            | Value::Var(def) if def == assign_def.def => {
                let new_value = arena_mut.inner.svalues[&assign_def.value].clone();
                arena_mut.inner.svalues.replace(self, new_value);
            }
            | Value::Var(_) => {}
            | Value::Closure(Closure { capture: _, stack: Bullet, body }) => {
                body.subst_assign_item_in_place(arena, item);
            }
            | Value::Ctor(Ctor(_, body)) => {
                body.subst_assign_item_in_place(arena, item);
            }
            | Value::Triv(Triv) => {}
            | Value::VCons(Cons(a, b)) => {
                a.subst_assign_item_in_place(arena, item.clone());
                b.subst_assign_item_in_place(arena, item);
            }
            | Value::Literal(_) => {}
            | Value::Complex(Complex { operator: _, operands }) => {
                operands.into_iter().for_each(|operand| {
                    operand.subst_assign_item_in_place(arena, item.clone());
                });
            }
        }
    }
}

impl SubstAssignItemInPlace for StackId {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        let arena_mut = arena.as_mut();
        let stack = arena_mut.inner.sstacks[&self].clone();
        use Stack;
        match stack {
            | Stack::Kont(Kont { binder: _, body }) => {
                body.subst_assign_item_in_place(arena, item);
            }
            | Stack::Var(Bullet) => match item {
                | AssignItem::Def(_) | AssignItem::Pattern(_) => {}
                | AssignItem::Stack(AssignStack { stack: assigned }) => {
                    let assigned = arena_mut.inner.sstacks[&assigned].clone();
                    arena_mut.inner.sstacks.replace(self, assigned);
                }
            },
            | Stack::Arg(Cons(value, stack)) => {
                value.subst_assign_item_in_place(arena, item.clone());
                stack.subst_assign_item_in_place(arena, item);
            }
            | Stack::Tag(Cons(_, stack)) => {
                stack.subst_assign_item_in_place(arena, item);
            }
        }
    }
}

impl SubstAssignItemInPlace for CompuId {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        let arena_mut = arena.as_mut();
        enum Action {
            Replace(AssignItem),
            KeepDef(AssignDef),
            KeepPattern(AssignPattern),
        }
        let action = match &item {
            | AssignItem::Def(AssignDef { def, value: _ }) if arena_mut.inner.users[&def] <= 1 => {
                Action::Replace(item)
            }
            | AssignItem::Stack(_) => Action::Replace(item),
            | AssignItem::Def(assign_def) => Action::KeepDef(assign_def.clone()),
            | AssignItem::Pattern(assign_pattern) => Action::KeepPattern(assign_pattern.clone()),
        };
        match action {
            | Action::Replace(item) => {
                let SComputation { compu, mut assignments } =
                    arena_mut.inner.scompus[&self].clone();
                assignments.subst_assign_item_in_place(arena, item.clone());
                use Computation as Compu;
                match compu {
                    | Compu::Hole(Hole) => {}
                    | Compu::Force(SForce { thunk, stack }) => {
                        thunk.subst_assign_item_in_place(arena, item.clone());
                        stack.subst_assign_item_in_place(arena, item);
                    }
                    | Compu::Ret(SReturn { stack, value }) => {
                        stack.subst_assign_item_in_place(arena, item.clone());
                        value.subst_assign_item_in_place(arena, item);
                    }
                    | Compu::Fix(SFix { capture: _, param: _, body }) => {
                        body.subst_assign_item_in_place(arena, item);
                    }
                    | Compu::Case(Match { scrut, arms }) => {
                        scrut.subst_assign_item_in_place(arena, item.clone());
                        arms.into_iter().for_each(|Matcher { binder: _, tail }| {
                            tail.subst_assign_item_in_place(arena, item.clone());
                        });
                    }
                    | Compu::Join(join) => match join {},
                    | Compu::LetArg(Let { binder: _, bindee, tail }) => {
                        bindee.subst_assign_item_in_place(arena, item.clone());
                        tail.subst_assign_item_in_place(arena, item);
                    }
                    | Compu::CoCase(CoMatch { arms }) => {
                        arms.into_iter().for_each(|CoMatcher { dtor: _, tail }| {
                            tail.subst_assign_item_in_place(arena, item.clone());
                        });
                    }
                    | Compu::ExternCall(ExternCall { function: _, stack: Bullet }) => {}
                }
            }
            | Action::KeepDef(_) => {
                todo!()
            }
            | Action::KeepPattern(_) => {
                todo!()
            }
        }
    }
}

impl SubstAssignItemInPlace for &mut SubstAssignVec {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        for inspecting in self.items.iter_mut().rev() {
            inspecting.subst_assign_item_in_place(arena, item.clone());
        }
    }
}

impl SubstAssignItemInPlace for &mut AssignItem {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        match self {
            | AssignItem::Def(assign_def) => assign_def.subst_assign_item_in_place(arena, item),
            | AssignItem::Pattern(assign_pattern) => {
                assign_pattern.subst_assign_item_in_place(arena, item)
            }
            | AssignItem::Stack(assign_stack) => {
                assign_stack.subst_assign_item_in_place(arena, item)
            }
        }
    }
}
impl SubstAssignItemInPlace for &mut AssignDef {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        self.value.subst_assign_item_in_place(arena, item)
    }
}
impl SubstAssignItemInPlace for &mut AssignPattern {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        self.value.subst_assign_item_in_place(arena, item)
    }
}
impl SubstAssignItemInPlace for &mut AssignStack {
    fn subst_assign_item_in_place<T>(self, arena: &mut T, item: AssignItem)
    where
        T: AsMut<SNormArena>,
    {
        self.stack.subst_assign_item_in_place(arena, item)
    }
}
