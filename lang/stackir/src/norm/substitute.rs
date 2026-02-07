use super::syntax::*;
use crate::sps::syntax::*;
use derive_more::From;
use im::Vector;
use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub struct SubstPatMap {
    /// Ordered map of value patterns to their corresponding values.
    /// The order is from innermost to outermost, namely,
    /// - we should apply the substitution to the underlying body from the innermost to the outermost.
    /// - or equivalently, we should apply the outermost substitution to both all inner substitutions
    ///   and the underlying body, and then take one step inner, and so on.
    ///
    /// If you feel confused, simply think of it as a bunch of reversed let bindings.
    values: IndexMap<VPatId, ValueId>,
    /// Ordered list of stacks substitutions.
    /// The order is from innermost to outermost.
    ///
    /// Reversed stack-let bindings similar to reversed value-let bindings [`Self::values`].
    stacks: Vec<StackId>,
}

impl SubstPatMap {
    pub fn new() -> Self {
        Self { values: IndexMap::new(), stacks: Vec::new() }
    }
    /// Substitute a pattern for a value,
    /// syntactically wrapping a layer of let-binding for the value,
    /// semantically substituting the pattern with the value.
    pub fn cascade_value(&mut self, pat: VPatId, value: ValueId) {
        self.values.insert(pat, value);
    }
    /// Add another layer of stack on top of the current stack,
    /// syntactically wrapping a layer of let-binding for the stack,
    /// semantically substituting the current bullet with the new (prev) stack.
    pub fn cascade_stack(&mut self, prev: StackId) {
        self.stacks.push(prev);
    }
    /// Iterate over value pattern substitutions.
    pub fn values(&self) -> impl Iterator<Item = (&VPatId, &ValueId)> {
        self.values.iter()
    }
    /// Iterate over stack substitutions.
    pub fn stacks(&self) -> impl Iterator<Item = &StackId> {
        self.stacks.iter()
    }
}

/// In-place substitution for normalized stack IR nodes.
pub trait SubstPatInPlace {
    /// Substitute the free variables in the term in place.
    ///
    /// The [`DefId`]s in the pattern part of the map are guaranteed to be free.
    fn subst_pat_in_place<T>(self, arena: &mut T, map: SubstPatMap)
    where
        T: AsMut<SNormArena>;
}

// No need to implement SubstPatInPlace for VPatId,
// because there's no shadowing/capturing of variables in value patterns.

// impl SubstPatInPlace for ValueId {
//     fn subst_pat_in_place<T>(self, arena: &mut T, map: SubstPatMap)
//     where
//         T: AsMut<SNormArena>,
//     {
//         let mut arena_mut = arena.as_mut();
//         let value = arena_mut.inner.svalues[&self].clone();
//         todo!()
//     }
// }

// impl SubstPatInPlace for StackId {
//     fn subst_pat_in_place<T>(self, arena: &mut T, map: SubstPatMap)
//     where
//         T: AsMut<SNormArena>,
//     {
//         let mut arena_mut = arena.as_mut();
//         let stack = arena_mut.inner.sstacks[&self].clone();
//         todo!()
//     }
// }

// impl SubstPatInPlace for CompuId {
//     fn subst_pat_in_place<T>(self, arena: &mut T, map: SubstPatMap)
//     where
//         T: AsMut<SNormArena>,
//     {
//         let mut arena_mut = arena.as_mut();
//         let SComputation { compu, map } = arena_mut.inner.scompus[&self].clone();
//         use Computation as Compu;
//         match compu {
//             | Compu::Hole(hole) => todo!(),
//             | Compu::Force(sforce) => todo!(),
//             | Compu::Ret(sreturn) => todo!(),
//             | Compu::Fix(sfix) => todo!(),
//             | Compu::Case(_) => todo!(),
//             | Compu::Join(_) => todo!(),
//             | Compu::LetArg(_) => todo!(),
//             | Compu::CoCase(co_match) => todo!(),
//             | Compu::ExternCall(extern_call) => todo!(),
//         }
//     }
// }

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
