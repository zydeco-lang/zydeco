//! Local representation analysis for first-order SPSLow.
//!
//! The analysis marks values and patterns that can avoid a GC heap cell. A value
//! that is constructed and consumed by a single projection-like use can be
//! represented by its fields directly. A variable bound to such a value can also
//! be expanded into several field slots when every use is a projection.
//!
//! SPSLow's single-occurrence invariant keeps the analysis local: a value node is
//! consumed exactly once, and sharing is explicit through variables.

use std::collections::{HashMap, HashSet};

use zydeco_stackir::{SpsLowProgram, sps_low::syntax as sk};

/// Values, patterns, and variables that the assembly lowerer may represent
/// without a GC heap cell.
#[derive(Debug, Default)]
pub struct LocalUnboxing {
    pub values: HashSet<sk::ValueId>,
    pub patterns: HashSet<sk::VPatId>,
    pub stack_values: HashSet<sk::ValueId>,
    pub unboxed_vars: HashMap<sk::DefId, usize>,
}

impl LocalUnboxing {
    pub fn collect(program: &SpsLowProgram) -> Self {
        let arena = program.arena();
        let mut collector = Collector { arena, unboxing: Self::default() };
        collector.compu(program.root());
        collector.unboxing
    }
}

struct Collector<'a> {
    arena: &'a sk::SpsLowArena,
    unboxing: LocalUnboxing,
}

impl Collector<'_> {
    fn compu(&mut self, id: sk::CompuId) {
        match self.arena.inner.compus[&id].clone() {
            | sk::Computation::Hole(sk::SHole(stack)) => self.stack(stack),
            | sk::Computation::Jump(sk::Jump { target, stack }) => {
                self.value(target);
                self.stack(stack);
            }
            | sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, body }) => {
                self.mark_product_pair(scrut, binder);
                self.value(scrut);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
                self.value(scrut);
                for sk::Matcher { binder, tail } in arms {
                    self.pattern(binder);
                    self.compu(tail);
                }
            }
            | sk::Computation::LetValue(sk::LetValue { binder, bindee, body }) => {
                self.mark_product_pair(bindee, binder);
                self.try_unbox_variable(binder, bindee, body);
                self.value(bindee);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::LetStack(sk::LetStack { bindee, body }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::LetArg(sk::LetArg { binder, bindee, body }) => {
                self.stack(bindee);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::CoCase(sk::SCoMatch { scrut, arms }) => {
                self.stack(scrut);
                for sk::CoMatcher { dtor: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::OpenClosure(sk::OpenClosure {
                package,
                environment,
                code,
                body,
            }) => {
                self.mark_closure(package, environment);
                self.value(package);
                self.pattern(environment);
                self.pattern(code);
                self.compu(body);
            }
            | sk::Computation::OpenContinuation(sk::OpenContinuation { package, code, body }) => {
                self.stack(package);
                self.pattern(code);
                self.compu(body);
            }
            | sk::Computation::ExternCall(sk::ExternCall { function: _, stack }) => {
                self.stack(stack);
            }
        }
    }

    fn value(&mut self, id: sk::ValueId) {
        match self.arena.inner.values[&id].clone() {
            | sk::Value::Hole(_)
            | sk::Value::Var(_)
            | sk::Value::Triv(_)
            | sk::Value::Literal(_) => {}
            | sk::Value::Block(sk::Block { label: _, body }) => self.compu(body),
            | sk::Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                self.value(environment);
                self.value(code);
            }
            | sk::Value::Ctor(sk::Ctor(_, value)) => self.value(value),
            | sk::Value::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.value(item);
                }
            }
            | sk::Value::Complex(sk::Complex { operator: _, operands }) => {
                for operand in operands {
                    self.value(operand);
                }
            }
        }
    }

    fn pattern(&mut self, id: sk::VPatId) {
        match self.arena.inner.vpats[&id].clone() {
            | sk::ValuePattern::Hole(_) | sk::ValuePattern::Var(_) | sk::ValuePattern::Triv(_) => {}
            | sk::ValuePattern::Ctor(sk::Ctor(_, pattern)) => self.pattern(pattern),
            | sk::ValuePattern::Alias(sk::Alias(patterns)) => {
                for pattern in patterns {
                    self.pattern(pattern);
                }
            }
            | sk::ValuePattern::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.pattern(item);
                }
            }
        }
    }

    fn stack(&mut self, id: sk::StackId) {
        match self.arena.inner.stacks[&id].clone() {
            | sk::Stack::Var(sk::Bullet) => {}
            | sk::Stack::Arg(sk::Cons(value, stack)) => {
                self.value(value);
                self.stack(stack);
            }
            | sk::Stack::Tag(sk::Cons(_, stack)) => self.stack(stack),
            | sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => {
                self.value(code);
                self.stack(residual);
            }
        }
    }

    fn mark_product_pair(&mut self, value: sk::ValueId, pattern: sk::VPatId) {
        let Some((value_items, value_arity)) = self.vcons_shape(value) else { return };
        let Some((pattern_items, pattern_arity)) = self.vpat_shape(pattern) else { return };
        if value_items == pattern_items && value_arity == pattern_arity {
            self.unboxing.values.insert(value);
            self.unboxing.patterns.insert(pattern);
        }
    }

    fn mark_closure(&mut self, package: sk::ValueId, environment: sk::VPatId) {
        let sk::Value::ClosurePackage(sk::ClosurePackage { environment: env_value, code: _ }) =
            &self.arena.inner.values[&package]
        else {
            return;
        };
        self.unboxing.values.insert(package);
        let Some((env_items, env_arity)) = self.vcons_shape(*env_value) else { return };
        let Some((pattern_items, pattern_arity)) = self.vpat_shape(environment) else { return };
        if env_items == pattern_items && env_arity == pattern_arity {
            self.unboxing.values.insert(*env_value);
            self.unboxing.patterns.insert(environment);
        }
    }

    fn try_unbox_variable(&mut self, binder: sk::VPatId, bindee: sk::ValueId, body: sk::CompuId) {
        let sk::ValuePattern::Var(def) = &self.arena.inner.vpats[&binder] else { return };
        let sk::Value::VCons(sk::VCons { items, layout }) = &self.arena.inner.values[&bindee]
        else {
            return;
        };
        let shape = (items.len(), layout.arity);
        let info = classify_var(&self.arena.inner, body, *def, shape);
        if info.all_projection && !info.escapes {
            self.unboxing.values.insert(bindee);
            self.unboxing.unboxed_vars.insert(*def, shape.0);
            for pattern in info.projections {
                self.unboxing.patterns.insert(pattern);
            }
        }
    }

    fn vcons_shape(&self, value: sk::ValueId) -> Option<(usize, usize)> {
        match &self.arena.inner.values[&value] {
            | sk::Value::VCons(sk::VCons { items, layout }) => Some((items.len(), layout.arity)),
            | _ => None,
        }
    }

    fn vpat_shape(&self, pattern: sk::VPatId) -> Option<(usize, usize)> {
        match &self.arena.inner.vpats[&pattern] {
            | sk::ValuePattern::VCons(sk::VCons { items, layout }) => {
                Some((items.len(), layout.arity))
            }
            | _ => None,
        }
    }
}

#[derive(Debug, Default)]
struct VarUse {
    all_projection: bool,
    escapes: bool,
    projections: Vec<sk::VPatId>,
}

fn classify_var(
    arena: &sk::SpsLowInnerArena, root: sk::CompuId, def: sk::DefId, shape: (usize, usize),
) -> VarUse {
    let mut info = VarUse { all_projection: true, escapes: false, projections: Vec::new() };
    let mut visitor = VarVisitor { arena, def, shape, info: &mut info };
    visitor.compu(root);
    info
}

struct VarVisitor<'a> {
    arena: &'a sk::SpsLowInnerArena,
    def: sk::DefId,
    shape: (usize, usize),
    info: &'a mut VarUse,
}

impl VarVisitor<'_> {
    fn compu(&mut self, id: sk::CompuId) {
        match self.arena.compus[&id].clone() {
            | sk::Computation::Hole(sk::SHole(stack)) => self.stack(stack),
            | sk::Computation::Jump(sk::Jump { target, stack }) => {
                self.value_escape(target);
                self.stack(stack);
            }
            | sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, body }) => {
                self.value_in_projection(scrut, Some(binder));
                self.compu(body);
            }
            | sk::Computation::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
                self.value_escape(scrut);
                for sk::Matcher { binder: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::LetValue(sk::LetValue { binder, bindee, body }) => {
                self.value_in_projection(bindee, Some(binder));
                self.compu(body);
            }
            | sk::Computation::LetStack(sk::LetStack { bindee, body }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::LetArg(sk::LetArg { binder: _, bindee, body }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::CoCase(sk::SCoMatch { scrut, arms }) => {
                self.stack(scrut);
                for sk::CoMatcher { dtor: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::OpenClosure(sk::OpenClosure {
                package,
                environment: _,
                code: _,
                body,
            }) => {
                self.value_escape(package);
                self.compu(body);
            }
            | sk::Computation::OpenContinuation(sk::OpenContinuation {
                package,
                code: _,
                body,
            }) => {
                self.stack(package);
                self.compu(body);
            }
            | sk::Computation::ExternCall(sk::ExternCall { function: _, stack }) => {
                self.stack(stack);
            }
        }
    }

    fn value_in_projection(&mut self, value: sk::ValueId, pattern: Option<sk::VPatId>) {
        if let sk::Value::Var(def) = &self.arena.values[&value]
            && *def == self.def
        {
            match pattern {
                | Some(pattern) if self.vpat_shape_matches(pattern) => {
                    self.info.projections.push(pattern);
                }
                | _ => {
                    self.info.all_projection = false;
                    self.info.escapes = true;
                }
            }
        } else {
            self.value_escape(value);
        }
    }

    fn value_escape(&mut self, value: sk::ValueId) {
        match self.arena.values[&value].clone() {
            | sk::Value::Var(def) if def == self.def => {
                self.info.all_projection = false;
                self.info.escapes = true;
            }
            | sk::Value::Var(_) => {}
            | sk::Value::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.value_escape(item);
                }
            }
            | sk::Value::ClosurePackage(_) | sk::Value::Ctor(_) | sk::Value::Complex(_) => {
                self.info.all_projection = false;
                self.info.escapes = true;
            }
            | sk::Value::Block(_) => {
                self.info.all_projection = false;
                self.info.escapes = true;
            }
            | sk::Value::Hole(_) | sk::Value::Triv(_) | sk::Value::Literal(_) => {}
        }
    }

    fn stack(&mut self, stack: sk::StackId) {
        match self.arena.stacks[&stack].clone() {
            | sk::Stack::Var(sk::Bullet) => {}
            | sk::Stack::Arg(sk::Cons(value, stack)) => {
                self.value_escape(value);
                self.stack(stack);
            }
            | sk::Stack::Tag(sk::Cons(_, stack)) => self.stack(stack),
            | sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => {
                self.value_escape(code);
                self.stack(residual);
            }
        }
    }

    fn vpat_shape_matches(&self, pattern: sk::VPatId) -> bool {
        match &self.arena.vpats[&pattern] {
            | sk::ValuePattern::VCons(sk::VCons { items, layout }) => {
                (items.len(), layout.arity) == self.shape
            }
            | _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_stackir::sps_low::arena::Construct as _;

    #[test]
    fn local_vcons_pair_is_unboxed() {
        let mut arena = sk::SpsLowArena::default();
        let field_a: sk::ValueId = sk::Triv.build(&mut arena, None);
        let field_b: sk::ValueId = sk::Triv.build(&mut arena, None);
        let items = sk::ConsN::from_vec(vec![field_a, field_b]).unwrap();
        let layout = sk::ProductLayout::conservative(2);
        let value: sk::ValueId = sk::VCons::new(items, layout.clone()).build(&mut arena, None);

        let pattern_a: sk::VPatId = sk::Hole.build(&mut arena, None);
        let pattern_b: sk::VPatId = sk::Hole.build(&mut arena, None);
        let pattern_items = sk::ConsN::from_vec(vec![pattern_a, pattern_b]).unwrap();
        let pattern: sk::VPatId = sk::VCons::new(pattern_items, layout).build(&mut arena, None);

        let stack: sk::StackId = sk::Bullet.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder: pattern, bindee: value, body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(unboxing.values.contains(&value));
        assert!(unboxing.patterns.contains(&pattern));
    }

    #[test]
    fn indirect_vcons_pair_is_not_unboxed() {
        let mut arena = sk::SpsLowArena::default();
        let field_a: sk::ValueId = sk::Triv.build(&mut arena, None);
        let field_b: sk::ValueId = sk::Triv.build(&mut arena, None);
        let items = sk::ConsN::from_vec(vec![field_a, field_b]).unwrap();
        let layout = sk::ProductLayout::conservative(2);
        let value: sk::ValueId = sk::VCons::new(items, layout.clone()).build(&mut arena, None);

        let binder: sk::VPatId = sk::Hole.build(&mut arena, None);
        let stack: sk::StackId = sk::Bullet.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder, bindee: value, body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(!unboxing.values.contains(&value));
    }
}
