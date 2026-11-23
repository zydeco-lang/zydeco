//! Whole-program physical representation table for `DefId`s in `SpsLowProgram`.
//!
//! A single linear traversal summarizes each bound variable's [`RepType`] from its pattern
//! protocol, binding RHS, product destructuring, memory load scalar, and closure/continuation
//! code bindings.

use super::{
    check::SpsLowProgram,
    syntax::*,
    traverse::{Edge, Node, Occurrence, Traversal, Visitor},
};
use crate::protocol::ValueProtocol;

/// Physical representation summary for every `DefId` bound in an [`SpsLowProgram`].
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RepTable {
    defs: SecondaryMap<DefId, RepType>,
}

impl RepTable {
    /// Compute per-definition physical representations in one linear traversal over `program`.
    pub fn analyze(program: &SpsLowProgram) -> Self {
        let mut collector = RepCollector { arena: &program.arena().inner, table: Self::default() };
        Traversal { arena: &program.arena().inner }.run(program.root().into(), &mut collector);
        collector.table
    }

    /// Physical representation of `def`, defaulting to polymorphic [`RepType::Word`].
    pub fn get(&self, def: DefId) -> RepType {
        self.defs.get(def).copied().unwrap_or(RepType::Word)
    }

    fn record(&mut self, def: DefId, rep: RepType) {
        if rep == RepType::Word {
            return;
        }
        match self.defs.get_mut(def) {
            | Some(existing) => {
                if *existing != rep {
                    *existing = RepType::Word;
                }
            }
            | None => {
                self.defs.insert_new(def, rep);
            }
        }
    }
}

struct RepCollector<'a> {
    arena: &'a SpsLowInnerArena,
    table: RepTable,
}

impl RepCollector<'_> {
    fn record_pattern(&mut self, pattern: VPatId, mut hint: RepType) {
        let mut stack = vec![(pattern, hint)];
        while let Some((pat, expected)) = stack.pop() {
            let pat_rep = self.arena.pattern_rep(pat);
            hint = if pat_rep != RepType::Word { pat_rep } else { expected };
            match &self.arena.vpats[pat] {
                | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Var(def) => self.table.record(*def, hint),
                | ValuePattern::Ctor(Ctor(_, body)) => stack.push((*body, RepType::Word)),
                | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                    for &item in head.iter().rev() {
                        stack.push((item, hint));
                    }
                    stack.push((*tail, hint));
                }
                | ValuePattern::VCons(VCons { items, .. }) => {
                    let field_protocols = match self.arena.pattern_protocols.get(pat) {
                        | Some(ValueProtocol::Product(fields)) => Some(fields.as_slice()),
                        | _ => None,
                    };
                    for (index, &item) in items.iter().enumerate() {
                        let field_rep = field_protocols
                            .and_then(|fields| fields.get(index))
                            .map_or(RepType::Word, RepType::from_protocol);
                        stack.push((item, field_rep));
                    }
                }
            }
        }
    }

    fn bind_value_fields(&mut self, value: ValueId, pattern: VPatId) {
        match (&self.arena.values[value], &self.arena.vpats[pattern]) {
            | (
                Value::VCons(VCons { items, .. }),
                ValuePattern::VCons(VCons { items: pats, .. }),
            ) if items.len() == pats.len() => {
                for (&val, &pat) in items.iter().zip(pats.iter()) {
                    self.record_pattern(pat, self.arena.value_rep(val));
                }
            }
            | _ => self.record_pattern(pattern, self.arena.value_rep(value)),
        }
    }
}

impl Visitor<SpsLow> for RepCollector<'_> {
    fn enter(&mut self, node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            return;
        }
        match node {
            | Node::Value(_, Value::Ext(ValueExt::Block(Block { label, entry, .. }))) => {
                self.table.record(*label, RepType::Closure);
                for (_, pattern) in entry.words() {
                    self.record_pattern(pattern, RepType::Word);
                }
            }
            | Node::Computation(_, Computation::LetValue(Let { binder, bindee, .. })) => {
                self.bind_value_fields(*bindee, *binder);
            }
            | Node::Computation(
                _,
                Computation::ProductMatch(SProductMatch { scrut, binder, .. }),
            ) => {
                self.bind_value_fields(*scrut, *binder);
            }
            | Node::Computation(
                _,
                Computation::Memory(MemoryStep::Load { scalar, result, .. }),
            ) => {
                self.record_pattern(*result, RepType::from_protocol(&ValueProtocol::from(*scalar)));
            }
            | Node::Computation(
                _,
                Computation::Ext(CompuExt::OpenClosure(OpenClosure { environment, code, .. })),
            ) => {
                self.record_pattern(*environment, RepType::Word);
                self.record_pattern(*code, RepType::Closure);
            }
            | Node::Computation(
                _,
                Computation::Ext(CompuExt::OpenContinuation(OpenContinuation { code, .. })),
            ) => {
                self.record_pattern(*code, RepType::Closure);
            }
            | Node::Computation(_, Computation::LetArg(Let { binder: Cons(binder, _), .. })) => {
                self.record_pattern(*binder, RepType::Word);
            }
            | Node::Computation(_, Computation::CoprodMatch(SCoprodMatch { arms, .. })) => {
                for arm in arms {
                    self.record_pattern(arm.binder, RepType::Word);
                }
            }
            | _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpsScheme, protocol::StackProtocol};
    use zydeco_syntax::{IntegerLiteral, IntegerType, memory::MemoryScalar, scalar::ScalarType};

    #[test]
    fn collects_scalar_unit_address_product_closure_and_polymorphic_reps() {
        let mut arena = SpsLowArena::default();
        let unit_def = arena.admin.alloc_def(VarName::new("unit"));
        let int_def = arena.admin.alloc_def(VarName::new("num"));
        let addr_def = arena.admin.alloc_def(VarName::new("addr"));
        let loaded_def = arena.admin.alloc_def(VarName::new("loaded"));
        let prod_def = arena.admin.alloc_def(VarName::new("pair"));
        let clo_def = arena.admin.alloc_def(VarName::new("clo"));
        let poly_def = arena.admin.alloc_def(VarName::new("poly"));
        let block_label = arena.admin.alloc_def(VarName::new("fn"));
        let env_pat_def = arena.admin.alloc_def(VarName::new("env"));
        let poly_arg_def = arena.admin.alloc_def(VarName::new("arg"));

        let env_pat = ValuePattern::Var(env_pat_def).build(&mut arena, None);
        let hole_stack = Stack::Var(StackVar).build(&mut arena, None);
        let block_body = Computation::Hole(SHole(hole_stack)).build(&mut arena, None);
        let block_val = Value::Ext(ValueExt::Block(Block {
            label: block_label,
            entry: EntryParameters::Closure { environment: env_pat },
            body: block_body,
        }))
        .build(&mut arena, None);
        let unit_val = Value::Triv(Triv).build(&mut arena, None);
        let closure_pkg = Value::Ext(ValueExt::ClosurePackage(ClosurePackage {
            environment: unit_val,
            code: block_val,
        }))
        .build(&mut arena, None);

        let poly_pat = ValuePattern::Var(poly_arg_def).build(&mut arena, None);
        let mut tail = {
            let s = Stack::Var(StackVar).build(&mut arena, None);
            Computation::Hole(SHole(s)).build(&mut arena, None)
        };

        let load_pat = ValuePattern::Var(loaded_def).build(&mut arena, None);
        let addr_rhs = Value::Var(addr_def).build(&mut arena, None);
        tail = Computation::Memory(MemoryStep::Load {
            scalar: MemoryScalar::Integer(IntegerType::Int32),
            address: addr_rhs,
            result: load_pat,
            next: tail,
        })
        .build(&mut arena, None);

        let poly_pat_b = ValuePattern::Var(poly_def).build(&mut arena, None);
        let poly_val = Value::Var(poly_arg_def).build(&mut arena, None);
        tail = LetValue { binder: poly_pat_b, bindee: poly_val, tail }.build(&mut arena, None);

        let clo_pat = ValuePattern::Var(clo_def).build(&mut arena, None);
        tail = LetValue { binder: clo_pat, bindee: closure_pkg, tail }.build(&mut arena, None);

        let item0 = Value::Triv(Triv).build(&mut arena, None);
        let item1 =
            Value::Literal(Literal::Integer(IntegerLiteral::Int64(7))).build(&mut arena, None);
        let pair_val = Value::VCons(VCons::new(vec![item0, item1], ProductLayout { arity: 2 }))
            .build(&mut arena, None);
        let pair_pat = ValuePattern::Var(prod_def).build(&mut arena, None);
        tail = LetValue { binder: pair_pat, bindee: pair_val, tail }.build(&mut arena, None);

        let addr_base = Value::Hole(Hole).build(&mut arena, None);
        arena.inner.value_protocols.insert_new(addr_base, ValueProtocol::Address);
        let addr_disp =
            Value::Literal(Literal::Integer(IntegerLiteral::Int(0))).build(&mut arena, None);
        let addr_val = Value::AddrOffset(AddrOffset { base: addr_base, displacement: addr_disp })
            .build(&mut arena, None);
        let addr_pat = ValuePattern::Var(addr_def).build(&mut arena, None);
        tail = LetValue { binder: addr_pat, bindee: addr_val, tail }.build(&mut arena, None);

        let num_val =
            Value::Literal(Literal::Integer(IntegerLiteral::Int32(4))).build(&mut arena, None);
        let num_pat = ValuePattern::Var(int_def).build(&mut arena, None);
        tail = LetValue { binder: num_pat, bindee: num_val, tail }.build(&mut arena, None);

        let u_val = Value::Triv(Triv).build(&mut arena, None);
        let u_pat = ValuePattern::Var(unit_def).build(&mut arena, None);
        tail = LetValue { binder: u_pat, bindee: u_val, tail }.build(&mut arena, None);

        let arg_stack = Stack::Var(StackVar).build(&mut arena, None);
        let root = LetArg { binder: Cons(poly_pat, StackVar), bindee: arg_stack, tail }
            .build(&mut arena, None);

        // Manually tag protocol graph minimally or use StackProtocol::Unknown
        let _ = StackProtocol::Unknown;
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
        let table = RepTable::analyze(&program);

        assert_eq!(table.get(unit_def), RepType::Unit);
        assert_eq!(table.get(int_def), RepType::Scalar(ScalarType::Integer(IntegerType::Int32)));
        assert_eq!(table.get(addr_def), RepType::Address);
        assert_eq!(table.get(loaded_def), RepType::Scalar(ScalarType::Integer(IntegerType::Int32)));
        assert_eq!(table.get(prod_def), RepType::Product { arity: 2 });
        assert_eq!(table.get(clo_def), RepType::Closure);
        assert_eq!(table.get(block_label), RepType::Closure);
        assert_eq!(table.get(poly_def), RepType::Word);
        assert_eq!(table.get(poly_arg_def), RepType::Word);
    }
}
