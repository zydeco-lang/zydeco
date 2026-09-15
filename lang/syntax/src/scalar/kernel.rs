//! A raw arithmetic body enclosed by one memory load and one memory store.

use super::*;

#[derive(Clone, Debug)]
pub enum KernelStep {
    Literal(Literal),
    Arithmetic { operation: PrimitiveOp, operands: [ScalarId; 2] },
}

/// Input zero comes from the load; later inputs are decoded ordinary scalar arguments.
/// Every definition stays raw until the final store. There are no calls or managed fields.
#[derive(Clone, Debug)]
pub struct KernelRegion {
    pub inputs: Vec<ScalarType>,
    pub output: ScalarType,
    pub steps: Vec<KernelStep>,
    pub result: ScalarId,
}

#[derive(Clone, Debug)]
pub struct ScalarKernel {
    region: KernelRegion,
}

impl KernelRegion {
    pub fn verify(self) -> Result<ScalarKernel, ScalarError> {
        if self.output.representation() != ScalarRepresentation::OpaqueBox {
            return Err(ScalarError::MemoryCarrier { ty: self.output });
        }
        if self.inputs.first() != Some(&self.output) {
            return Err(ScalarError::MemoryInput);
        }
        let mut representations =
            self.inputs.iter().copied().map(ScalarRepr::Raw).collect::<Vec<_>>();
        for step in &self.steps {
            match step {
                | KernelStep::Literal(literal) => {
                    let ty =
                        ScalarType::of_literal(literal).ok_or(ScalarError::NonScalarLiteral)?;
                    representations.push(ScalarRepr::Raw(ty));
                }
                | KernelStep::Arithmetic { operation, operands } => {
                    ScalarStep::Arithmetic { operation: *operation, operands: *operands }
                        .verify(&mut representations)?;
                }
            }
        }
        let at = ScalarId(representations.len());
        let found = *representations
            .get(self.result.0)
            .ok_or(ScalarError::Unavailable { at, value: self.result })?;
        let expected = ScalarRepr::Raw(self.output);
        if found != expected {
            return Err(ScalarError::Representation { at, value: self.result, expected, found });
        }
        Ok(ScalarKernel { region: self })
    }
}

impl ScalarKernel {
    pub fn region(&self) -> &KernelRegion {
        &self.region
    }

    pub fn definitions(&self) -> usize {
        self.region.inputs.len() + self.region.steps.len()
    }

    /// External stack operands consist of two addresses and the remaining ordinary inputs.
    pub fn stack_inputs(&self) -> usize {
        self.region.inputs.len() + 1
    }

    pub fn scalar(&self) -> crate::memory::MemoryScalar {
        self.region.output.into()
    }

    pub fn evaluate(&self, inputs: &[Literal]) -> Result<Literal, PrimitiveError> {
        if inputs.len() != self.region.inputs.len()
            || inputs
                .iter()
                .zip(&self.region.inputs)
                .any(|(value, ty)| ScalarType::of_literal(value) != Some(*ty))
        {
            return Err(PrimitiveError::OperandType);
        }
        let mut values = inputs.to_vec();
        for step in &self.region.steps {
            let value = match step {
                | KernelStep::Literal(literal) => literal.clone(),
                | KernelStep::Arithmetic { operation, operands } => {
                    operation.evaluate(&operands.map(|id| values[id.0].clone()))?
                }
            };
            values.push(value);
        }
        Ok(values[self.region.result.0].clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{IntegerArithmetic, IntegerLiteral};

    fn increment() -> KernelRegion {
        let ty = ScalarType::Integer(IntegerType::Int64);
        KernelRegion {
            inputs: vec![ty],
            output: ty,
            steps: vec![
                KernelStep::Literal(Literal::Integer(IntegerLiteral::Int64(1))),
                KernelStep::Arithmetic {
                    operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
                    operands: [ScalarId(0), ScalarId(1)],
                },
            ],
            result: ScalarId(2),
        }
    }

    #[test]
    fn kernel_reuses_scalar_checks_without_changing_ordinary_exits() {
        let program = increment().verify().unwrap();
        assert_eq!(
            program.evaluate(&[Literal::Integer(IntegerLiteral::Int64(i64::MAX))]).unwrap(),
            Literal::Integer(IntegerLiteral::Int64(i64::MIN))
        );
        let mut wrong = increment();
        wrong.steps[0] = KernelStep::Literal(Literal::Integer(IntegerLiteral::UInt64(1)));
        assert!(matches!(wrong.verify(), Err(ScalarError::Representation { .. })));
        let mut forward = increment();
        forward.result = ScalarId(3);
        assert!(matches!(forward.verify(), Err(ScalarError::Unavailable { .. })));
        let mut wrong_carrier = increment();
        wrong_carrier.output = ScalarType::Integer(IntegerType::Int);
        assert!(matches!(wrong_carrier.verify(), Err(ScalarError::MemoryCarrier { .. })));
        let mut missing_load = increment();
        missing_load.inputs.clear();
        assert!(matches!(missing_load.verify(), Err(ScalarError::MemoryInput)));
        let ty = ScalarType::Integer(IntegerType::Int64);
        assert!(matches!(
            ScalarRegion {
                inputs: vec![ty],
                output: ty,
                steps: vec![ScalarStep::Decode { ty, value: ScalarId(0) }],
                result: ScalarId(1),
            }
            .verify(),
            Err(ScalarError::RawResult { .. })
        ));
    }

    #[test]
    fn kernel_preserves_traps_before_the_store_even_for_unused_arithmetic() {
        let mut region = increment();
        region.steps[0] = KernelStep::Literal(Literal::Integer(IntegerLiteral::Int64(0)));
        region.steps[1] = KernelStep::Arithmetic {
            operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Div),
            operands: [ScalarId(0), ScalarId(1)],
        };
        region.result = ScalarId(0);
        let program = region.verify().unwrap();
        assert_eq!(
            program.evaluate(&[Literal::Integer(IntegerLiteral::Int64(7))]),
            Err(PrimitiveError::DivisionByZero)
        );
    }
}
