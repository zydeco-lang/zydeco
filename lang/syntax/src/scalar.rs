//! Checked scalar representation changes inside a straight-line computation.
//!
//! Inputs and the result use the ordinary value ABI. Raw values are local to the
//! region; its instruction set cannot call source code or construct scanned fields.

use crate::word::ScalarRepresentation;
use crate::{FloatType, IntegerType, Literal, PrimitiveError, PrimitiveOp};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarType {
    Integer(IntegerType),
    Float(FloatType),
}

impl ScalarType {
    pub fn representation(self) -> ScalarRepresentation {
        match self {
            | Self::Integer(ty) => ty.representation(),
            | Self::Float(ty) => ty.representation(),
        }
    }

    pub fn of_literal(literal: &Literal) -> Option<Self> {
        match literal {
            | Literal::Integer(value) => value.integer_type().map(Self::Integer),
            | Literal::Float(value) => Some(Self::Float(value.float_type())),
            | Literal::Char(_) | Literal::String(_) => None,
        }
    }
}

impl PrimitiveOp {
    pub fn scalar_type(self) -> ScalarType {
        match self {
            | Self::Integer(ty, _) => ScalarType::Integer(ty),
            | Self::Float(ty, _) => ScalarType::Float(ty),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarRepr {
    Value(ScalarType),
    Raw(ScalarType),
}

/// Inputs precede instruction results in one region-local definition sequence.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ScalarId(pub usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarStep {
    Decode { ty: ScalarType, value: ScalarId },
    Encode { ty: ScalarType, raw: ScalarId },
    Arithmetic { operation: PrimitiveOp, operands: [ScalarId; 2] },
}

impl ScalarStep {
    fn map(self, mut f: impl FnMut(ScalarId) -> ScalarId) -> Self {
        match self {
            | Self::Decode { ty, value } => Self::Decode { ty, value: f(value) },
            | Self::Encode { ty, raw } => Self::Encode { ty, raw: f(raw) },
            | Self::Arithmetic { operation, operands } => {
                Self::Arithmetic { operation, operands: operands.map(f) }
            }
        }
    }

    fn operands(self) -> Vec<ScalarId> {
        match self {
            | Self::Decode { value, .. } | Self::Encode { raw: value, .. } => vec![value],
            | Self::Arithmetic { operands, .. } => operands.to_vec(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum ScalarBoxing {
    Keep,
    #[default]
    Eliminate,
}

/// Unchecked input to the representation verifier, also used by rewrites.
#[derive(Clone, Debug)]
pub struct ScalarRegion {
    pub inputs: Vec<ScalarType>,
    pub output: ScalarType,
    pub steps: Vec<ScalarStep>,
    pub result: ScalarId,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ScalarError {
    #[error("scalar definition {value:?} is unavailable at definition {at:?}")]
    Unavailable { at: ScalarId, value: ScalarId },
    #[error("scalar definition {at:?} expects {expected:?} from {value:?}, found {found:?}")]
    Representation { at: ScalarId, value: ScalarId, expected: ScalarRepr, found: ScalarRepr },
    #[error("scalar region result {value:?} must use the ordinary value ABI, found {found:?}")]
    RawResult { value: ScalarId, found: ScalarRepr },
    #[error("scalar storage has {found} homes for {expected} definitions")]
    StorageCount { expected: usize, found: usize },
    #[error(
        "scalar definition {value:?} with representation {representation:?} has invalid home {slot:?}"
    )]
    StorageClass { value: ScalarId, representation: ScalarRepr, slot: ScalarSlot },
    #[error("scalar input {input:?} must use incoming value home {expected:?}, found {found:?}")]
    InputStorage { input: ScalarId, expected: ScalarSlot, found: ScalarSlot },
    #[error("scalar home {slot:?} is duplicated or outside its storage area")]
    StorageSlot { slot: ScalarSlot },
}

/// Only verification can publish this immutable region to a backend.
#[derive(Clone, Debug)]
pub struct ScalarProgram {
    region: ScalarRegion,
    representations: Vec<ScalarRepr>,
}

impl ScalarRegion {
    pub fn verify(self) -> Result<ScalarProgram, ScalarError> {
        let mut representations =
            self.inputs.iter().copied().map(ScalarRepr::Value).collect::<Vec<_>>();
        for step in &self.steps {
            let at = ScalarId(representations.len());
            let check = |value: ScalarId, expected| {
                let found =
                    *representations.get(value.0).ok_or(ScalarError::Unavailable { at, value })?;
                if found != expected {
                    return Err(ScalarError::Representation { at, value, expected, found });
                }
                Ok(())
            };
            let representation = match *step {
                | ScalarStep::Decode { ty, value } => {
                    check(value, ScalarRepr::Value(ty))?;
                    ScalarRepr::Raw(ty)
                }
                | ScalarStep::Encode { ty, raw } => {
                    check(raw, ScalarRepr::Raw(ty))?;
                    ScalarRepr::Value(ty)
                }
                | ScalarStep::Arithmetic { operation, operands } => {
                    let representation = ScalarRepr::Raw(operation.scalar_type());
                    for operand in operands {
                        check(operand, representation)?;
                    }
                    representation
                }
            };
            representations.push(representation);
        }
        let found = *representations.get(self.result.0).ok_or(ScalarError::Unavailable {
            at: ScalarId(representations.len()),
            value: self.result,
        })?;
        if !matches!(found, ScalarRepr::Value(_)) {
            return Err(ScalarError::RawResult { value: self.result, found });
        }
        let expected = ScalarRepr::Value(self.output);
        if found != expected {
            return Err(ScalarError::Representation {
                at: ScalarId(representations.len()),
                value: self.result,
                expected,
                found,
            });
        }
        Ok(ScalarProgram { region: self, representations })
    }
}

impl ScalarProgram {
    /// Cancel decode(encode(raw)), retain arithmetic order (including unused traps),
    /// then remove conversion results with no remaining consumer and verify again.
    pub fn eliminate_boxes(&self) -> Result<Self, ScalarError> {
        let inputs = self.region.inputs.len();
        let mut aliases = (0..inputs).map(ScalarId).collect::<Vec<_>>();
        let mut steps = Vec::new();
        for step in &self.region.steps {
            let step = step.map(|id| aliases[id.0]);
            if let ScalarStep::Decode { ty, value } = step
                && value.0 >= inputs
                && let ScalarStep::Encode { ty: encoded, raw } = steps[value.0 - inputs]
                && ty == encoded
            {
                aliases.push(raw);
            } else {
                aliases.push(ScalarId(inputs + steps.len()));
                steps.push(step);
            }
        }
        let result = aliases[self.region.result.0];
        let mut live = vec![false; inputs + steps.len()];
        live[result.0] = true;
        for (index, step) in steps.iter().enumerate().rev() {
            if live[inputs + index] || matches!(step, ScalarStep::Arithmetic { .. }) {
                live[inputs + index] = true;
                for operand in step.operands() {
                    live[operand.0] = true;
                }
            }
        }
        let mut remap = (0..inputs).map(ScalarId).collect::<Vec<_>>();
        let mut retained = Vec::new();
        for (index, step) in steps.into_iter().enumerate() {
            remap.push(ScalarId(inputs + retained.len()));
            if live[inputs + index] {
                retained.push(step.map(|id| remap[id.0]));
            }
        }
        ScalarRegion {
            inputs: self.region.inputs.clone(),
            output: self.region.output,
            steps: retained,
            result: remap[result.0],
        }
        .verify()
    }

    pub fn primitive(operation: PrimitiveOp) -> Self {
        let ty = operation.scalar_type();
        ScalarRegion {
            inputs: vec![ty; 2],
            output: ty,
            steps: vec![
                ScalarStep::Decode { ty, value: ScalarId(0) },
                ScalarStep::Decode { ty, value: ScalarId(1) },
                ScalarStep::Arithmetic { operation, operands: [ScalarId(2), ScalarId(3)] },
                ScalarStep::Encode { ty, raw: ScalarId(4) },
            ],
            result: ScalarId(5),
        }
        .verify()
        .expect("primitive construction establishes scalar representations")
    }

    pub fn region(&self) -> &ScalarRegion {
        &self.region
    }
    pub fn representations(&self) -> &[ScalarRepr] {
        &self.representations
    }
    pub fn result_type(&self) -> ScalarType {
        let ScalarRepr::Value(ty) = self.representations[self.region.result.0] else {
            unreachable!()
        };
        ty
    }

    pub fn allocation_count(&self) -> usize {
        self.region.steps.iter().filter(|step| matches!(step,
            ScalarStep::Encode { ty, .. } if ty.representation() == ScalarRepresentation::OpaqueBox
        )).count()
    }

    /// The semantic interpreter preserves source scalars across representation changes.
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
            let value = match *step {
                | ScalarStep::Decode { value, .. } | ScalarStep::Encode { raw: value, .. } => {
                    values[value.0].clone()
                }
                | ScalarStep::Arithmetic { operation, operands } => {
                    operation.evaluate(&operands.map(|id| values[id.0].clone()))?
                }
            };
            values.push(value);
        }
        Ok(values[self.region.result.0].clone())
    }
}

/// Native homes separate collector-visible words from untraced payload bits.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ScalarSlot {
    Value(usize),
    Raw(usize),
}

#[derive(Clone, Debug)]
pub struct ScalarStorage {
    slots: Vec<ScalarSlot>,
    value_words: usize,
    raw_words: usize,
}

impl ScalarStorage {
    pub fn for_program(program: &ScalarProgram) -> Self {
        let mut values = 0;
        let mut raw = 0;
        let slots = program
            .representations
            .iter()
            .map(|representation| match representation {
                | ScalarRepr::Value(_) => {
                    let slot = ScalarSlot::Value(values);
                    values += 1;
                    slot
                }
                | ScalarRepr::Raw(_) => {
                    let slot = ScalarSlot::Raw(raw);
                    raw += 1;
                    slot
                }
            })
            .collect();
        Self::verify(program, slots).expect("separate scalar storage areas")
    }

    pub fn verify(program: &ScalarProgram, slots: Vec<ScalarSlot>) -> Result<Self, ScalarError> {
        if slots.len() != program.representations.len() {
            return Err(ScalarError::StorageCount {
                expected: program.representations.len(),
                found: slots.len(),
            });
        }
        let value_words = program
            .representations
            .iter()
            .filter(|repr| matches!(repr, ScalarRepr::Value(_)))
            .count();
        let raw_words = slots.len() - value_words;
        let mut seen = std::collections::HashSet::new();
        for (index, (&representation, &slot)) in
            program.representations.iter().zip(&slots).enumerate()
        {
            if index < program.region.inputs.len() && slot != ScalarSlot::Value(index) {
                return Err(ScalarError::InputStorage {
                    input: ScalarId(index),
                    expected: ScalarSlot::Value(index),
                    found: slot,
                });
            }
            let valid = match (representation, slot) {
                | (ScalarRepr::Value(_), ScalarSlot::Value(index)) => index < value_words,
                | (ScalarRepr::Raw(_), ScalarSlot::Raw(index)) => index < raw_words,
                | _ => {
                    return Err(ScalarError::StorageClass {
                        value: ScalarId(index),
                        representation,
                        slot,
                    });
                }
            };
            if !valid || !seen.insert(slot) {
                return Err(ScalarError::StorageSlot { slot });
            }
        }
        Ok(Self { slots, value_words, raw_words })
    }

    pub fn slot(&self, id: ScalarId) -> ScalarSlot {
        self.slots[id.0]
    }
    pub fn value_words(&self) -> usize {
        self.value_words
    }
    pub fn raw_words(&self) -> usize {
        self.raw_words
    }
}

#[cfg(test)]
mod tests;
