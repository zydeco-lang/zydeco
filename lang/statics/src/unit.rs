//! Preserve complete classifiers for the initial native unit profile before static erasure.

use crate::{arena::StaticsArena, syntax::*};

#[derive(Clone, Debug, thiserror::Error)]
pub enum UnitClassifierError {
    #[error("native unit import requires `Thk (Ret Exports)`")]
    Initializer,
    #[error(
        "native unit interface supports only closed unit, scalar, named product, and thunk argument/return types; unsupported classifier {classifier:?}"
    )]
    Unsupported { classifier: TypeId },
    #[error(transparent)]
    Interface(#[from] UnitInterfaceError),
}

pub struct UnitClassifier<'a> {
    pub statics: &'a StaticsArena,
}

impl UnitClassifier<'_> {
    pub fn import(
        &self, target: ForeignTarget, classifier: TypeId,
    ) -> Result<UnitImport, UnitClassifierError> {
        if target.abi != ForeignAbi::Zydeco {
            return Err(UnitClassifierError::Initializer);
        }
        let Some(Type::App(App(thunk, computation))) = self.view(classifier) else {
            return Err(UnitClassifierError::Initializer);
        };
        let Some(Type::App(App(ret, exports))) = self.view(*computation) else {
            return Err(UnitClassifierError::Initializer);
        };
        if !matches!(self.view(*thunk), Some(Type::Thk(_)))
            || !matches!(self.view(*ret), Some(Type::Ret(_)))
        {
            return Err(UnitClassifierError::Initializer);
        }
        Ok(UnitImport { target, exports: self.exports(*exports)? })
    }

    pub fn exports(&self, classifier: TypeId) -> Result<UnitValueType, UnitClassifierError> {
        let mut remaining = UnitValueType::MAX_NODES;
        let value = self.value(classifier, 0, &mut remaining)?;
        value.validate()?;
        Ok(value)
    }

    fn view(&self, classifier: TypeId) -> Option<&Type> {
        self.statics.normalized_at(classifier)
    }

    fn visit(
        &self, classifier: TypeId, depth: usize, remaining: &mut usize,
    ) -> Result<&Type, UnitClassifierError> {
        if depth > UnitValueType::MAX_DEPTH || *remaining == 0 {
            return Err(UnitClassifierError::Interface(UnitInterfaceError));
        }
        *remaining -= 1;
        self.view(classifier).ok_or(UnitClassifierError::Unsupported { classifier })
    }

    fn value(
        &self, classifier: TypeId, depth: usize, remaining: &mut usize,
    ) -> Result<UnitValueType, UnitClassifierError> {
        Ok(match self.visit(classifier, depth, remaining)? {
            | Type::Unit(_) => UnitValueType::Unit,
            | Type::Primitive(PrimitiveTy(primitive)) => match primitive {
                | PrimitiveType::Integer(integer) => UnitValueType::Integer(*integer),
                | PrimitiveType::Float(float) => UnitValueType::Float(*float),
                | PrimitiveType::Char => UnitValueType::Char,
                | PrimitiveType::String => UnitValueType::String,
            },
            | Type::Label(Label(name, child)) => UnitValueType::Named {
                name: name.0.clone(),
                value: Box::new(self.value(*child, depth + 1, remaining)?),
            },
            | Type::Prod(Prod(fields)) => UnitValueType::Product(
                fields
                    .iter()
                    .map(|ty| self.value(*ty, depth + 1, remaining))
                    .collect::<Result<_, _>>()?,
            ),
            | Type::App(App(thunk, stack)) if matches!(self.view(*thunk), Some(Type::Thk(_))) => {
                UnitValueType::Thunk(Box::new(self.stack(*stack, depth + 1, remaining)?))
            }
            | _ => return Err(UnitClassifierError::Unsupported { classifier }),
        })
    }

    fn stack(
        &self, classifier: TypeId, depth: usize, remaining: &mut usize,
    ) -> Result<UnitStackType, UnitClassifierError> {
        Ok(match self.visit(classifier, depth, remaining)? {
            | Type::Arrow(Arrow(argument, tail)) => UnitStackType::Arrow(
                Box::new(self.value(*argument, depth + 1, remaining)?),
                Box::new(self.stack(*tail, depth + 1, remaining)?),
            ),
            | Type::App(App(ret, value)) if matches!(self.view(*ret), Some(Type::Ret(_))) => {
                UnitStackType::Return(Box::new(self.value(*value, depth + 1, remaining)?))
            }
            | _ => return Err(UnitClassifierError::Unsupported { classifier }),
        })
    }
}
