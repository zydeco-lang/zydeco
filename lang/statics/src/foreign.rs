//! Static interpretation of source-level foreign imports.

use crate::{arena::StaticsArena, syntax as ss};
use std::collections::HashSet;
use thiserror::Error;
use zydeco_syntax::{
    App, Arrow, ForeignImport, ForeignParameter, ForeignResult, ForeignSignature,
    ForeignSignatureError, ForeignTarget, IntegerType, Named, PrimitiveType,
};
use zydeco_utils::prelude::ArenaAccess;

/// A foreign annotation whose classifier is outside the implemented C ABI subset.
#[derive(Clone, Debug, Error)]
pub enum ForeignClassifierError {
    #[error("C ffi requires a thunk classified by `Thk (A1 -> ... -> Ret UInt64)`")]
    ExpectedThunk { classifier: ss::TypeId },
    #[error("C ffi argument {index} must have type `Bytes` or `UInt64`")]
    UnsupportedParameter { index: usize, classifier: ss::TypeId },
    #[error("C ffi computation must end in `Ret UInt64`")]
    ExpectedReturn { classifier: ss::TypeId },
    #[error("C ffi currently supports only a `UInt64` result")]
    UnsupportedResult { classifier: ss::TypeId },
    #[error(transparent)]
    Signature(#[from] ForeignSignatureError),
}

/// Derives an explicit marshalling protocol from a normalized CBPV classifier.
pub struct ForeignClassifier<'a> {
    statics: &'a StaticsArena,
}

impl<'a> ForeignClassifier<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    pub fn validate(
        &self, target: ForeignTarget, classifier: ss::TypeId,
    ) -> Result<ForeignImport, ForeignClassifierError> {
        let mut body = self
            .unary_application(classifier, ForeignConstructor::Thunk)
            .ok_or(ForeignClassifierError::ExpectedThunk { classifier })?;
        let mut parameters = Vec::new();
        let mut visited = HashSet::new();
        loop {
            if !visited.insert(body) {
                return Err(ForeignClassifierError::ExpectedReturn { classifier: body });
            }
            let Some(ss::Type::Arrow(Arrow(parameter, tail))) = self.type_view(body) else {
                break;
            };
            let representation = match self.primitive(parameter) {
                | Some(PrimitiveType::Bytes) => ForeignParameter::BorrowedBytes,
                | Some(PrimitiveType::Integer(IntegerType::UInt64)) => ForeignParameter::UInt64,
                | _ => {
                    return Err(ForeignClassifierError::UnsupportedParameter {
                        index: parameters.len() + 1,
                        classifier: parameter,
                    });
                }
            };
            parameters.push(representation);
            body = tail;
        }
        let result = self
            .unary_application(body, ForeignConstructor::Return)
            .ok_or(ForeignClassifierError::ExpectedReturn { classifier: body })?;
        if self.primitive(result) != Some(PrimitiveType::Integer(IntegerType::UInt64)) {
            return Err(ForeignClassifierError::UnsupportedResult { classifier: result });
        }
        let signature = ForeignSignature::new(parameters, ForeignResult::UInt64)?;
        Ok(ForeignImport { target, signature })
    }

    fn unary_application(
        &self, ty: ss::TypeId, constructor: ForeignConstructor,
    ) -> Option<ss::TypeId> {
        let ss::Type::App(App(found, body)) = self.type_view(ty)? else {
            return None;
        };
        self.is_constructor(found, constructor).then_some(body)
    }

    fn primitive(&self, ty: ss::TypeId) -> Option<PrimitiveType> {
        match self.type_view(ty)? {
            | ss::Type::Primitive(ss::PrimitiveTy(primitive)) => Some(primitive),
            | _ => None,
        }
    }

    fn is_constructor(&self, ty: ss::TypeId, expected: ForeignConstructor) -> bool {
        matches!((self.type_view(ty), expected), |(
            Some(ss::Type::Thk(_)),
            ForeignConstructor::Thunk,
        )| (
            Some(ss::Type::Ret(_)),
            ForeignConstructor::Return
        ))
    }

    fn type_view(&self, ty: ss::TypeId) -> Option<ss::Type> {
        let mut current = ty;
        let mut visited = HashSet::new();
        loop {
            if !visited.insert(current) {
                return None;
            }
            match self.statics.normalized_at(current)?.clone() {
                | ss::Type::Named(Named(_, inner)) => current = inner,
                | ss::Type::Var(def) => {
                    let ss::AnnId::Type(inner) = self.statics.annotations_var.get(&def).copied()?
                    else {
                        return None;
                    };
                    current = inner;
                }
                | ty => return Some(ty),
            }
        }
    }
}

#[derive(Copy, Clone)]
enum ForeignConstructor {
    Thunk,
    Return,
}
