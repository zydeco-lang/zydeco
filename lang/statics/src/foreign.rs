//! Static interpretation of source-level foreign imports.

use crate::{arena::StaticsArena, syntax as ss};
use std::collections::HashSet;
use thiserror::Error;
use zydeco_syntax::{
    App, Arrow, ForeignImport, ForeignParameter, ForeignResult, ForeignSignature, ForeignTarget,
    IntegerType, Named, PrimitiveType,
};
use zydeco_utils::prelude::ArenaAccess;

/// A foreign annotation whose checked classifier is outside the implemented safe ABI profile.
#[derive(Clone, Debug, Error)]
pub enum ForeignClassifierError {
    #[error(
        "C ffi currently supports classifier `Thk (Bytes -> UInt64 -> Ret UInt64)`, but found type {classifier:?}"
    )]
    Unsupported { classifier: ss::TypeId },
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
        let valid = self
            .unary_application(classifier, ForeignConstructor::Thunk)
            .and_then(|body| {
                let ss::Type::Arrow(Arrow(bytes, body)) = self.type_view(body)? else {
                    return None;
                };
                let ss::Type::Arrow(Arrow(seed, body)) = self.type_view(body)? else {
                    return None;
                };
                let result = self.unary_application(body, ForeignConstructor::Return)?;
                (self.primitive(bytes) == Some(PrimitiveType::Bytes)
                    && self.primitive(seed) == Some(PrimitiveType::Integer(IntegerType::UInt64))
                    && self.primitive(result) == Some(PrimitiveType::Integer(IntegerType::UInt64)))
                .then_some(())
            })
            .is_some();
        if !valid {
            return Err(ForeignClassifierError::Unsupported { classifier });
        }
        Ok(ForeignImport {
            target,
            signature: ForeignSignature {
                parameters: vec![ForeignParameter::BorrowedBytes, ForeignParameter::UInt64],
                result: ForeignResult::UInt64,
            },
        })
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
