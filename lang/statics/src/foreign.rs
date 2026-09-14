//! Static interpretation of source-level foreign imports.

use crate::{arena::StaticsArena, syntax as ss};
use std::collections::HashSet;
use thiserror::Error;
use zydeco_syntax::{
    App, Arrow, BuiltinRole, BuiltinTypeRole, ForeignImport, ForeignParameter, ForeignResult,
    ForeignSignature, ForeignSignatureError, ForeignTarget, IntegerType, Named, PrimitiveType,
    Prod,
};
use zydeco_utils::prelude::ArenaAccess;

/// A foreign annotation whose classifier is outside the implemented C ABI subset.
#[derive(Clone, Debug, Error)]
pub enum ForeignClassifierError {
    #[error(
        "C export argument {index} must be a fixed-width integer; incoming memory grants are not supported"
    )]
    UnsupportedExportParameter { index: usize, classifier: ss::TypeId },
    #[error("C ffi requires a thunk classified by `Thk (A1 -> ... -> Ret B)`")]
    ExpectedThunk { classifier: ss::TypeId },
    #[error(
        "C ffi argument {index} must be a readable `Access * Addr * Int64` window or a fixed-width integer"
    )]
    UnsupportedParameter { index: usize, classifier: ss::TypeId },
    #[error("C ffi computation must end in `Ret B`")]
    ExpectedReturn { classifier: ss::TypeId },
    #[error("C ffi result must be a fixed-width integer or `Unit`")]
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
        Ok(ForeignImport {
            target,
            signature: self.signature(classifier, ForeignDirection::Import)?,
        })
    }

    /// C entry has its own conversion plan: a borrowed outgoing window cannot be reconstructed.
    pub fn validate_export(
        &self, classifier: ss::TypeId,
    ) -> Result<ForeignSignature, ForeignClassifierError> {
        self.signature(classifier, ForeignDirection::Export)
    }

    fn signature(
        &self, classifier: ss::TypeId, direction: ForeignDirection,
    ) -> Result<ForeignSignature, ForeignClassifierError> {
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
                | Some(PrimitiveType::Integer(integer)) => ForeignParameter::Integer(integer),
                | _ if matches!(direction, ForeignDirection::Import)
                    && self.memory_window(parameter) =>
                {
                    ForeignParameter::BorrowedMemory
                }
                | _ if matches!(direction, ForeignDirection::Export) => {
                    return Err(ForeignClassifierError::UnsupportedExportParameter {
                        index: parameters.len() + 1,
                        classifier: parameter,
                    });
                }
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
        let representation = match self.type_view(result) {
            | Some(ss::Type::Primitive(ss::PrimitiveTy(PrimitiveType::Integer(integer)))) => {
                ForeignResult::Integer(integer)
            }
            | Some(ss::Type::Unit(_)) => ForeignResult::Unit,
            | _ => return Err(ForeignClassifierError::UnsupportedResult { classifier: result }),
        };
        Ok(ForeignSignature::new(parameters, representation)?)
    }

    fn memory_window(&self, ty: ss::TypeId) -> bool {
        let Some(ss::Type::Prod(Prod(fields))) = self.type_view(ty) else {
            return false;
        };
        let [access, address, length] = fields.as_slice() else {
            return false;
        };
        self.capability(*access, BuiltinTypeRole::Access)
            && self.capability(*address, BuiltinTypeRole::Addr)
            && self.primitive(*length) == Some(PrimitiveType::Integer(IntegerType::Int64))
    }

    fn capability(&self, ty: ss::TypeId, role: BuiltinTypeRole) -> bool {
        let Some(ss::Type::Abst(witness)) = self.type_view(ty) else {
            return false;
        };
        self.statics.builtin_roles.witness(witness) == Some(BuiltinRole::Type(role))
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

    pub(crate) fn type_view(&self, ty: ss::TypeId) -> Option<ss::Type> {
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

#[derive(Copy, Clone)]
enum ForeignDirection {
    Import,
    Export,
}
