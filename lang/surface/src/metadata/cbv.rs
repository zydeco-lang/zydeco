//! The `@[cbv]` annotation's typed meaning and its argument contract.
//!
//! The annotation carries no arguments; its payload is a call-by-value
//! expression that type checking elaborates into the CBPV core.

use super::{MetadataKind, MetadataValidationError};
use thiserror::Error;
use zydeco_syntax::{Meta, SpecializeMeta};

/// The typed meaning of a `@[cbv]` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CbvMeta;

impl SpecializeMeta for CbvMeta {
    type Error = CbvMetaError;

    fn name() -> &'static str {
        MetadataKind::Cbv.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Cbv.definition().validate_arguments(arguments).map_err(
            |error| match error {
                | MetadataValidationError::Arity { found, .. } => CbvMetaError::Arguments { found },
                | _ => unreachable!("cbv has only an empty argument contract"),
            },
        )?;
        Ok(Self)
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum CbvMetaError {
    #[error("cbv does not accept arguments, but found {found}")]
    Arguments { found: usize },
}
