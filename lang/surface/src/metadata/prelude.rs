use super::*;
use crate::textual::{LexicalTokenKind, LexicalTokens};
use zydeco_syntax::VarName;

/// The fixed lexical vocabulary selected by a prelude annotation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PreludeProfile {
    Core,
}

impl PreludeProfile {
    /// Projection order is part of the abbreviation's contract.
    pub fn names(self) -> &'static [&'static str] {
        match self {
            | Self::Core => &[
                "VType", "CType", "Thk", "Ret", "OS", "Unit", "Int8", "Int16", "Int32", "Int64",
                "Int", "UInt8", "UInt16", "UInt32", "UInt64", "UInt", "Float32", "Float64", "Char",
                "String", "Bool", "Option", "Either",
            ],
        }
    }
}

/// A lexical prelude opening; it never acquires or constructs a provider.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreludeMeta {
    pub profile: PreludeProfile,
    pub source: VarName,
}

impl PreludeMeta {
    pub(super) fn arguments() -> MetadataArguments {
        MetadataArguments::Positional(vec![
            MetadataParameter::new("profile", MetadataValue::Identifier(vec!["core".into()])),
            MetadataParameter::new(
                "provider",
                MetadataValue::Call(Box::new(MetadataDefinition::new(
                    "from",
                    "Use a prelude view already bound in lexical scope.",
                    MetadataArguments::Positional(vec![MetadataParameter::new(
                        "view",
                        MetadataValue::Binding,
                    )]),
                ))),
            ),
        ])
    }

    pub(super) fn is_binding(source: &str) -> bool {
        let mut tokens = LexicalTokens::new(source);
        matches!(tokens.next(), Some(token)
            if matches!(token.kind, LexicalTokenKind::UpperIdentifier | LexicalTokenKind::LowerIdentifier)
                && token.range == (0..source.len()))
            && tokens.next().is_none()
    }
}

impl SpecializeMeta for PreludeMeta {
    type Error = MetadataValidationError;

    fn name() -> &'static str {
        MetadataKind::Prelude.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Prelude.definition().validate_arguments(arguments)?;
        let [Meta::Ident(_), Meta::Apply { args, .. }] = arguments else {
            unreachable!("the prelude catalog validates the profile and from call")
        };
        let [Meta::Ident(source)] = args.as_slice() else {
            unreachable!("the from catalog validates one lexical identifier")
        };
        Ok(Self { profile: PreludeProfile::Core, source: VarName(source.clone()) })
    }
}
