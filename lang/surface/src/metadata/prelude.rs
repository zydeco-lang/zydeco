use super::*;
use crate::textual::{LexicalTokenKind, LexicalTokens};
use zydeco_syntax::VarName;

/// The fixed vocabulary of the default prelude and explicit replacement views.
pub struct CorePrelude;

impl CorePrelude {
    /// Projection order is stable; replacement views additionally supply `OS`.
    pub fn names() -> &'static [&'static str] {
        &[
            "VType", "CType", "Thk", "Ret", "OS", "Unit", "Int8", "Int16", "Int32", "Int64", "Int",
            "UInt8", "UInt16", "UInt32", "UInt64", "UInt", "Float32", "Float64", "Char", "String",
            "Bool", "Option", "Either",
        ]
    }

    /// Closed names come from the standard package; `OS` comes from an explicit opening.
    pub fn closed_names() -> impl DoubleEndedIterator<Item = &'static str> {
        Self::names().iter().copied().filter(|name| *name != "OS")
    }
}

/// An exception to the implicit default, never an instruction to acquire a provider.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PreludeMeta {
    None,
    From(VarName),
}

impl PreludeMeta {
    pub(super) fn arguments() -> MetadataArguments {
        MetadataArguments::Choice(vec![
            MetadataDefinition::new(
                "none",
                "Disable implicit prelude names in this expression.",
                MetadataArguments::None,
            ),
            MetadataDefinition::new(
                "from",
                "Replace the prelude with a view already bound in lexical scope.",
                MetadataArguments::Positional(vec![MetadataParameter::new(
                    "view",
                    MetadataValue::Binding,
                )]),
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
        match &arguments[0] {
            | Meta::Ident(_) => Ok(Self::None),
            | Meta::Apply { callee, .. } if callee == "none" => Ok(Self::None),
            | Meta::Apply { args, .. } => {
                let [Meta::Ident(source)] = args.as_slice() else {
                    unreachable!("the from catalog validates one lexical identifier")
                };
                Ok(Self::From(VarName(source.clone())))
            }
            | _ => unreachable!("the prelude catalog validates an exclusive option"),
        }
    }
}
