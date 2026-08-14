use std::convert::Infallible;
use thiserror::Error;
use zydeco_syntax::{BuiltinRole, IntrinsicRole, Meta, SpecializeMeta};

use crate::textual::fmt::{IndentWidth, LayoutIntentions, Parentheses};

/// The typed meaning of a `@[doc]` or `@[doc(...)]` annotation.
///
/// Arguments remain ordinary metadata values so documentation renderers can
/// define presentation policies without extending the surface parser.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DocMeta {
    pub arguments: Vec<Meta>,
}

impl SpecializeMeta for DocMeta {
    const NAME: &'static str = "doc";
    type Error = Infallible;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        Ok(Self { arguments: arguments.to_vec() })
    }
}

/// The typed meaning of a `@[monadic]` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MonadicMeta;

impl SpecializeMeta for MonadicMeta {
    const NAME: &'static str = "monadic";
    type Error = MonadicMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [] => Ok(Self),
            | arguments => Err(MonadicMetaError::Arguments { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum MonadicMetaError {
    #[error("monadic does not accept arguments, but found {found}")]
    Arguments { found: usize },
}

/// The typed meaning of a `@[literal]` annotation.
///
/// The annotation replaces its hole payload with the text of an attached
/// `--|` block, interpreted verbatim as a string literal.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiteralMeta;

impl SpecializeMeta for LiteralMeta {
    const NAME: &'static str = "literal";
    type Error = LiteralMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [] => Ok(Self),
            | arguments => Err(LiteralMetaError::Arguments { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum LiteralMetaError {
    #[error("literal does not accept arguments, but found {found}")]
    Arguments { found: usize },
}

/// A decoded `intrinsic(role)` splice annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntrinsicMeta {
    pub role: IntrinsicRole,
}

impl SpecializeMeta for IntrinsicMeta {
    const NAME: &'static str = "intrinsic";
    type Error = IntrinsicMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [Meta::Ident(role)] => IntrinsicRole::from_source_name(role)
                .map(|role| Self { role })
                .ok_or_else(|| IntrinsicMetaError::UnknownRole(role.clone())),
            | [_] => Err(IntrinsicMetaError::RoleNotIdentifier),
            | arguments => Err(IntrinsicMetaError::RoleArity { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum IntrinsicMetaError {
    #[error("intrinsic expects one role identifier, but found {found} arguments")]
    RoleArity { found: usize },
    #[error("intrinsic role must be an identifier")]
    RoleNotIdentifier,
    #[error("unknown intrinsic role `{0}`")]
    UnknownRole(String),
}

/// A decoded `builtin(role)` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BuiltinMeta {
    pub role: BuiltinRole,
}

impl SpecializeMeta for BuiltinMeta {
    const NAME: &'static str = "builtin";
    type Error = BuiltinMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [Meta::Ident(role)] => BuiltinRole::from_source_name(role)
                .map(|role| Self { role })
                .ok_or_else(|| BuiltinMetaError::UnknownRole(role.clone())),
            | [_] => Err(BuiltinMetaError::RoleNotIdentifier),
            | arguments => Err(BuiltinMetaError::RoleArity { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum BuiltinMetaError {
    #[error("builtin expects one role identifier, but found {found} arguments")]
    RoleArity { found: usize },
    #[error("builtin role must be an identifier")]
    RoleNotIdentifier,
    #[error("unknown builtin role `{0}`")]
    UnknownRole(String),
}

/// A decoded `format(...)` directive controlling how the pretty printer
/// renders the annotated expression and everything inside it.
///
/// Each option is a nested call, as in
/// `@[format(width(100), indent(4), layout(blank_lines))]`.
/// A directive without options leaves the surrounding policy unchanged,
/// so options always compose with enclosing directives.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct FormatMeta {
    pub width: Option<usize>,
    pub indent: Option<IndentWidth>,
    pub layout: Option<LayoutIntentions>,
    pub parentheses: Option<Parentheses>,
}

/// Guards a `format` option slot against a repeated spelling.
macro_rules! ensure_unique {
    ($slot:expr, $name:expr, $error:path) => {
        if $slot.is_some() {
            return Err($error($name.clone()));
        }
    };
}

impl SpecializeMeta for FormatMeta {
    const NAME: &'static str = "format";
    type Error = FormatMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        let mut meta = Self::default();
        for argument in arguments {
            let Meta::Apply { callee, args } = argument else {
                return Err(FormatMetaError::OptionNotCall(argument.clone()));
            };
            let [value] = args.as_slice() else {
                return Err(FormatMetaError::OptionArity {
                    name: callee.clone(),
                    found: args.len(),
                });
            };
            match callee.as_str() {
                | "width" => {
                    ensure_unique!(meta.width, callee, FormatMetaError::DuplicateOption);
                    let Meta::Integer(value) = value else {
                        return Err(FormatMetaError::OptionNotInteger(
                            callee.clone(),
                            value.clone(),
                        ));
                    };
                    let width = usize::try_from(*value)
                        .ok()
                        .filter(|width| *width > 0)
                        .ok_or(FormatMetaError::WidthOutOfRange(*value))?;
                    meta.width = Some(width);
                }
                | "indent" => {
                    ensure_unique!(meta.indent, callee, FormatMetaError::DuplicateOption);
                    let Meta::Integer(value) = value else {
                        return Err(FormatMetaError::OptionNotInteger(
                            callee.clone(),
                            value.clone(),
                        ));
                    };
                    let indent = usize::try_from(*value)
                        .ok()
                        .and_then(IndentWidth::new)
                        .ok_or(FormatMetaError::IndentOutOfRange(*value))?;
                    meta.indent = Some(indent);
                }
                | "layout" => {
                    ensure_unique!(meta.layout, callee, FormatMetaError::DuplicateOption);
                    let Meta::Ident(value) = value else {
                        return Err(FormatMetaError::OptionNotIdentifier(
                            callee.clone(),
                            value.clone(),
                        ));
                    };
                    meta.layout = Some(match value.as_str() {
                        | "preserve" => LayoutIntentions::Preserve,
                        | "blank_lines" => LayoutIntentions::BlankLinesOnly,
                        | "ignore" => LayoutIntentions::Ignore,
                        | _ => return Err(FormatMetaError::UnknownLayout(value.clone())),
                    });
                }
                | "parentheses" => {
                    ensure_unique!(meta.parentheses, callee, FormatMetaError::DuplicateOption);
                    let Meta::Ident(value) = value else {
                        return Err(FormatMetaError::OptionNotIdentifier(
                            callee.clone(),
                            value.clone(),
                        ));
                    };
                    meta.parentheses = Some(match value.as_str() {
                        | "minimal" => Parentheses::Minimal,
                        | "preserve" => Parentheses::Preserve,
                        | _ => return Err(FormatMetaError::UnknownParentheses(value.clone())),
                    });
                }
                | _ => return Err(FormatMetaError::UnknownOption(callee.clone())),
            }
        }
        Ok(meta)
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum FormatMetaError {
    #[error("format option must be a call such as `width(80)`, but found `{0}`")]
    OptionNotCall(Meta),
    #[error("`{name}` expects one argument, but found {found}")]
    OptionArity { name: String, found: usize },
    #[error("unknown format option `{0}`")]
    UnknownOption(String),
    #[error("duplicate format option `{0}`")]
    DuplicateOption(String),
    #[error("`{0}` expects an integer, but found `{1}`")]
    OptionNotInteger(String, Meta),
    #[error("`{0}` expects an identifier, but found `{1}`")]
    OptionNotIdentifier(String, Meta),
    #[error("line width must be a positive integer, but found {0}")]
    WidthOutOfRange(i64),
    #[error("indentation width must be a positive integer, but found {0}")]
    IndentOutOfRange(i64),
    #[error("unknown layout policy `{0}`; expected preserve, blank_lines, or ignore")]
    UnknownLayout(String),
    #[error("unknown parenthesis policy `{0}`; expected minimal or preserve")]
    UnknownParentheses(String),
}
