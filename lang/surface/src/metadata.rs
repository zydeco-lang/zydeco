use std::{collections::HashSet, convert::Infallible, sync::LazyLock};
use strum::VariantArray as _;
use thiserror::Error;
use zydeco_syntax::{
    BuiltinRole, ForeignAbi, ForeignLibraryName, ForeignSymbolName, ForeignTarget, IntrinsicRole,
    Meta, SpecializeMeta,
};

use crate::textual::fmt::{IndentWidth, LayoutIntentions, Parentheses};

/// A compiler-recognized metadata annotation.
///
/// This enum is the closed catalog used by both metadata decoders and editor
/// tooling. The structural [`Meta`] syntax remains open: an unlisted name is
/// still valid metadata, but it has no compiler-defined completion contract.
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum MetadataKind {
    Doc,
    Import,
    Literal,
    Intrinsic,
    Builtin,
    Ffi,
    Monadic,
    TypeOf,
    Format,
    Debug,
}

impl MetadataKind {
    pub fn name(self) -> &'static str {
        self.into()
    }

    pub fn definition(self) -> &'static MetadataDefinition {
        MetadataCatalog::definition(self.name())
            .expect("every metadata kind has one canonical definition")
    }

    fn build_definition(self) -> MetadataDefinition {
        let description = match self {
            | Self::Doc => "Attach documentation metadata to an expression.",
            | Self::Import => "Import a source file or numbered interactive input.",
            | Self::Literal => "Splice an attached text block as a string literal.",
            | Self::Intrinsic => "Splice a compiler-defined CBPV intrinsic.",
            | Self::Builtin => "Assign a compiler-defined Builtin package role.",
            | Self::Ffi => "Declare a native foreign-function target.",
            | Self::Monadic => "Translate an expression using the lexical monadic basis.",
            | Self::TypeOf => "Extract an expression's type or kind without running it.",
            | Self::Format => "Override source formatting policy for an expression.",
            | Self::Debug => "Record a checked term as a compiler observation.",
        };
        let arguments = match self {
            | Self::Doc | Self::Debug => MetadataArguments::Arbitrary { label: "value" },
            | Self::Import => MetadataArguments::Positional(vec![MetadataParameter::new(
                "source",
                MetadataValue::Source,
            )]),
            | Self::Literal | Self::Monadic | Self::TypeOf => MetadataArguments::None,
            | Self::Intrinsic => MetadataArguments::Positional(vec![MetadataParameter::new(
                "role",
                MetadataValue::Identifier(
                    IntrinsicRole::all().map(|role| role.source_name().to_owned()).collect(),
                ),
            )]),
            | Self::Builtin => MetadataArguments::Positional(vec![MetadataParameter::new(
                "role",
                MetadataValue::Identifier(
                    BuiltinRole::all().map(BuiltinRole::source_name).collect(),
                ),
            )]),
            | Self::Ffi => MetadataArguments::Positional(
                std::iter::once(MetadataParameter::new(
                    "abi",
                    MetadataValue::Identifier(
                        ForeignAbi::VARIANTS
                            .iter()
                            .copied()
                            .map(|abi| abi.source_name().to_owned())
                            .collect(),
                    ),
                ))
                .chain(FfiComponent::VARIANTS.iter().copied().map(|component| {
                    MetadataParameter::new(
                        component.name(),
                        MetadataValue::Call(Box::new(component.definition())),
                    )
                }))
                .collect(),
            ),
            | Self::Format => MetadataArguments::Options(
                FormatOption::VARIANTS.iter().copied().map(FormatOption::definition).collect(),
            ),
        };
        MetadataDefinition::new(self.name(), description, arguments)
    }
}

static METADATA_DEFINITIONS: LazyLock<Vec<MetadataDefinition>> = LazyLock::new(|| {
    MetadataKind::VARIANTS.iter().copied().map(MetadataKind::build_definition).collect()
});

/// Namespace for the canonical compiler metadata catalog.
pub struct MetadataCatalog;

impl MetadataCatalog {
    pub fn definitions() -> &'static [MetadataDefinition] {
        METADATA_DEFINITIONS.as_slice()
    }

    pub fn definition(name: &str) -> Option<&'static MetadataDefinition> {
        Self::definitions().iter().find(|definition| definition.name == name)
    }
}

/// One completion-visible metadata call and its argument contract.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MetadataDefinition {
    name: &'static str,
    description: &'static str,
    arguments: MetadataArguments,
}

impl MetadataDefinition {
    fn new(name: &'static str, description: &'static str, arguments: MetadataArguments) -> Self {
        Self { name, description, arguments }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn description(&self) -> &'static str {
        self.description
    }

    pub fn arguments(&self) -> &MetadataArguments {
        &self.arguments
    }

    /// Validate metadata arguments against the same tree exposed to tooling.
    pub fn validate_arguments(&self, arguments: &[Meta]) -> Result<(), MetadataValidationError> {
        match &self.arguments {
            | MetadataArguments::None => self.validate_arity(arguments, 0),
            | MetadataArguments::Arbitrary { .. } => Ok(()),
            | MetadataArguments::Positional(parameters) => {
                self.validate_arity(arguments, parameters.len())?;
                parameters
                    .iter()
                    .zip(arguments)
                    .try_for_each(|(parameter, argument)| parameter.validate(self.name, argument))
            }
            | MetadataArguments::Options(options) => {
                let mut seen = HashSet::new();
                arguments.iter().try_for_each(|argument| {
                    let Some(name) = argument.callee() else {
                        return Err(MetadataValidationError::ExpectedOption {
                            definition: self.name,
                            found: argument.clone(),
                        });
                    };
                    let Some(option) = options.iter().find(|option| option.name == name) else {
                        return Err(MetadataValidationError::UnknownOption {
                            definition: self.name,
                            found: name.to_owned(),
                        });
                    };
                    if !seen.insert(option.name) {
                        return Err(MetadataValidationError::DuplicateOption {
                            definition: self.name,
                            option: option.name,
                        });
                    }
                    match argument {
                        | Meta::Apply { args, .. } => option.validate_arguments(args),
                        | Meta::Ident(_) if matches!(option.arguments, MetadataArguments::None) => {
                            Ok(())
                        }
                        | Meta::Ident(_) => Err(MetadataValidationError::ExpectedCall {
                            definition: self.name,
                            expected: option.name,
                            found: argument.clone(),
                        }),
                        | Meta::String(_) | Meta::Integer(_) => unreachable!(),
                    }
                })
            }
        }
    }

    fn validate_arity(
        &self, arguments: &[Meta], expected: usize,
    ) -> Result<(), MetadataValidationError> {
        (arguments.len() == expected).then_some(()).ok_or(MetadataValidationError::Arity {
            definition: self.name,
            expected,
            found: arguments.len(),
        })
    }
}

/// The argument family accepted by a metadata call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MetadataArguments {
    None,
    Arbitrary { label: &'static str },
    Positional(Vec<MetadataParameter>),
    Options(Vec<MetadataDefinition>),
}

/// One positional metadata argument.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MetadataParameter {
    label: &'static str,
    value: MetadataValue,
}

impl MetadataParameter {
    fn new(label: &'static str, value: MetadataValue) -> Self {
        Self { label, value }
    }

    pub fn label(&self) -> &'static str {
        self.label
    }

    pub fn value(&self) -> &MetadataValue {
        &self.value
    }

    fn validate(
        &self, definition: &'static str, argument: &Meta,
    ) -> Result<(), MetadataValidationError> {
        match &self.value {
            | MetadataValue::Identifier(choices) => match argument {
                | Meta::Ident(value) if choices.contains(value) => Ok(()),
                | Meta::Ident(value) => Err(MetadataValidationError::UnknownIdentifier {
                    definition,
                    parameter: self.label,
                    found: value.clone(),
                }),
                | _ => Err(MetadataValidationError::ExpectedIdentifier {
                    definition,
                    parameter: self.label,
                    found: argument.clone(),
                }),
            },
            | MetadataValue::String => match argument {
                | Meta::String(_) => Ok(()),
                | _ => Err(MetadataValidationError::ExpectedString {
                    definition,
                    parameter: self.label,
                    found: argument.clone(),
                }),
            },
            | MetadataValue::Integer => match argument {
                | Meta::Integer(_) => Ok(()),
                | _ => Err(MetadataValidationError::ExpectedInteger {
                    definition,
                    parameter: self.label,
                    found: argument.clone(),
                }),
            },
            | MetadataValue::Source => match argument {
                | Meta::String(path) if !path.is_empty() => Ok(()),
                | Meta::Integer(number) if *number > 0 => Ok(()),
                | _ => Err(MetadataValidationError::ExpectedSource {
                    definition,
                    parameter: self.label,
                    found: argument.clone(),
                }),
            },
            | MetadataValue::Call(expected) => match argument {
                | Meta::Apply { callee, args } if callee == expected.name => {
                    expected.validate_arguments(args)
                }
                | _ => Err(MetadataValidationError::ExpectedCall {
                    definition,
                    expected: expected.name,
                    found: argument.clone(),
                }),
            },
        }
    }
}

/// The value family accepted in one metadata argument position.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MetadataValue {
    Identifier(Vec<String>),
    String,
    Integer,
    /// A quoted source path or a positive numbered-input identity.
    Source,
    Call(Box<MetadataDefinition>),
}

/// A structural mismatch against a [`MetadataDefinition`].
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum MetadataValidationError {
    #[error("`{definition}` expects {expected} arguments, but found {found}")]
    Arity { definition: &'static str, expected: usize, found: usize },
    #[error("`{definition}` parameter `{parameter}` expects an identifier, but found `{found}`")]
    ExpectedIdentifier { definition: &'static str, parameter: &'static str, found: Meta },
    #[error("unknown identifier `{found}` for `{definition}` parameter `{parameter}`")]
    UnknownIdentifier { definition: &'static str, parameter: &'static str, found: String },
    #[error("`{definition}` parameter `{parameter}` expects a string, but found `{found}`")]
    ExpectedString { definition: &'static str, parameter: &'static str, found: Meta },
    #[error("`{definition}` parameter `{parameter}` expects an integer, but found `{found}`")]
    ExpectedInteger { definition: &'static str, parameter: &'static str, found: Meta },
    #[error("`{definition}` parameter `{parameter}` expects a source, but found `{found}`")]
    ExpectedSource { definition: &'static str, parameter: &'static str, found: Meta },
    #[error("`{definition}` expects `{expected}(...)`, but found `{found}`")]
    ExpectedCall { definition: &'static str, expected: &'static str, found: Meta },
    #[error("`{definition}` expects a named option, but found `{found}`")]
    ExpectedOption { definition: &'static str, found: Meta },
    #[error("unknown option `{found}` for `{definition}`")]
    UnknownOption { definition: &'static str, found: String },
    #[error("duplicate option `{option}` for `{definition}`")]
    DuplicateOption { definition: &'static str, option: &'static str },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, strum::IntoStaticStr, strum::VariantArray)]
#[strum(serialize_all = "lowercase")]
enum FfiComponent {
    Library,
    Symbol,
}

impl FfiComponent {
    fn name(self) -> &'static str {
        self.into()
    }

    fn definition(self) -> MetadataDefinition {
        MetadataDefinition::new(
            self.name(),
            match self {
                | Self::Library => "Select the platform linker library name.",
                | Self::Symbol => "Select the unmangled foreign symbol name.",
            },
            MetadataArguments::Positional(vec![MetadataParameter::new(
                self.name(),
                MetadataValue::String,
            )]),
        )
    }
}

#[derive(
    Copy, Clone, Debug, PartialEq, Eq, strum::EnumString, strum::IntoStaticStr, strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
enum FormatOption {
    Width,
    Indent,
    Layout,
    Parentheses,
    Verbatim,
}

impl FormatOption {
    fn name(self) -> &'static str {
        self.into()
    }

    fn from_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }

    fn definition(self) -> MetadataDefinition {
        let (description, arguments) = match self {
            | Self::Width => (
                "Set the maximum rendered line width.",
                MetadataArguments::Positional(vec![MetadataParameter::new(
                    "columns",
                    MetadataValue::Integer,
                )]),
            ),
            | Self::Indent => (
                "Set the indentation width.",
                MetadataArguments::Positional(vec![MetadataParameter::new(
                    "columns",
                    MetadataValue::Integer,
                )]),
            ),
            | Self::Layout => (
                "Choose which source line breaks the formatter preserves.",
                MetadataArguments::Positional(vec![MetadataParameter::new(
                    "policy",
                    MetadataValue::Identifier(
                        LayoutIntentions::VARIANTS
                            .iter()
                            .copied()
                            .map(|policy| policy.source_name().to_owned())
                            .collect(),
                    ),
                )]),
            ),
            | Self::Parentheses => (
                "Choose whether removable grouping parentheses are preserved.",
                MetadataArguments::Positional(vec![MetadataParameter::new(
                    "policy",
                    MetadataValue::Identifier(
                        Parentheses::VARIANTS
                            .iter()
                            .copied()
                            .map(|policy| policy.source_name().to_owned())
                            .collect(),
                    ),
                )]),
            ),
            | Self::Verbatim => (
                "Copy the annotated source region without reformatting it.",
                MetadataArguments::None,
            ),
        };
        MetadataDefinition::new(self.name(), description, arguments)
    }
}

/// The typed meaning of a `@[doc]` or `@[doc(...)]` annotation.
///
/// Arguments remain ordinary metadata values so documentation renderers can
/// define presentation policies without extending the surface parser.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DocMeta {
    pub arguments: Vec<Meta>,
}

impl SpecializeMeta for DocMeta {
    type Error = Infallible;

    fn name() -> &'static str {
        MetadataKind::Doc.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        Ok(Self { arguments: arguments.to_vec() })
    }
}

/// The typed meaning of a `@[monadic]` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MonadicMeta;

impl SpecializeMeta for MonadicMeta {
    type Error = MonadicMetaError;

    fn name() -> &'static str {
        MetadataKind::Monadic.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Monadic.definition().validate_arguments(arguments).map_err(|error| {
            match error {
                | MetadataValidationError::Arity { found, .. } => {
                    MonadicMetaError::Arguments { found }
                }
                | _ => unreachable!("monadic has only an empty argument contract"),
            }
        })?;
        Ok(Self)
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum MonadicMetaError {
    #[error("monadic does not accept arguments, but found {found}")]
    Arguments { found: usize },
}

/// Extract the classifier of an expression during type checking.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeOfMeta;

impl SpecializeMeta for TypeOfMeta {
    type Error = MetadataValidationError;

    fn name() -> &'static str {
        MetadataKind::TypeOf.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::TypeOf.definition().validate_arguments(arguments)?;
        Ok(Self)
    }
}

/// The typed meaning of a `@[literal]` annotation.
///
/// The annotation replaces its hole payload with the text of an attached
/// `--|` block, interpreted verbatim as a string literal.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiteralMeta;

impl SpecializeMeta for LiteralMeta {
    type Error = LiteralMetaError;

    fn name() -> &'static str {
        MetadataKind::Literal.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Literal.definition().validate_arguments(arguments).map_err(|error| {
            match error {
                | MetadataValidationError::Arity { found, .. } => {
                    LiteralMetaError::Arguments { found }
                }
                | _ => unreachable!("literal has only an empty argument contract"),
            }
        })?;
        Ok(Self)
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
    type Error = IntrinsicMetaError;

    fn name() -> &'static str {
        MetadataKind::Intrinsic.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Intrinsic
            .definition()
            .validate_arguments(arguments)
            .map_err(IntrinsicMetaError::from_validation)?;
        let [Meta::Ident(role)] = arguments else {
            unreachable!("the intrinsic metadata contract validates one role identifier")
        };
        Ok(Self {
            role: IntrinsicRole::from_source_name(role)
                .expect("the intrinsic metadata contract validates its role domain"),
        })
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

impl IntrinsicMetaError {
    fn from_validation(error: MetadataValidationError) -> Self {
        match error {
            | MetadataValidationError::Arity { found, .. } => Self::RoleArity { found },
            | MetadataValidationError::ExpectedIdentifier { .. } => Self::RoleNotIdentifier,
            | MetadataValidationError::UnknownIdentifier { found, .. } => Self::UnknownRole(found),
            | _ => unreachable!("intrinsic has one closed identifier argument"),
        }
    }
}

/// A decoded `builtin(role)` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BuiltinMeta {
    pub role: BuiltinRole,
}

impl SpecializeMeta for BuiltinMeta {
    type Error = BuiltinMetaError;

    fn name() -> &'static str {
        MetadataKind::Builtin.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Builtin
            .definition()
            .validate_arguments(arguments)
            .map_err(BuiltinMetaError::from_validation)?;
        let [Meta::Ident(role)] = arguments else {
            unreachable!("the builtin metadata contract validates one role identifier")
        };
        Ok(Self {
            role: BuiltinRole::from_source_name(role)
                .expect("the builtin metadata contract validates its role domain"),
        })
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

impl BuiltinMetaError {
    fn from_validation(error: MetadataValidationError) -> Self {
        match error {
            | MetadataValidationError::Arity { found, .. } => Self::RoleArity { found },
            | MetadataValidationError::ExpectedIdentifier { .. } => Self::RoleNotIdentifier,
            | MetadataValidationError::UnknownIdentifier { found, .. } => Self::UnknownRole(found),
            | _ => unreachable!("builtin has one closed identifier argument"),
        }
    }
}

/// A decoded `ffi(c, library("..."), symbol("..."))` annotation.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FfiMeta {
    pub target: ForeignTarget,
}

impl FfiMeta {
    fn string_option(argument: &Meta, expected: FfiComponent) -> String {
        let expected = expected.name();
        let Meta::Apply { callee, args } = argument else {
            unreachable!("the ffi metadata contract validates a nested call")
        };
        let [Meta::String(value)] = args.as_slice() else {
            unreachable!("the ffi metadata contract validates one string option")
        };
        debug_assert_eq!(callee, expected);
        value.clone()
    }
}

impl SpecializeMeta for FfiMeta {
    type Error = FfiMetaError;

    fn name() -> &'static str {
        MetadataKind::Ffi.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Ffi
            .definition()
            .validate_arguments(arguments)
            .map_err(FfiMetaError::from_validation)?;
        let [Meta::Ident(abi), library, symbol] = arguments else {
            unreachable!("the ffi metadata contract validates three typed arguments")
        };
        let abi = ForeignAbi::from_source_name(abi)
            .expect("the ffi metadata contract validates its ABI domain");
        let library = Self::string_option(library, FfiComponent::Library);
        let library = ForeignLibraryName::parse(library.clone())
            .ok_or(FfiMetaError::InvalidLibrary(library))?;
        let symbol = Self::string_option(symbol, FfiComponent::Symbol);
        let symbol =
            ForeignSymbolName::parse(symbol.clone()).ok_or(FfiMetaError::InvalidSymbol(symbol))?;
        Ok(Self { target: ForeignTarget { abi, library, symbol } })
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum FfiMetaError {
    #[error("ffi expects an ABI, library, and symbol, but found {found} arguments")]
    Arity { found: usize },
    #[error("ffi ABI must be an identifier")]
    AbiNotIdentifier,
    #[error("unknown ffi ABI `{0}`")]
    UnknownAbi(String),
    #[error("ffi expects `{option}(\"...\")` in this position")]
    ExpectedOption { option: &'static str },
    #[error("ffi option `{option}` expects one argument, but found {found}")]
    OptionArity { option: &'static str, found: usize },
    #[error("ffi option `{option}` expects a string")]
    OptionNotString { option: &'static str },
    #[error("invalid foreign library name `{0}`")]
    InvalidLibrary(String),
    #[error("invalid C symbol name `{0}`")]
    InvalidSymbol(String),
}

impl FfiMetaError {
    fn from_validation(error: MetadataValidationError) -> Self {
        match error {
            | MetadataValidationError::Arity { definition, found, .. }
                if definition == MetadataKind::Ffi.name() =>
            {
                Self::Arity { found }
            }
            | MetadataValidationError::Arity { definition, found, .. } => {
                Self::OptionArity { option: definition, found }
            }
            | MetadataValidationError::ExpectedIdentifier { .. } => Self::AbiNotIdentifier,
            | MetadataValidationError::UnknownIdentifier { found, .. } => Self::UnknownAbi(found),
            | MetadataValidationError::ExpectedCall { expected, .. } => {
                Self::ExpectedOption { option: expected }
            }
            | MetadataValidationError::ExpectedString { definition, .. } => {
                Self::OptionNotString { option: definition }
            }
            | _ => unreachable!("ffi has three positional arguments with closed shapes"),
        }
    }
}

/// A decoded `format(...)` directive controlling how the pretty printer
/// renders the annotated expression and everything inside it.
///
/// Each option is a nested call, as in
/// `@[format(width(100), indent(4), layout(blank_lines))]`;
/// the `verbatim` option may also be written without arguments.
/// A directive without options leaves the surrounding policy unchanged,
/// so options always compose with enclosing directives.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct FormatMeta {
    pub width: Option<usize>,
    pub indent: Option<IndentWidth>,
    pub layout: Option<LayoutIntentions>,
    pub parentheses: Option<Parentheses>,
    pub verbatim: bool,
}

impl SpecializeMeta for FormatMeta {
    type Error = FormatMetaError;

    fn name() -> &'static str {
        MetadataKind::Format.name()
    }

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        MetadataKind::Format
            .definition()
            .validate_arguments(arguments)
            .map_err(FormatMetaError::from_validation)?;
        let mut meta = Self::default();
        for argument in arguments {
            match argument {
                | Meta::Ident(_) => meta.verbatim = true,
                | Meta::Apply { callee, args } => {
                    let option = FormatOption::from_name(callee)
                        .expect("the format metadata contract validates option names");
                    if option == FormatOption::Verbatim {
                        meta.verbatim = true;
                        continue;
                    }
                    let [value] = args.as_slice() else {
                        unreachable!("the format metadata contract validates option arity")
                    };
                    match option {
                        | FormatOption::Width => {
                            let Meta::Integer(value) = value else {
                                unreachable!("the width metadata contract validates an integer")
                            };
                            let width = usize::try_from(*value)
                                .ok()
                                .filter(|width| *width > 0)
                                .ok_or(FormatMetaError::WidthOutOfRange(*value))?;
                            meta.width = Some(width);
                        }
                        | FormatOption::Indent => {
                            let Meta::Integer(value) = value else {
                                unreachable!("the indent metadata contract validates an integer")
                            };
                            let indent = usize::try_from(*value)
                                .ok()
                                .and_then(IndentWidth::new)
                                .ok_or(FormatMetaError::IndentOutOfRange(*value))?;
                            meta.indent = Some(indent);
                        }
                        | FormatOption::Layout => {
                            let Meta::Ident(value) = value else {
                                unreachable!("the layout metadata contract validates an identifier")
                            };
                            meta.layout = Some(LayoutIntentions::from_source_name(value).expect(
                                "the layout metadata contract validates its policy domain",
                            ));
                        }
                        | FormatOption::Parentheses => {
                            let Meta::Ident(value) = value else {
                                unreachable!(
                                    "the parentheses metadata contract validates an identifier"
                                )
                            };
                            meta.parentheses = Some(Parentheses::from_source_name(value).expect(
                                "the parentheses metadata contract validates its policy domain",
                            ));
                        }
                        | FormatOption::Verbatim => unreachable!(),
                    }
                }
                | Meta::String(_) | Meta::Integer(_) => {
                    unreachable!("the format metadata contract validates named options")
                }
            }
        }
        Ok(meta)
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum FormatMetaError {
    #[error(
        "format option must be a call such as `width(80)` or the bare option `verbatim`, but found `{0}`"
    )]
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

impl FormatMetaError {
    fn from_validation(error: MetadataValidationError) -> Self {
        match error {
            | MetadataValidationError::ExpectedOption { found, .. }
            | MetadataValidationError::ExpectedCall { found, .. } => Self::OptionNotCall(found),
            | MetadataValidationError::UnknownOption { found, .. } => Self::UnknownOption(found),
            | MetadataValidationError::DuplicateOption { option, .. } => {
                Self::DuplicateOption(option.to_owned())
            }
            | MetadataValidationError::Arity { definition, found, .. } => {
                Self::OptionArity { name: definition.to_owned(), found }
            }
            | MetadataValidationError::ExpectedInteger { definition, found, .. } => {
                Self::OptionNotInteger(definition.to_owned(), found)
            }
            | MetadataValidationError::ExpectedIdentifier { definition, found, .. } => {
                Self::OptionNotIdentifier(definition.to_owned(), found)
            }
            | MetadataValidationError::UnknownIdentifier { definition, found, .. } => {
                match FormatOption::from_name(definition) {
                    | Some(FormatOption::Layout) => Self::UnknownLayout(found),
                    | Some(FormatOption::Parentheses) => Self::UnknownParentheses(found),
                    | _ => {
                        unreachable!("only format policy options have closed identifier domains")
                    }
                }
            }
            | _ => unreachable!("format options have only catalog-defined structural errors"),
        }
    }
}

#[cfg(test)]
mod ffi_tests {
    use super::*;

    fn annotation(library: &str, symbol: &str) -> Vec<Meta> {
        vec![
            Meta::ident("c"),
            Meta::apply("library", [Meta::string(library)]),
            Meta::apply("symbol", [Meta::string(symbol)]),
        ]
    }

    #[test]
    fn ffi_metadata_decodes_a_valid_c_target() {
        let meta = FfiMeta::from_arguments(&annotation("xxhash", "XXH64")).unwrap();

        assert_eq!(meta.target.abi, ForeignAbi::C);
        assert_eq!(meta.target.library.as_str(), "xxhash");
        assert_eq!(meta.target.symbol.as_str(), "XXH64");
    }

    #[test]
    fn ffi_metadata_rejects_linker_and_assembly_injection() {
        assert!(matches!(
            FfiMeta::from_arguments(&annotation("xxhash\ninvalid", "XXH64")),
            Err(FfiMetaError::InvalidLibrary(_))
        ));
        assert!(matches!(
            FfiMeta::from_arguments(&annotation("xxhash", "XXH64; call injected")),
            Err(FfiMetaError::InvalidSymbol(_))
        ));
    }
}

#[cfg(test)]
mod catalog_tests {
    use super::*;
    use std::collections::BTreeSet;

    #[test]
    fn compiler_metadata_names_have_one_catalog_entry() {
        let definitions = MetadataCatalog::definitions();
        let names = definitions.iter().map(MetadataDefinition::name).collect::<BTreeSet<_>>();

        assert_eq!(definitions.len(), MetadataKind::VARIANTS.len());
        assert_eq!(names.len(), definitions.len());
        assert_eq!(DocMeta::name(), MetadataKind::Doc.name());
        assert_eq!(MonadicMeta::name(), MetadataKind::Monadic.name());
        assert_eq!(TypeOfMeta::name(), "typeof");
        assert_eq!(LiteralMeta::name(), MetadataKind::Literal.name());
        assert_eq!(IntrinsicMeta::name(), MetadataKind::Intrinsic.name());
        assert_eq!(BuiltinMeta::name(), MetadataKind::Builtin.name());
        assert_eq!(FfiMeta::name(), MetadataKind::Ffi.name());
        assert_eq!(FormatMeta::name(), MetadataKind::Format.name());
    }

    #[test]
    fn completion_domains_round_trip_through_their_typed_decoders() {
        IntrinsicRole::all().for_each(|role| {
            let source_name = role.source_name();
            assert_eq!(
                IntrinsicMeta::from_arguments(&[Meta::ident(source_name)]).unwrap().role,
                role,
            );
        });
        BuiltinRole::all().for_each(|role| {
            let source_name = role.source_name();
            assert_eq!(
                BuiltinMeta::from_arguments(&[Meta::ident(source_name)]).unwrap().role,
                role,
            );
        });
        LayoutIntentions::VARIANTS.iter().copied().for_each(|policy| {
            assert_eq!(LayoutIntentions::from_source_name(policy.source_name()), Some(policy));
        });
        Parentheses::VARIANTS.iter().copied().for_each(|policy| {
            assert_eq!(Parentheses::from_source_name(policy.source_name()), Some(policy));
        });
    }
}
