//! Source-directed type-checking diagnostics and internal checker traces.

use crate::validate::{CoverageError, PackageError, ValueFunctionError};
use crate::*;
use zydeco_utils::span::Span;

pub use zydeco_utils::err::*;

/// The shape required at the next step of a generalized copattern clause.
#[derive(Debug, Clone, Copy)]
pub enum CopatternStepKind {
    Pattern,
    Destructor,
    Body,
}

impl std::fmt::Display for CopatternStepKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Pattern => write!(f, "an abstraction pattern"),
            | Self::Destructor => write!(f, "a destructor"),
            | Self::Body => write!(f, "the clause body"),
        }
    }
}

/// The source item found at one generalized copattern step.
#[derive(Debug, Clone)]
pub enum CopatternStep {
    Pattern,
    Destructor(DtorName),
    End,
}

impl std::fmt::Display for CopatternStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Pattern => write!(f, "an abstraction pattern"),
            | Self::Destructor(dtor) => write!(f, "destructor .{dtor}"),
            | Self::End => write!(f, "the end of the copattern"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyckError {
    MissingAnnotation,
    MissingSeal,
    MissingSolution(Vec<FillId>),
    UnconstrainedInference(Vec<FillId>),
    OccursCheck(FillId),
    MissingStructure(TypeId),
    SortMismatch,
    SignatureNotType,
    TypeOfKind,
    KindMismatch,
    TypeMismatch { expected: TypeId, found: TypeId },
    TypeExpected { expected: String, found: TypeId },
    NamedLabelMismatch { expected: FieldName, found: FieldName },
    MissingNamedTypeField { field: FieldName, found: KindId },
    AmbiguousNamedTypeField { field: FieldName, found: KindId },
    MissingNamedField { field: FieldName, found: TypeId },
    DuplicateNamedField { field: FieldName, found: TypeId },
    PatternAliasRequiresValue,
    RefutablePatternAlias,
    RefutableFieldProjectionPattern,
    UnknownDataConstructor(CtorName),
    UnknownCoDataDestructor(DtorName),
    CopatternStepMismatch { expected: CopatternStepKind, found: CopatternStep },
    OverlappingCopatternClauses,
    MultiplePackPiCopatternClauses,
    NonExhaustiveCopattern { expected: TypeId },
    Coverage(CoverageError),
    FirstClassValueFunction(ValueFunctionError),
    FirstClassPackage(PackageError),
    PackageWitnessesUnavailable { package: ValueId },
    PackageWitnessArityMismatch { expected: usize, found: usize },
    EscapingExistential { witnesses: Vec<AbstId>, result: TypeId },
    InvalidBuiltinAttachment { role: BuiltinRole, expected: &'static str },
    InvalidBuiltinSignature(BuiltinSignatureError),
    ConflictingBuiltinRole { existing: BuiltinRole, found: BuiltinRole },
    InvalidForeignAttachment,
    InvalidForeignClassifier(ForeignClassifierError),
    ConflictingForeignImport { existing: ForeignTarget, found: ForeignTarget },
    MissingBuiltinTypeRole { role: BuiltinTypeRole },
    AmbiguousBuiltinTypeRole { role: BuiltinTypeRole, witnesses: Vec<AbstId> },
    IntegerLiteralOutOfRange { value: i128, integer_type: IntegerType },
    FloatLiteralOutOfRange { value: f64, float_type: FloatType },
    Expressivity(&'static str),
    NotInlinable(DefId),
    NotInlinableSeal(AbstId),
}

/// Stable source-facing category of a type-checking diagnostic.
///
/// The checker keeps the payload-rich [`TyckError`] internally. Frontends use
/// this payload-free category as a diagnostic code.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TyckDiagnosticCode {
    MissingAnnotation,
    MissingSeal,
    MissingSolution,
    UnconstrainedInference,
    OccursCheck,
    MissingStructure,
    SortMismatch,
    SignatureNotType,
    TypeOfKind,
    KindMismatch,
    TypeMismatch,
    TypeExpected,
    NamedLabelMismatch,
    MissingNamedTypeField,
    AmbiguousNamedTypeField,
    MissingNamedField,
    DuplicateNamedField,
    PatternAliasRequiresValue,
    RefutablePatternAlias,
    RefutableFieldProjectionPattern,
    UnknownDataConstructor,
    UnknownCoDataDestructor,
    CopatternStepMismatch,
    OverlappingCopatternClauses,
    MultiplePackPiCopatternClauses,
    NonExhaustiveCopattern,
    Coverage,
    FirstClassValueFunction,
    FirstClassPackage,
    PackageWitnessesUnavailable,
    PackageWitnessArityMismatch,
    EscapingExistential,
    InvalidBuiltinAttachment,
    InvalidBuiltinSignature,
    ConflictingBuiltinRole,
    InvalidForeignAttachment,
    InvalidForeignClassifier,
    ConflictingForeignImport,
    MissingBuiltinTypeRole,
    AmbiguousBuiltinTypeRole,
    IntegerLiteralOutOfRange,
    FloatLiteralOutOfRange,
    Expressivity,
    NotInlinable,
    NotInlinableSeal,
}

impl TyckDiagnosticCode {
    pub fn as_str(self) -> &'static str {
        match self {
            | Self::MissingAnnotation => "tyck.missing-annotation",
            | Self::MissingSeal => "tyck.missing-seal",
            | Self::MissingSolution => "tyck.missing-solution",
            | Self::UnconstrainedInference => "tyck.unconstrained-inference",
            | Self::OccursCheck => "tyck.occurs-check",
            | Self::MissingStructure => "tyck.missing-structure",
            | Self::SortMismatch => "tyck.sort-mismatch",
            | Self::SignatureNotType => "tyck.signature-not-type",
            | Self::TypeOfKind => "tyck.typeof-kind",
            | Self::KindMismatch => "tyck.kind-mismatch",
            | Self::TypeMismatch => "tyck.type-mismatch",
            | Self::TypeExpected => "tyck.type-expected",
            | Self::NamedLabelMismatch => "tyck.named-label-mismatch",
            | Self::MissingNamedTypeField => "tyck.missing-named-type-field",
            | Self::AmbiguousNamedTypeField => "tyck.ambiguous-named-type-field",
            | Self::MissingNamedField => "tyck.missing-named-field",
            | Self::DuplicateNamedField => "tyck.duplicate-named-field",
            | Self::PatternAliasRequiresValue => "tyck.pattern-alias-requires-value",
            | Self::RefutablePatternAlias => "tyck.refutable-pattern-alias",
            | Self::RefutableFieldProjectionPattern => "tyck.refutable-field-projection-pattern",
            | Self::UnknownDataConstructor => "tyck.unknown-data-constructor",
            | Self::UnknownCoDataDestructor => "tyck.unknown-codata-destructor",
            | Self::CopatternStepMismatch => "tyck.copattern-step-mismatch",
            | Self::OverlappingCopatternClauses => "tyck.overlapping-copattern-clauses",
            | Self::MultiplePackPiCopatternClauses => "tyck.multiple-pack-pi-copattern-clauses",
            | Self::NonExhaustiveCopattern => "tyck.non-exhaustive-copattern",
            | Self::Coverage => "tyck.coverage",
            | Self::FirstClassValueFunction => "tyck.first-class-value-function",
            | Self::FirstClassPackage => "tyck.first-class-package",
            | Self::PackageWitnessesUnavailable => "tyck.package-witnesses-unavailable",
            | Self::PackageWitnessArityMismatch => "tyck.package-witness-arity-mismatch",
            | Self::EscapingExistential => "tyck.escaping-existential",
            | Self::InvalidBuiltinAttachment => "tyck.invalid-builtin-attachment",
            | Self::InvalidBuiltinSignature => "tyck.invalid-builtin-signature",
            | Self::ConflictingBuiltinRole => "tyck.conflicting-builtin-role",
            | Self::InvalidForeignAttachment => "tyck.invalid-foreign-attachment",
            | Self::InvalidForeignClassifier => "tyck.invalid-foreign-classifier",
            | Self::ConflictingForeignImport => "tyck.conflicting-foreign-import",
            | Self::MissingBuiltinTypeRole => "tyck.missing-builtin-type-role",
            | Self::AmbiguousBuiltinTypeRole => "tyck.ambiguous-builtin-type-role",
            | Self::IntegerLiteralOutOfRange => "tyck.integer-literal-out-of-range",
            | Self::FloatLiteralOutOfRange => "tyck.float-literal-out-of-range",
            | Self::Expressivity => "tyck.expressivity",
            | Self::NotInlinable => "tyck.not-inlinable",
            | Self::NotInlinableSeal => "tyck.not-inlinable-seal",
        }
    }
}

impl std::fmt::Display for TyckDiagnosticCode {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl From<&TyckError> for TyckDiagnosticCode {
    fn from(error: &TyckError) -> Self {
        match error {
            | TyckError::MissingAnnotation => Self::MissingAnnotation,
            | TyckError::MissingSeal => Self::MissingSeal,
            | TyckError::MissingSolution(_) => Self::MissingSolution,
            | TyckError::UnconstrainedInference(_) => Self::UnconstrainedInference,
            | TyckError::OccursCheck(_) => Self::OccursCheck,
            | TyckError::MissingStructure(_) => Self::MissingStructure,
            | TyckError::SortMismatch => Self::SortMismatch,
            | TyckError::SignatureNotType => Self::SignatureNotType,
            | TyckError::TypeOfKind => Self::TypeOfKind,
            | TyckError::KindMismatch => Self::KindMismatch,
            | TyckError::TypeMismatch { .. } => Self::TypeMismatch,
            | TyckError::TypeExpected { .. } => Self::TypeExpected,
            | TyckError::NamedLabelMismatch { .. } => Self::NamedLabelMismatch,
            | TyckError::MissingNamedTypeField { .. } => Self::MissingNamedTypeField,
            | TyckError::AmbiguousNamedTypeField { .. } => Self::AmbiguousNamedTypeField,
            | TyckError::MissingNamedField { .. } => Self::MissingNamedField,
            | TyckError::DuplicateNamedField { .. } => Self::DuplicateNamedField,
            | TyckError::PatternAliasRequiresValue => Self::PatternAliasRequiresValue,
            | TyckError::RefutablePatternAlias => Self::RefutablePatternAlias,
            | TyckError::RefutableFieldProjectionPattern => Self::RefutableFieldProjectionPattern,
            | TyckError::UnknownDataConstructor(_) => Self::UnknownDataConstructor,
            | TyckError::UnknownCoDataDestructor(_) => Self::UnknownCoDataDestructor,
            | TyckError::CopatternStepMismatch { .. } => Self::CopatternStepMismatch,
            | TyckError::OverlappingCopatternClauses => Self::OverlappingCopatternClauses,
            | TyckError::MultiplePackPiCopatternClauses => Self::MultiplePackPiCopatternClauses,
            | TyckError::NonExhaustiveCopattern { .. } => Self::NonExhaustiveCopattern,
            | TyckError::Coverage(_) => Self::Coverage,
            | TyckError::FirstClassValueFunction(_) => Self::FirstClassValueFunction,
            | TyckError::FirstClassPackage(_) => Self::FirstClassPackage,
            | TyckError::PackageWitnessesUnavailable { .. } => Self::PackageWitnessesUnavailable,
            | TyckError::PackageWitnessArityMismatch { .. } => Self::PackageWitnessArityMismatch,
            | TyckError::EscapingExistential { .. } => Self::EscapingExistential,
            | TyckError::InvalidBuiltinAttachment { .. } => Self::InvalidBuiltinAttachment,
            | TyckError::InvalidBuiltinSignature(_) => Self::InvalidBuiltinSignature,
            | TyckError::ConflictingBuiltinRole { .. } => Self::ConflictingBuiltinRole,
            | TyckError::InvalidForeignAttachment => Self::InvalidForeignAttachment,
            | TyckError::InvalidForeignClassifier(_) => Self::InvalidForeignClassifier,
            | TyckError::ConflictingForeignImport { .. } => Self::ConflictingForeignImport,
            | TyckError::MissingBuiltinTypeRole { .. } => Self::MissingBuiltinTypeRole,
            | TyckError::AmbiguousBuiltinTypeRole { .. } => Self::AmbiguousBuiltinTypeRole,
            | TyckError::IntegerLiteralOutOfRange { .. } => Self::IntegerLiteralOutOfRange,
            | TyckError::FloatLiteralOutOfRange { .. } => Self::FloatLiteralOutOfRange,
            | TyckError::Expressivity(_) => Self::Expressivity,
            | TyckError::NotInlinable(_) => Self::NotInlinable,
            | TyckError::NotInlinableSeal(_) => Self::NotInlinableSeal,
        }
    }
}

/// One source label carried independently of any frontend renderer.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TyckDiagnosticLabel {
    pub span: Span,
    pub message: String,
}

/// A presentation-neutral type-checking diagnostic.
///
/// The CLI and TUI render this with Ariadne, while the language server maps the
/// same spans to LSP ranges. Parent checker tasks are deliberately absent:
/// related labels represent semantic relationships rather than syntax nesting.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TyckDiagnostic {
    pub code: TyckDiagnosticCode,
    pub message: String,
    pub primary: Option<TyckDiagnosticLabel>,
    pub related: Vec<TyckDiagnosticLabel>,
    pub help: Vec<String>,
}

#[derive(Clone, Debug)]
enum MissingAnnotationSubject {
    Constructor(CtorName),
    EmptyMatch,
    CoMatch,
    PatternHole,
    TypeOfOperand,
    Unspecified,
}

#[derive(Clone)]
pub struct TyckErrorEntry {
    pub(crate) error: TyckError,
    pub(crate) blame: &'static std::panic::Location<'static>,
    pub(crate) stack: rpds::VectorSync<TyckTask>,
    // Todo: dump related arena entries if needed
}

impl<'a> Tycker<'a> {
    fn statics_term_source_span(&self, term: TermId) -> Option<Span> {
        self.statics.terms.source(&term).map(|term| *term.span(self))
    }

    fn inference_site_source_span(&self, site: InferenceSite) -> Span {
        match site {
            | InferenceSite::Term(term) => *term.span(self),
            | InferenceSite::Pattern(pattern) => *pattern.span(self),
        }
    }

    fn inference_site_output(&self, site: InferenceSite) -> String {
        match site {
            | InferenceSite::Term(term) => {
                format!("{} ({})", self.ugly_scoped(term), term.span(self))
            }
            | InferenceSite::Pattern(pattern) => {
                format!("{} ({})", self.ugly_scoped(pattern), pattern.span(self))
            }
        }
    }

    fn error_output(&'a self, error: TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => "Missing annotation".to_string(),
            | TyckError::MissingSeal => "Missing seal".to_string(),
            | TyckError::MissingSolution(fills) => {
                let mut s = String::new();
                s += "Missing solution for:";
                for fill in fills.iter() {
                    let site = self.statics.fills[fill];
                    s += &format!("\n\t>> {}", self.inference_site_output(site))
                }
                s
            }
            | TyckError::UnconstrainedInference(fills) => {
                let sites = fills
                    .iter()
                    .map(|fill| self.inference_site_output(self.statics.fills[fill]))
                    .collect::<Vec<_>>()
                    .join("\n\t>> ");
                format!("Cannot infer a complete type for:\n\t>> {sites}")
            }
            | TyckError::OccursCheck(fill) => {
                let site = self.inference_site_output(self.statics.fills[&fill]);
                format!("Occurs check failed for inference variable introduced at {site}")
            }
            | TyckError::MissingStructure(ty) => {
                format!("Missing structure for type: {}", self.pretty_statics_nested(ty, "\t"))
            }
            | TyckError::SortMismatch => "Sort mismatch".to_string(),
            | TyckError::SignatureNotType => "A `.zyi` signature root must be a type".to_string(),
            | TyckError::TypeOfKind => {
                "`typeof` cannot produce `Set`, the classifier of kinds".to_owned()
            }
            | TyckError::KindMismatch => "Kind mismatch".to_string(),
            | TyckError::TypeMismatch { expected, found } => {
                format!(
                    "Type mismatch: expected {}\n, found {}",
                    self.pretty_statics_nested(expected, "\t"),
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::TypeExpected { expected, found } => {
                format!(
                    "Type expected: {}, found {}",
                    expected,
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::NamedLabelMismatch { expected, found } => {
                format!("Named label mismatch: expected `{expected}`, found `{found}`")
            }
            | TyckError::MissingNamedTypeField { field, found } => {
                format!(
                    "Missing named type field `{field}` in {}",
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::AmbiguousNamedTypeField { field, found } => {
                format!(
                    "Ambiguous named type field `{field}` in {}",
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::MissingNamedField { field, found } => {
                format!(
                    "Missing named field `{field}` in {}",
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::DuplicateNamedField { field, found } => {
                format!(
                    "Ambiguous named field `{field}` in {}",
                    self.pretty_statics_nested(found, "\t")
                )
            }
            | TyckError::PatternAliasRequiresValue => {
                "Pattern aliasing currently requires a value pattern".to_string()
            }
            | TyckError::RefutablePatternAlias => {
                "Pattern alias members must currently be irrefutable".to_string()
            }
            | TyckError::RefutableFieldProjectionPattern => {
                "Field projection payload patterns must currently be irrefutable".to_string()
            }
            | TyckError::UnknownDataConstructor(ctor) => {
                format!("Unknown data constructor: +{ctor}")
            }
            | TyckError::UnknownCoDataDestructor(dtor) => {
                format!("Unknown codata destructor: .{dtor}")
            }
            | TyckError::CopatternStepMismatch { expected, found } => {
                format!("Copattern step mismatch: expected {expected}, found {found}")
            }
            | TyckError::OverlappingCopatternClauses => {
                "Overlapping copattern clauses have the same observation path".to_string()
            }
            | TyckError::MultiplePackPiCopatternClauses => {
                "Package-dependent arrows currently accept one copattern clause".to_string()
            }
            | TyckError::NonExhaustiveCopattern { expected } => format!(
                "Non-exhaustive copattern clauses for {}",
                self.pretty_statics_nested(expected, "\t")
            ),
            | TyckError::Coverage(error) => error.to_string(),
            | TyckError::FirstClassValueFunction(error) => match error {
                | ValueFunctionError::FirstClassValue { position, .. }
                | ValueFunctionError::FirstClassType { position, .. } => {
                    format!("Value functions are second-class and cannot be {position}")
                }
            },
            | TyckError::FirstClassPackage(error) => match error {
                | PackageError::FirstClassValue { position, .. }
                | PackageError::FirstClassType { position, .. } => {
                    format!("Packages are second-class and cannot be {position}")
                }
            },
            | TyckError::PackageWitnessesUnavailable { package } => {
                format!(
                    "Package-dependent application requires manifest existential witnesses, \
                     but they are hidden by {}",
                    self.pretty_statics_nested(package, "\t")
                )
            }
            | TyckError::PackageWitnessArityMismatch { expected, found } => {
                format!(
                    "Package witness arity mismatch: expected {expected} witness(es), \
                     found {found}"
                )
            }
            | TyckError::EscapingExistential { witnesses, result } => {
                let witnesses = witnesses
                    .into_iter()
                    .map(|witness| self.pretty_statics_nested(witness, "\t"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Existential witness escapes its scope through result type {}: {}",
                    self.pretty_statics_nested(result, "\t"),
                    witnesses
                )
            }
            | TyckError::InvalidBuiltinAttachment { role, expected } => {
                format!("Builtin role `{role}` must annotate {expected}")
            }
            | TyckError::InvalidBuiltinSignature(error) => error.to_string(),
            | TyckError::ConflictingBuiltinRole { existing, found } => {
                format!(
                    "Conflicting Builtin roles on one package entry: `{existing}` and `{found}`"
                )
            }
            | TyckError::InvalidForeignAttachment => {
                "An ffi annotation must implement a value classified by `Thk (...)`".to_string()
            }
            | TyckError::InvalidForeignClassifier(error) => error.to_string(),
            | TyckError::ConflictingForeignImport { existing, found } => format!(
                "Conflicting foreign implementations `{}` and `{}`",
                existing.symbol, found.symbol
            ),
            | TyckError::MissingBuiltinTypeRole { role } => {
                format!("Builtin type role `{}` is unavailable in this scope", role.source_name())
            }
            | TyckError::AmbiguousBuiltinTypeRole { role, witnesses } => {
                let witnesses = witnesses
                    .into_iter()
                    .map(|witness| self.pretty_statics_nested(witness, "\t"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Builtin type role `{}` is ambiguous in this scope: {witnesses}",
                    role.source_name()
                )
            }
            | TyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                format!("Integer literal {value} is outside the range of {integer_type}")
            }
            | TyckError::FloatLiteralOutOfRange { value, float_type } => {
                format!(
                    "Floating-point literal {value} is outside the finite range of {float_type}"
                )
            }
            | TyckError::Expressivity(s) => s.to_string(),
            | TyckError::NotInlinable(def) => {
                let span = def.span(self);
                format!(
                    "Cannot inline definition: {} ({})",
                    self.pretty_statics_nested(def, "\t"),
                    span
                )
            }
            | TyckError::NotInlinableSeal(abst) => {
                use zydeco_utils::arena::ArenaAccess;
                let hint_msg = match self.statics.abst_hints.get(&abst) {
                    | Some(hint) => {
                        format!(
                            ", defined by {} ({})",
                            self.pretty_statics_nested(hint, "\t"),
                            hint.span(self)
                        )
                    }
                    | None => "".to_string(),
                };
                format!(
                    "Cannot inline sealed abstract type: {}{}",
                    self.pretty_statics_nested(abst, "\t"),
                    hint_msg
                )
            }
        }
    }
    pub fn error_entry_output(
        &self, TyckErrorEntry { error, blame, stack }: TyckErrorEntry,
    ) -> String {
        // let budget = 80;
        let budget = usize::MAX;
        let truncated = |mut s: String| {
            if s.len() > budget {
                s.truncate(budget - 3);
                s.push_str("...");
            }
            s
        };

        let mut s = String::new();
        s += &format!("Blame: {}\n", blame);
        for task in stack.iter() {
            match task {
                | TyckTask::Pat(pat, switch) => {
                    s += &format!("\t- when tycking pattern ({}):\n", pat.span(self));
                    s += &format!("\t\t>> {}\n", truncated(self.ugly_scoped(pat)));
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            s += &format!(
                                "\t\t<< (ana) {}\n",
                                truncated(self.pretty_statics_nested(ann, "\t\t\t"))
                            );
                        }
                    }
                }
                | TyckTask::Term(term, switch) => {
                    s += &format!("\t- when tycking term ({}):\n", term.span(self));
                    s += &format!("\t\t>> {}\n", truncated(self.ugly_scoped(term)));
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            s += &format!(
                                "\t\t<< (ana) {}\n",
                                truncated(self.pretty_statics_nested(ann, "\t\t\t"))
                            );
                        }
                    }
                }
                | TyckTask::Lub(lhs, rhs) => {
                    s += "\t- when computing least upper bound:\n";
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(self.pretty_statics_nested(lhs, "\t\t\t"))
                    );
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(self.pretty_statics_nested(rhs, "\t\t\t"))
                    );
                }
                | TyckTask::SignatureGen(ann) => {
                    s += "\t- when generating signature:\n";
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(self.pretty_statics_nested(ann, "\t\t\t"))
                    );
                }
                | TyckTask::StructureGen(ann) => {
                    s += "\t- when generating structure:\n";
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(self.pretty_statics_nested(ann, "\t\t\t"))
                    );
                }
                | TyckTask::MonadicLiftPat(pat) => match pat {
                    | PatId::Kind(kind) => {
                        s += "\t- when performing monadic lift of kind pattern:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(kind, "\t\t\t"))
                        );
                    }
                    | PatId::Type(ty) => {
                        s += "\t- when performing monadic lift of type pattern:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(ty, "\t\t\t"))
                        );
                    }
                    | PatId::Value(value) => {
                        s += "\t- when performing monadic lift of value pattern:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(value, "\t\t\t"))
                        );
                    }
                },
                | TyckTask::MonadicLiftTerm(term) => match term {
                    | TermId::Kind(_) => unreachable!(),
                    | TermId::Type(ty) => {
                        s += "\t- when performing monadic lift of type:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(ty, "\t\t\t"))
                        );
                    }
                    | TermId::Value(value) => {
                        s += "\t- when performing monadic lift of value:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(value, "\t\t\t"))
                        );
                    }
                    | TermId::Compu(compu) => {
                        s += "\t- when performing monadic lift of computation:\n";
                        s += &format!(
                            "\t\t>> {}\n",
                            truncated(self.pretty_statics_nested(compu, "\t\t\t"))
                        );
                    }
                },
            }
        }
        s += &format!("Error: {}\n", self.error_output(error));
        s
    }

    /// Get the source span carried directly by an error payload.
    fn error_source_span(&self, error: &TyckError) -> Option<Span> {
        match error {
            | TyckError::PackageWitnessesUnavailable { package } => {
                self.statics_term_source_span((*package).into())
            }
            | TyckError::MissingSolution(fills) | TyckError::UnconstrainedInference(fills) => {
                fills.first().map(|fill| self.inference_site_source_span(self.statics.fills[fill]))
            }
            | TyckError::OccursCheck(fill) => {
                Some(self.inference_site_source_span(self.statics.fills[fill]))
            }
            | TyckError::NotInlinable(def) => Some(*def.span(self)),
            | TyckError::NotInlinableSeal(abst) => {
                use zydeco_utils::arena::ArenaAccess;
                self.statics.abst_hints.get(abst).map(|hint| *hint.span(self))
            }
            | TyckError::Coverage(error) => {
                self.statics_term_source_span(error.computation().into())
            }
            | TyckError::FirstClassValueFunction(error) => {
                self.statics_term_source_span(error.term())
            }
            | TyckError::FirstClassPackage(error) => self.statics_term_source_span(error.term()),
            | _ => None,
        }
    }

    /// The innermost source term or pattern active when an error was raised.
    fn task_source_span(&self, stack: &rpds::VectorSync<TyckTask>) -> Option<Span> {
        stack.iter().rev().find_map(|task| match task {
            | TyckTask::Pat(pattern, _) => Some(*pattern.span(self)),
            | TyckTask::Term(term, _) => Some(*term.span(self)),
            | TyckTask::Lub(_, _)
            | TyckTask::SignatureGen(_)
            | TyckTask::StructureGen(_)
            | TyckTask::MonadicLiftPat(_)
            | TyckTask::MonadicLiftTerm(_) => None,
        })
    }

    /// Get the error message text.
    pub(super) fn error_message(&self, error: &TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => "Missing annotation".to_string(),
            | TyckError::MissingSeal => "Missing seal".to_string(),
            | TyckError::MissingSolution(fills) => {
                format!("Missing solution for {} hole(s)", fills.len())
            }
            | TyckError::UnconstrainedInference(fills) => {
                format!("Cannot infer a complete type for {} pattern(s)", fills.len())
            }
            | TyckError::OccursCheck(_) => {
                "Occurs check failed: an inference variable contains itself".to_string()
            }
            | TyckError::MissingStructure(_) => "Missing structure for type".to_string(),
            | TyckError::SortMismatch => "Sort mismatch".to_string(),
            | TyckError::SignatureNotType => "A `.zyi` signature root must be a type".to_string(),
            | TyckError::TypeOfKind => {
                "`typeof` cannot produce `Set`, the classifier of kinds".to_owned()
            }
            | TyckError::KindMismatch => "Kind mismatch".to_string(),
            | TyckError::TypeMismatch { expected, found } => {
                format!(
                    "Type mismatch: expected {}, found {}",
                    self.pretty_statics_nested(*expected, ""),
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::TypeExpected { expected, found } => {
                format!(
                    "Type expected: {}, found {}",
                    expected,
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::NamedLabelMismatch { expected, found } => {
                format!("Named label mismatch: expected `{expected}`, found `{found}`")
            }
            | TyckError::MissingNamedTypeField { field, found } => {
                format!(
                    "Missing named type field `{field}` in {}",
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::AmbiguousNamedTypeField { field, found } => {
                format!(
                    "Ambiguous named type field `{field}` in {}",
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::MissingNamedField { field, found } => {
                format!(
                    "Missing named field `{field}` in {}",
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::DuplicateNamedField { field, found } => {
                format!(
                    "Ambiguous named field `{field}` in {}",
                    self.pretty_statics_nested(*found, "")
                )
            }
            | TyckError::PatternAliasRequiresValue => {
                "Pattern aliasing currently requires a value pattern".to_string()
            }
            | TyckError::RefutablePatternAlias => {
                "Pattern alias members must currently be irrefutable".to_string()
            }
            | TyckError::RefutableFieldProjectionPattern => {
                "Field projection payload patterns must currently be irrefutable".to_string()
            }
            | TyckError::UnknownDataConstructor(ctor) => {
                format!("Unknown data constructor `+{ctor}`")
            }
            | TyckError::UnknownCoDataDestructor(dtor) => {
                format!("Unknown codata destructor `.{dtor}`")
            }
            | TyckError::CopatternStepMismatch { expected, found } => {
                format!("Copattern step mismatch: expected {expected}, found {found}")
            }
            | TyckError::OverlappingCopatternClauses => {
                "Overlapping copattern clauses have the same observation path".to_string()
            }
            | TyckError::MultiplePackPiCopatternClauses => {
                "Package-dependent arrows currently accept one copattern clause".to_string()
            }
            | TyckError::NonExhaustiveCopattern { expected } => format!(
                "Non-exhaustive copattern clauses for {}",
                self.pretty_statics_nested(*expected, "")
            ),
            | TyckError::Coverage(error) => error.to_string(),
            | TyckError::FirstClassValueFunction(error) => match error {
                | ValueFunctionError::FirstClassValue { position, .. }
                | ValueFunctionError::FirstClassType { position, .. } => {
                    format!("Value functions are second-class and cannot be {position}")
                }
            },
            | TyckError::FirstClassPackage(error) => match error {
                | PackageError::FirstClassValue { position, .. }
                | PackageError::FirstClassType { position, .. } => {
                    format!("Packages are second-class and cannot be {position}")
                }
            },
            | TyckError::PackageWitnessesUnavailable { package } => format!(
                "Package-dependent application requires manifest existential witnesses, \
                 but they are hidden by {}",
                self.pretty_statics_nested(*package, "")
            ),
            | TyckError::PackageWitnessArityMismatch { expected, found } => {
                format!(
                    "Package witness arity mismatch: expected {expected} witness(es), found {found}"
                )
            }
            | TyckError::EscapingExistential { result, .. } => format!(
                "Existential witness escapes through result type {}",
                self.pretty_statics_nested(*result, "")
            ),
            | TyckError::InvalidBuiltinAttachment { role, expected } => {
                format!("Builtin role `{role}` must annotate {expected}")
            }
            | TyckError::InvalidBuiltinSignature(error) => error.to_string(),
            | TyckError::ConflictingBuiltinRole { existing, found } => format!(
                "Conflicting Builtin roles on one package entry: `{existing}` and `{found}`"
            ),
            | TyckError::InvalidForeignAttachment => {
                "An ffi annotation must implement a value classified by `Thk (...)`".to_string()
            }
            | TyckError::InvalidForeignClassifier(error) => error.to_string(),
            | TyckError::ConflictingForeignImport { existing, found } => format!(
                "Conflicting foreign implementations `{}` and `{}`",
                existing.symbol, found.symbol
            ),
            | TyckError::MissingBuiltinTypeRole { role } => {
                format!("Builtin type role `{}` is unavailable", role.source_name())
            }
            | TyckError::AmbiguousBuiltinTypeRole { role, .. } => {
                format!("Builtin type role `{}` is ambiguous", role.source_name())
            }
            | TyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                format!("Integer literal {value} is outside the range of {integer_type}")
            }
            | TyckError::FloatLiteralOutOfRange { value, float_type } => {
                format!(
                    "Floating-point literal {value} is outside the finite range of {float_type}"
                )
            }
            | TyckError::Expressivity(s) => s.to_string(),
            | TyckError::NotInlinable(_) => "Cannot inline definition".to_string(),
            | TyckError::NotInlinableSeal(_) => "Cannot inline sealed abstract type".to_string(),
        }
    }

    fn missing_annotation_subject(
        &self, stack: &rpds::VectorSync<TyckTask>,
    ) -> MissingAnnotationSubject {
        stack
            .iter()
            .rev()
            .find_map(|task| match task {
                | TyckTask::Term(term, _) => match &self.scoped.terms[term] {
                    | su::Term::Ctor(su::Ctor(name, _)) => {
                        Some(MissingAnnotationSubject::Constructor(name.clone()))
                    }
                    | su::Term::Match(su::Match { arms, .. }) if arms.is_empty() => {
                        Some(MissingAnnotationSubject::EmptyMatch)
                    }
                    | su::Term::CoMatchClauses(_) | su::Term::CoMatch(_) => {
                        Some(MissingAnnotationSubject::CoMatch)
                    }
                    | su::Term::TypeOf(_) => Some(MissingAnnotationSubject::TypeOfOperand),
                    | _ => None,
                },
                | TyckTask::Pat(pattern, _)
                    if matches!(self.scoped.pats[pattern], su::Pattern::Hole(_)) =>
                {
                    Some(MissingAnnotationSubject::PatternHole)
                }
                | TyckTask::Pat(_, _)
                | TyckTask::Lub(_, _)
                | TyckTask::SignatureGen(_)
                | TyckTask::StructureGen(_)
                | TyckTask::MonadicLiftPat(_)
                | TyckTask::MonadicLiftTerm(_) => None,
            })
            .unwrap_or(MissingAnnotationSubject::Unspecified)
    }

    fn contextual_error_message(
        &self, error: &TyckError, stack: &rpds::VectorSync<TyckTask>,
    ) -> String {
        match error {
            | TyckError::MissingAnnotation => match self.missing_annotation_subject(stack) {
                | MissingAnnotationSubject::Constructor(constructor) => {
                    format!("Cannot infer the data type of constructor `+{constructor}`")
                }
                | MissingAnnotationSubject::EmptyMatch => {
                    "Cannot infer the result type of an empty match".to_owned()
                }
                | MissingAnnotationSubject::CoMatch => {
                    "Cannot infer the type of this comatch".to_owned()
                }
                | MissingAnnotationSubject::PatternHole => {
                    "Cannot infer the type of this pattern hole".to_owned()
                }
                | MissingAnnotationSubject::TypeOfOperand => {
                    "Cannot infer the classifier of this `typeof` operand".to_owned()
                }
                | MissingAnnotationSubject::Unspecified => self.error_message(error),
            },
            | _ => self.error_message(error),
        }
    }

    fn primary_label_message(error: &TyckError) -> &'static str {
        match error {
            | TyckError::MissingAnnotation => "an annotation is required here",
            | TyckError::MissingSeal => "a seal is required here",
            | TyckError::MissingSolution(_) => "this hole needs a solution",
            | TyckError::UnconstrainedInference(_) => "this pattern type remains unconstrained",
            | TyckError::OccursCheck(_) => "the inference variable was introduced here",
            | TyckError::TypeMismatch { .. } => "this term has the mismatched type",
            | TyckError::TypeExpected { .. } => "this term has an incompatible type",
            | TyckError::SignatureNotType => "this signature root is not a type",
            | TyckError::TypeOfKind => "Set is not a source term",
            | TyckError::KindMismatch => "this type has the wrong kind",
            | TyckError::SortMismatch => "this term has the wrong sort",
            | TyckError::Coverage(_) => "this match is not exhaustive",
            | TyckError::FirstClassValueFunction(ValueFunctionError::FirstClassValue {
                ..
            }) => "this value function can only be applied",
            | TyckError::FirstClassValueFunction(ValueFunctionError::FirstClassType { .. }) => {
                "this classifier admits stored value functions"
            }
            | TyckError::FirstClassPackage(PackageError::FirstClassValue { .. }) => {
                "this package can only be opened, nested, or applied"
            }
            | TyckError::FirstClassPackage(PackageError::FirstClassType { .. }) => {
                "this classifier admits stored packages"
            }
            | _ => "error occurs here",
        }
    }

    fn diagnostic_help(
        &self, error: &TyckError, stack: &rpds::VectorSync<TyckTask>,
    ) -> Vec<String> {
        match error {
            | TyckError::MissingAnnotation => match self.missing_annotation_subject(stack) {
                | MissingAnnotationSubject::Constructor(_) => vec![
                    "add a type ascription to the constructor or otherwise provide an expected type"
                        .to_owned(),
                ],
                | MissingAnnotationSubject::EmptyMatch => {
                    vec!["add a result-type annotation to the empty match".to_owned()]
                }
                | MissingAnnotationSubject::CoMatch => {
                    vec!["add a computation-type annotation to the comatch".to_owned()]
                }
                | MissingAnnotationSubject::PatternHole => {
                    vec!["add a type annotation to the pattern".to_owned()]
                }
                | MissingAnnotationSubject::TypeOfOperand => {
                    vec![
                        "supply an expression or annotate the operand inside `@[typeof]`"
                            .to_owned(),
                    ]
                }
                | MissingAnnotationSubject::Unspecified => {
                    vec!["add an annotation that supplies the expected kind or type".to_owned()]
                }
            },
            | TyckError::MissingSeal => {
                vec!["seal the recursive type definition before it refers to itself".to_owned()]
            }
            | TyckError::UnconstrainedInference(_) => {
                vec!["add a type annotation to the unconstrained pattern".to_owned()]
            }
            | TyckError::SignatureNotType => {
                vec!["make the `.zyi` root evaluate to a type".to_owned()]
            }
            | TyckError::TypeOfKind => {
                vec!["apply `@[typeof]` to a value, computation, or type".to_owned()]
            }
            | TyckError::FirstClassValueFunction(_) => {
                vec![
                    "apply the function directly where it is used, or store the \
                     computation behind `Thk` instead"
                        .to_owned(),
                ]
            }
            | TyckError::FirstClassPackage(_) => {
                vec![
                    "open the package where it is used, or store its operations in a \
                     product of thunks instead"
                        .to_owned(),
                ]
            }
            | _ => Vec::new(),
        }
    }

    /// Lower one checker failure to a presentation-neutral source diagnostic.
    pub(crate) fn error_entry_diagnostic(
        &self, TyckErrorEntry { error, blame: _, stack }: TyckErrorEntry,
    ) -> TyckDiagnostic {
        let error_span = self.error_source_span(&error).filter(|span| !span.is_dummy());
        let task_span = self.task_source_span(&stack).filter(|span| !span.is_dummy());
        let primary_span = match error {
            // Inference, coverage, and occurrence-validation errors carry source
            // entities whose spans are more precise than their enclosing checking
            // task. Ordinary type ids may be interned or normalized, so their
            // representative source span is not a reliable blame site.
            | TyckError::MissingSolution(_)
            | TyckError::UnconstrainedInference(_)
            | TyckError::OccursCheck(_)
            | TyckError::Coverage(_)
            | TyckError::FirstClassValueFunction(_)
            | TyckError::FirstClassPackage(_) => error_span.or(task_span),
            | _ => task_span.or(error_span),
        };
        let primary = primary_span.map(|span| TyckDiagnosticLabel {
            span,
            message: Self::primary_label_message(&error).to_owned(),
        });
        let mut related = Vec::new();
        let mut add_related = |span: Option<Span>, message: &'static str| {
            let Some(span) = span.filter(|span| !span.is_dummy()) else { return };
            if primary.as_ref().is_some_and(|primary| primary.span == span)
                || related.iter().any(|label: &TyckDiagnosticLabel| label.span == span)
            {
                return;
            }
            related.push(TyckDiagnosticLabel { span, message: message.to_owned() });
        };
        match &error {
            | TyckError::MissingSolution(fills) | TyckError::UnconstrainedInference(fills) => {
                let message = if matches!(error, TyckError::MissingSolution(_)) {
                    "this hole also needs a solution"
                } else {
                    "this pattern type also remains unconstrained"
                };
                fills.iter().skip(1).for_each(|fill| {
                    add_related(
                        Some(self.inference_site_source_span(self.statics.fills[fill])),
                        message,
                    )
                });
            }
            | TyckError::NotInlinableSeal(abst) => {
                use zydeco_utils::arena::ArenaAccess;
                add_related(
                    self.statics.abst_hints.get(abst).map(|hint| *hint.span(self)),
                    "sealed type defined here",
                );
            }
            | _ => {}
        }
        TyckDiagnostic {
            code: (&error).into(),
            message: self.contextual_error_message(&error, &stack),
            primary,
            related,
            help: self.diagnostic_help(&error, &stack),
        }
    }
}

pub type Result<T> = std::result::Result<T, Box<TyckErrorEntry>>;
