//! Error messages in the type checker.
//! Shows the error message, where to look at in the source code, and the stack trace.

use crate::validate::CoverageError;
use crate::{syntax::*, *};
use ariadne::{Label, Report, ReportKind};
use std::ops::Range;
use zydeco_utils::span::PathDisplay;

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
    PackageWitnessesUnavailable { package: ValueId },
    PackageWitnessArityMismatch { expected: usize, found: usize },
    EscapingExistential { witnesses: Vec<AbstId>, result: TypeId },
    InvalidBuiltinAttachment { role: BuiltinRole, expected: &'static str },
    InvalidBuiltinSignature(BuiltinSignatureError),
    ConflictingBuiltinRole { existing: BuiltinRole, found: BuiltinRole },
    MissingBuiltinTypeRole { role: BuiltinTypeRole },
    AmbiguousBuiltinTypeRole { role: BuiltinTypeRole, witnesses: Vec<AbstId> },
    IntegerLiteralOutOfRange { value: i128, integer_type: IntegerType },
    FloatLiteralOutOfRange { value: f64, float_type: FloatType },
    Expressivity(&'static str),
    NotInlinable(DefId),
    NotInlinableSeal(AbstId),
}

#[derive(Clone)]
pub struct TyckErrorEntry {
    pub(crate) error: TyckError,
    pub(crate) blame: &'static std::panic::Location<'static>,
    pub(crate) stack: im::Vector<TyckTask>,
    // Todo: dump related arena entries if needed
}

impl<'a> Tycker<'a> {
    fn statics_term_ariadne_span(&self, term: TermId) -> Option<(PathDisplay, Range<usize>)> {
        self.statics.terms.source(&term).map(|term| term.span(self).to_ariadne_span())
    }

    fn statics_pat_ariadne_span(&self, pat: PatId) -> Option<(PathDisplay, Range<usize>)> {
        self.statics.pats.source(&pat).map(|pat| pat.span(self).to_ariadne_span())
    }

    fn type_ariadne_span(&self, ty: &TypeId) -> Option<(PathDisplay, Range<usize>)> {
        self.statics_term_ariadne_span((*ty).into())
    }

    fn inference_site_ariadne_span(&self, site: InferenceSite) -> (PathDisplay, Range<usize>) {
        match site {
            | InferenceSite::Term(term) => term.span(self).to_ariadne_span(),
            | InferenceSite::Pattern(pattern) => pattern.span(self).to_ariadne_span(),
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

    /// Get the primary span for an error (where the error actually occurred).
    pub(super) fn error_primary_span(
        &self, error: &TyckError,
    ) -> Option<(PathDisplay, Range<usize>)> {
        match error {
            | TyckError::TypeMismatch { expected: _, found } => {
                // Use the found type's span as primary
                self.type_ariadne_span(found)
            }
            | TyckError::TypeExpected { found, .. } => self.type_ariadne_span(found),
            | TyckError::MissingNamedField { found, .. }
            | TyckError::DuplicateNamedField { found, .. } => self.type_ariadne_span(found),
            | TyckError::MissingStructure(ty) => self.type_ariadne_span(ty),
            | TyckError::PackageWitnessesUnavailable { package } => {
                self.statics_term_ariadne_span((*package).into())
            }
            | TyckError::EscapingExistential { result, .. } => self.type_ariadne_span(result),
            | TyckError::MissingSolution(fills) | TyckError::UnconstrainedInference(fills) => {
                fills.first().map(|fill| self.inference_site_ariadne_span(self.statics.fills[fill]))
            }
            | TyckError::OccursCheck(fill) => {
                Some(self.inference_site_ariadne_span(self.statics.fills[fill]))
            }
            | TyckError::NotInlinable(def) => Some(def.span(self).to_ariadne_span()),
            | TyckError::NotInlinableSeal(abst) => {
                // AbstId doesn't have a direct span, but we can get it from the hint if available
                use zydeco_utils::arena::ArenaAccess;
                self.statics.abst_hints.get(abst).map(|hint| hint.span(self).to_ariadne_span())
            }
            | TyckError::Coverage(error) => {
                self.statics_term_ariadne_span(error.computation().into())
            }
            | _ => None,
        }
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

    /// Create an Ariadne report for this error entry.
    pub fn error_entry_report(
        &self, TyckErrorEntry { error, blame, stack }: TyckErrorEntry,
    ) -> Report<'static, (PathDisplay, Range<usize>)> {
        use ariadne::ColorGenerator;
        let mut colors = ColorGenerator::new();
        let primary_color = colors.next();

        // Determine primary span (where the error occurred)
        let primary_span = self
            .error_primary_span(&error)
            .or_else(|| {
                // If no primary span in error, try to get from the last stack frame
                stack.last().and_then(|task| match task {
                    | TyckTask::Pat(pat, _) => Some(pat.span(self).to_ariadne_span()),
                    | TyckTask::Term(term, _) => Some(term.span(self).to_ariadne_span()),
                    | _ => None,
                })
            })
            .unwrap_or_else(|| (PathDisplay::from(std::path::PathBuf::from("<internal>")), 0..0));

        let error_msg = self.error_message(&error);
        let mut report =
            Report::build(ReportKind::Error, primary_span.clone()).with_message(&error_msg);

        // Add labels for the error itself if we have specific error spans
        match &error {
            | TyckError::TypeMismatch { expected, found } => {
                let expected_span = self.type_ariadne_span(expected);
                let found_span = self.type_ariadne_span(found);
                if let Some(found_span) = found_span {
                    report = report.with_label(
                        Label::new(found_span)
                            .with_message("found this type")
                            .with_color(primary_color),
                    );
                }
                if let Some(expected_span) = expected_span {
                    report = report.with_label(
                        Label::new(expected_span)
                            .with_message("expected this type")
                            .with_color(colors.next()),
                    );
                }
            }
            | TyckError::MissingSolution(fills) | TyckError::UnconstrainedInference(fills) => {
                let message = if matches!(error, TyckError::MissingSolution(_)) {
                    "hole needs a solution"
                } else {
                    "pattern type remains unconstrained"
                };
                for fill in fills.iter() {
                    let site = self.statics.fills[fill];
                    let site_span = self.inference_site_ariadne_span(site);
                    report = report.with_label(
                        Label::new(site_span).with_message(message).with_color(primary_color),
                    );
                }
            }
            | TyckError::OccursCheck(fill) => {
                let site_span = self.inference_site_ariadne_span(self.statics.fills[fill]);
                report = report.with_label(
                    Label::new(site_span)
                        .with_message("inference variable introduced here")
                        .with_color(primary_color),
                );
            }
            | TyckError::NotInlinableSeal(abst) => {
                use zydeco_utils::arena::ArenaAccess;
                if let Some(hint) = self.statics.abst_hints.get(abst) {
                    let hint_span = hint.span(self).to_ariadne_span();
                    report = report.with_label(
                        Label::new(hint_span)
                            .with_message("defined here")
                            .with_color(colors.next()),
                    );
                }
            }
            | _ => {
                // Add a label for the primary span if we have one
                if primary_span.0.as_path() != &std::path::PathBuf::from("<internal>") {
                    report = report.with_label(
                        Label::new(primary_span.clone())
                            .with_message("error occurred here")
                            .with_color(primary_color),
                    );
                }
            }
        }

        // Add stack trace as labels (context)
        for task in stack.iter().rev() {
            let task_label = match task {
                | TyckTask::Pat(pat, _switch) => {
                    let span = pat.span(self).to_ariadne_span();
                    Some(Label::new(span).with_message("when tycking pattern"))
                }
                | TyckTask::Term(term, _switch) => {
                    let span = term.span(self).to_ariadne_span();
                    Some(Label::new(span).with_message("when tycking term"))
                }
                | TyckTask::Lub(lhs, _rhs) => {
                    // AnnId can be Set, Kind, or Type - extract span if possible
                    match lhs {
                        | AnnId::Set => None,
                        | AnnId::Kind(kd) => self.statics_term_ariadne_span((*kd).into()),
                        | AnnId::Type(ty) => self.type_ariadne_span(ty),
                    }
                    .map(|span| Label::new(span).with_message("when computing least upper bound"))
                }
                | TyckTask::SignatureGen(ann) => {
                    // AnnId can be Set, Kind, or Type - extract span if possible
                    match ann {
                        | AnnId::Set => None,
                        | AnnId::Kind(kd) => self.statics_term_ariadne_span((*kd).into()),
                        | AnnId::Type(ty) => self.type_ariadne_span(ty),
                    }
                    .map(|span| Label::new(span).with_message("when generating signature"))
                }
                | TyckTask::StructureGen(ann) => {
                    // AnnId can be Set, Kind, or Type - extract span if possible
                    match ann {
                        | AnnId::Set => None,
                        | AnnId::Kind(kd) => self.statics_term_ariadne_span((*kd).into()),
                        | AnnId::Type(ty) => self.type_ariadne_span(ty),
                    }
                    .map(|span| Label::new(span).with_message("when generating structure"))
                }
                | TyckTask::MonadicLiftPat(pat) => match pat {
                    | PatId::Kind(_) => None,
                    | PatId::Type(_) => self.statics_pat_ariadne_span(*pat).map(|span| {
                        Label::new(span)
                            .with_message("when performing monadic lift of type pattern")
                    }),
                    | PatId::Value(_) => self.statics_pat_ariadne_span(*pat).map(|span| {
                        Label::new(span)
                            .with_message("when performing monadic lift of value pattern")
                    }),
                },
                | TyckTask::MonadicLiftTerm(term) => match term {
                    | TermId::Kind(_) => None,
                    | TermId::Type(_) => self.statics_term_ariadne_span(*term).map(|span| {
                        Label::new(span).with_message("when performing monadic lift of type")
                    }),
                    | TermId::Value(_) => self.statics_term_ariadne_span(*term).map(|span| {
                        Label::new(span).with_message("when performing monadic lift of value")
                    }),
                    | TermId::Compu(_) => self.statics_term_ariadne_span(*term).map(|span| {
                        Label::new(span).with_message("when performing monadic lift of computation")
                    }),
                },
            };

            if let Some(mut label) = task_label {
                label = label.with_color(colors.next());
                report = report.with_label(label);
            }
        }

        // Add note about blame location for debugging
        report = report.with_note(format!("Error location: {}", blame));

        report.finish()
    }
}

pub type Result<T> = std::result::Result<T, Box<TyckErrorEntry>>;
