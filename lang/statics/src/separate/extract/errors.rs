//! Relative conversion for coverage errors, diagnostics, and observations.

use super::body::BodyExtractor;
use super::*;

impl BodyExtractor<'_> {
    pub(super) fn rel_coverage(&self, err: &CoverageError) -> RelCoverageError {
        match err {
            | CoverageError::NonExhaustiveMatch { term, missing, truncated } => {
                RelCoverageError::NonExhaustiveMatch {
                    term: self.term_id(*term),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | CoverageError::NonExhaustiveCopatternMatch { computation, missing, truncated } => {
                RelCoverageError::NonExhaustiveCopatternMatch {
                    computation: self.compu(*computation),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | CoverageError::NonExhaustiveCoMatch { computation, missing } => {
                RelCoverageError::NonExhaustiveCoMatch {
                    computation: self.compu(*computation),
                    missing: missing.clone(),
                }
            }
            | CoverageError::DuplicateCoMatchArms { computation, duplicates } => {
                RelCoverageError::DuplicateCoMatchArms {
                    computation: self.compu(*computation),
                    duplicates: duplicates.clone(),
                }
            }
        }
    }

    pub(super) fn rel_observation(&self, obs: &crate::check::TyckObservation) -> RelObservation {
        match obs {
            | crate::check::TyckObservation::HoleSolution { site, solution } => {
                RelObservation::HoleSolution {
                    site: *site,
                    solution: solution.map(|a| self.ann(a)),
                }
            }
            | crate::check::TyckObservation::Debug { metadata, result } => RelObservation::Debug {
                metadata: metadata.clone(),
                result: self.rel_term_ann(*result),
            },
        }
    }

    pub(super) fn rel_error_entry(
        &self, entry: &crate::check::TyckErrorEntry, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> RelErrorEntry {
        let error = self.rel_error(&entry.error, def_cb);
        let stack = entry
            .stack
            .iter()
            .map(|task| match task {
                | TyckTask::Pat(p, sw) => RelTyckTask::Pat(*p, self.rel_switch(*sw)),
                | TyckTask::Term(t, sw) => RelTyckTask::Term(*t, self.rel_switch(*sw)),
                | TyckTask::Lub(a, b) => RelTyckTask::Lub(self.ann(*a), self.ann(*b)),
                | TyckTask::SignatureGen(a) => RelTyckTask::SignatureGen(self.ann(*a)),
                | TyckTask::StructureGen(a) => RelTyckTask::StructureGen(self.ann(*a)),
                | TyckTask::MonadicLiftPat(p) => RelTyckTask::MonadicLiftPat(self.pat_id(*p)),
                | TyckTask::MonadicLiftTerm(t) => RelTyckTask::MonadicLiftTerm(self.term_id(*t)),
            })
            .collect();
        RelErrorEntry { error, stack }
    }

    fn rel_switch(&self, sw: crate::check::Switch<AnnId>) -> crate::check::Switch<RelAnnId> {
        match sw {
            | crate::check::Switch::Syn => crate::check::Switch::Syn,
            | crate::check::Switch::Ana(a) => crate::check::Switch::Ana(self.ann(a)),
        }
    }

    fn rel_unit_classifier_error(&self, err: &UnitClassifierError) -> RelUnitClassifierError {
        match err {
            | UnitClassifierError::Initializer => RelUnitClassifierError::Initializer,
            | UnitClassifierError::Unsupported { classifier } => {
                RelUnitClassifierError::Unsupported { classifier: self.ty(*classifier) }
            }
            | UnitClassifierError::Interface(i) => RelUnitClassifierError::Interface(i.clone()),
        }
    }

    fn rel_static_elimination(&self, err: &StaticEliminationError) -> RelStaticEliminationError {
        match err {
            | StaticEliminationError::UnresolvedApplication { function } => {
                RelStaticEliminationError::UnresolvedApplication { function: self.value(*function) }
            }
            | StaticEliminationError::UnresolvedMatch { value } => {
                RelStaticEliminationError::UnresolvedMatch { value: self.value(*value) }
            }
            | StaticEliminationError::UnresolvedInteger { value } => {
                RelStaticEliminationError::UnresolvedInteger { value: self.value(*value) }
            }
            | StaticEliminationError::RuntimeValue { value, ty } => {
                RelStaticEliminationError::RuntimeValue {
                    value: self.value(*value),
                    ty: self.ty(*ty),
                }
            }
            | StaticEliminationError::RuntimeComputation { computation, ty } => {
                RelStaticEliminationError::RuntimeComputation {
                    computation: self.compu(*computation),
                    ty: self.ty(*ty),
                }
            }
            | StaticEliminationError::ReductionLimit { function } => {
                RelStaticEliminationError::ReductionLimit { function: self.value(*function) }
            }
        }
    }

    fn rel_builtin_signature(
        &self, sig: &crate::builtin::BuiltinSignatureError,
    ) -> RelBuiltinSignatureError {
        RelBuiltinSignatureError {
            duplicates: sig
                .duplicates
                .iter()
                .map(|d| match d {
                    | DuplicateBuiltinRole::Type { role, witnesses } => {
                        RelDuplicateBuiltinRole::Type {
                            role: *role,
                            witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                        }
                    }
                    | DuplicateBuiltinRole::Value { role, entries } => {
                        RelDuplicateBuiltinRole::Value {
                            role: *role,
                            entries: entries.iter().copied().map(|t| self.ty(t)).collect(),
                        }
                    }
                })
                .collect(),
            classifiers: sig
                .classifiers
                .iter()
                .map(|c| match c {
                    | BuiltinClassifierError::Mismatch { role, entry, expected } => {
                        RelBuiltinClassifierError::Mismatch {
                            role: *role,
                            entry: self.ty(*entry),
                            expected: expected.clone(),
                        }
                    }
                })
                .collect(),
        }
    }

    fn rel_foreign_classifier(&self, fc: &ForeignClassifierError) -> RelForeignClassifierError {
        match fc {
            | ForeignClassifierError::Abi => RelForeignClassifierError::Abi,
            | ForeignClassifierError::Unit(u) => {
                RelForeignClassifierError::Unit(self.rel_unit_classifier_error(u))
            }
            | ForeignClassifierError::UnsupportedExportParameter { index, classifier } => {
                RelForeignClassifierError::UnsupportedExportParameter {
                    index: *index,
                    classifier: self.ty(*classifier),
                }
            }
            | ForeignClassifierError::ExpectedThunk { classifier } => {
                RelForeignClassifierError::ExpectedThunk { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::UnsupportedParameter { index, classifier } => {
                RelForeignClassifierError::UnsupportedParameter {
                    index: *index,
                    classifier: self.ty(*classifier),
                }
            }
            | ForeignClassifierError::ExpectedReturn { classifier } => {
                RelForeignClassifierError::ExpectedReturn { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::UnsupportedResult { classifier } => {
                RelForeignClassifierError::UnsupportedResult { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::Signature(s) => {
                RelForeignClassifierError::Signature(s.clone())
            }
        }
    }

    fn rel_error(
        &self, err: &TyckError, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> RelTyckError {
        match err {
            | TyckError::MissingAnnotation => RelTyckError::MissingAnnotation,
            | TyckError::MissingSeal => RelTyckError::MissingSeal,
            | TyckError::InvalidBindingCycle(p) => RelTyckError::InvalidBindingCycle(*p),
            | TyckError::MissingSolution(fs) => {
                RelTyckError::MissingSolution(fs.iter().copied().map(|f| self.fill(f)).collect())
            }
            | TyckError::UnconstrainedInference(fs) => RelTyckError::UnconstrainedInference(
                fs.iter().copied().map(|f| self.fill(f)).collect(),
            ),
            | TyckError::OccursCheck(f) => RelTyckError::OccursCheck(self.fill(*f)),
            | TyckError::MissingStructure(t) => RelTyckError::MissingStructure(self.ty(*t)),
            | TyckError::SortMismatch => RelTyckError::SortMismatch,
            | TyckError::SignatureNotType => RelTyckError::SignatureNotType,
            | TyckError::TypeOfKind => RelTyckError::TypeOfKind,
            | TyckError::KindMismatch => RelTyckError::KindMismatch,
            | TyckError::TypeMismatch { expected, found } => {
                RelTyckError::TypeMismatch { expected: self.ty(*expected), found: self.ty(*found) }
            }
            | TyckError::TypeExpected { expected, found } => {
                RelTyckError::TypeExpected { expected: expected.clone(), found: self.ty(*found) }
            }
            | TyckError::NamedLabelMismatch { expected, found } => {
                RelTyckError::NamedLabelMismatch { expected: *expected, found: *found }
            }
            | TyckError::MissingNamedTypeField { field, found } => {
                RelTyckError::MissingNamedTypeField { field: *field, found: self.kind(*found) }
            }
            | TyckError::AmbiguousNamedTypeField { field, found } => {
                RelTyckError::AmbiguousNamedTypeField { field: *field, found: self.kind(*found) }
            }
            | TyckError::MissingNamedField { field, found } => {
                RelTyckError::MissingNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::DuplicateNamedField { field, found } => {
                RelTyckError::DuplicateNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::SealedNamedField { field, found } => {
                RelTyckError::SealedNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::StaticNamedField { field, found } => {
                RelTyckError::StaticNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::PatternAliasRequiresValue => RelTyckError::PatternAliasRequiresValue,
            | TyckError::RefutablePatternAlias => RelTyckError::RefutablePatternAlias,
            | TyckError::RefutableBinding(p) => RelTyckError::RefutableBinding(*p),
            | TyckError::RefutableFieldProjectionPattern => {
                RelTyckError::RefutableFieldProjectionPattern
            }
            | TyckError::UnknownDataConstructor(c) => RelTyckError::UnknownDataConstructor(*c),
            | TyckError::UnknownCoDataDestructor(d) => RelTyckError::UnknownCoDataDestructor(*d),
            | TyckError::CopatternStepMismatch { expected, found } => {
                RelTyckError::CopatternStepMismatch { expected: *expected, found: found.clone() }
            }
            | TyckError::OverlappingCopatternClauses => RelTyckError::OverlappingCopatternClauses,
            | TyckError::MultiplePackPiCopatternClauses => {
                RelTyckError::MultiplePackPiCopatternClauses
            }
            | TyckError::NonExhaustiveCopattern { expected } => {
                RelTyckError::NonExhaustiveCopattern { expected: self.ty(*expected) }
            }
            | TyckError::Coverage(c) => RelTyckError::Coverage(self.rel_coverage(c)),
            | TyckError::StaticElimination(e) => {
                RelTyckError::StaticElimination(self.rel_static_elimination(e))
            }
            | TyckError::PackageWitnessesUnavailable { package } => {
                RelTyckError::PackageWitnessesUnavailable { package: self.value(*package) }
            }
            | TyckError::PackageWitnessArityMismatch { expected, found } => {
                RelTyckError::PackageWitnessArityMismatch { expected: *expected, found: *found }
            }
            | TyckError::EscapingExistential { witnesses, result } => {
                RelTyckError::EscapingExistential {
                    witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                    result: self.ty(*result),
                }
            }
            | TyckError::EscapingDeclaration { declarations, result } => {
                RelTyckError::EscapingDeclaration {
                    declarations: declarations.iter().copied().map(|a| self.abst(a)).collect(),
                    result: self.ty(*result),
                }
            }
            | TyckError::InvalidBuiltinAttachment { role, expected } => {
                RelTyckError::InvalidBuiltinAttachment { role: *role, expected }
            }
            | TyckError::InvalidBuiltinSignature(sig) => {
                RelTyckError::InvalidBuiltinSignature(self.rel_builtin_signature(sig))
            }
            | TyckError::ConflictingBuiltinRole { existing, found } => {
                RelTyckError::ConflictingBuiltinRole { existing: *existing, found: *found }
            }
            | TyckError::InvalidForeignAttachment => RelTyckError::InvalidForeignAttachment,
            | TyckError::InvalidForeignClassifier(fc) => {
                RelTyckError::InvalidForeignClassifier(self.rel_foreign_classifier(fc))
            }
            | TyckError::ConflictingForeignImport { existing, found } => {
                RelTyckError::ConflictingForeignImport {
                    existing: existing.clone(),
                    found: found.clone(),
                }
            }
            | TyckError::MissingBuiltinTypeRole { role } => {
                RelTyckError::MissingBuiltinTypeRole { role: *role }
            }
            | TyckError::AmbiguousBuiltinTypeRole { role, witnesses } => {
                RelTyckError::AmbiguousBuiltinTypeRole {
                    role: *role,
                    witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                }
            }
            | TyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                RelTyckError::IntegerLiteralOutOfRange {
                    value: *value,
                    integer_type: *integer_type,
                }
            }
            | TyckError::FloatLiteralOutOfRange { value, float_type } => {
                RelTyckError::FloatLiteralOutOfRange { value: *value, float_type: *float_type }
            }
            | TyckError::Expressivity(s) => RelTyckError::Expressivity(s),
            | TyckError::NotInlinable(d) => RelTyckError::NotInlinable(def_cb(*d)),
            | TyckError::NotInlinableSeal(a) => RelTyckError::NotInlinableSeal(self.abst(*a)),
        }
    }
}
