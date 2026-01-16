//! Error messages in the type checker.
//! Shows the error message, where to look at in the source code, and the stack trace.

use super::{syntax::*, *};
use ariadne::{Label, Report, ReportKind};
use std::ops::Range;

pub use zydeco_utils::err::*;

#[derive(Debug, Clone)]
pub enum TyckError {
    MissingAnnotation,
    MissingSeal,
    MissingSolution(Vec<FillId>),
    MissingStructure(TypeId),
    SortMismatch,
    KindMismatch,
    TypeMismatch { expected: TypeId, found: TypeId },
    TypeExpected { expected: String, found: TypeId },
    MissingDataArm(CtorName),
    MissingCoDataArm(DtorName),
    NonExhaustiveCoDataArms(std::collections::HashMap<DtorName, TypeId>),
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
    fn error_output(&'a self, error: TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => format!("Missing annotation"),
            | TyckError::MissingSeal => format!("Missing seal"),
            | TyckError::MissingSolution(fills) => {
                let mut s = String::new();
                s += "Missing solution for:";
                for fill in fills.iter() {
                    let site = self.statics.fills[&fill];
                    s += &format!("\n\t>> {} ({})", self.ugly_scoped(site), site.span(self))
                }
                s
            }
            | TyckError::MissingStructure(ty) => {
                format!("Missing structure for type: {}", self.pretty_statics_nested(ty, "\t"))
            }
            | TyckError::SortMismatch => format!("Sort mismatch"),
            | TyckError::KindMismatch => format!("Kind mismatch"),
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
            | TyckError::MissingDataArm(ctor) => format!("Missing data arm: {:?}", ctor),
            | TyckError::MissingCoDataArm(dtor) => format!("Missing codata arm: {:?}", dtor),
            | TyckError::NonExhaustiveCoDataArms(arms) => {
                format!("Non-exhaustive data arms: {:?}", arms)
            }
            | TyckError::Expressivity(s) => format!("{}", s),
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
                | TyckTask::DeclHead(decl) => {
                    s += &format!("\t- when tycking external declaration ({}):\n", decl.span(self));
                    s += &format!("\t\t{}\n", truncated(self.ugly_scoped(decl)));
                }
                | TyckTask::DeclUni(decl) => {
                    s += &format!("\t- when tycking single declaration ({}):\n", decl.span(self));
                    s += &format!("\t\t{}\n", truncated(self.ugly_scoped(decl)));
                }
                | TyckTask::DeclScc(decls) => {
                    s += "\t- when tycking scc declarations:\n";
                    for decl in decls.iter() {
                        s += &format!(
                            "\t\t({})\n\t\t{}\n",
                            decl.span(self),
                            truncated(self.ugly_scoped(decl))
                        );
                    }
                }
                | TyckTask::Exec(exec) => {
                    s += &format!("\t- when tycking execution ({}):\n", exec.span(self));
                    s += &format!("\t\t{}\n", truncated(self.ugly_scoped(exec)));
                }
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
    fn error_primary_span(&self, error: &TyckError) -> Option<(String, Range<usize>)> {
        match error {
            | TyckError::TypeMismatch { expected: _, found } => {
                // Use the found type's span as primary
                Some(found.span(self).to_ariadne_span())
            }
            | TyckError::TypeExpected { found, .. } => {
                Some(found.span(self).to_ariadne_span())
            }
            | TyckError::MissingStructure(ty) => {
                Some(ty.span(self).to_ariadne_span())
            }
            | TyckError::MissingSolution(fills) => {
                fills.first().map(|fill| {
                    let site = self.statics.fills[fill];
                    site.span(self).to_ariadne_span()
                })
            }
            | TyckError::NotInlinable(def) => {
                Some(def.span(self).to_ariadne_span())
            }
            | TyckError::NotInlinableSeal(abst) => {
                // AbstId doesn't have a direct span, but we can get it from the hint if available
                use zydeco_utils::arena::ArenaAccess;
                self.statics.abst_hints.get(abst)
                    .map(|hint| hint.span(self).to_ariadne_span())
            }
            | _ => None,
        }
    }

    /// Get the error message text.
    fn error_message(&self, error: &TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => "Missing annotation".to_string(),
            | TyckError::MissingSeal => "Missing seal".to_string(),
            | TyckError::MissingSolution(fills) => {
                format!("Missing solution for {} hole(s)", fills.len())
            }
            | TyckError::MissingStructure(_) => "Missing structure for type".to_string(),
            | TyckError::SortMismatch => "Sort mismatch".to_string(),
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
            | TyckError::MissingDataArm(ctor) => format!("Missing data arm: {:?}", ctor),
            | TyckError::MissingCoDataArm(dtor) => format!("Missing codata arm: {:?}", dtor),
            | TyckError::NonExhaustiveCoDataArms(arms) => {
                format!("Non-exhaustive codata arms: {} missing", arms.len())
            }
            | TyckError::Expressivity(s) => s.to_string(),
            | TyckError::NotInlinable(_) => "Cannot inline definition".to_string(),
            | TyckError::NotInlinableSeal(_) => "Cannot inline sealed abstract type".to_string(),
        }
    }

    /// Create an Ariadne report for this error entry.
    pub fn error_entry_report(
        &self, TyckErrorEntry { error, blame, stack }: TyckErrorEntry,
    ) -> Report<'static, (String, Range<usize>)> {
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
                    | TyckTask::DeclHead(decl) | TyckTask::DeclUni(decl) | TyckTask::Exec(decl) => {
                        Some(decl.span(self).to_ariadne_span())
                    }
                    | TyckTask::DeclScc(decls) => decls.first().map(|decl| decl.span(self).to_ariadne_span()),
                    | _ => None,
                })
            })
            .unwrap_or_else(|| ("<internal>".to_string(), 0..0));

        let error_msg = self.error_message(&error);
        let (primary_file, primary_offset) = (primary_span.0.clone(), primary_span.1.start);
        let mut report = Report::build(ReportKind::Error, primary_file, primary_offset)
            .with_message(&error_msg);

        // Add labels for the error itself if we have specific error spans
        match &error {
            | TyckError::TypeMismatch { expected, found } => {
                let expected_span = expected.span(self).to_ariadne_span();
                let found_span = found.span(self).to_ariadne_span();
                if expected_span.0 == found_span.0 {
                    // Same file
                    report = report.with_label(
                        Label::new(found_span.clone())
                            .with_message("found this type")
                            .with_color(primary_color),
                    );
                    let secondary_color = colors.next();
                    report = report.with_label(
                        Label::new(expected_span)
                            .with_message("expected this type")
                            .with_color(secondary_color),
                    );
                } else {
                    // Different files
                    report = report
                        .with_label(
                            Label::new(found_span)
                                .with_message("found this type")
                                .with_color(primary_color),
                        )
                        .with_label(
                            Label::new(expected_span)
                                .with_message("expected this type")
                                .with_color(primary_color),
                        );
                }
            }
            | TyckError::MissingSolution(fills) => {
                for fill in fills.iter() {
                    let site = self.statics.fills[fill];
                    let site_span = site.span(self).to_ariadne_span();
                    report = report.with_label(
                        Label::new(site_span)
                            .with_message("hole needs a solution")
                            .with_color(primary_color),
                    );
                }
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
                if primary_span.0 != "<internal>" {
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
                | TyckTask::DeclHead(decl) => {
                    let span = decl.span(self).to_ariadne_span();
                    Some(Label::new(span).with_message("when tycking external declaration"))
                }
                | TyckTask::DeclUni(decl) => {
                    let span = decl.span(self).to_ariadne_span();
                    Some(Label::new(span).with_message("when tycking single declaration"))
                }
                | TyckTask::DeclScc(decls) => {
                    // Use the first declaration as the span
                    decls.first().map(|decl| {
                        let span = decl.span(self).to_ariadne_span();
                        Label::new(span).with_message(format!("when tycking {} declarations", decls.len()))
                    })
                }
                | TyckTask::Exec(exec) => {
                    let span = exec.span(self).to_ariadne_span();
                    Some(Label::new(span).with_message("when tycking execution"))
                }
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
                        | AnnId::Kind(kd) => Some(kd.span(self).to_ariadne_span()),
                        | AnnId::Type(ty) => Some(ty.span(self).to_ariadne_span()),
                    }.map(|span| Label::new(span).with_message("when computing least upper bound"))
                }
                | TyckTask::SignatureGen(ann) => {
                    // AnnId can be Set, Kind, or Type - extract span if possible
                    match ann {
                        | AnnId::Set => None,
                        | AnnId::Kind(kd) => Some(kd.span(self).to_ariadne_span()),
                        | AnnId::Type(ty) => Some(ty.span(self).to_ariadne_span()),
                    }.map(|span| Label::new(span).with_message("when generating signature"))
                }
                | TyckTask::StructureGen(ann) => {
                    // AnnId can be Set, Kind, or Type - extract span if possible
                    match ann {
                        | AnnId::Set => None,
                        | AnnId::Kind(kd) => Some(kd.span(self).to_ariadne_span()),
                        | AnnId::Type(ty) => Some(ty.span(self).to_ariadne_span()),
                    }.map(|span| Label::new(span).with_message("when generating structure"))
                }
                | TyckTask::MonadicLiftPat(pat) => match pat {
                    | PatId::Type(ty) => {
                        let span = ty.span(self).to_ariadne_span();
                        Some(Label::new(span).with_message("when performing monadic lift of type pattern"))
                    }
                    | PatId::Value(value) => {
                        let span = value.span(self).to_ariadne_span();
                        Some(Label::new(span).with_message("when performing monadic lift of value pattern"))
                    }
                },
                | TyckTask::MonadicLiftTerm(term) => match term {
                    | TermId::Kind(_) => None,
                    | TermId::Type(ty) => {
                        let span = ty.span(self).to_ariadne_span();
                        Some(Label::new(span).with_message("when performing monadic lift of type"))
                    }
                    | TermId::Value(value) => {
                        let span = value.span(self).to_ariadne_span();
                        Some(Label::new(span).with_message("when performing monadic lift of value"))
                    }
                    | TermId::Compu(compu) => {
                        let span = compu.span(self).to_ariadne_span();
                        Some(Label::new(span).with_message("when performing monadic lift of computation"))
                    }
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
