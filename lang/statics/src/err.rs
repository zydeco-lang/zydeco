use crate::{syntax::*, *};

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
    NotInlinable(ss::DefId),
}

#[derive(Clone)]
pub struct TyckErrorEntry {
    pub(crate) error: TyckError,
    pub(crate) blame: &'static std::panic::Location<'static>,
    pub(crate) stack: im::Vector<TyckTask>,
    // Todo: dump related arena entries if needed
}

impl Tycker {
    fn error_output(&self, error: TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => format!("Missing annotation"),
            | TyckError::MissingSeal => format!("Missing seal"),
            | TyckError::MissingSolution(fills) => {
                use zydeco_surface::scoped::fmt::*;
                let mut s = String::new();
                s += "Missing solution for:";
                for fill in fills.iter() {
                    let site = self.statics.fills[&fill];
                    s += &format!(
                        "\n\t>> {} ({})",
                        site.ugly(&Formatter::new(&self.scoped)),
                        site.span(self)
                    )
                }
                s
            }
            | TyckError::MissingStructure(ty) => {
                use crate::fmt::*;
                format!(
                    "Missing structure for type: {}",
                    ty.ugly(&Formatter::new(&self.scoped, &self.statics))
                )
            }
            | TyckError::SortMismatch => format!("Sort mismatch"),
            | TyckError::KindMismatch => format!("Kind mismatch"),
            | TyckError::TypeMismatch { expected, found } => {
                use crate::fmt::*;
                format!(
                    "Type mismatch: expected `{}`, found `{}`",
                    expected.ugly(&Formatter::new(&self.scoped, &self.statics)),
                    found.ugly(&Formatter::new(&self.scoped, &self.statics))
                )
            }
            | TyckError::TypeExpected { expected, found } => {
                use crate::fmt::*;
                format!(
                    "Type expected: {}, found `{}`",
                    expected,
                    found.ugly(&Formatter::new(&self.scoped, &self.statics))
                )
            }
            | TyckError::MissingDataArm(ctor) => format!("Missing data arm: {:?}", ctor),
            | TyckError::MissingCoDataArm(dtor) => format!("Missing codata arm: {:?}", dtor),
            | TyckError::NonExhaustiveCoDataArms(arms) => {
                format!("Non-exhaustive data arms: {:?}", arms)
            }
            | TyckError::Expressivity(s) => format!("{}", s),
            | TyckError::NotInlinable(def) => {
                let span = def.span(self).to_owned();
                format!(
                    "Cannot inline definition: {} ({})",
                    {
                        use crate::fmt::*;
                        def.ugly(&Formatter::new(&self.scoped, &self.statics))
                    },
                    span
                )
            }
        }
    }
    pub fn error_entry_output(
        &self, TyckErrorEntry { error, blame, stack }: TyckErrorEntry,
    ) -> String {
        use zydeco_surface::scoped::fmt::*;
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
                    s += &format!("\t\t{}\n", truncated(decl.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::DeclUni(decl) => {
                    s += &format!("\t- when tycking single declaration ({}):\n", decl.span(self));
                    s += &format!("\t\t{}\n", truncated(decl.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::DeclScc(decls) => {
                    s += "\t- when tycking scc declarations:\n";
                    for decl in decls.iter() {
                        s += &format!(
                            "\t\t({})\n\t\t{}\n",
                            decl.span(self),
                            truncated(decl.ugly(&Formatter::new(&self.scoped)))
                        );
                    }
                }
                | TyckTask::Exec(exec) => {
                    s += &format!("\t- when tycking execution ({}):\n", exec.span(self));
                    s += &format!("\t\t{}\n", truncated(exec.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::Pat(pat, switch) => {
                    s += &format!("\t- when tycking pattern ({}):\n", pat.span(self));
                    s +=
                        &format!("\t\t>> {}\n", truncated(pat.ugly(&Formatter::new(&self.scoped))));
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            use crate::fmt::*;
                            s += &format!(
                                "\t\t<< (ana) {}\n",
                                truncated(ann.ugly(&Formatter::new(&self.scoped, &self.statics)))
                            );
                        }
                    }
                }
                | TyckTask::Term(term, switch) => {
                    s += &format!("\t- when tycking term ({}):\n", term.span(self));
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(term.ugly(&Formatter::new(&self.scoped)))
                    );
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            use crate::fmt::*;
                            s += &format!(
                                "\t\t<< (ana) {}\n",
                                truncated(ann.ugly(&Formatter::new(&self.scoped, &self.statics)))
                            );
                        }
                    }
                }
                | TyckTask::Lub(lhs, rhs) => {
                    use crate::fmt::*;
                    s += "\t- when computing least upper bound:\n";
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(lhs.ugly(&Formatter::new(&self.scoped, &self.statics)))
                    );
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(rhs.ugly(&Formatter::new(&self.scoped, &self.statics)))
                    );
                }
                | TyckTask::Lift(term) => {
                    use crate::fmt::*;
                    match term {
                        | TermId::Kind(_) => unreachable!(),
                        | TermId::Type(ty) => {
                            s += "\t- when lifting type:\n";
                            s += &format!(
                                "\t\t>> {}\n",
                                truncated(ty.ugly(&Formatter::new(&self.scoped, &self.statics)))
                            );
                        }
                        | TermId::Value(value) => {
                            s += "\t- when lifting value:\n";
                            s += &format!(
                                "\t\t>> {}\n",
                                truncated(value.ugly(&Formatter::new(&self.scoped, &self.statics)))
                            );
                        }
                        | TermId::Compu(compu) => {
                            s += "\t- when lifting computation:\n";
                            s += &format!(
                                "\t\t>> {}\n",
                                truncated(compu.ugly(&Formatter::new(&self.scoped, &self.statics)))
                            );
                        }
                    }
                }
                | TyckTask::Algebra(ty) => {
                    use crate::fmt::*;
                    s += &format!("\t- when generating algebra:\n");
                    s += &format!(
                        "\t\t>> {}\n",
                        truncated(ty.ugly(&Formatter::new(&self.scoped, &self.statics)))
                    );
                }
            }
        }
        s += &format!("Error: {}\n", self.error_output(error));
        s
    }
}

pub type Result<T> = std::result::Result<T, TyckErrorEntry>;
pub type ResultKont<T> = std::result::Result<T, ()>;
