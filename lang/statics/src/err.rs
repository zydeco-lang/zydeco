use crate::{syntax::*, *};

#[derive(Debug, Clone)]
pub enum TyckError {
    MissingAnnotation,
    MissingSeal,
    MissingSolution(FillId),
    SortMismatch,
    KindMismatch,
    TypeMismatch { expected: TypeId, found: TypeId },
    TypeExpected { expected: String, found: TypeId },
    MissingDataArm(CtorName),
    MissingCoDataArm(DtorName),
    NonExhaustiveCoDataArms(im::HashMap<DtorName, TypeId>),
    Expressivity(&'static str),
    MultipleMonads,
    NeitherMonadNorAlgebra(su::TermId, TypeId),
    MissingMonad,
    AlgebraGenerationFailure,
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
            | TyckError::MissingSolution(fill) => {
                use zydeco_surface::scoped::fmt::*;
                let site = self.statics.fills[&fill];
                let prev = self.scoped.textual.back(&(site.into())).unwrap();
                format!(
                    "Missing solution for {} ({})",
                    site.ugly(&Formatter::new(&self.scoped)),
                    self.spans[prev]
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
            | TyckError::MultipleMonads => format!("Multiple monad implementations"),
            | TyckError::NeitherMonadNorAlgebra(term, ty) => {
                let span = self.spans[self.scoped.textual.back(&term.into()).unwrap()].to_owned();
                format!(
                    "Neither monad nor algebra: ({} : {}) ({})",
                    {
                        use zydeco_surface::scoped::fmt::*;
                        term.ugly(&Formatter::new(&self.scoped))
                    },
                    {
                        use crate::fmt::*;
                        ty.ugly(&Formatter::new(&self.scoped, &self.statics))
                    },
                    span
                )
            }
            | TyckError::MissingMonad => format!("Missing monad"),
            | TyckError::AlgebraGenerationFailure => {
                format!("Cannot generate algebra for this type")
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
                    let prev = self.scoped.textual.back(&((*decl).into())).unwrap();
                    s +=
                        &format!("\t- when tycking external declaration ({}):\n", self.spans[prev]);
                    s += &format!("\t\t{}\n", truncated(decl.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::DeclUni(decl) => {
                    let prev = self.scoped.textual.back(&((*decl).into())).unwrap();
                    s += &format!("\t- when tycking single declaration ({}):\n", self.spans[prev]);
                    s += &format!("\t\t{}\n", truncated(decl.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::DeclScc(decls) => {
                    s += "\t- when tycking scc declarations:\n";
                    for decl in decls.iter() {
                        let prev = self.scoped.textual.back(&((*decl).into())).unwrap();
                        s += &format!(
                            "\t\t({})\n\t\t{}\n",
                            self.spans[prev],
                            truncated(decl.ugly(&Formatter::new(&self.scoped)))
                        );
                    }
                }
                | TyckTask::Exec(exec) => {
                    let prev = self.scoped.textual.back(&((*exec).into())).unwrap();
                    s += &format!("\t- when tycking execution ({}):\n", self.spans[prev]);
                    s += &format!("\t\t{}\n", truncated(exec.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::Pat(pat, switch) => {
                    let prev = self.scoped.textual.back(&((*pat).into())).unwrap();
                    s += &format!("\t- when tycking pattern ({}):\n", self.spans[prev]);
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
                    let prev = self.scoped.textual.back(&((*term).into())).unwrap();
                    s += &format!("\t- when tycking term ({}):\n", self.spans[prev]);
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
                | TyckTask::Algebra(ty) => {
                    use crate::fmt::*;
                    s += &format!("\t- when generating algebra:\n");
                    s += &format!(
                        "\t\t{}\n",
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
