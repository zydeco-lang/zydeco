use crate::{syntax::*, *};
use thiserror::Error;

// Todo: writer monad instead of error monad

#[derive(Error, Debug, Clone)]
pub enum TyckError {
    #[error("Missing annotation")]
    MissingAnnotation,
    #[error("Sort mismatch")]
    SortMismatch,
    #[error("Kind mismatch")]
    KindMismatch,
    #[error("Type mismatch")]
    TypeMismatch,
    #[error("Missing data arm: {0:?}")]
    MissingDataArm(CtorName),
    #[error("Missing codata arm: {0:?}")]
    MissingCoDataArm(DtorName),
    #[error("Non-exhaustive data arms: {0:?}")]
    NonExhaustiveCoDataArms(im::HashMap<DtorName, TypeId>),
    #[error("{0}")]
    Expressivity(&'static str),
    #[error("Multiple monad implementations")]
    MultipleMonads,
    #[error("Neither monad nor algebra")]
    NeitherMonadNorAlgebra,
    #[error("Missing monad")]
    MissingMonad,
}

#[derive(Clone)]
pub struct TyckErrorEntry {
    pub(crate) error: TyckError,
    pub(crate) blame: &'static std::panic::Location<'static>,
    pub(crate) stack: im::Vector<TyckTask>,
    // Todo: dump related arena entries if needed
}

impl Tycker {
    pub fn error_output(&self, TyckErrorEntry { error, blame, stack }: TyckErrorEntry) -> String {
        use zydeco_surface::scoped::fmt::*;
        let budget = 80;
        // let budget = usize::MAX;
        let truncated = |mut s: String| {
            if s.len() > budget {
                s.truncate(budget - 3);
                s.push_str("...");
            }
            s
        };

        let mut s = String::new();
        s += &format!("Error: {}\n", error);
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
                | TyckTask::Pat(pat, _) => {
                    let prev = self.scoped.textual.back(&((*pat).into())).unwrap();
                    s += &format!("\t- when tycking pattern ({}):\n", self.spans[prev]);
                    s += &format!("\t\t{}\n", truncated(pat.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::Term(term, _) => {
                    let prev = self.scoped.textual.back(&((*term).into())).unwrap();
                    s += &format!("\t- when tycking term ({}):\n", self.spans[prev]);
                    s += &format!("\t\t{}\n", truncated(term.ugly(&Formatter::new(&self.scoped))));
                }
                | TyckTask::Lub(_, _) => {
                    s += "\t- when computing least upper bound\n";
                }
            }
        }
        s
    }
}

pub type Result<T> = std::result::Result<T, TyckError>;
pub type ResultKont<T> = std::result::Result<T, ()>;
