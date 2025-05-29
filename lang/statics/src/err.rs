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

impl Tycker {
    fn error_output(&self, error: TyckError) -> String {
        match error {
            | TyckError::MissingAnnotation => format!("Missing annotation"),
            | TyckError::MissingSeal => format!("Missing seal"),
            | TyckError::MissingSolution(fills) => {
                let mut s = String::new();
                s += "Missing solution for:";
                for fill in fills.iter() {
                    let site = self.statics.fills[&fill];
                    s += &format!("\n\t>> {} ({})", self.dump_scoped(site), site.span(self))
                }
                s
            }
            | TyckError::MissingStructure(ty) => {
                format!("Missing structure for type: {}", self.dump_statics(ty))
            }
            | TyckError::SortMismatch => format!("Sort mismatch"),
            | TyckError::KindMismatch => format!("Kind mismatch"),
            | TyckError::TypeMismatch { expected, found } => {
                format!(
                    "Type mismatch: expected `{}`, found `{}`",
                    self.dump_statics(expected),
                    self.dump_statics(found)
                )
            }
            | TyckError::TypeExpected { expected, found } => {
                format!("Type expected: {}, found `{}`", expected, self.dump_statics(found))
            }
            | TyckError::MissingDataArm(ctor) => format!("Missing data arm: {:?}", ctor),
            | TyckError::MissingCoDataArm(dtor) => format!("Missing codata arm: {:?}", dtor),
            | TyckError::NonExhaustiveCoDataArms(arms) => {
                format!("Non-exhaustive data arms: {:?}", arms)
            }
            | TyckError::Expressivity(s) => format!("{}", s),
            | TyckError::NotInlinable(def) => {
                let span = def.span(self);
                format!("Cannot inline definition: {} ({})", self.dump_statics(def), span)
            }
            | TyckError::NotInlinableSeal(abst) => {
                use zydeco_utils::arena::ArenaAccess;
                let hint_msg = match self.statics.abst_hints.get(&abst) {
                    | Some(hint) => {
                        format!(", defined by {} ({})", self.dump_statics(hint), hint.span(self))
                    }
                    | None => "".to_string(),
                };
                format!(
                    "Cannot inline sealed abstract type: {}{}",
                    self.dump_statics(abst),
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
                    s += &format!("\t\t{}\n", truncated(self.dump_scoped(decl)));
                }
                | TyckTask::DeclUni(decl) => {
                    s += &format!("\t- when tycking single declaration ({}):\n", decl.span(self));
                    s += &format!("\t\t{}\n", truncated(self.dump_scoped(decl)));
                }
                | TyckTask::DeclScc(decls) => {
                    s += "\t- when tycking scc declarations:\n";
                    for decl in decls.iter() {
                        s += &format!(
                            "\t\t({})\n\t\t{}\n",
                            decl.span(self),
                            truncated(self.dump_scoped(decl))
                        );
                    }
                }
                | TyckTask::Exec(exec) => {
                    s += &format!("\t- when tycking execution ({}):\n", exec.span(self));
                    s += &format!("\t\t{}\n", truncated(self.dump_scoped(exec)));
                }
                | TyckTask::Pat(pat, switch) => {
                    s += &format!("\t- when tycking pattern ({}):\n", pat.span(self));
                    s += &format!("\t\t>> {}\n", truncated(self.dump_scoped(pat)));
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            s += &format!("\t\t<< (ana) {}\n", truncated(self.dump_statics(ann)));
                        }
                    }
                }
                | TyckTask::Term(term, switch) => {
                    s += &format!("\t- when tycking term ({}):\n", term.span(self));
                    s += &format!("\t\t>> {}\n", truncated(self.dump_scoped(term)));
                    match switch {
                        | Switch::Syn => {
                            s += "\t\t<< (syn)\n";
                        }
                        | Switch::Ana(ann) => {
                            s += &format!("\t\t<< (ana) {}\n", truncated(self.dump_statics(ann)));
                        }
                    }
                }
                | TyckTask::Lub(lhs, rhs) => {
                    s += "\t- when computing least upper bound:\n";
                    s += &format!("\t\t>> {}\n", truncated(self.dump_statics(lhs)));
                    s += &format!("\t\t>> {}\n", truncated(self.dump_statics(rhs)));
                }
                // | TyckTask::Lift(term) => match term {
                //     | TermId::Kind(_) => unreachable!(),
                //     | TermId::Type(ty) => {
                //         s += "\t- when lifting type:\n";
                //         s += &format!("\t\t>> {}\n", truncated(self.dump_statics(ty)));
                //     }
                //     | TermId::Value(value) => {
                //         s += "\t- when lifting value:\n";
                //         s += &format!("\t\t>> {}\n", truncated(self.dump_statics(value)));
                //     }
                //     | TermId::Compu(compu) => {
                //         s += "\t- when lifting computation:\n";
                //         s += &format!("\t\t>> {}\n", truncated(self.dump_statics(compu)));
                //     }
                // },
                // | TyckTask::Algebra(ty) => {
                //     s += &format!("\t- when performing algebra translation:\n");
                //     s += &format!("\t\t>> {}\n", truncated(self.dump_statics(ty)));
                // }
            }
        }
        s += &format!("Error: {}\n", self.error_output(error));
        s
    }
}

pub type Result<T> = std::result::Result<T, TyckErrorEntry>;
pub type ResultKont<T> = std::result::Result<T, ()>;
