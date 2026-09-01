//! Coverage and exhaustiveness validation for typed eliminations.

use crate::{arena::StaticsArena, syntax::*};
use std::{collections::HashSet, fmt};
use zydeco_utils::arena::ArenaAccess;

const MAX_REPORTED_MISSING_PATTERNS: usize = 8;

/// A concrete value shape demonstrating that a data match is not exhaustive.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoveragePattern {
    Wildcard,
    Constructor(CtorName, Box<CoveragePattern>),
    Unit,
    Product(Vec<CoveragePattern>),
    Named(FieldName, Box<CoveragePattern>),
    Package(Box<CoveragePattern>),
}

impl fmt::Display for CoveragePattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Wildcard => write!(f, "_"),
            | Self::Constructor(name, argument) => match argument.as_ref() {
                | Self::Unit => write!(f, "+{name}()"),
                | Self::Product(items) => {
                    let items =
                        items.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                    write!(f, "+{name}({items})")
                }
                | _ => write!(f, "+{name}({argument})"),
            },
            | Self::Unit => write!(f, "()"),
            | Self::Product(items) => {
                let items = items.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                write!(f, "({items})")
            }
            | Self::Named(name, pattern) => write!(f, "{name} = {pattern}"),
            | Self::Package(pattern) => write!(f, "(_, {pattern})"),
        }
    }
}

/// A post-check coverage failure tied to one typed computation.
#[derive(Clone, Debug)]
pub enum CoverageError {
    NonExhaustiveMatch {
        computation: CompuId,
        missing: Vec<CoveragePattern>,
        truncated: bool,
    },
    NonExhaustiveCopatternMatch {
        computation: CompuId,
        missing: Vec<CoveragePattern>,
        truncated: bool,
    },
    NonExhaustiveCoMatch {
        computation: CompuId,
        missing: Vec<DtorName>,
    },
    DuplicateCoMatchArms {
        computation: CompuId,
        duplicates: Vec<DtorName>,
    },
}

impl CoverageError {
    pub fn computation(&self) -> CompuId {
        match self {
            | Self::NonExhaustiveMatch { computation, .. }
            | Self::NonExhaustiveCopatternMatch { computation, .. }
            | Self::NonExhaustiveCoMatch { computation, .. }
            | Self::DuplicateCoMatchArms { computation, .. } => *computation,
        }
    }

    fn destructor_list(destructors: &[DtorName]) -> String {
        destructors.iter().map(|name| format!(".{name}")).collect::<Vec<_>>().join(", ")
    }

    fn write_missing(
        f: &mut fmt::Formatter<'_>, subject: &str, missing: &[CoveragePattern], truncated: bool,
    ) -> fmt::Result {
        let missing = missing.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        if truncated {
            write!(f, "{subject}; examples of missing patterns: {missing}, ...")
        } else {
            write!(f, "{subject}; missing pattern(s): {missing}")
        }
    }
}

impl fmt::Display for CoverageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::NonExhaustiveMatch { missing, truncated, .. } => {
                Self::write_missing(f, "Non-exhaustive match", missing, *truncated)
            }
            | Self::NonExhaustiveCopatternMatch { missing, truncated, .. } => Self::write_missing(
                f,
                "Non-exhaustive comatch argument patterns",
                missing,
                *truncated,
            ),
            | Self::NonExhaustiveCoMatch { missing, .. } => write!(
                f,
                "Non-exhaustive comatch; missing destructor arm(s): {}",
                Self::destructor_list(missing)
            ),
            | Self::DuplicateCoMatchArms { duplicates, .. } => write!(
                f,
                "Duplicate comatch destructor arm(s): {}",
                Self::destructor_list(duplicates)
            ),
        }
    }
}

/// Post-check validator for pattern coverage and codata arm completeness.
pub struct CoverageChecker<'a> {
    statics: &'a StaticsArena,
}

impl<'a> CoverageChecker<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    pub fn validate(&self) -> Vec<CoverageError> {
        self.statics
            .compus
            .iter()
            .flat_map(|(computation, term)| self.validate_computation(*computation, term))
            .collect()
    }

    fn validate_computation(&self, computation: CompuId, term: &Computation) -> Vec<CoverageError> {
        let term_errors = match term {
            | Computation::Match(Match { scrut, arms }) => {
                self.validate_match(computation, *scrut, arms)
            }
            | Computation::CoMatch(CoMatch { arms }) => self.validate_comatch(computation, arms),
            | _ => Vec::new(),
        };
        let binder_errors = self
            .statics
            .copattern_pack_pi_binders
            .get(&computation)
            .copied()
            .map(|binder| {
                self.validate_pattern_matrix(
                    computation,
                    std::iter::once(binder),
                    Some(HeadSpace::Package),
                    true,
                )
            })
            .unwrap_or_default();
        term_errors.into_iter().chain(binder_errors).collect()
    }

    fn validate_match(
        &self, computation: CompuId, scrutinee: ValueId, arms: &[Matcher<VPatId, CompuId>],
    ) -> Vec<CoverageError> {
        let expected = self.statics.data_hints.get(&scrutinee).copied().map(HeadSpace::Data);
        self.validate_pattern_matrix(
            computation,
            arms.iter().map(|Matcher { binder, .. }| *binder),
            expected,
            self.statics.copattern_matches.get(&computation).is_some(),
        )
    }

    fn validate_pattern_matrix(
        &self, computation: CompuId, binders: impl IntoIterator<Item = VPatId>,
        expected: Option<HeadSpace>, copattern: bool,
    ) -> Vec<CoverageError> {
        let matrix = binders
            .into_iter()
            .map(|binder| vec![MatrixPattern::from_typed(binder, self.statics)])
            .collect();
        let mut missing = CoverageMatrix::new(self.statics).uncovered(matrix, 1, expected);
        let truncated = missing.len() > MAX_REPORTED_MISSING_PATTERNS;
        missing.truncate(MAX_REPORTED_MISSING_PATTERNS);
        let missing = missing
            .into_iter()
            .map(|row| {
                let [pattern]: [CoveragePattern; 1] = row
                    .try_into()
                    .expect("single-scrutinee coverage must produce one witness pattern");
                pattern
            })
            .collect::<Vec<_>>();
        (!missing.is_empty())
            .then_some({
                if copattern {
                    CoverageError::NonExhaustiveCopatternMatch { computation, missing, truncated }
                } else {
                    CoverageError::NonExhaustiveMatch { computation, missing, truncated }
                }
            })
            .into_iter()
            .collect()
    }

    fn validate_comatch(
        &self, computation: CompuId, arms: &[CoMatcher<DtorName, CompuId>],
    ) -> Vec<CoverageError> {
        let Some(codata) = self.statics.codata_hints.get(&computation).copied() else {
            return Vec::new();
        };
        let supplied = arms.iter().map(|arm| arm.dtor.clone()).collect::<HashSet<_>>();
        let mut declared = HashSet::new();
        let missing = self.statics.codatas[&codata]
            .iter()
            .filter_map(|(destructor, _)| {
                (declared.insert(destructor.clone()) && !supplied.contains(destructor))
                    .then_some(destructor.clone())
            })
            .collect::<Vec<_>>();

        let mut seen = HashSet::new();
        let mut duplicate_names = HashSet::new();
        let duplicates = arms
            .iter()
            .filter_map(|arm| {
                (!seen.insert(arm.dtor.clone()) && duplicate_names.insert(arm.dtor.clone()))
                    .then_some(arm.dtor.clone())
            })
            .collect::<Vec<_>>();

        [
            (!missing.is_empty())
                .then_some(CoverageError::NonExhaustiveCoMatch { computation, missing }),
            (!duplicates.is_empty())
                .then_some(CoverageError::DuplicateCoMatchArms { computation, duplicates }),
        ]
        .into_iter()
        .flatten()
        .collect()
    }
}

#[derive(Clone, Debug)]
enum MatrixPattern {
    Wildcard,
    /// A refutable view pattern contributes no structural coverage fact.
    Opaque,
    Constructor {
        data: DataId,
        name: CtorName,
        argument: Box<MatrixPattern>,
    },
    Unit,
    Product(Vec<MatrixPattern>),
    Named(FieldName, Box<MatrixPattern>),
    Package(Box<MatrixPattern>),
}

impl MatrixPattern {
    fn from_typed(pattern: VPatId, statics: &StaticsArena) -> Self {
        match &statics.vpats[&pattern] {
            | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Alias(_) => {
                Self::Wildcard
            }
            | ValuePattern::Named(Named(name, inner)) => {
                Self::Named(name.clone(), Box::new(Self::from_typed(*inner, statics)))
            }
            | ValuePattern::Ctor(Ctor(name, argument)) => Self::Constructor {
                data: statics
                    .data_pat_hints
                    .get(&pattern)
                    .copied()
                    .expect("typed constructor patterns retain their data definition"),
                name: name.clone(),
                argument: Box::new(Self::from_typed(*argument, statics)),
            },
            | ValuePattern::Triv(_) => Self::Unit,
            | ValuePattern::VCons(components) => Self::Product(
                components.iter().map(|item| Self::from_typed(*item, statics)).collect(),
            ),
            | ValuePattern::SCons(ConsN(_, tail)) => {
                Self::Package(Box::new(Self::from_typed(*tail, statics)))
            }
            | ValuePattern::View(view) => {
                let nested = Self::from_typed(view.pattern, statics);
                if matches!(nested, Self::Wildcard) { Self::Wildcard } else { Self::Opaque }
            }
        }
    }

    fn head_space(&self) -> Option<HeadSpace> {
        match self {
            | Self::Wildcard | Self::Opaque => None,
            | Self::Constructor { data, .. } => Some(HeadSpace::Data(*data)),
            | Self::Unit => Some(HeadSpace::Unit),
            | Self::Product(items) => Some(HeadSpace::Product(items.len())),
            | Self::Named(name, _) => Some(HeadSpace::Named(name.clone())),
            | Self::Package(_) => Some(HeadSpace::Package),
        }
    }
}

#[derive(Clone, Debug)]
enum HeadSpace {
    Data(DataId),
    Unit,
    Product(usize),
    Named(FieldName),
    Package,
}

impl HeadSpace {
    fn constructors(&self, statics: &StaticsArena) -> Vec<Constructor> {
        match self {
            | Self::Data(data) => {
                let mut seen = HashSet::new();
                statics.datas[data]
                    .iter()
                    .filter_map(|(name, _)| {
                        seen.insert(name.clone())
                            .then_some(Constructor::Data { name: name.clone() })
                    })
                    .collect()
            }
            | Self::Unit => vec![Constructor::Unit],
            | Self::Product(arity) => vec![Constructor::Product(*arity)],
            | Self::Named(name) => vec![Constructor::Named(name.clone())],
            | Self::Package => vec![Constructor::Package],
        }
    }
}

#[derive(Clone, Debug)]
enum Constructor {
    Data { name: CtorName },
    Unit,
    Product(usize),
    Named(FieldName),
    Package,
}

impl Constructor {
    fn arity(&self) -> usize {
        match self {
            | Self::Data { .. } | Self::Named(_) | Self::Package => 1,
            | Self::Unit => 0,
            | Self::Product(arity) => *arity,
        }
    }

    fn specialize(&self, pattern: &MatrixPattern) -> Option<Vec<MatrixPattern>> {
        match (self, pattern) {
            | (_, MatrixPattern::Wildcard) => Some(vec![MatrixPattern::Wildcard; self.arity()]),
            | (
                Self::Data { name },
                MatrixPattern::Constructor { name: found_name, argument, .. },
            ) if name == found_name => Some(vec![argument.as_ref().clone()]),
            | (Self::Unit, MatrixPattern::Unit) => Some(Vec::new()),
            | (Self::Product(arity), MatrixPattern::Product(items)) if *arity == items.len() => {
                Some(items.clone())
            }
            | (Self::Named(name), MatrixPattern::Named(found_name, pattern))
                if name == found_name =>
            {
                Some(vec![pattern.as_ref().clone()])
            }
            | (Self::Package, MatrixPattern::Package(pattern)) => {
                Some(vec![pattern.as_ref().clone()])
            }
            | _ => None,
        }
    }

    fn rebuild(&self, mut row: Vec<CoveragePattern>) -> Vec<CoveragePattern> {
        let rest = row.split_off(self.arity());
        let head = match self {
            | Self::Data { name } => CoveragePattern::Constructor(
                name.clone(),
                Box::new(row.into_iter().next().expect("data constructors have one argument")),
            ),
            | Self::Unit => CoveragePattern::Unit,
            | Self::Product(_) => CoveragePattern::Product(row),
            | Self::Named(name) => CoveragePattern::Named(
                name.clone(),
                Box::new(row.into_iter().next().expect("named patterns have one payload")),
            ),
            | Self::Package => CoveragePattern::Package(Box::new(
                row.into_iter().next().expect("package patterns have one dynamic payload"),
            )),
        };
        std::iter::once(head).chain(rest).collect()
    }
}

struct CoverageMatrix<'a> {
    statics: &'a StaticsArena,
}

impl<'a> CoverageMatrix<'a> {
    fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    fn uncovered(
        &self, matrix: Vec<Vec<MatrixPattern>>, columns: usize, expected: Option<HeadSpace>,
    ) -> Vec<Vec<CoveragePattern>> {
        if columns == 0 {
            return matrix.is_empty().then_some(Vec::new()).into_iter().collect();
        }
        if matrix.is_empty() {
            return match expected {
                | Some(space) => self.uncovered_finite(matrix, columns, space),
                | None => vec![vec![CoveragePattern::Wildcard; columns]],
            };
        }

        match expected.or_else(|| matrix.iter().filter_map(|row| row.first()?.head_space()).next())
        {
            | Some(space) => self.uncovered_finite(matrix, columns, space),
            | None => self.uncovered_default(matrix, columns),
        }
    }

    fn uncovered_finite(
        &self, matrix: Vec<Vec<MatrixPattern>>, columns: usize, space: HeadSpace,
    ) -> Vec<Vec<CoveragePattern>> {
        space
            .constructors(self.statics)
            .into_iter()
            .flat_map(|constructor| {
                let specialized = matrix
                    .iter()
                    .filter_map(|row| {
                        let fields = constructor.specialize(row.first()?)?;
                        Some(fields.into_iter().chain(row.iter().skip(1).cloned()).collect())
                    })
                    .collect();
                self.uncovered(specialized, columns - 1 + constructor.arity(), None)
                    .into_iter()
                    .map(move |row| constructor.rebuild(row))
            })
            .take(MAX_REPORTED_MISSING_PATTERNS + 1)
            .collect()
    }

    fn uncovered_default(
        &self, matrix: Vec<Vec<MatrixPattern>>, columns: usize,
    ) -> Vec<Vec<CoveragePattern>> {
        let defaults = matrix
            .into_iter()
            .filter_map(|row| {
                matches!(row.first(), Some(MatrixPattern::Wildcard))
                    .then(|| row.into_iter().skip(1).collect())
            })
            .collect();
        self.uncovered(defaults, columns - 1, None)
            .into_iter()
            .map(|row| std::iter::once(CoveragePattern::Wildcard).chain(row).collect())
            .take(MAX_REPORTED_MISSING_PATTERNS + 1)
            .collect()
    }
}
