//! Consumer demands used while rebuilding lexical high SPS.

use super::syntax::*;
use std::collections::{BTreeMap, HashMap};

/// The observable part of a value. Shape demand survives until its unpack does.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Demand {
    #[default]
    Absent,
    /// Physical product positions. An empty map still requires a product.
    Fields(BTreeMap<usize, Demand>),
    /// An unknown consumer observes the whole value.
    Used,
}

impl Demand {
    pub fn is_absent(&self) -> bool {
        matches!(self, Self::Absent)
    }

    pub(super) fn join(self, other: Self) -> Self {
        match (self, other) {
            | (Self::Used, _) | (_, Self::Used) => Self::Used,
            | (Self::Absent, rest) | (rest, Self::Absent) => rest,
            | (Self::Fields(mut left), Self::Fields(right)) => {
                for (position, demand) in right {
                    let joined = left.remove(&position).unwrap_or_default().join(demand);
                    left.insert(position, joined);
                }
                Self::Fields(left)
            }
        }
    }

    /// A logical cons item may represent the entire remaining physical suffix.
    /// That suffix must keep its shape even when all its fields are dead.
    pub(super) fn item(&self, position: usize, count: usize, layout: ProductLayout) -> Self {
        match self {
            | Self::Used => Self::Used,
            | Self::Fields(fields) if position + 1 == count && count < layout.arity => {
                Self::Fields(
                    fields
                        .range(position..)
                        .map(|(field, demand)| (field - position, demand.clone()))
                        .collect(),
                )
            }
            | Self::Fields(fields) => fields.get(&position).cloned().unwrap_or_default(),
            | Self::Absent => Self::Absent,
        }
    }
}

/// Demands on the free definitions of one surviving subtree. Lexical binders
/// consume their entries; no checked-AST tables or global fixed point are needed.
#[derive(Default)]
pub(super) struct Demands(HashMap<DefId, Demand>);

impl Demands {
    pub fn singleton(def: DefId, demand: Demand) -> Self {
        Self(HashMap::from([(def, demand)]))
    }

    pub fn join(mut self, other: Self) -> Self {
        for (def, demand) in other.0 {
            let joined = self.0.remove(&def).unwrap_or_default().join(demand);
            self.0.insert(def, joined);
        }
        self
    }

    pub fn contains(&self, def: &DefId) -> bool {
        self.0.contains_key(def)
    }

    pub fn remove(&mut self, def: &DefId) {
        self.0.remove(def);
    }

    pub fn pattern(&self, arena: &StackirArena, binder: VPatId) -> Demand {
        match &arena.inner.vpats[&binder] {
            | ValuePattern::Var(def) => self.0.get(def).cloned().unwrap_or_default(),
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => Demand::Absent,
            | ValuePattern::Ctor(_) => Demand::Used,
            | ValuePattern::Alias(Alias(patterns)) => patterns
                .iter()
                .map(|pattern| self.pattern(arena, *pattern))
                .fold(Demand::Absent, Demand::join),
            | ValuePattern::VCons(VCons { items, layout }) => {
                let fields = items
                    .iter()
                    .enumerate()
                    .flat_map(|(position, pattern)| {
                        let demand = self.pattern(arena, *pattern);
                        if position + 1 == items.len() && items.len() < layout.arity {
                            match demand {
                                | Demand::Fields(fields) => fields
                                    .into_iter()
                                    .map(|(field, demand)| (position + field, demand))
                                    .collect(),
                                | Demand::Used => (position..layout.arity)
                                    .map(|field| (field, Demand::Used))
                                    .collect(),
                                | Demand::Absent => Vec::new(),
                            }
                        } else if demand.is_absent() {
                            Vec::new()
                        } else {
                            vec![(position, demand)]
                        }
                    })
                    .collect();
                Demand::Fields(fields)
            }
        }
    }
}
