use crate::EmitError;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use zydeco_stackir::SpsLowProgram;
use zydeco_stackir::low::representation::{RepresentationPolicy, RepresentationStrategy};
use zydeco_stackir::low::scalar::ScalarPlans;
use zydeco_stackir::low::syntax::{
    self as sps, CompuId, Computation, DefId, ExternalFunction, LetLabel, SpsLowArena, Value,
    ValueExt, ValueId,
};
use zydeco_stackir::low::unbox::LocalUnboxing;
use zydeco_syntax::{ForeignImport, ForeignLibraryName, HostOp, Literal, UnitImport};

/// A first-order native procedure compiled as its own `CallConv::Tail` function.
#[derive(Clone, Copy)]
pub(crate) struct Case {
    pub(crate) index: usize,
    pub(crate) label: Option<DefId>,
    pub(crate) entry: Option<sps::EntryParameters>,
    pub(crate) body: CompuId,
}

/// Static immutable UTF-8 `HeapString` record (`[len: u64 LE, utf8_bytes..., padding]`) in `.rodata`.
pub(crate) struct StaticStringLiteral {
    pub(crate) symbol: String,
    pub(crate) bytes: Vec<u8>,
}

/// Deterministic compilation plan for one `SpsLowProgram`.
pub(crate) struct ModulePlan {
    pub(crate) cases: Vec<Case>,
    pub(crate) case_labels: HashMap<DefId, usize>,
    pub(crate) static_strings: Vec<StaticStringLiteral>,
    pub(crate) string_indices: HashMap<ValueId, usize>,
    pub(crate) host_roles: BTreeSet<HostOp>,
    pub(crate) foreign_imports: Vec<ForeignImport>,
    pub(crate) foreign_libraries: Vec<ForeignLibraryName>,
    pub(crate) unit_imports: Vec<UnitImport>,
    pub(crate) unboxing: LocalUnboxing,
    pub(crate) scalars: ScalarPlans,
}

impl ModulePlan {
    pub(crate) fn new(
        program: &SpsLowProgram, strategy: RepresentationStrategy,
    ) -> Result<Self, EmitError> {
        let arena = program.arena();
        let (cases, case_labels) = collect_cases(arena, program.root());
        let (static_strings, string_indices) = collect_strings(arena);
        let (host_roles, foreign_imports, foreign_libraries, unit_imports) = collect_externs(arena);
        let unboxing = LocalUnboxing::with_policy(program, &strategy);
        let scalars = ScalarPlans::new(program, strategy.scalar_boxing());

        Ok(Self {
            cases,
            case_labels,
            static_strings,
            string_indices,
            host_roles,
            foreign_imports,
            foreign_libraries,
            unit_imports,
            unboxing,
            scalars,
        })
    }

    pub(crate) fn is_code_label(&self, def: DefId) -> bool {
        self.case_labels.contains_key(&def)
    }
}

fn collect_cases(arena: &SpsLowArena, root: CompuId) -> (Vec<Case>, HashMap<DefId, usize>) {
    let mut named = Vec::new();
    for (_, value) in arena.inner.values.iter() {
        if let Value::Ext(ValueExt::Block(block)) = value {
            named.push((block.label, Some(block.entry), block.body));
        }
    }
    for (_, compu) in arena.inner.compus.iter() {
        if let Computation::Ext(sps::CompuExt::LetLabel(LetLabel {
            label,
            kind: sps::LabelKind::Closed,
            body,
            ..
        })) = compu
        {
            named.push((*label, None, *body));
        }
    }
    // Sort by label id so emitted functions and symbols are byte-reproducible.
    named.sort_unstable_by_key(|(label, _, _)| *label);

    let mut cases = Vec::with_capacity(1 + named.len());
    cases.push(Case { index: 0, label: None, entry: None, body: root });
    for (offset, (label, entry, body)) in named.into_iter().enumerate() {
        cases.push(Case { index: 1 + offset, label: Some(label), entry, body });
    }
    let case_labels =
        cases.iter().filter_map(|case| case.label.map(|label| (label, case.index))).collect();
    (cases, case_labels)
}

fn collect_strings(arena: &SpsLowArena) -> (Vec<StaticStringLiteral>, HashMap<ValueId, usize>) {
    let mut entries = Vec::new();
    for (id, value) in arena.inner.values.iter() {
        if let Value::Literal(Literal::String(text)) = value {
            entries.push((id, text.as_str().to_owned()));
        }
    }
    entries.sort_unstable_by_key(|(id, _)| *id);

    let mut deduplicated: BTreeMap<String, usize> = BTreeMap::new();
    let mut literals = Vec::new();
    let mut string_indices = HashMap::new();
    for (id, text) in entries {
        let index = if let Some(&existing) = deduplicated.get(&text) {
            existing
        } else {
            let index = literals.len();
            let raw = text.as_bytes();
            let total = (8 + raw.len()).next_multiple_of(8);
            let mut bytes = Vec::with_capacity(total);
            bytes.extend_from_slice(&(raw.len() as u64).to_le_bytes());
            bytes.extend_from_slice(raw);
            bytes.resize(total, 0);
            literals.push(StaticStringLiteral { symbol: format!("zydeco_str_{index}"), bytes });
            deduplicated.insert(text, index);
            index
        };
        string_indices.insert(id, index);
    }
    (literals, string_indices)
}

fn collect_externs(
    arena: &SpsLowArena,
) -> (BTreeSet<HostOp>, Vec<ForeignImport>, Vec<ForeignLibraryName>, Vec<UnitImport>) {
    let mut host_roles = BTreeSet::new();
    let mut foreign_map: BTreeMap<(String, String), ForeignImport> = BTreeMap::new();
    let mut foreign_libs = BTreeSet::new();
    let mut unit_map: BTreeMap<(String, String), UnitImport> = BTreeMap::new();

    for (_, compu) in arena.inner.compus.iter() {
        let Computation::ExternCall(call) = compu else { continue };
        match &call.function {
            | ExternalFunction::Host(role) => {
                host_roles.insert(*role);
            }
            | ExternalFunction::Foreign(import) => {
                foreign_libs.insert(import.target.library.clone());
                foreign_map.insert(
                    (
                        import.target.library.as_str().to_owned(),
                        import.target.symbol.as_str().to_owned(),
                    ),
                    import.clone(),
                );
            }
            | ExternalFunction::Unit(import) => {
                unit_map.insert(
                    (
                        import.target.library.as_str().to_owned(),
                        import.target.symbol.as_str().to_owned(),
                    ),
                    import.clone(),
                );
            }
        }
    }

    (
        host_roles,
        foreign_map.into_values().collect(),
        foreign_libs.into_iter().collect(),
        unit_map.into_values().collect(),
    )
}
