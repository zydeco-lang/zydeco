//! Compare static allocation sites for the local representation policies.
//! These counts describe generated code, not executed allocations or peak memory.

use std::path::PathBuf;
use zydeco_cli::{BackendProgram, CommandCompiler};
use zydeco_stackir::{
    SpsLowProgram,
    low::{
        representation::{
            RepresentationPolicy, RepresentationStrategy, Shared, UnboxingOpportunity,
        },
        syntax::{ClosurePackage, Value, ValueExt},
        unbox::LocalUnboxing,
    },
};

/// A type-level policy experiment that needs no change to the analysis or emitters.
struct FieldLimit<P, const MAX: usize>(P);

impl<P: RepresentationPolicy, const MAX: usize> RepresentationPolicy for FieldLimit<P, MAX> {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
        opportunity.fields <= MAX && self.0.unbox(opportunity)
    }
}

#[derive(Default)]
struct Measurement {
    cells: usize,
    payload_words: usize,
}

impl Measurement {
    fn collect(program: &SpsLowProgram, unboxing: &LocalUnboxing) -> Self {
        program.arena().inner.values.iter().fold(Self::default(), |mut measurement, (id, value)| {
            if unboxing.values.contains(id) {
                return measurement;
            }
            match value {
                | Value::VCons(vcons) => {
                    measurement.cells += 1;
                    measurement.payload_words += vcons.layout.arity;
                }
                | Value::Ext(ValueExt::ClosurePackage(_)) => {
                    measurement.cells += 1;
                    measurement.payload_words += ClosurePackage::FIELDS;
                }
                | _ => {}
            }
            measurement
        })
    }

    fn print(
        &self, source: &std::path::Path, policy: &str, unboxing: &LocalUnboxing,
        program: &SpsLowProgram,
    ) {
        let eliminated = unboxing
            .values
            .iter()
            .filter(|id| {
                matches!(
                    program.arena().inner.values[id],
                    Value::VCons(_) | Value::Ext(ValueExt::ClosurePackage(_))
                )
            })
            .count();
        println!(
            "{},{policy},{},{},{},{}",
            source.display(),
            self.cells,
            self.payload_words,
            eliminated,
            unboxing.unboxed_vars.len()
        );
    }
}

fn main() {
    if let Err(error) = Experiment::run() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

struct Experiment;

impl Experiment {
    fn run() -> Result<(), Box<dyn std::error::Error>> {
        let sources = std::env::args_os().skip(1).map(PathBuf::from).collect::<Vec<_>>();
        if sources.is_empty() {
            return Err("supply one or more executable Zydeco source paths".into());
        }
        println!(
            "source,policy,product_cells,product_payload_words,eliminated_cells,expanded_variables"
        );
        let compiler = CommandCompiler::default();
        for source in sources {
            let backend = compiler.lower(&source)?;
            for &strategy in RepresentationStrategy::ALL {
                let unboxing = LocalUnboxing::with_policy(&backend.sps_low, &strategy);
                Measurement::collect(&backend.sps_low, &unboxing).print(
                    &source,
                    strategy.name(),
                    &unboxing,
                    &backend.sps_low,
                );
            }
            Self::measure_static::<1>(&source, &backend);
            Self::measure_static::<4>(&source, &backend);
        }
        Ok(())
    }

    fn measure_static<const MAX: usize>(source: &std::path::Path, backend: &BackendProgram) {
        let unboxing = LocalUnboxing::with_policy(&backend.sps_low, &FieldLimit::<_, MAX>(Shared));
        Measurement::collect(&backend.sps_low, &unboxing).print(
            source,
            &format!("shared-max-{MAX}"),
            &unboxing,
            &backend.sps_low,
        );
    }
}
