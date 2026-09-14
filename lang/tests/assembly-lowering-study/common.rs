#![allow(dead_code)]
use std::{
    hint::black_box,
    path::PathBuf,
    time::{Duration, Instant},
};
use zydeco_assembly::benchmark::{Input, Variant};
use zydeco_cli::CommandCompiler;

pub struct AllocationStats {
    pub allocs: usize,
    pub reallocs: usize,
    pub requested_bytes: usize,
    pub peak_extra_bytes: usize,
}

pub struct Experiment;

impl Experiment {
    fn sources() -> Vec<PathBuf> {
        let sources = std::env::args_os().skip(1).map(PathBuf::from).collect::<Vec<_>>();
        assert!(!sources.is_empty(), "supply source files");
        sources
    }

    fn verify(input: &Input<'_>, native: bool) -> [usize; 4] {
        let baseline = input.prepare(Variant::Original, native).run();
        let counts = baseline.counts();
        let expected = baseline.snapshot();
        for variant in Variant::ALL {
            let output = input.prepare(variant, native).run();
            assert_eq!(expected, output.snapshot(), "output drift: {variant:?}, native={native}");
            assert_eq!(counts, output.counts());
        }
        counts
    }

    pub fn timings() -> Result<(), Box<dyn std::error::Error>> {
        let compiler = CommandCompiler::default();
        for source in Self::sources() {
            let backend = compiler.lower(&source)?;
            let input = Input {
                spans: &backend.spans,
                scoped: &backend.scoped,
                statics: &backend.statics,
                program: &backend.sps_low,
            };
            let arena = &backend.sps_low.arena().inner;
            let input_nodes =
                arena.values.len() + arena.vpats.len() + arena.stacks.len() + arena.compus.len();
            for native in [false, true] {
                let counts = Self::verify(&input, native);
                // Warm each implementation before calibrating one common batch size.
                for _ in 0..8 {
                    for variant in Variant::ALL {
                        drop(black_box(input.prepare(variant, native).run()));
                    }
                }
                let mut calibration = Duration::ZERO;
                for _ in 0..8 {
                    let prepared = input.prepare(Variant::Original, native);
                    let started = Instant::now();
                    let output = black_box(prepared.run());
                    calibration += started.elapsed();
                    drop(output);
                }
                let iterations =
                    ((5_000_000u128 * 8) / calibration.as_nanos()).clamp(1, 1024) as usize;
                for round in 0..32 {
                    // Each implementation occupies every position equally often.
                    for index in 0..4 {
                        let variant = Variant::ALL[(round + index) % 4];
                        let mut lowering = Duration::ZERO;
                        let mut teardown = Duration::ZERO;
                        for _ in 0..iterations {
                            // Representation analysis and construction of the lowerer are outside timing.
                            let prepared = input.prepare(variant, native);
                            let started = Instant::now();
                            let output = black_box(prepared.run());
                            lowering += started.elapsed();
                            let started = Instant::now();
                            drop(output);
                            teardown += started.elapsed();
                        }
                        println!(
                            "{}",
                            serde_json::json!({
                                "source": source, "native": native, "variant": variant.name(),
                                "round": round, "iterations": iterations,
                                "lower_ns": lowering.as_nanos(), "drop_ns": teardown.as_nanos(),
                                "input_nodes": input_nodes, "output_counts": counts,
                            })
                        );
                    }
                }
            }
        }
        Ok(())
    }

    pub fn allocations(
        start: fn(), finish: fn() -> AllocationStats,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let compiler = CommandCompiler::default();
        for source in Self::sources() {
            let backend = compiler.lower(&source)?;
            let input = Input {
                spans: &backend.spans,
                scoped: &backend.scoped,
                statics: &backend.statics,
                program: &backend.sps_low,
            };
            for native in [false, true] {
                let counts = Self::verify(&input, native);
                for round in 0..8 {
                    for index in 0..4 {
                        let variant = Variant::ALL[(round + index) % 4];
                        let prepared = input.prepare(variant, native);
                        start();
                        let output = black_box(prepared.run());
                        let stats = finish();
                        drop(output);
                        println!(
                            "{}",
                            serde_json::json!({
                                "source": source, "native": native, "variant": variant.name(), "round": round,
                                "allocs": stats.allocs, "reallocs": stats.reallocs,
                                "requested_bytes": stats.requested_bytes, "peak_extra_bytes": stats.peak_extra_bytes,
                                "output_counts": counts,
                            })
                        );
                    }
                }
            }
        }
        Ok(())
    }
}
