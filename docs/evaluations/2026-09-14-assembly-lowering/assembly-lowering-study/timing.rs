#[path = "assembly_bench/common.rs"]
mod common;

fn main() {
    common::Experiment::timings().unwrap();
}
