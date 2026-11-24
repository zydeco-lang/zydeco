// Consolidated integration tests for `zydeco-tests`.

#[path = "support/memory.rs"]
mod memory;

mod arena_invariants;
mod builtin;
mod cbn;
mod cbv;
mod cbv_decisions;
mod cbv_sources;
mod constructor_fallthrough;
mod core;
mod delimcc;
mod demand;
mod demos;
mod desugaring;
mod diagnostics;
mod effect_library;
mod effects;
mod existential;
mod ffi;
mod ffi_examples;
mod general_memory;
mod inference;
mod memory_views;
mod monadic;
mod monadic_nominal;
mod monadic_roots;
mod monadic_value_functions;
mod native_gc;
mod native_model;
mod nondet;
mod oopsla;
mod pack;
mod seal;
mod partial;
mod prelude;
mod representation;
mod representation_policies;
mod represented_calls;
mod short_circuit;
mod spell;
mod sps_schemes;
mod stack_protocols;
mod static_elimination;
mod static_layout;
mod std;
mod storage;
mod templates;
mod transformers;
#[path = "typeof.rs"]
mod r#typeof;
mod value_calculation;
mod value_pi;
