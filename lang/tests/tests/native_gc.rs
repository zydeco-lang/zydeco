// The native executable links a generated entry point. Include its independent
// collector here so its unit tests also run in the workspace suite on every host.
#[path = "../../../runtime/gc.rs"]
mod gc;
