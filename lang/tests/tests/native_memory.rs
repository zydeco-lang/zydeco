use zydeco_tests::e2e_sources;

e2e_sources!({
    source_bytes => "tests/std/source-bytes.zy",
    memory_views => "tests/std/memory-views.zy",
    memory_faults => "tests/std/memory-faults.zy",
});
