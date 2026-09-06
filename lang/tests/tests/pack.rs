use zydeco_tests::e2e_sources;

e2e_sources!({
    exists => "tests/pack/exists.zy",
    manifest => "tests/pack/manifest.zy",
    interleaved => "tests/pack/interleaved.zy",
    named => "tests/pack/named.zy",
    monadic => "tests/pack/monadic.zy",
    uniform => "tests/pack/uniform.zy",
});
