use zydeco_tests::e2e_sources;

e2e_sources!({
    apply => "tests/cbv/apply.zy",
    bindings => "tests/cbv/bindings.zy",
    blocks => "tests/cbv/blocks.zy",
    data => "tests/cbv/data.zy",
    codata => "tests/cbv/codata.zy",
    packages => "tests/cbv/packages.zy",
    polymorphism => "tests/cbv/polymorphism.zy",
    recursion => "tests/cbv/recursion.zy",
    interpreter => "tests/cbv/interpreter.zy",
    breadth_first => "tests/cbv/breadth-first.zy",
    effects => "tests/cbv/effects.zy",
    effects_on_data => "tests/cbv/effects-on-data.zy",
    multi_argument => "tests/cbv/multi-argument.zy",
    multi_argument_effects => "tests/cbv/multi-argument-effects.zy",
});
