use zydeco_tests::e2e_io_source;
use zydeco_tests::e2e_sources;

e2e_sources!({
    y => "tests/demos/Y.zydeco",
    abort => "tests/demos/abort.zy",
    abort_variadic => "tests/demos/abort-variadic.zydeco",
    add => "tests/demos/add.zy",
    alias => "tests/demos/alias.zy",
    avl_hash => "tests/demos/avl-hash.zy",
    backtrace => "tests/demos/backtrace.zydeco",
    bigmac => "tests/demos/bigmac.zy",
    choice => "tests/demos/choice.zy",
    comment => "tests/demos/comment.zy",
    defunctionalization => "tests/demos/defunctionalization.zydeco",
    deterministic_pushdown_automaton => "tests/demos/deterministic-pushdown-automaton.zydeco",
    even_odd_codata => "tests/demos/even-odd-codata.zy",
    even_odd_data => "tests/demos/even-odd-data.zy",
    even_odd_fix => "tests/demos/even-odd-fix.zy",
    fn_opt => "tests/demos/fn-opt.zy",
    forall => "tests/demos/forall.zy",
    ifz => "tests/demos/ifz.zy",
    interpreter => "tests/demos/interpreter.zydeco",
    invert => "tests/demos/invert.zydeco",
    list => "tests/demos/list.zydeco",
    listm => "tests/demos/listm.zydeco",
    literal => "tests/demos/literal.zy",
    merge => "tests/demos/merge.zydeco",
    num => "tests/demos/num.zy",
    oo => "tests/demos/oo.zydeco",
    regex => "tests/demos/regex.zy",
    ret => "tests/demos/ret.zydeco",
    unit => "tests/demos/unit.zy",
    variadic => "tests/demos/variadic.zy",
});

// `algebra` and `echo_sum` read stdin in a loop, so they declare their input
// and assert the exact interaction on every backend.
e2e_io_source!(echo_sum, "tests/demos/echo_sum.zydeco", "1\n2\n3\n", "1 = sum\n3 = sum\n6 = sum\n");
e2e_io_source!(
    algebra_accumulates,
    "tests/demos/algebra.zydeco",
    "10\n20\n",
    "10 = sum\n30 = sum\n"
);
// The exception transformer's failure branch fires once the sum reaches 256.
e2e_io_source!(
    algebra_rejects_the_threshold,
    "tests/demos/algebra.zydeco",
    "100\n200\n",
    "100 = sum\nsum >= 256\n",
    1
);
