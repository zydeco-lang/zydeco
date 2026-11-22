use zydeco_tests::{e2e_io_source, e2e_sources};

e2e_sources!({
    abort => "tests/demos/abort.zy",
    abort_variadic => "tests/demos/abort-variadic.zydeco",
    add => "tests/demos/add.zy",
    alias => "tests/demos/alias.zy",
    avl_hash => "tests/demos/avl-hash.zy",
    backtrace => "tests/demos/backtrace.zydeco",
    bigmac => "tests/demos/bigmac.zy",
    comment => "tests/demos/comment.zy",
    even_odd_codata => "tests/demos/even-odd-codata.zy",
    even_odd_data => "tests/demos/even-odd-data.zy",
    even_odd_fix => "tests/demos/even-odd-fix.zy",
    fn_opt => "tests/demos/fn-opt.zy",
    forall => "tests/demos/forall.zy",
    ifz => "tests/demos/ifz.zy",
    invert => "tests/demos/invert.zydeco",
    merge => "tests/demos/merge.zydeco",
    num => "tests/demos/num.zy",
    ret => "tests/demos/ret.zydeco",
});

e2e_io_source!(y, "tests/demos/Y.zydeco", "", "");
e2e_io_source!(choice, "tests/demos/choice.zy", "", "0\n1\no\n");
e2e_io_source!(defunctionalization, "tests/demos/defunctionalization.zydeco", "", "18\n");
e2e_io_source!(
    deterministic_pushdown_automaton,
    "tests/demos/deterministic-pushdown-automaton.zydeco",
    "",
    concat!(
        "The following parens are balanced? (())\n",
        "true\n",
        "The following parens are balanced? ()()\n",
        "true\n",
        "The following parens are balanced? )(()\n",
        "false\n",
        "The following parens are balanced? ()(\n",
        "false\n",
        "The following parens are balanced? ()\n",
        "true\n",
        "The following parens are balanced? (\n",
        "false\n",
        "The following parens are balanced? )\n",
        "false\n",
        "The following parens are balanced? \n",
        "true\n",
    )
);
e2e_io_source!(self_interpreter, "tests/demos/interpreter.zydeco", "", "false\n");
e2e_io_source!(list, "tests/demos/list.zydeco", "", "4\n");
e2e_io_source!(listm, "tests/demos/listm.zydeco", "", "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
e2e_io_source!(literal, "tests/demos/literal.zy", "", "Hello literal\nsecond line\n");
e2e_io_source!(oo, "tests/demos/oo.zydeco", "", "42\n");
e2e_io_source!(regex, "tests/demos/regex.zy", "", "\\^o^/\n");
e2e_io_source!(unit, "tests/demos/unit.zy", "", "()\n");
e2e_io_source!(variadic, "tests/demos/variadic.zy", "", "hello\nworld\nhello\nworld\n");

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

// End of input and an unparseable line stop each loop without an error.
e2e_io_source!(
    echo_sum_stops_at_invalid_input,
    "tests/demos/echo_sum.zydeco",
    "1\n2\nnot-an-integer\n",
    "1 = sum\n3 = sum\n"
);
e2e_io_source!(algebra_exits_at_eof, "tests/demos/algebra.zydeco", "", "");

// The configuration-free root program reports its declared status.
e2e_io_source!(playground, "playground/main.zydeco", "", "", 42);
