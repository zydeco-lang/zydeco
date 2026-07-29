use zydeco_tests::interp_proj_bin;

interp_proj_bin!(tests / exec, abort, "abort");
interp_proj_bin!(tests / exec, add, "add");
interp_proj_bin!(tests / exec, alg, "alg");
interp_proj_bin!(tests / exec, alias, "alias");
interp_proj_bin!(tests / exec, backtrack, "backtrack");
interp_proj_bin!(tests / exec, bigmac, "bigmac");
interp_proj_bin!(tests / exec, cbv, "cbv");
interp_proj_bin!(tests / exec, cbpv, "cbpv");
interp_proj_bin!(tests / exec, cbpv_monadic, "cbpv-monadic");
interp_proj_bin!(tests / exec, choice, "choice");
interp_proj_bin!(tests / exec, defunctionalization, "defunctionalization");
interp_proj_bin!(
    tests / exec,
    deterministic_pushdown_automaton,
    "deterministic-pushdown-automaton"
);
interp_proj_bin!(tests / exec, even_odd_codata, "even-odd-codata");
interp_proj_bin!(tests / exec, even_odd_data, "even-odd-data");
interp_proj_bin!(tests / exec, even_odd_fix, "even-odd-fix");
interp_proj_bin!(tests / exec, exists, "exists");
interp_proj_bin!(tests / exec, explosion, "explosion");
interp_proj_bin!(tests / exec, fn_opt, "fn-opt");
interp_proj_bin!(tests / exec, forall, "forall");
interp_proj_bin!(tests / exec, free, "free");
interp_proj_bin!(tests / exec, free_, "free'");
interp_proj_bin!(tests / exec, ifz, "ifz");
interp_proj_bin!(tests / exec, interpreter, "interpreter");
interp_proj_bin!(tests / exec, list, "list");
interp_proj_bin!(tests / exec, listm, "listm");
interp_proj_bin!(tests / exec, loop_, "loop");
interp_proj_bin!(tests / exec, loopy, "loopy");
interp_proj_bin!(tests / exec, named_tuple, "named-tuple");
interp_proj_bin!(tests / exec, named_nested, "named-nested");
interp_proj_bin!(tests / exec, named_pattern, "named-pattern");
interp_proj_bin!(tests / exec, named_data, "named-data");
interp_proj_bin!(tests / exec, named_function, "named-function");
interp_proj_bin!(tests / exec, named_codata, "named-codata");
interp_proj_bin!(tests / exec, named_pun, "named-pun");
interp_proj_bin!(tests / exec, named_mixed, "named-mixed");
interp_proj_bin!(tests / exec, named, "named");
interp_proj_bin!(tests / exec, num, "num");
interp_proj_bin!(tests / exec, oo, "oo");
interp_proj_bin!(tests / exec, optiont, "optiont");
interp_proj_bin!(tests / exec, partial_annotation, "partial-annotation");
interp_proj_bin!(tests / exec, regex, "regex");
interp_proj_bin!(tests / exec, ret, "ret");
interp_proj_bin!(tests / exec, trans, "trans");
interp_proj_bin!(tests / exec, unit, "unit");
interp_proj_bin!(tests / exec, variadic, "variadic");
interp_proj_bin!(tests / exec, y, "Y");
