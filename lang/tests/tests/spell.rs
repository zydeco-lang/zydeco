use zydeco_tests::interp_proj_bin;

interp_proj_bin!(spell, toplevel, "0-toplevel");
interp_proj_bin!(spell, thunk_ret, "1-thunk-ret");
interp_proj_bin!(spell, data_codata, "2-data-codata");
interp_proj_bin!(spell, object, "3-object");
interp_proj_bin!(spell, y_combinator, "4-y-combinator");
interp_proj_bin!(spell, mutual_rec, "5-mutual-rec");
interp_proj_bin!(spell, cps, "6-cps");
// lib_proj_bin!(spell, call_by_need, "7-call-by-need");
interp_proj_bin!(spell, lense_prism_optics, "8-lense-prism-optics");
