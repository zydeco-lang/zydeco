use zydeco_tests::interp_proj_bin;

interp_proj_bin!(tests / stack, invert, "invert");
interp_proj_bin!(tests / stack, backtrace, "backtrace");
interp_proj_bin!(tests / stack, cbv, "cbv");
