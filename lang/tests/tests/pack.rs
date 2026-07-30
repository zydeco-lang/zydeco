use zydeco_tests::interp_proj_bin;

interp_proj_bin!(tests / pack, manifest, "manifest");
interp_proj_bin!(tests / pack, interleaved, "interleaved");
interp_proj_bin!(tests / pack, named, "named");
interp_proj_bin!(tests / pack, monadic, "monadic");
interp_proj_bin!(tests / pack, uniform, "uniform");
