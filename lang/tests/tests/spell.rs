use zydeco_tests::e2e_sources;

e2e_sources!({
    one_term => "spell/0-toplevel.zy",
    thunk_ret => "spell/1-thunk-ret.zy",
    data_codata => "spell/2-data-codata.zy",
    object => "spell/3-object.zy",
    y_combinator => "spell/4-y-combinator.zy",
    mutual_recursion => "spell/5-mutual-rec.zy",
    cps => "spell/6-cps.zy",
    optics => "spell/8-lense-prism-optics.zy",
});
