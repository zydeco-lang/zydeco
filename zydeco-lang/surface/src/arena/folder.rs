pub trait AstFolder {
    type DefId;
    type PatId;
    type CoPatId;
    type TermId;

    fn fold_def(&mut self, id: Self::DefId);
    fn fold_pat(&mut self, id: Self::PatId);
    fn fold_copat(&mut self, id: Self::CoPatId);
    fn fold_term(&mut self, id: Self::TermId);
}
