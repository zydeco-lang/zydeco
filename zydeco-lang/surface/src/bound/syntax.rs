pub use crate::textual::syntax::*;
use derive_more::From;
use slotmap::SecondaryMap;

#[derive(From)]
pub enum Declaration {
    Type(TypeDef),
    Define(Define),
    Main(Main),
}

#[derive(Default)]
pub struct TopLevel(pub Vec<Declaration>);

impl Extend<Declaration> for TopLevel {
    fn extend<T: IntoIterator<Item = Declaration>>(&mut self, iter: T) {
        let Self(top) = self;
        top.extend(iter);
    }
}

#[derive(Default)]
pub struct Ctx {
    // arenas
    pub patterns: SecondaryMap<PatternId, Sp<Pattern>>,
    pub terms: SecondaryMap<TermId, Sp<Term<DefId>>>,

    // meta
    /// for matching backwards from reference site to definition site
    pub lookup: im::HashMap<NameRef<VarName>, DefId>,
    /// for matching forwards from definition site to a declaration site;
    /// typically used by type definitions without a body
    pub peeks: im::HashMap<NameRef<VarName>, DefId>,
}

impl Ctx {
    pub fn pattern(&mut self, id: PatternId, pattern: Sp<Pattern>) -> PatternId {
        let res = self.patterns.insert(id.clone(), pattern);
        if res.is_some() {
            panic!("duplicate pattern inserted")
        }
        id
    }
    pub fn term(&mut self, id: TermId, term: Sp<Term<DefId>>) -> TermId {
        let res = self.terms.insert(id.clone(), term);
        if res.is_some() {
            panic!("duplicate term inserted")
        }
        id
    }
}
