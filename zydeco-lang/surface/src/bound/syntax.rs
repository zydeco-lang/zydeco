use std::cell::OnceCell;

pub use crate::textual::syntax::*;
use derive_more::From;
use slotmap::SecondaryMap;

#[derive(From)]
pub enum Declaration {
    Type(TypeDef),
    Define(Define),
    Main(Main),
}

#[derive(From, Clone)]
pub enum PublicDec {
    Module(ModName),
    Def(VarName),
    // Use(NameRef<UseEnum>),
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
pub struct SymbolTable {
    pub set: OnceCell<DefId>,
    pub vtype_kind: OnceCell<DefId>,
    pub ctype_kind: OnceCell<DefId>,
    pub thunk_type: OnceCell<DefId>,
    pub ret_type: OnceCell<DefId>,
    pub fn_type: OnceCell<DefId>,
    pub int_type: OnceCell<DefId>,
    pub string_type: OnceCell<DefId>,
}

#[derive(Default)]
pub struct Ctx {
    // symbol table
    pub symbol_table: SymbolTable,

    // arenas
    pub patterns: SecondaryMap<PatternId, Pattern>,
    pub terms: SecondaryMap<TermId, Term<DefId>>,
    // meta
    /// for matching backwards from reference site to definition site
    pub lookup: im::HashMap<NameRef<VarName>, DefId>,
    // . / means local scope, will be destroyed and remain the pubs
    pub lookup_new: im::HashMap<Vec<ModName>, im::HashMap<VarName, DefId>>,
    pub cur_module: Vec<ModName>,
    // absolute module tree
    // pub module_tree: Tree<ModName>,

    /// for matching forwards from definition site to a declaration site;
    /// typically used by type definitions without a body
    pub peeks: im::HashMap<VarName, DefId>,
    // no nameRef needed
}

impl Ctx {
    pub fn pattern(&mut self, id: PatternId, pattern: Pattern) -> PatternId {
        let res = self.patterns.insert(id.clone(), pattern);
        if res.is_some() {
            panic!("duplicate pattern inserted")
        }
        id
    }
    pub fn term(&mut self, id: TermId, term: Term<DefId>) -> TermId {
        let res = self.terms.insert(id.clone(), term);
        if res.is_some() {
            panic!("duplicate term inserted")
        }
        id
    }
    pub fn debug_print_lookup(&mut self, situation: String) {
        println!("When {}:", situation);
        for lookup in self.lookup_new.clone() {
            print!("Module path:");
            for ModName(mod_name) in lookup.0.clone() {
                print!("{}/", mod_name)
            }
            print!("\n");
            for (VarName(name), _) in lookup.1.clone() {
                print!("{}, ", name)
            }
            print!("\n");
        }
    }
    pub fn debug_insert_lookup(&mut self, path: Vec<ModName>, name: VarName) {
        print!("Insert {} into: ", name);
        for ModName(mod_name) in path {
            print!("{}/", mod_name)
        }
        println!("")
    }
}
