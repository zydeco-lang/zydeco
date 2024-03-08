use crate::bitter::syntax as b;
use std::collections::HashMap;
use zydeco_syntax::VarName;

pub enum Symbol {
    Module(SymbolTable),
    Def(b::DefId),
}

pub struct SymbolTable(pub HashMap<VarName, b::Modifiers<Symbol>>);
impl SymbolTable {
    fn new() -> SymbolTable {
        Self(HashMap::new())
    }
}

pub struct Collector {
    pub spans: b::SpanArenaBitter,
    pub ctx: b::Ctx,
    pub global: SymbolTable,
}

pub trait Collect {
    fn collect(&self, collector: &Collector) -> SymbolTable;
}

impl Collect for b::TopLevel {
    fn collect(&self, collector: &Collector) -> SymbolTable {
        let b::TopLevel(decls) = self;
        let mut symtbl = SymbolTable::new();
        let SymbolTable(map) = &mut symtbl;
        for b::Modifiers { public, inner } in decls {
            let public = *public;
            use b::Declaration as Decl;
            match inner {
                Decl::Alias(b::Alias { binder, bindee: _ }) => {
                    for (b, d) in collector.ctx.pats[*binder].var_intro() {
                        let inner = Symbol::Def(d);
                        // Fixme: Report if the symbol is defined twice
                        map.insert(b, b::Modifiers { public, inner });
                    }
                }
                Decl::Extern(b::Extern { binder, params: _, ty: _ }) => {
                    for (b, d) in collector.ctx.pats[*binder].var_intro() {
                        let inner = Symbol::Def(d);
                        // Fixme: Report if the symbol is defined twice
                        map.insert(b, b::Modifiers { public, inner });
                    }
                }
                Decl::Module(b::Module { name, top }) => {
                    let inner = Symbol::Module(top.collect(collector));
                    if let Some(m) = map.get_mut(name) {
                        // Fixme: Report if the symbol is defined as a definition
                        // Fixme: Report if the module visibility is inconsistent
                        if let b::Modifiers { inner: Symbol::Module(_m), .. } = m {
                            todo!()
                        }
                    } else {
                        map.insert(name.clone(), b::Modifiers { public, inner });
                    }
                }
                Decl::UseDef(_) => todo!(),
                Decl::UseBlock(_) => todo!(),
                Decl::Main(_) => todo!(),
            }
        }
        symtbl
    }
}

pub trait VarIntro {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)>;
}

impl VarIntro for b::Pattern {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)> {
        todo!()
    }
}

impl VarIntro for b::UsePath {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)> {
        todo!()
    }
}
