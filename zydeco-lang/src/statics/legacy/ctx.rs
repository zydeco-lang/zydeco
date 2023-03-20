use super::tyck::TypeCheck;
use crate::{
    parse::legacy::syntax::*,
    statics::{err::TypeCheckError, resolve::*},
    syntax::{binder::*, Span},
};
use indexmap::IndexMap;
use std::{collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
pub struct Arity(pub Vec<Kind>, pub Kind);

#[derive(Clone, Debug)]
pub struct Ctx {
    vmap: HashMap<TermV, Type>,
    pub tmap: HashMap<TypeV, Arity>,
    pub data: HashMap<TypeV, Data>,
    pub coda: HashMap<TypeV, Codata>,
    defs: IndexMap<TermV, (Option<Type>, Value)>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            vmap: HashMap::new(),
            tmap: HashMap::new(),
            data: HashMap::new(),
            coda: HashMap::new(),
            defs: IndexMap::new(),
        }
    }
    pub fn push(&mut self, x: TermV, t: Type) {
        self.vmap.insert(x, t);
    }
    pub fn extend(&mut self, other: impl IntoIterator<Item = (TermV, Type)>) {
        self.vmap.extend(other);
    }
    pub fn lookup(&self, x: &TermV) -> Option<&Type> {
        self.vmap.get(x)
    }
    pub fn decl(&mut self, d: &Declare) -> Result<(), Span<NameResolveError>> {
        match d {
            Declare::Data(data @ Data { name, params, ann, .. }) => {
                self.data.insert(name.clone(), data.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(ann.make(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                        }))
                    },
                )?;
                self.tmap.insert(
                    name.clone(),
                    Arity(
                        params.into_iter().map(|(_, k)| *k).collect(),
                        Kind::VType,
                    ),
                );
                Ok(())
            }
            Declare::Codata(codata @ Codata { name, params, ann, .. }) => {
                self.coda.insert(name.clone(), codata.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(ann.make(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                        }))
                    },
                )?;
                self.tmap.insert(
                    name.clone(),
                    Arity(
                        params.into_iter().map(|(_, k)| *k).collect(),
                        Kind::CType,
                    ),
                );
                Ok(())
            }
            Declare::Define { name, ty: Some(ty), def: None, .. } => {
                self.push(name.clone(), *ty.to_owned());
                Ok(())
            }
            Declare::Define { name, ty, def: Some(def), .. } => {
                if let Some(ty) = ty {
                    self.push(name.clone(), *ty.to_owned());
                }
                self.defs.insert(
                    name.clone(),
                    (
                        ty.as_ref().map(|t| t.as_ref().clone()),
                        def.as_ref().clone(),
                    ),
                );
                Ok(())
            }
            Declare::Define { name, ty: None, def: None, ann, .. } => Err(ann
                .make(NameResolveError::EmptyDeclaration {
                    name: name.name().to_owned(),
                })),
        }
    }
    fn add_type_param(&mut self, x: TypeV, k: Kind) {
        self.tmap.insert(x, Arity(vec![], k));
    }
    pub fn tyck_types(&self) -> Result<(), Span<TypeCheckError>> {
        for (_, data) in &self.data {
            let mut ctx = self.clone();
            for (x, kind) in &data.params {
                ctx.add_type_param(x.clone(), kind.clone());
            }
            for (_, args) in &data.ctors {
                for arg in args {
                    arg.syn(&ctx)?;
                }
            }
        }
        for (_, coda) in &self.coda {
            let mut ctx = self.clone();
            for (x, kind) in &coda.params {
                ctx.add_type_param(x.clone(), kind.clone());
            }
            for (_, (args, ret)) in &coda.dtors {
                for arg in args {
                    arg.syn(&ctx)?;
                }
                ret.syn(&ctx)?;
            }
        }
        Ok(())
    }
    pub fn tyck_definitions(&mut self) -> Result<(), Span<TypeCheckError>> {
        for (name, (ty, def)) in &self.defs {
            let ty = match ty {
                Some(ty) => {
                    def.ana(ty, self)?;
                    ty.clone()
                }
                None => def.syn(self)?,
            };
            self.vmap.insert(name.clone(), ty);
        }
        Ok(())
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ctor.hash(state);
        self.args.hash(state);
    }
}
