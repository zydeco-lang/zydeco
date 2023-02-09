use super::{err::TypeCheckError, resolve::*, tyck::TypeCheck};
use crate::{parse::syntax::*, syntax::binders::*};
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
    pub fn decl(&mut self, d: &Declare) -> Result<(), NameResolveError> {
        match d {
            Declare::Data(data @ Data { name, params, ann, .. }) => {
                self.data.insert(name.clone(), data.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
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
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
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
                    (ty.clone().map(|t| *t), def.as_ref().clone()),
                );
                Ok(())
            }
            Declare::Define { name, ty: None, def: None, ann, .. } => {
                Err(NameResolveError::EmptyDeclaration {
                    name: name.name().to_owned(),
                    ann: ann.clone(),
                })
            }
        }
    }
    pub fn type_validation(&self) -> Result<(), TypeCheckError> {
        for (_, data) in &self.data {
            for (_, args) in &data.ctors {
                for arg in args {
                    arg.syn(self)?;
                }
            }
        }
        for (_, codata) in &self.coda {
            for (_, args, ret) in &codata.dtors {
                for arg in args {
                    arg.syn(self)?;
                }
                ret.syn(self)?;
            }
        }
        Ok(())
    }
    pub fn tyck_definitions(&mut self) -> Result<(), TypeCheckError> {
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
