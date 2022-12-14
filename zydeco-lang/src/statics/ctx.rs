use super::{err::TypeCheckError, resolve::*, tyck::TypeCheck};
use crate::parse::syntax::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Sort {
    TVal,
    TComp,
}

#[derive(Clone, Debug)]
pub struct Ctx {
    vmap: HashMap<VVar, TValue>,
    pub tmap: HashMap<TVar, Sort>,
    data: HashMap<TVar, Vec<(Ctor, Vec<TValue>)>>,
    pub ctors: HashMap<Ctor, (TVar, Vec<TValue>)>,
    coda: HashMap<TVar, Vec<(Dtor, Vec<TValue>, TCompute)>>,
    pub dtors: HashMap<Dtor, (TVar, Vec<TValue>, TCompute)>,
    pub defs: HashMap<VVar, (Option<TValue>, Value)>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            vmap: HashMap::new(),
            tmap: HashMap::new(),
            data: HashMap::new(),
            ctors: HashMap::new(),
            coda: HashMap::new(),
            dtors: HashMap::new(),
            defs: HashMap::new(),
        }
    }
    pub fn push(&mut self, x: VVar, t: TValue) {
        self.vmap.insert(x, t);
    }
    pub fn extend(&mut self, other: impl IntoIterator<Item = (VVar, TValue)>) {
        self.vmap.extend(other);
    }
    pub fn lookup(&self, x: &VVar) -> Option<&TValue> {
        self.vmap.get(x)
    }
    pub fn decl(&mut self, d: &Declare) -> Result<(), NameResolveError> {
        match d {
            Declare::Data { name, ctors, ann } => {
                self.data.insert(name.clone(), ctors.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
                    },
                )?;
                self.tmap.insert(name.clone(), Sort::TVal);
                for (ctor, args) in ctors {
                    self.ctors
                        .insert(ctor.clone(), (name.clone(), args.clone()))
                        .map_or(Ok(()), |_| {
                            Err(NameResolveError::DuplicateDeclaration {
                                name: ctor.name().to_string(),
                                ann: ann.clone(),
                            })
                        })?;
                }
                Ok(())
            }
            Declare::Codata { name, dtors, ann } => {
                self.coda.insert(name.clone(), dtors.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
                    },
                )?;
                self.tmap.insert(name.clone(), Sort::TComp);
                for (dtor, args, ret) in dtors {
                    self.dtors
                        .insert(
                            dtor.clone(),
                            (name.clone(), args.clone(), ret.clone()),
                        )
                        .map_or(Ok(()), |_| {
                            Err(NameResolveError::DuplicateDeclaration {
                                name: dtor.name().to_string(),
                                ann: ann.clone(),
                            })
                        })?;
                }
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
    pub fn tyck_pre(&self) -> Result<(), TypeCheckError> {
        for (_, ctors) in &self.data {
            for (_, args) in ctors {
                for arg in args {
                    arg.tyck(self)?;
                }
            }
        }
        for (_, dtors) in &self.coda {
            for (_, args, ret) in dtors {
                for arg in args {
                    arg.tyck(self)?;
                }
                ret.tyck(self)?;
            }
        }
        for (_, (ty, def)) in &self.defs {
            match ty {
                Some(ty) => {
                    def.tyck(self)?;
                    let ty_ = def.tyck(self)?;
                    ty.eqv(&ty_).ok_or_else(|| {
                        TypeCheckError::TypeMismatch {
                            expected: ty.clone().into(),
                            found: ty_.clone().into(),
                        }
                    })?
                }
                None => {
                    def.tyck(self)?;
                }
            }
        }
        Ok(())
    }
    pub fn tyck_post(&mut self) -> Result<(), TypeCheckError> {
        for (name, (ty, def)) in self.defs.to_owned() {
            match ty {
                Some(_) => {}
                None => {
                    let ty = def.tyck(self).expect("checked in tyck_pre");
                    self.push(name, ty)
                }
            }
        }
        Ok(())
    }
}
