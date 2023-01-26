use super::{
    err::TypeCheckError,
    resolve::*,
    tyck::{Eqv, TypeCheck},
};
use crate::parse::syntax::*;
use std::{collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
pub struct Ctx {
    vmap: HashMap<VVar, Type>,
    pub tmap: HashMap<TVar, Kind>,
    pub data: HashMap<TVar, Data>,
    pub ctors: HashMap<Ctor, Vec<TVar>>,
    pub coda: HashMap<TVar, Codata>,
    pub dtors: HashMap<Dtor, Vec<TVar>>,
    pub defs: HashMap<VVar, (Option<Type>, Value)>,
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
    pub fn push(&mut self, x: VVar, t: Type) {
        self.vmap.insert(x, t);
    }
    pub fn extend(&mut self, other: impl IntoIterator<Item = (VVar, Type)>) {
        self.vmap.extend(other);
    }
    pub fn lookup(&self, x: &VVar) -> Option<&Type> {
        self.vmap.get(x)
    }
    pub fn decl(&mut self, d: &Declare) -> Result<(), NameResolveError> {
        match d {
            Declare::Data(data @ Data { name, ctors, ann, .. }) => {
                self.data.insert(name.clone(), data.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
                    },
                )?;
                self.tmap.insert(name.clone(), Kind::ValType);
                for (ctor, _) in ctors {
                    self.ctors
                        .entry(ctor.clone())
                        .or_default()
                        .push(name.clone());
                }
                Ok(())
            }
            Declare::Codata(codata @ Codata { name, dtors, ann, .. }) => {
                self.coda.insert(name.clone(), codata.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
                    },
                )?;
                self.tmap.insert(name.clone(), Kind::CompType);
                for (dtor, ..) in dtors {
                    self.dtors
                        .entry(dtor.clone())
                        .or_default()
                        .push(name.clone());
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
        for (_, (ty, def)) in &self.defs {
            match ty {
                Some(ty) => {
                    def.syn(self)?;
                    let ty_ = def.syn(self)?;
                    ty.eqv(&ty_).ok_or_else(|| {
                        TypeCheckError::TypeMismatch {
                            expected: ty.clone().into(),
                            found: ty_.clone().into(),
                        }
                    })?
                }
                None => {
                    def.syn(self)?;
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
                    let ty = def.syn(self).expect("checked in tyck_pre");
                    self.push(name, ty)
                }
            }
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
