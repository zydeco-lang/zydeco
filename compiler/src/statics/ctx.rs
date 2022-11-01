use super::resolve::*;
use crate::parse::syntax::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Ctx<Ann> {
    vmap: HashMap<VVar<Ann>, TValue<Ann>>,
    data: HashMap<TVar<Ann>, Vec<(Ctor<Ann>, Vec<TValue<Ann>>)>>,
    pub ctors: HashMap<Ctor<Ann>, (TVar<Ann>, Vec<TValue<Ann>>)>,
    codata:
        HashMap<TVar<Ann>, Vec<(Dtor<Ann>, Vec<TValue<Ann>>, TCompute<Ann>)>>,
    pub dtors: HashMap<Dtor<Ann>, (TVar<Ann>, Vec<TValue<Ann>>, TCompute<Ann>)>,
}

impl<Ann: Clone> Ctx<Ann> {
    pub fn new() -> Self {
        Self {
            vmap: HashMap::new(),
            data: HashMap::new(),
            ctors: HashMap::new(),
            codata: HashMap::new(),
            dtors: HashMap::new(),
        }
    }
    pub fn push(&mut self, x: VVar<Ann>, t: TValue<Ann>) {
        self.vmap.insert(x, t);
    }
    pub fn extend(
        &mut self, other: impl IntoIterator<Item = (VVar<Ann>, TValue<Ann>)>,
    ) {
        self.vmap.extend(other);
    }
    pub fn lookup(&self, x: &VVar<Ann>) -> Option<&TValue<Ann>> {
        self.vmap.get(x)
    }
    pub fn decl(
        &mut self, d: &Declare<Ann>,
    ) -> Result<(), NameResolveError<Ann>> {
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
                self.codata.insert(name.clone(), dtors.clone()).map_or(
                    Ok(()),
                    |_| {
                        Err(NameResolveError::DuplicateDeclaration {
                            name: name.name().to_string(),
                            ann: ann.clone(),
                        })
                    },
                )?;
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
        }
    }
}
