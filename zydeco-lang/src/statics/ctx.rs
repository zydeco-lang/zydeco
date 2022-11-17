use super::{err::TypeCheckError, resolve::*, tyck::TypeCheck};
use crate::{parse::syntax::*, utils::ann::AnnT};
use std::collections::HashMap;

#[derive(Clone, Debug)]
enum Sort {
    TVal,
    TComp,
}

#[derive(Clone, Debug)]
pub struct Ctx<Ann> {
    vmap: HashMap<VVar<Ann>, TValue<Ann>>,
    tmap: HashMap<TVar<Ann>, Sort>,
    data: HashMap<TVar<Ann>, Vec<(Ctor<Ann>, Vec<TValue<Ann>>)>>,
    pub ctors: HashMap<Ctor<Ann>, (TVar<Ann>, Vec<TValue<Ann>>)>,
    codata:
        HashMap<TVar<Ann>, Vec<(Dtor<Ann>, Vec<TValue<Ann>>, TCompute<Ann>)>>,
    pub dtors: HashMap<Dtor<Ann>, (TVar<Ann>, Vec<TValue<Ann>>, TCompute<Ann>)>,
}

impl<Ann: AnnT> Ctx<Ann> {
    pub fn new() -> Self {
        Self {
            vmap: HashMap::new(),
            tmap: HashMap::new(),
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
                self.codata.insert(name.clone(), dtors.clone()).map_or(
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
            Declare::Define { .. } => todo!(),
        }
    }
    pub fn tyck(&self) -> Result<(), TypeCheckError<Ann>> {
        for (_, ctors) in &self.data {
            for (_, args) in ctors {
                for arg in args {
                    arg.tyck(self)?;
                }
            }
        }
        for (_, dtors) in &self.codata {
            for (_, args, ret) in dtors {
                for arg in args {
                    arg.tyck(self)?;
                }
                ret.tyck(self)?;
            }
        }
        Ok(())
    }
}

impl<Ann: AnnT> TypeCheck<Ann> for TValue<Ann> {
    type Type = ();
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            TValue::Var(x, ann) => ctx.tmap.get(x).map_or(
                Err(TypeCheckError::NameResolve(
                    NameResolveError::UnknownIdentifier {
                        name: x.name().to_owned(),
                        ann: ann.clone(),
                    },
                )),
                |sort| {
                    if let Sort::TVal = sort {
                        Ok(())
                    } else {
                        Err(TypeCheckError::Explosion(format!(
                            "expect value type, found computation type"
                        )))
                    }
                },
            ),
            TValue::Comp(_, _)
            | TValue::Bool(_)
            | TValue::Int(_)
            | TValue::String(_)
            | TValue::Char(_)
            | TValue::Unit(_) => Ok(()),
        }
    }
}

impl<Ann: AnnT> TypeCheck<Ann> for TCompute<Ann> {
    type Type = ();
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            TCompute::Var(x, ann) => ctx.tmap.get(x).map_or(
                Err(TypeCheckError::NameResolve(
                    NameResolveError::UnknownIdentifier {
                        name: x.name().to_owned(),
                        ann: ann.clone(),
                    },
                )),
                |sort| {
                    if let Sort::TComp = sort {
                        Ok(())
                    } else {
                        Err(TypeCheckError::Explosion(format!(
                            "expect value type, found computation type"
                        )))
                    }
                },
            ),
            TCompute::Ret(_, _) | TCompute::Lam(_, _, _) | TCompute::Os => {
                Ok(())
            }
        }
    }
}
