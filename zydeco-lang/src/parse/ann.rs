use crate::{
    parse::syntax::{Codata, Compute, Data, Declare, Program, Type, Value},
    syntax::ann::{AnnHolder, AnnInfo},
};

impl AnnHolder for Program {
    fn ann(&self) -> &AnnInfo {
        &self.ann
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        (f.clone())(&mut self.ann);
        for d in &mut self.decls {
            d.ann_map_mut(f.clone());
        }
        self.comp.ann_map_mut(f);
    }
}

impl AnnHolder for Declare {
    fn ann(&self) -> &AnnInfo {
        match self {
            Declare::Data(Data { ann, .. }) => ann,
            Declare::Codata(Codata { ann, .. }) => ann,
            Declare::Define { ann, .. } => ann,
        }
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        match self {
            Declare::Data(Data { ann, ctors, params, .. }) => {
                (f.clone())(ann);
                for (v, _) in params {
                    v.ann_map_mut(f.clone());
                }
                for (_, args) in ctors {
                    for a in args {
                        a.ann_map_mut(f.clone());
                    }
                }
            }
            Declare::Codata(Codata { params, ann, dtors, .. }) => {
                (f.clone())(ann);
                for (v, _) in params {
                    v.ann_map_mut(f.clone());
                }
                for (_, args, ret) in dtors {
                    for a in args {
                        a.ann_map_mut(f.clone());
                    }
                    ret.ann_map_mut(f.clone());
                }
            }
            Declare::Define { ty, def, ann, .. } => {
                f(ann);
                if let Some(ty) = ty {
                    ty.ann_map_mut(f.clone());
                }
                if let Some(def) = def {
                    def.ann_map_mut(f.clone());
                }
            }
        }
    }
}

impl AnnHolder for Type {
    fn ann(&self) -> &AnnInfo {
        &self.ann
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        f(&mut self.ann);
        for a in &mut self.args {
            a.ann_map_mut(f.clone());
        }
    }
}

impl AnnHolder for Value {
    fn ann(&self) -> &AnnInfo {
        match self {
            Value::TermAnn(_, _, a) => a,
            Value::Var(_, a) => a,
            Value::Thunk(_, a) => a,
            Value::Ctor(_, _, a) => a,
            Value::Int(_, a) => a,
            Value::String(_, a) => a,
            Value::Char(_, a) => a,
        }
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        match self {
            Value::TermAnn(v, t, a) => {
                f(a);
                v.ann_map_mut(f.clone());
                t.ann_map_mut(f);
            }
            Value::Var(v, a) => {
                f(a);
                v.ann_map_mut(f);
            }
            Value::Thunk(c, a) => {
                f(a);
                c.ann_map_mut(f);
            }
            Value::Ctor(c, vs, a) => {
                f(a);
                c.ann_map_mut(f.clone());
                for v in vs {
                    v.ann_map_mut(f.clone());
                }
            }
            Value::Int(_, a) => {
                f(a);
            }
            Value::String(_, a) => {
                f(a);
            }
            Value::Char(_, a) => {
                f(a);
            }
        }
    }
}

impl AnnHolder for Compute {
    fn ann(&self) -> &AnnInfo {
        match self {
            Compute::TermAnn(_, _, a) => a,
            Compute::Let { ann, .. } => ann,
            Compute::Do { ann, .. } => ann,
            Compute::Force(_, a) => a,
            Compute::App(_, _, a) => a,
            Compute::Lam { ann, .. } => ann,
            Compute::Return(_, a) => a,
            Compute::Rec { ann, .. } => ann,
            Compute::Match { ann, .. } => ann,
            Compute::CoMatch { ann, .. } => ann,
            Compute::CoApp { ann, .. } => ann,
        }
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        match self {
            Compute::TermAnn(c, t, a) => {
                f(a);
                c.ann_map_mut(f.clone());
                t.ann_map_mut(f);
            }
            Compute::Let { binding: (v, t, def), body, ann } => {
                f(ann);
                v.ann_map_mut(f.clone());
                if let Some(t) = t {
                    t.ann_map_mut(f.clone());
                }
                def.ann_map_mut(f.clone());
                body.ann_map_mut(f);
            }
            Compute::Do { binding: (v, t, def), body, ann } => {
                f(ann);
                v.ann_map_mut(f.clone());
                if let Some(t) = t {
                    t.ann_map_mut(f.clone());
                }
                def.ann_map_mut(f.clone());
                body.ann_map_mut(f);
            }
            Compute::Force(v, a) => {
                f(a);
                v.ann_map_mut(f);
            }
            Compute::App(c, v, a) => {
                f(a);
                c.ann_map_mut(f.clone());
                v.ann_map_mut(f.clone());
            }
            Compute::Lam { arg: (v, t), body, ann } => {
                f(ann);
                v.ann_map_mut(f.clone());
                if let Some(t) = t {
                    t.ann_map_mut(f.clone());
                }
                body.ann_map_mut(f);
            }
            Compute::Return(v, a) => {
                f(a);
                v.ann_map_mut(f);
            }
            Compute::Rec { arg: (v, t), body, ann } => {
                f(ann);
                v.ann_map_mut(f.clone());
                if let Some(t) = t {
                    t.ann_map_mut(f.clone());
                }
                body.ann_map_mut(f);
            }
            Compute::Match { scrut, arms: cases, ann } => {
                f(ann);
                scrut.ann_map_mut(f.clone());
                for (ctor, vs, c) in cases {
                    ctor.ann_map_mut(f.clone());
                    for v in vs {
                        v.ann_map_mut(f.clone());
                    }
                    c.ann_map_mut(f.clone());
                }
            }
            Compute::CoMatch { arms: cases, ann } => {
                f(ann);
                for (dtor, vs, c) in cases {
                    dtor.ann_map_mut(f.clone());
                    for v in vs {
                        v.ann_map_mut(f.clone());
                    }
                    c.ann_map_mut(f.clone());
                }
            }
            Compute::CoApp { body, dtor, args, ann } => {
                (f.clone())(ann);
                body.ann_map_mut(f.clone());
                dtor.ann_map_mut(f.clone());
                for arg in args {
                    arg.ann_map_mut(f.clone());
                }
            }
        }
    }
}
