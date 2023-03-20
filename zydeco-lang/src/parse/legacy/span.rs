use super::syntax::{Codata, Compute, Data, Declare, Program, Type, Value};
use crate::syntax::span::{SpanHolder, SpanInfo};

impl SpanHolder for Program {
    fn span(&self) -> &SpanInfo {
        &self.ann
    }
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        (f.clone())(&mut self.ann);
        for d in &mut self.decls {
            d.span_map_mut(f.clone());
        }
        self.comp.span_map_mut(f);
    }
}

impl SpanHolder for Declare {
    fn span(&self) -> &SpanInfo {
        match self {
            Declare::Data(Data { ann, .. }) => ann,
            Declare::Codata(Codata { ann, .. }) => ann,
            Declare::Define { ann, .. } => ann,
        }
    }
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            Declare::Data(Data { ann, ctors, params, .. }) => {
                (f.clone())(ann);
                for (v, _) in params {
                    v.span_map_mut(f.clone());
                }
                for (_, args) in ctors {
                    for a in args {
                        a.span_map_mut(f.clone());
                    }
                }
            }
            Declare::Codata(Codata { params, ann, dtors, .. }) => {
                (f.clone())(ann);
                for (v, _) in params {
                    v.span_map_mut(f.clone());
                }
                for (_, (args, ret)) in dtors {
                    for a in args {
                        a.span_map_mut(f.clone());
                    }
                    ret.span_map_mut(f.clone());
                }
            }
            Declare::Define { ty, def, ann, .. } => {
                f(ann);
                if let Some(ty) = ty {
                    ty.span_map_mut(f.clone());
                }
                if let Some(def) = def {
                    def.span_map_mut(f.clone());
                }
            }
        }
    }
}

impl SpanHolder for Type {
    fn span(&self) -> &SpanInfo {
        &self.ann
    }
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        f(&mut self.ann);
        for a in &mut self.args {
            a.span_map_mut(f.clone());
        }
    }
}

impl SpanHolder for Value {
    fn span(&self) -> &SpanInfo {
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
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            Value::TermAnn(v, t, a) => {
                f(a);
                v.span_map_mut(f.clone());
                t.span_map_mut(f);
            }
            Value::Var(v, a) => {
                f(a);
                v.span_map_mut(f);
            }
            Value::Thunk(c, a) => {
                f(a);
                c.span_map_mut(f);
            }
            Value::Ctor(c, vs, a) => {
                f(a);
                c.span_map_mut(f.clone());
                for v in vs {
                    v.span_map_mut(f.clone());
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

impl SpanHolder for Compute {
    fn span(&self) -> &SpanInfo {
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
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            Compute::TermAnn(c, t, a) => {
                f(a);
                c.span_map_mut(f.clone());
                t.span_map_mut(f);
            }
            Compute::Let { binding: (v, t, def), body, ann } => {
                f(ann);
                v.span_map_mut(f.clone());
                if let Some(t) = t {
                    t.span_map_mut(f.clone());
                }
                def.span_map_mut(f.clone());
                body.span_map_mut(f);
            }
            Compute::Do { binding: (v, t, def), body, ann } => {
                f(ann);
                v.span_map_mut(f.clone());
                if let Some(t) = t {
                    t.span_map_mut(f.clone());
                }
                def.span_map_mut(f.clone());
                body.span_map_mut(f);
            }
            Compute::Force(v, a) => {
                f(a);
                v.span_map_mut(f);
            }
            Compute::App(c, v, a) => {
                f(a);
                c.span_map_mut(f.clone());
                v.span_map_mut(f.clone());
            }
            Compute::Lam { arg: (v, t), body, ann } => {
                f(ann);
                v.span_map_mut(f.clone());
                if let Some(t) = t {
                    t.span_map_mut(f.clone());
                }
                body.span_map_mut(f);
            }
            Compute::Return(v, a) => {
                f(a);
                v.span_map_mut(f);
            }
            Compute::Rec { arg: (v, t), body, ann } => {
                f(ann);
                v.span_map_mut(f.clone());
                if let Some(t) = t {
                    t.span_map_mut(f.clone());
                }
                body.span_map_mut(f);
            }
            Compute::Match { scrut, arms: cases, ann } => {
                f(ann);
                scrut.span_map_mut(f.clone());
                for (ctor, vs, c) in cases {
                    ctor.span_map_mut(f.clone());
                    for v in vs {
                        v.span_map_mut(f.clone());
                    }
                    c.span_map_mut(f.clone());
                }
            }
            Compute::CoMatch { arms: cases, ann } => {
                f(ann);
                for (dtor, vs, c) in cases {
                    dtor.span_map_mut(f.clone());
                    for v in vs {
                        v.span_map_mut(f.clone());
                    }
                    c.span_map_mut(f.clone());
                }
            }
            Compute::CoApp { body, dtor, args, ann } => {
                (f.clone())(ann);
                body.span_map_mut(f.clone());
                dtor.span_map_mut(f.clone());
                for arg in args {
                    arg.span_map_mut(f.clone());
                }
            }
        }
    }
}
