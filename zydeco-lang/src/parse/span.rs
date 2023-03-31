use super::syntax as syn;
use crate::utils::span::*;

impl SpanHolder for syn::TypeApp {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TypeApp(ty1, ty2) = self;
        ty1.span_map_mut(f.clone());
        ty2.span_map_mut(f);
    }
}

impl SpanHolder for syn::Arrow {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Arrow(ty1, ty2) = self;
        ty1.span_map_mut(f.clone());
        ty2.span_map_mut(f);
    }
}

impl SpanHolder for syn::Forall {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Forall(params, ty) = self;
        for (ty, kd) in params {
            ty.span_map_mut(f.clone());
            kd.span_map_mut(f.clone());
        }
        ty.span_map_mut(f);
    }
}

impl SpanHolder for syn::Exists {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Exists(params, ty) = self;
        for (ty, kd) in params {
            ty.span_map_mut(f.clone());
            kd.span_map_mut(f.clone());
        }
        ty.span_map_mut(f);
    }
}

impl SpanHolder for syn::Type {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            syn::Type::Basic(ty) => ty.span_map_mut(f),
            syn::Type::App(ty) => ty.span_map_mut(f),
            syn::Type::Arrow(ty) => ty.span_map_mut(f),
            syn::Type::Forall(ty) => ty.span_map_mut(f),
            syn::Type::Exists(ty) => ty.span_map_mut(f),
            syn::Type::Hole(ty) => ty.span_map_mut(f),
        }
    }
}

impl SpanHolder for syn::TermValue {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            syn::TermValue::TermAnn(te) => te.span_map_mut(f),
            syn::TermValue::Var(te) => te.span_map_mut(f),
            syn::TermValue::Thunk(te) => te.span_map_mut(f),
            syn::TermValue::Ctor(te) => te.span_map_mut(f),
            syn::TermValue::Literal(te) => te.span_map_mut(f),
            syn::TermValue::Pack(te) => te.span_map_mut(f),
        }
    }
}

impl SpanHolder for syn::Abstraction {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Abstraction { params, body } = self;
        for (var, ty) in params {
            var.span_map_mut(f.clone());
            if let Some(ty) = ty {
                ty.span_map_mut(f.clone());
            }
        }
        body.span_map_mut(f);
    }
}

impl SpanHolder for syn::Application {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Application { body, arg } = self;
        body.span_map_mut(f.clone());
        arg.span_map_mut(f);
    }
}

impl SpanHolder for syn::GenLet {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::GenLet { rec: _, fun: _, name, params, def } = self;
        name.span_map_mut(f.clone());
        for (var, ty) in params {
            var.span_map_mut(f.clone());
            if let Some(ty) = ty {
                ty.span_map_mut(f.clone());
            }
        }
        def.span_map_mut(f.clone());
    }
}

impl SpanHolder for syn::Let {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Let { gen, body } = self;
        gen.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl SpanHolder for syn::TypAbs {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TypAbs { params, body } = self;
        for (var, kd) in params {
            var.span_map_mut(f.clone());
            kd.span_map_mut(f.clone());
        }
        body.span_map_mut(f);
    }
}

impl SpanHolder for syn::TypApp {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TypApp { body, arg } = self;
        body.span_map_mut(f.clone());
        arg.span_map_mut(f);
    }
}

impl SpanHolder for syn::MatchPack {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::MatchPack { scrut, tvar, var, body } = self;
        scrut.span_map_mut(f.clone());
        tvar.span_map_mut(f.clone());
        var.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl SpanHolder for syn::TermComputation {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            syn::TermComputation::TermAnn(te) => te.span_map_mut(f),
            syn::TermComputation::Ret(te) => te.span_map_mut(f),
            syn::TermComputation::Force(te) => te.span_map_mut(f),
            syn::TermComputation::Let(te) => te.span_map_mut(f),
            syn::TermComputation::Do(te) => te.span_map_mut(f),
            syn::TermComputation::Rec(te) => te.span_map_mut(f),
            syn::TermComputation::Match(te) => te.span_map_mut(f),
            syn::TermComputation::Abs(te) => te.span_map_mut(f),
            syn::TermComputation::App(te) => te.span_map_mut(f),
            syn::TermComputation::CoMatch(te) => te.span_map_mut(f),
            syn::TermComputation::Dtor(te) => te.span_map_mut(f),
            syn::TermComputation::TypAbs(te) => te.span_map_mut(f),
            syn::TermComputation::TypApp(te) => te.span_map_mut(f),
            syn::TermComputation::MatchPack(te) => te.span_map_mut(f),
        }
    }
}

impl SpanHolder for syn::Term {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            syn::Term::Value(te) => te.span_map_mut(f),
            syn::Term::Computation(te) => te.span_map_mut(f),
        }
    }
}

impl SpanHolder for syn::Declaration {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            syn::Declaration::Data(decl) => decl.span_map_mut(f),
            syn::Declaration::Codata(decl) => decl.span_map_mut(f),
            syn::Declaration::Define(decl) => decl.span_map_mut(f),
        }
    }
}

impl SpanHolder for syn::Module {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Module { name: _, declarations } = self;
        declarations.span_map_mut(f.clone());
    }
}

impl SpanHolder for syn::Program {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Program { module, entry } = self;
        module.span_map_mut(f.clone());
        entry.span_map_mut(f);
    }
}
