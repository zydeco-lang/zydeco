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

impl SpanHolder for syn::Abstraction {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Abstraction { params, body } = self;
        for param in params {
            param.span_map_mut(f.clone());
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
        def.span_map_mut(f);
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

impl SpanHolder for syn::TyAppTerm {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TyAppTerm { body, arg } = self;
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

impl SpanHolder for syn::Module {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Module { name: _, declarations } = self;
        declarations.span_map_mut(f);
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
