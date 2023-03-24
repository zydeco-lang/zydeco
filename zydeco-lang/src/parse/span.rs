use super::syntax as syn;
use crate::utils::span::*;

impl SpanHolder for syn::TypeApp {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TypeApp(ref mut ty1, ref mut ty2) = self;
        ty1.span_map_mut(f.clone());
        ty2.span_map_mut(f);
    }
}

impl SpanHolder for syn::Arrow {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Arrow(ref mut ty1, ref mut ty2) = self;
        ty1.span_map_mut(f.clone());
        ty2.span_map_mut(f);
    }
}

impl SpanHolder for syn::Forall {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::Forall(ref mut params, ref mut ty) = self;
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
        let syn::Exists(ref mut params, ref mut ty) = self;
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
            syn::Type::Basic(ref mut ty) => ty.span_map_mut(f),
            syn::Type::App(ref mut ty) => ty.span_map_mut(f),
            syn::Type::Arrow(ref mut ty) => ty.span_map_mut(f),
            syn::Type::Forall(ref mut ty) => ty.span_map_mut(f),
            syn::Type::Exists(ref mut ty) => ty.span_map_mut(f),
        }
    }
}
