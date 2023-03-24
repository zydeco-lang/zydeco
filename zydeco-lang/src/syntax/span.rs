use super::*;
use crate::utils::span::*;

impl<K> SpanHolder for TypeArity<K>
where
    K: KindT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TypeArity { ref mut params, ref mut kd } = self;
        for param in params {
            param.span_map_mut(f.clone());
        }
        kd.span_map_mut(f);
    }
}

impl<Type, Kind> SpanHolder for TypeAnn<Type, Kind>
where
    Type: SpanHolder,
    Kind: SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TypeAnn { ref mut ty, ref mut kd } = self;
        ty.span_map_mut(f.clone());
        kd.span_map_mut(f);
    }
}

impl SpanHolder for TCtor {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        match self {
            TCtor::Var(ref mut var) => var.span_map_mut(f),
            _ => (),
        }
    }
}

impl<TyV, T> SpanHolder for TypeApp<TyV, T>
where
    TyV: SpanHolder,
    T: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TypeApp { ref mut tctor, ref mut args } = self;
        tctor.span_map_mut(f.clone());
        for arg in args {
            arg.span_map_mut(f.clone());
        }
    }
}

impl<Term, Type> SpanHolder for TermAnn<Term, Type>
where
    Term: SpanHolder,
    Type: SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TermAnn { ref mut term, ref mut ty } = self;
        term.span_map_mut(f.clone());
        ty.span_map_mut(f);
    }
}
