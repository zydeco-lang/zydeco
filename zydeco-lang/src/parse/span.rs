use super::syntax::{self as syn, *};
use crate::utils::span::*;

impl SpanHolder for syn::TypeApp {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let syn::TypeApp(ref mut ty, ref mut args) = self;
    }
}
