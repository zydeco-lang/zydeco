//! Derive macros for Zydeco.
//!
//! Most features are pivoted to using `derive_more`, `impl-tools`, and `educe`.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(AsRefSelf)]
pub fn derive_as_ref_self(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let expanded = quote! {
        impl #impl_generics ::core::convert::AsRef<#name #ty_generics> for #name #ty_generics #where_clause {
            fn as_ref(&self) -> &#name #ty_generics {
                self
            }
        }
    };
    TokenStream::from(expanded)
}

#[proc_macro_derive(AsMutSelf)]
pub fn derive_as_mut_self(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let expanded = quote! {
        impl #impl_generics ::core::convert::AsMut<#name #ty_generics> for #name #ty_generics #where_clause {
            fn as_mut(&mut self) -> &mut #name #ty_generics {
                self
            }
        }
    };
    TokenStream::from(expanded)
}
