//! Derive macros for Zydeco.
//!
//! Most features are pivoted to using `derive_more`, `impl-tools`, and `educe`.

use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

mod token_metadata;

/// Derive payload-free token kinds and metadata from Logos token declarations.
///
/// `#[token_metadata(kind = TokenKind)]` names the generated companion enum.
/// Variants may select a registered `canonical = "spelling"`, override their
/// `parser = "terminal name"`, or `skip` parser expectations. Multiple fixed
/// spellings require an explicit canonical choice. The consumer needs `strum`
/// for the generated kind's `VariantArray` implementation.
#[proc_macro_derive(TokenMetadata, attributes(token_metadata))]
pub fn derive_token_metadata(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    token_metadata::TokenMetadata::parse(&input)
        .map(|metadata| metadata.expand())
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

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
