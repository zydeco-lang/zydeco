use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(SpanHolder)]
pub fn span_holder_derive(input: TokenStream) -> TokenStream {
    let mut res = TokenStream::new();
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let params = &input.generics.params.iter().collect::<Vec<_>>();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = if where_clause.is_none() {
        // Fixme: `where` is not working
        quote! { where #( #params: crate::utils::span::SpanHolder ),* }
        // quote! {}
    } else {
        quote! { #where_clause, #( #params: crate::utils::span::SpanHolder ),* }
    };
    let span_map_mut_body;
    match input.data {
        | Data::Enum(data) => {
            let mut arms = Vec::new();
            for variant in data.variants {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    | Fields::Unnamed(field) if field.unnamed.len() == 1 => {
                        arms.push(quote! {
                            #ident::#variant_ident(t) => t.span_map_mut(f)
                        });
                    }
                    | Fields::Unit => {
                        arms.push(quote! {
                            #ident::#variant_ident => {}
                        });
                    }
                    | Fields::Unnamed(_) | Fields::Named(_) => {
                        panic!("Only single-tuple variants can be derived")
                    }
                }
            }
            span_map_mut_body = quote!(
                match self {
                    #( #arms ),*
                }
            );
        }
        | Data::Struct(data) => match data.fields {
            | Fields::Named(fields) => {
                let field_idents: Vec<_> = (&fields.named).into_iter().map(|f| &f.ident).collect();
                span_map_mut_body = quote!(
                    #( self.#field_idents.span_map_mut(f.clone()) );*
                );
            }
            | Fields::Unnamed(fields) => {
                let idx: Vec<_> = (0..fields.unnamed.len()).map(syn::Index::from).collect();
                span_map_mut_body = quote!(
                    #( self.#idx.span_map_mut(f.clone()) );*
                );
            }
            | Fields::Unit => {
                span_map_mut_body = quote!();
            }
        },
        | _ => unreachable!("SpanHolder can only be derived for enums and structs"),
    }
    let gen = quote!(
        impl #impl_generics crate::utils::span::SpanHolder for #ident #ty_generics #where_clause
        {
            fn span_map_mut<F>(&mut self, f: F) where F: Fn(&mut crate::utils::span::Span) + Clone,
            { #span_map_mut_body }
        }
    );
    res.extend(TokenStream::from(gen));
    res
}

#[proc_macro_derive(FmtArgs)]
pub fn fmt_args_derive(input: TokenStream) -> TokenStream {
    let mut res = TokenStream::new();
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut variant_idents = Vec::new();
    let Data::Enum(data) = input.data else { panic!("Only enums can be derived") };
    for variant in data.variants {
        variant_idents.push(variant.ident);
        match &variant.fields {
            | Fields::Unnamed(field) if field.unnamed.len() == 1 => {}
            | Fields::Unnamed(_) | Fields::Named(_) | Fields::Unit => {
                panic!("Only single-tuple variants can be derived")
            }
        }
    }
    let gen = quote!(
        impl #impl_generics crate::utils::fmt::FmtArgs for #ident #ty_generics #where_clause {
            fn fmt_args(&self, fargs: crate::utils::fmt::Args) -> String {
                match self {
                    #( #ident::#variant_idents(t) => t.fmt_args(fargs) ),*
                }
            }
        }
    );
    res.extend(TokenStream::from(gen));
    res
}
