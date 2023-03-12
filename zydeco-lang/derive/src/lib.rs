use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(EnumGenerator)]
pub fn enum_generator_derive(input: TokenStream) -> TokenStream {
    let mut res = TokenStream::new();
    let input = parse_macro_input!(input as DeriveInput);
    let cod = input.ident;
    match input.data {
        Data::Enum(data) => {
            for variant in &data.variants {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    Fields::Unnamed(field) => {
                        if field.unnamed.len() == 1 {
                            let dom = &field.unnamed.first().expect("EnumGenerator can only be derived for enum variants with one field").ty;
                            let gen = quote! {
                                impl From<#dom> for #cod {
                                    fn from(item: #dom) -> Self {
                                        #cod::#variant_ident(item)
                                    }
                                }
                            };
                            res.extend(TokenStream::from(gen));
                        } else {
                            let dom: Vec<_> = (&field.unnamed)
                                .into_iter()
                                .map(|f| &f.ty)
                                .collect();
                            let idx: Vec<_> = (0..dom.len())
                                .map(|i| syn::Index::from(i))
                                .collect();
                            let gen = quote! {
                                impl From<(#(#dom),*)> for #cod {
                                    fn from(item: (#(#dom),*)) -> Self {
                                        #cod::#variant_ident(#(item.#idx),*)
                                    }
                                }
                            };
                            res.extend(TokenStream::from(gen));
                        }
                    }
                    Fields::Named(field) => {
                        if field.named.len() == 1 {
                            let field = field.named.first().expect("EnumGenerator can only be derived for enum variants with one field");
                            let field_ident = &field.ident;

                            let dom = &field.ty;
                            let gen = quote! {
                                impl From<#dom> for #cod {
                                    fn from(item: #dom) -> Self {
                                        #cod::#variant_ident { #field_ident: item }
                                    }
                                }
                            };
                            res.extend(TokenStream::from(gen));
                        } else {
                            let dom: Vec<_> = (&field.named)
                                .into_iter()
                                .map(|f| &f.ty)
                                .collect();
                            let idx: Vec<_> = (0..dom.len())
                                .map(|i| syn::Index::from(i))
                                .collect();
                            let field_idents: Vec<_> = (&field.named)
                                .into_iter()
                                .map(|f| &f.ident)
                                .collect();
                            let gen = quote! {
                                impl From<(#(#dom),*)> for #cod {
                                    fn from(item: (#(#dom),*)) -> Self {
                                        #cod::#variant_ident { #( #field_idents: item.#idx ),* }
                                    }
                                }
                            };
                            res.extend(TokenStream::from(gen));
                        }
                    }
                    Fields::Unit => {
                        let gen = quote! {
                            impl From<()> for #cod {
                                fn from(_: ()) -> Self {
                                    #cod::#variant_ident
                                }
                            }
                        };
                        res.extend(TokenStream::from(gen));
                    }
                };
            }
        }
        _ => unreachable!("EnumGenerator can only be derived for enums"),
    }
    res
}
