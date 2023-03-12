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
                let field = match &variant.fields {
                    Fields::Unnamed(field) => field.unnamed.first().expect("EnumGenerator can only be derived for enum variants with one field"),
                    _ => unreachable!("EnumGenerator can only be derived for enum variants with one field"),
                };
                let dom = &field.ty;
                let gen = quote! {
                    impl From<#dom> for #cod {
                        fn from(item: #dom) -> Self {
                            #cod::#variant_ident(item)
                        }
                    }
                };
                res.extend(TokenStream::from(gen));
            }
        }
        _ => unreachable!("EnumGenerator can only be derived for enums"),
    }
    res
}
