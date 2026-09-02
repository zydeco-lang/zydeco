use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeSet;
use syn::{Data, DeriveInput, Fields, Ident, LitStr, Token, Variant};

pub(super) struct TokenMetadata<'input> {
    input: &'input DeriveInput,
    kind: Ident,
    variants: Vec<TokenVariant<'input>>,
}

impl<'input> TokenMetadata<'input> {
    pub(super) fn parse(input: &'input DeriveInput) -> syn::Result<Self> {
        let Data::Enum(data) = &input.data else {
            return Err(syn::Error::new_spanned(input, "TokenMetadata requires an enum"));
        };
        if data.variants.is_empty() {
            return Err(syn::Error::new_spanned(input, "a token enum must have variants"));
        }
        let mut kind = None;
        for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("token_metadata")) {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("kind") && kind.is_none() {
                    kind = Some(meta.value()?.parse::<Ident>()?);
                    Ok(())
                } else {
                    Err(meta.error("expected one `kind = Name` setting"))
                }
            })?;
        }
        let kind = kind.unwrap_or_else(|| format_ident!("{}Kind", input.ident));
        if kind == input.ident {
            return Err(syn::Error::new_spanned(kind, "the token and kind names must differ"));
        }
        let variants =
            data.variants.iter().map(TokenVariant::parse).collect::<syn::Result<Vec<_>>>()?;
        let mut names = BTreeSet::new();
        for terminal in variants.iter().filter_map(|variant| variant.terminal.as_ref()) {
            if !names.insert(terminal.value()) {
                return Err(syn::Error::new_spanned(terminal, "duplicate parser terminal name"));
            }
        }
        Ok(Self { input, kind, variants })
    }

    pub(super) fn expand(&self) -> TokenStream {
        let Self { input, kind, variants } = self;
        let name = &input.ident;
        let visibility = &input.vis;
        let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
        let declarations = variants.iter().map(|variant| {
            let ident = &variant.variant.ident;
            let attrs = variant.cfg_attributes();
            quote!(#(#attrs)* #ident,)
        });
        let conversions = variants.iter().map(|variant| {
            let ident = &variant.variant.ident;
            let attrs = variant.cfg_attributes();
            let fields = match variant.variant.fields {
                | Fields::Unit => quote!(),
                | Fields::Unnamed(_) => quote!((..)),
                | Fields::Named(_) => quote!({ .. }),
            };
            quote!(#(#attrs)* Self::#ident #fields => #kind::#ident,)
        });
        let spellings = variants.iter().map(|variant| {
            let ident = &variant.variant.ident;
            let attrs = variant.cfg_attributes();
            let spellings = &variant.spellings;
            quote!(#(#attrs)* Self::#ident => &[#(#spellings),*],)
        });
        let canonical = variants.iter().map(|variant| variant.optional_arm(&variant.canonical));
        let terminals = variants.iter().map(|variant| variant.optional_arm(&variant.terminal));
        let from_name = variants.iter().filter_map(|variant| {
            let terminal = variant.terminal.as_ref()?;
            let ident = &variant.variant.ident;
            let attrs = variant.cfg_attributes();
            Some(quote!(#(#attrs)* #terminal => ::core::option::Option::Some(Self::#ident),))
        });
        let display = variants.iter().map(|variant| {
            let ident = &variant.variant.ident;
            let attrs = variant.cfg_attributes();
            let label = variant
                .terminal
                .as_ref()
                .or(variant.canonical.as_ref())
                .cloned()
                .unwrap_or_else(|| LitStr::new(&ident.to_string(), ident.span()));
            quote!(#(#attrs)* Self::#ident => formatter.write_str(#label),)
        });
        quote! {
            /// A lexical token category without a source-text payload.
            #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, ::strum::VariantArray)]
            #visibility enum #kind { #(#declarations)* }

            impl #impl_generics #name #type_generics #where_clause {
                /// Erase this token's payload, preserving its lexical category.
                pub const fn kind(&self) -> #kind {
                    match self { #(#conversions)* }
                }
            }

            impl #kind {
                /// Every fixed spelling recognized for this token, including aliases.
                pub const fn source_spellings(self) -> &'static [&'static str] {
                    match self { #(#spellings)* }
                }

                /// The chosen fixed spelling, or `None` for a variable lexical category.
                pub const fn source_spelling(self) -> ::core::option::Option<&'static str> {
                    match self { #(#canonical)* }
                }

                /// Its grammar terminal name; trivia and malformed tokens return `None`.
                pub const fn parser_name(self) -> ::core::option::Option<&'static str> {
                    match self { #(#terminals)* }
                }

                /// Decode an unquoted grammar terminal name at the parser boundary.
                pub(crate) fn from_parser_name(name: &str) -> ::core::option::Option<Self> {
                    match name { #(#from_name)* _ => ::core::option::Option::None }
                }
            }

            impl ::core::fmt::Display for #kind {
                fn fmt(&self, formatter: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    match self { #(#display)* }
                }
            }
        }
    }
}

struct TokenVariant<'input> {
    variant: &'input Variant,
    spellings: Vec<LitStr>,
    canonical: Option<LitStr>,
    terminal: Option<LitStr>,
}

impl<'input> TokenVariant<'input> {
    fn parse(variant: &'input Variant) -> syn::Result<Self> {
        let mut canonical = None;
        let mut parser = None;
        let mut skip = false;
        for attr in variant.attrs.iter().filter(|attr| attr.path().is_ident("token_metadata")) {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("canonical") && canonical.is_none() {
                    canonical = Some(meta.value()?.parse::<LitStr>()?);
                } else if meta.path.is_ident("parser") && parser.is_none() {
                    parser = Some(meta.value()?.parse::<LitStr>()?);
                } else if meta.path.is_ident("skip") && !skip {
                    skip = true;
                } else {
                    return Err(meta.error("unknown or duplicate token metadata setting"));
                }
                Ok(())
            })?;
        }
        if skip && parser.is_some() {
            return Err(syn::Error::new_spanned(
                variant,
                "a skipped token cannot name a parser terminal",
            ));
        }
        let spellings = variant
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("token"))
            .map(|attr| {
                attr.parse_args_with(|input: syn::parse::ParseStream<'_>| {
                    let spelling: LitStr = input.parse()?;
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                        // Callbacks and priority options belong to Logos, not token metadata.
                        let _: TokenStream = input.parse()?;
                    }
                    if spelling.value().is_empty() {
                        return Err(syn::Error::new_spanned(
                            spelling,
                            "token spellings must not be empty",
                        ));
                    }
                    Ok(spelling)
                })
            })
            .collect::<syn::Result<Vec<_>>>()?;
        let canonical = match (canonical, spellings.as_slice()) {
            | (Some(choice), _) => {
                if !spellings.iter().any(|spelling| spelling.value() == choice.value()) {
                    return Err(syn::Error::new_spanned(
                        choice,
                        "canonical spelling must match a #[token] attribute",
                    ));
                }
                Some(choice)
            }
            | (None, []) => None,
            | (None, [spelling]) => Some(spelling.clone()),
            | (None, _) => {
                return Err(syn::Error::new_spanned(
                    variant,
                    "multiple token spellings require a canonical choice",
                ));
            }
        };
        let terminal = if skip {
            None
        } else {
            let terminal = parser
                .or_else(|| canonical.clone())
                .unwrap_or_else(|| LitStr::new(&variant.ident.to_string(), variant.ident.span()));
            if terminal.value().is_empty() {
                return Err(syn::Error::new_spanned(
                    terminal,
                    "parser terminal names must not be empty",
                ));
            }
            Some(terminal)
        };
        Ok(Self { variant, spellings, canonical, terminal })
    }

    fn cfg_attributes(&self) -> impl Iterator<Item = &syn::Attribute> {
        self.variant.attrs.iter().filter(|attr| attr.path().is_ident("cfg"))
    }

    fn optional_arm(&self, value: &Option<LitStr>) -> TokenStream {
        let ident = &self.variant.ident;
        let attrs = self.cfg_attributes();
        let value = match value {
            | Some(value) => quote!(::core::option::Option::Some(#value)),
            | None => quote!(::core::option::Option::None),
        };
        quote!(#(#attrs)* Self::#ident => #value,)
    }
}

#[cfg(test)]
mod tests;
