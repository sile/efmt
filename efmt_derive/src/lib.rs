// See: https://github.com/dtolnay/syn/blob/master/examples/heapsize/heapsize_derive/src/lib.rs
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics};

#[proc_macro_derive(Region)]
pub fn derive_region_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = add_region_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let region = generate_region_method_body(&input.data);
    let expanded = quote! {
        impl #impl_generics crate::token::Region for #name #ty_generics #where_clause {
            fn region(&self) -> crate::token::TokenRegion {
                #region
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn add_region_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(crate::token::Region));
        }
    }
    generics
}

fn generate_region_method_body(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => match (fields.named.first(), fields.named.last()) {
                (Some(start), Some(end)) => {
                    let start_name = &start.ident;
                    let end_name = &end.ident;
                    quote! {
                        crate::token::TokenRegion::new(
                            crate::token::Region::region(&self.#start_name).start(),
                            crate::token::Region::region(&self.#end_name).end()
                        )
                    }
                }
                _ => unimplemented!(),
            },
            Fields::Unnamed(ref fields) => {
                assert_eq!(fields.unnamed.len(), 1);
                quote! { self.0.region() }
            }
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(ref data) => {
            let arms = data.variants.iter().map(|variant| {
                let name = &variant.ident;
                if let Fields::Unnamed(fields) = &variant.fields {
                    assert_eq!(fields.unnamed.len(), 1);
                } else {
                    unimplemented!();
                }
                quote_spanned! { variant.span() => Self::#name(x) => x.region(), }
            });
            quote! {
                match self {
                    #(#arms)*
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    }
}

#[proc_macro_derive(Parse)]
pub fn derive_parse_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = add_parse_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let parse = generate_parse_fun_body(&input.data);
    let expanded = quote! {
        impl #impl_generics crate::parse::Parse for #name #ty_generics #where_clause {
            fn parse(parser: &mut crate::parse::Parser) -> crate::parse::Result<Self> {
                #parse
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn add_parse_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(crate::parse::Parse));
        }
    }
    generics
}

fn generate_parse_fun_body(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let parse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span() => #name: crate::parse::Parse::parse(parser)? }
                });
                quote! {
                    Ok(Self{
                        #(#parse ,)*
                    })
                }
            }
            Fields::Unnamed(ref fields) => {
                assert_eq!(fields.unnamed.len(), 1);
                quote! { parser.parse().map(Self) }
            }
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(ref data) => {
            let arms = data.variants.iter().map(|variant| {
                let name = &variant.ident;
                if let Fields::Unnamed(fields) = &variant.fields {
                    assert_eq!(fields.unnamed.len(), 1);
                } else {
                    unimplemented!();
                }
                quote_spanned! { variant.span() =>  Ok(Self::#name(x)) }
            });
            quote! {
                #( if let Some(x) = parser.try_parse() { #arms } else )*
                {
                    Err(parser.take_last_error().expect("unreachable"))
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    }
}

#[proc_macro_derive(Format)]
pub fn derive_format_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = add_format_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let format = generate_format_method_body(&input.data);
    let expanded = quote! {
        impl #impl_generics crate::format::Format for #name #ty_generics #where_clause {
            fn format<W: ::std::io::Write>(&self, fmt: &mut crate::format::Formatter<W>) -> crate::format::Result<()> {
                #format
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn add_format_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(crate::format::Format));
        }
    }
    generics
}

fn generate_format_method_body(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let format = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span() => fmt.format(&self.#name)? }
                });
                quote! {
                    #(#format ;)*
                    Ok(())
                }
            }
            Fields::Unnamed(ref fields) => {
                assert_eq!(fields.unnamed.len(), 1);
                quote! { self.0.format(fmt) }
            }
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(ref data) => {
            let arms = data.variants.iter().map(|variant| {
                let name = &variant.ident;
                if let Fields::Unnamed(fields) = &variant.fields {
                    assert_eq!(fields.unnamed.len(), 1);
                } else {
                    unimplemented!();
                }
                quote_spanned! { variant.span() => Self::#name(x) => x.format(fmt), }
            });
            quote! {
                match self {
                    #(#arms)*
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    }
}
