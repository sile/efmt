// See: https://github.com/dtolnay/syn/blob/master/examples/heapsize/heapsize_derive/src/lib.rs
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics};

#[proc_macro_derive(Region)]
pub fn derive_region_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    // Used in the quasi-quotation below as `#name`.
    let name = input.ident;

    // Add a bound `T: Region` to every type parameter T.
    let generics = add_region_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Generate an expression to sum up the heap size of each field.
    let region = token_region(&input.data);

    let expanded = quote! {
        // The generated impl.
        impl #impl_generics crate::token::Region for #name #ty_generics #where_clause {
            fn region(&self) -> crate::token::TokenRegion {
                #region
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

// Add a bound `T: Region` to every type parameter T.
fn add_region_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(crate::token::Region));
        }
    }
    generics
}

// Generate an expression to calculate the region of the struct.
fn token_region(data: &Data) -> TokenStream {
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
