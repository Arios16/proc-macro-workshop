use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, Type};

fn unwrap_option(ty: &Type) -> Option<Type> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty
    {
        if segments.len() != 1 {
            return None;
        }
        let segment = segments.first().unwrap();
        if segment.ident != "Option" {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            ref args,
            ..
        }) = segment.arguments
        {
            if args.len() != 1 {
                return None;
            }
            let arg = args.first().unwrap();
            if let syn::GenericArgument::Type(ty) = arg {
                return Some(ty.clone());
            }
        }
    };
    None
}

struct StructField {
    ident: Ident,
    ty: Type,
    optional: bool,
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let builder_name = format_ident!("{}Builder", name);

    let fields = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields
                .named
                .into_iter()
                .map(|field| {
                    let ident = field.ident.unwrap();
                    let ty = field.ty;
                    if let Some(inner) = unwrap_option(&ty) {
                        StructField {
                            ident,
                            ty: inner,
                            optional: true,
                        }
                    } else {
                        StructField {
                            ident,
                            ty,
                            optional: false,
                        }
                    }
                })
                .collect::<Vec<_>>(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };
    let builder_fields = fields.iter().map(|StructField { ident, ty, .. }| {
        quote! {#ident: Option<#ty>}
    });
    let builder_fields_init = fields.iter().map(|StructField { ident, .. }| {
        quote! {#ident: None}
    });

    let builder_methods = fields.iter().map(|StructField { ident, ty, .. }| {
        quote! {
            pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                self.#ident=Some(#ident);
                self
            }
        }
    });

    let build_function_field_setting = fields.iter().map(|StructField { ident, optional, .. }| {
        if *optional {
            quote! {
                #ident: self.#ident.take()
            }
        } else {
            quote! {
                #ident: self.#ident.take().ok_or(concat!("Field ", stringify!(#ident), " not set"))?
            }
        }
    });

    let expanded = quote! {
        pub struct #builder_name{
            #(#builder_fields),*
        }

        impl #name {
            pub fn builder() -> #builder_name{
                #builder_name {
                    #(#builder_fields_init),*
                }
            }
        }
        impl #builder_name{
            #(#builder_methods)*
        }
        impl #builder_name{
            pub fn build(&mut self)->Result<#name, Box<dyn std::error::Error>>{
                let r = #name {
                    #(#build_function_field_setting),*
                };
                Ok(r)
            }
        }
    };
    expanded.into()
}
