use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields, Ident, Meta, Type};

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

fn get_inner_type(ty: &Type) -> Option<Type> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty
    {
        if segments.len() != 1 {
            return None;
        }
        let segment = segments.first().unwrap();
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

fn get_append_fn(f: &Field) -> Result<Option<Ident>, syn::Error> {
    assert!(f.attrs.len() <= 1);
    if let Some(attr) = f.attrs.first() {
        let meta = attr.parse_meta().unwrap();
        let ident = meta.path().segments[0].ident.clone();
        assert!(ident == "builder");
        match meta {
            Meta::List(ref metalist) => {
                assert!(metalist.nested.len() == 1);
                match &metalist.nested[0] {
                    syn::NestedMeta::Meta(inner_meta) => {
                        assert!(inner_meta.path().segments.len() == 1);
                        if inner_meta.path().segments[0].ident != "each" {
                            return Err(syn::Error::new_spanned(
                                meta,
                                "expected `builder(each = \"...\")`",
                            ));
                        }
                        match inner_meta {
                            Meta::NameValue(name_value) => match &name_value.lit {
                                syn::Lit::Str(strlit) => {
                                    return Ok(Some(syn::Ident::new(
                                        &strlit.value(),
                                        ident.span(),
                                    )));
                                }
                                _ => panic!("Expected literal string as argument to each"),
                            },
                            _ => panic!("Wrong syntax on builder attribute"),
                        }
                    }
                    _ => panic!("Wrong syntax on builder attribute"),
                }
            }
            _ => panic!("Wrong syntax on builder attribute"),
        }
    }
    Ok(None)
}

#[derive(Debug)]
struct StructField {
    ident: Ident,
    ty: Type,
    optional: bool,
    append_fn: Option<Ident>,
}

#[proc_macro_derive(Builder, attributes(builder))]
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
                    let append_fn = match get_append_fn(&field) {
                        Ok(v) => v,
                        Err(s) => return Err(s),
                    };
                    let ident = field.ident.unwrap();
                    let ty = field.ty;
                    let (ty, optional) = match unwrap_option(&ty) {
                        Some(inner) => (inner, true),
                        None => (ty, false),
                    };
                    let ty = if append_fn.is_some() {
                        get_inner_type(&ty).unwrap()
                    } else {
                        ty
                    };
                    Ok(StructField {
                        ident,
                        ty,
                        optional,
                        append_fn: append_fn,
                    })
                })
                .collect::<Result<Vec<_>, _>>(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };
    let fields = match fields {
        Ok(v) => v,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };
    let builder_fields = fields.iter().map(
        |StructField {
             ident,
             ty,
             append_fn,
             ..
         }| {
            if append_fn.is_some() {
                quote! {#ident: std::vec::Vec<#ty>}
            } else {
                quote! {#ident: std::option::Option<#ty>}
            }
        },
    );
    let builder_fields_init = fields.iter().map(
        |StructField {
             ident, append_fn, ..
         }| {
            if append_fn.is_some() {
                quote! {#ident: std::vec::Vec::new()}
            } else {
                quote! {#ident: std::option::Option::None}
            }
        },
    );

    let builder_methods = fields.iter().map(
        |StructField {
             ident,
             ty,
             append_fn,
             ..
         }| {
            let append_quote = if let Some(append_fn) = append_fn {
                quote! {
                    pub fn #append_fn(&mut self, #append_fn: #ty)->&mut Self{
                        self.#ident.push(#append_fn);
                        self
                    }
                }
            } else {
                quote! {}
            };

            let set_quote = if append_fn.is_none() {
                quote! {
                    pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                        self.#ident=std::option::Option::Some(#ident);
                        self
                    }
                }
            } else if append_fn.as_ref().unwrap().to_string() != ident.to_string() {
                quote! {
                    pub fn #ident(&mut self, #ident:std::vec::Vec<#ty>)->&mut Self{
                        self.#ident=#ident;
                        self
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                #set_quote
                #append_quote
            }
        },
    );

    let build_function_field_setting = fields.iter().map(|StructField { ident, optional, append_fn, .. }| {
        if *optional {
            quote! {
                #ident: self.#ident.take()
            }
        } else if append_fn.is_some() {
            quote! {
                #ident: std::mem::replace(&mut self.#ident, std::vec::Vec::new())
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
            pub fn build(&mut self)->std::result::Result<#name, std::boxed::Box<dyn std::error::Error>>{
                let r = #name {
                    #(#build_function_field_setting),*
                };
                Ok(r)
            }
        }
    };
    expanded.into()
}
