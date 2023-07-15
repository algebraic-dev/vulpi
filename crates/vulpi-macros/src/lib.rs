#![allow(clippy::redundant_clone)]

extern crate proc_macro;
use std::collections::HashSet;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Item, ItemStruct, ItemTrait, Token,
};

#[proc_macro_derive(Tree, attributes(helper))]
pub fn derive_helper_attr(item: TokenStream) -> TokenStream {
    let parsed = syn::parse::<Item>(item).unwrap();

    let name;
    let mut sttms = vec![quote! {
        use vulpi_show::{TreeDisplay};
    }];

    let gen;

    match parsed {
        Item::Enum(enum_) => {
            name = enum_.ident;

            gen = enum_.generics;

            let mut variants = vec![];

            for variant in &enum_.variants {
                let mut counter = 0;

                let name_str = variant.ident.to_string();
                let mut variant_fields = vec![quote! { let res = TreeDisplay::label(#name_str); }];
                let mut names = Vec::new();

                for field in &variant.fields {
                    if let Some(ident) = &field.ident {
                        names.push(ident.clone());
                        let name_str = ident.to_string();
                        variant_fields.push(quote! {
                            let res = res.with(TreeDisplay::label(#name_str).with(#name.show()));
                        });
                    } else {
                        let name = syn::Ident::new(
                            &format!("field{}", counter),
                            proc_macro2::Span::call_site(),
                        );
                        names.push(name.clone());
                        counter += 1;
                        variant_fields.push(quote! {
                            let res = res.with(#name.show());
                        });
                    };
                }

                let variant = variant.ident.clone();

                if names.is_empty() {
                    variants.push(quote! {
                        #name::#variant => {
                            #(#variant_fields)*
                            res
                        }
                    });
                } else {
                    variants.push(quote! {
                        #name::#variant(#(#names),*) => {
                            #(#variant_fields)*
                            res
                        }
                    });
                }
            }

            sttms.push(quote! {
                let res = match self {
                    #(#variants)*
                };
            });
        }
        Item::Struct(struct_) => {
            name = struct_.ident;

            gen = struct_.generics;

            for (i, field) in struct_.fields.iter().enumerate() {
                if let Some(ident) = &field.ident {
                    let ident_str = ident.to_string();
                    sttms.push(quote! {
                        let res = res.with(TreeDisplay::label(#ident_str).with(self.#ident.show()));
                    });
                } else {
                    let num = syn::Index::from(i);
                    sttms.push(quote! {
                        let res = res.with(self.#num.show());
                    });
                }
            }

            let name_str = name.to_string();
            sttms.insert(1, quote! { let res = TreeDisplay::label(#name_str); });
        }
        _ => panic!("Only structs and enums are supported"),
    }

    let mut gen_changed = gen.clone();

    for gen in &mut gen_changed.params {
        if let syn::GenericParam::Type(type_) = gen {
            type_.bounds.push(syn::parse_quote!(vulpi_show::Show));
        }
    }

    quote! {
        impl #gen_changed vulpi_show::Show for #name #gen {
            fn show(&self) -> vulpi_show::TreeDisplay {
                #(#sttms)*
                res
            }
        }
    }
    .into()
}

struct Args {
    pub vars: HashSet<Ident>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vars = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        Ok(Self {
            vars: vars.into_iter().collect(),
        })
    }
}

#[proc_macro_attribute]
pub fn node_of(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut parsed = syn::parse::<ItemStruct>(item).unwrap();

    let name = parsed.ident.clone();

    let visitor = format!("visit_{}", name.to_string().to_case(Case::Snake));
    let visitor = syn::Ident::new(&visitor, proc_macro2::Span::call_site());

    let mut fields = vec![];

    for (i, field) in parsed.fields.iter_mut().enumerate() {
        if !field.attrs.is_empty() {
            field.attrs = vec![];
            continue;
        }

        let num = if let Some(ident) = &field.ident {
            quote! { #ident }
        } else {
            let index = syn::Index::from(i);
            quote! { #index }
        };
        fields.push(quote! { self.#num.accept(visitor); });
    }

    let parse = syn::parse::<Args>(attr);

    let mut impls = Vec::new();

    for ident in parse.unwrap().vars {
        impls.push(quote! {impl #ident for #name {}})
    }

    quote! {
        #parsed

        impl Node for #name {
            fn accept(&mut self, visitor: &mut dyn Visitor) {
                visitor.#visitor(self);
            }
        }

        #(#impls)*

        impl Walkable for #name {
            fn walk(&mut self, visitor: &mut dyn Visitor) {
                #(#fields)*
            }
        }

    }
    .into()
}

#[proc_macro_attribute]
pub fn node(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed = syn::parse::<ItemTrait>(item).unwrap();

    let name = parsed.ident.clone();

    quote! {
        #parsed

        impl Node for Box<dyn #name> {
            fn accept(&mut self, visitor: &mut dyn Visitor) {
                (**self).accept(visitor);
            }
        }

        impl Show for Box<dyn #name> {
            fn show(&self) -> TreeDisplay {
                (**self).show()
            }
        }

    }
    .into()
}
