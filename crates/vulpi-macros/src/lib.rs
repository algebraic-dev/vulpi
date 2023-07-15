#![allow(clippy::redundant_clone)]

extern crate proc_macro;
use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::quote;
use syn::{Ident, Item, ItemStruct};

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

#[proc_macro_attribute]
pub fn node_of(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ident: Ident = syn::parse(attr).unwrap();
    let parsed = syn::parse::<ItemStruct>(item).unwrap();

    let name = parsed.ident.clone();

    let visitor = format!("visit_{}", name.to_string().to_case(Case::Snake));
    let visitor = syn::Ident::new(&visitor, proc_macro2::Span::call_site());

    quote! {
        #parsed

        impl #ident for #name {
            fn accept(&mut self, visitor: &mut dyn Visitor) {
                visitor.#visitor(self);
            }
        }

    }
    .into()
}
