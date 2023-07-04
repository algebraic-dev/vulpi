extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::ItemStruct;

#[proc_macro_attribute]
pub fn new_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed: syn::Expr = syn::parse(attr).unwrap();
    let item = syn::parse_macro_input!(item as ItemStruct);
    let name = &item.ident;

    quote! {
       #[derive(Debug)]
       #item

       impl crate::tree::Specialized for #name<'_> {
           const KIND: crate::tree::TreeKind = #parsed;

           fn tree(&self) -> &Tree {
               self.0
           }

           fn make(node: &Tree) -> Self {
               Self(node)
           }
       }
    }
    .into()
}
