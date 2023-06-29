extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::ItemStruct;

#[proc_macro_attribute]
pub fn newtype(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed: syn::Expr = syn::parse(attr).unwrap();

    let item = syn::parse_macro_input!(item as ItemStruct);

    let name = &item.ident;

    quote! {
       #item

       impl<'a> crate::tree::Specialized<'a> for #name<'a> {
           const KIND: crate::tree::TreeKind = #parsed;

           fn tree(&self) -> &'a Tree<'a> {
               self.0
           }

           fn make(node: &'a Tree<'a>) -> Self {
               Self(node)
           }
       }
    }
    .into()
}
