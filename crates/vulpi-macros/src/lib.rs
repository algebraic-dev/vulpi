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

       impl<'a> crate::tree::Specialized<'a> for #name<'a> {
           const KIND: crate::tree::TreeKind = #parsed;

           fn tree(&mut self) -> &mut Tree {
               self.0
           }

           fn make(node: &'a mut Tree) -> Self {
               Self(node)
           }
       }

       impl<'a> std::fmt::Display for #name<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }
    }
    .into()
}
