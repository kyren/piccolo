extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use quote::{ToTokens, Tokens};

fn collect_derive(s: synstructure::Structure) -> quote::Tokens {
    let mut needs_trace_body = Tokens::new();
    quote!(false).to_tokens(&mut needs_trace_body);
    for v in s.variants() {
        for b in v.bindings() {
            let ty = &b.ast().ty;
            quote!(|| <#ty as gc_arena::Collect>::needs_trace()).to_tokens(&mut needs_trace_body);
        }
    }

    let trace_body = s.each(|bi| {
        quote!{
            gc_arena::Collect::trace(#bi, cc)
        }
    });

    let collect_impl = s.gen_impl(quote! {
        extern crate gc_arena;

        gen unsafe impl gc_arena::Collect for @Self {
            #[inline]
            fn needs_trace() -> bool {
                #needs_trace_body
            }

            #[inline]
            fn trace(&self, cc: ::gc_arena::CollectionContext) {
                match *self { #trace_body }
            }
        }
    });

    let drop_impl = s.gen_impl(quote! {
        gen impl Drop for @Self {
            fn drop(&mut self) {}
        }
    });

    quote! {
        #collect_impl
        #drop_impl
    }
}

decl_derive!([Collect] => collect_derive);
