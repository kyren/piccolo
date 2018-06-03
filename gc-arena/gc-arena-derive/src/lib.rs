extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use quote::{ToTokens, Tokens};

fn collect_derive(s: synstructure::Structure) -> quote::Tokens {
    // Use `#[collect(require_copy)]` to tell this derive macro to require `Copy` on the
    // implementation of `Collect` rather than implementing an empty `Drop`, otherwise the empty
    // `Drop` impl will conflict with an implementation of `Copy`.
    let use_copy = s.ast()
        .attrs
        .iter()
        .any(|attr| match attr.interpret_meta() {
            Some(syn::Meta::List(syn::MetaList { ident, nested, .. })) => {
                ident == syn::Ident::from("collect")
                    && if let Some(syn::punctuated::Pair::End(nmeta)) = nested.first() {
                        nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::from(
                                "require_copy",
                            )))
                    } else {
                        false
                    }
            }
            _ => false,
        });

    let mut needs_trace_body = Tokens::new();
    quote!(false).to_tokens(&mut needs_trace_body);
    for v in s.variants() {
        for b in v.bindings() {
            let ty = &b.ast().ty;
            quote!(|| <#ty as gc_arena::Collect>::needs_trace()).to_tokens(&mut needs_trace_body);
        }
    }

    let trace_body = s.each(|bi| quote!(gc_arena::Collect::trace(#bi, cc)));

    let where_clause = if use_copy {
        quote!(where Self: Copy)
    } else {
        quote!()
    };

    let collect_impl = s.gen_impl(quote! {
        extern crate gc_arena;

        gen unsafe impl gc_arena::Collect for @Self #where_clause {
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

    let drop_impl = if use_copy {
        quote!()
    } else {
        s.gen_impl(quote! {
            gen impl Drop for @Self {
                fn drop(&mut self) {}
            }
        })
    };

    quote! {
        #collect_impl
        #drop_impl
    }
}

decl_derive!([Collect, attributes(collect)] => collect_derive);
