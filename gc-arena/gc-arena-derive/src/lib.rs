extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use quote::{ToTokens, Tokens};

fn collect_derive(s: synstructure::Structure) -> quote::Tokens {
    // Use `#[collect(require_copy)]` to tell this derive macro to require `Copy` on the
    // implementation of Collect, use #[collect(empty_drop)] to generate a trivial empty Drop
    // implementation, and use `#[collect(unsafe_drop)]` to allow for a custom Drop implementation
    // (which may lead to unsafety!)
    #[derive(PartialEq)]
    enum Mode {
        Unset,
        RequireCopy,
        EmptyDrop,
        UnsafeDrop,
    }

    const MODE_ERR_MSG: &'static str = "When using `#[derive(Collect)]`, you must include exactly *one* of `#[collect(require_copy)]`, `#[collect(empty_drop)]`, or `#[collect(unsafe_drop)]`";

    let mut mode = Mode::Unset;

    for attr in &s.ast().attrs {
        match attr.interpret_meta() {
            Some(syn::Meta::List(syn::MetaList { ident, nested, .. })) => {
                if ident == syn::Ident::from("collect") {
                    if let Some(syn::punctuated::Pair::End(nmeta)) = nested.first() {
                        if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::from(
                                "require_copy",
                            ))) {
                            if mode != Mode::Unset {
                                panic!(MODE_ERR_MSG);
                            }
                            mode = Mode::RequireCopy;
                        } else if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::from(
                                "empty_drop",
                            ))) {
                            if mode != Mode::Unset {
                                panic!(MODE_ERR_MSG);
                            }
                            mode = Mode::EmptyDrop;
                        } else if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::from(
                                "unsafe_drop",
                            ))) {
                            if mode != Mode::Unset {
                                panic!(MODE_ERR_MSG);
                            }
                            mode = Mode::UnsafeDrop;
                        } else {
                            panic!("`#[collect]` requires either \"require_copy\", \"empty_drop\", or \"unsafe_drop\" as an argument");
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if mode == Mode::Unset {
        panic!(MODE_ERR_MSG);
    }

    let mut needs_trace_body = Tokens::new();
    quote!(false).to_tokens(&mut needs_trace_body);
    for v in s.variants() {
        for b in v.bindings() {
            let ty = &b.ast().ty;
            quote!(|| <#ty as gc_arena::Collect>::needs_trace()).to_tokens(&mut needs_trace_body);
        }
    }

    let trace_body = s.each(|bi| quote!(gc_arena::Collect::trace(#bi, cc)));

    let where_clause = if mode == Mode::RequireCopy {
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

    let drop_impl = if mode == Mode::EmptyDrop {
        s.gen_impl(quote! {
            gen impl Drop for @Self {
                fn drop(&mut self) {}
            }
        })
    } else {
        quote!()
    };

    quote! {
        #collect_impl
        #drop_impl
    }
}

decl_derive!([Collect, attributes(collect)] => collect_derive);
