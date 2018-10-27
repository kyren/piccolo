extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use quote::{ToTokens, Tokens};

fn collect_derive(s: synstructure::Structure) -> quote::Tokens {
    // Deriving `Collect` must be done with care, because an implementation of `Drop` is not
    // necessarily safe for `Collect` types.  This derive macro has four possible modes to ensure
    // that this is safe:
    //   1) Require that the type be 'static with `#[collect(require_static)]`.  This is the
    //      default if no other option is specified.
    //   2) Require that the type be `Copy` with `#[collect(require_copy)]`
    //   3) Generate a safe empty `Drop` impl with `#[collect(empty_drop)]`
    //   4) Allow a custom `Drop` impl that might be unsafe with `#[collect(unsafe_drop)]`.  Such
    //      `Drop` impls must *not* access garbage collected pointers during `Drop::drop`.
    #[derive(PartialEq)]
    enum Mode {
        RequireStatic,
        RequireCopy,
        EmptyDrop,
        UnsafeDrop,
    }

    let mut mode = None;

    for attr in &s.ast().attrs {
        match attr.interpret_meta() {
            Some(syn::Meta::List(syn::MetaList { ident, nested, .. })) => {
                if ident == syn::Ident::from("collect") {
                    if let Some(prev_mode) = mode {
                        let prev_mode_str = match prev_mode {
                            Mode::RequireStatic => "require_static",
                            Mode::RequireCopy => "require_copy",
                            Mode::EmptyDrop => "empty_drop",
                            Mode::UnsafeDrop => "unsafe_drop",
                        };
                        panic!("`Collect` mode was already specified with `#[collect({})]`, cannot specify twice", prev_mode_str);
                    }

                    if let Some(syn::punctuated::Pair::End(nmeta)) = nested.first() {
                        if nmeta == &syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::from(
                            "require_static",
                        ))) {
                            mode = Some(Mode::RequireStatic);
                        } else if nmeta == &syn::NestedMeta::Meta(syn::Meta::Word(
                            syn::Ident::from("require_copy"),
                        )) {
                            mode = Some(Mode::RequireCopy);
                        } else if nmeta == &syn::NestedMeta::Meta(syn::Meta::Word(
                            syn::Ident::from("empty_drop"),
                        )) {
                            mode = Some(Mode::EmptyDrop);
                        } else if nmeta == &syn::NestedMeta::Meta(syn::Meta::Word(
                            syn::Ident::from("unsafe_drop"),
                        )) {
                            mode = Some(Mode::UnsafeDrop);
                        } else {
                            panic!("`#[collect]` requires one of: \"require_static\", \"require_copy\", \"empty_drop\", or \"unsafe_drop\" as an argument");
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let mode = mode.unwrap_or(Mode::RequireStatic);

    let mut needs_trace_body = Tokens::new();
    quote!(false).to_tokens(&mut needs_trace_body);
    for v in s.variants() {
        for b in v.bindings() {
            let ty = &b.ast().ty;
            quote!(|| <#ty as gc_arena::Collect>::needs_trace()).to_tokens(&mut needs_trace_body);
        }
    }

    let trace_body = s.each(|bi| quote!(gc_arena::Collect::trace(#bi, cc)));

    let where_clause = if mode == Mode::RequireStatic {
        quote!(where Self: 'static)
    } else if mode == Mode::RequireCopy {
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
