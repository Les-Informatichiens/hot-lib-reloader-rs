use std::path::PathBuf;

use quote::ToTokens;
use syn::{
    Attribute, ForeignItemFn, Ident, Item, ItemMacro, LitStr, Macro, Result, Visibility,
    spanned::Spanned, token,
};
use syn::{Error, LitBool};

use super::HotModuleAttribute;
use super::code_gen::{
    gen_hot_module_function_for, gen_lib_change_subscription_function, generate_lib_loader_items,
};
use crate::hot_module::code_gen::{gen_lib_version_function, gen_lib_was_updated_function};
use crate::util::read_functions_from_file;

pub(crate) struct HotModule {
    pub(crate) vis: Visibility,
    pub(crate) ident: Ident,
    pub(crate) items: Vec<Item>,
    #[allow(dead_code)]
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) hot_module_args: Option<super::HotModuleAttribute>,
}

impl syn::parse::Parse for HotModule {
    fn parse(stream: syn::parse::ParseStream) -> Result<Self> {
        let attributes = syn::Attribute::parse_outer(stream)?;

        let vis = stream
            .parse::<syn::Visibility>()
            .unwrap_or(Visibility::Inherited);

        stream.parse::<token::Mod>()?;

        let ident = stream.parse::<Ident>()?;

        let module_body_stream;
        syn::braced!(module_body_stream in stream);

        let mut items = Vec::new();

        while !module_body_stream.is_empty() {
            let item = module_body_stream.parse::<syn::Item>()?;

            match item {
                // parses the hot_functions_from_file!("path/to/file.rs") marker
                Item::Macro(ItemMacro {
                    mac: Macro { path, tokens, .. },
                    ..
                }) if path.is_ident("hot_functions_from_file") => {
                    let span = path.span();
                    let mut iter = tokens.into_iter();

                    // get filename
                    let file_name = iter
                        .next()
                        .ok_or_else(|| {
                            syn::Error::new(span, "expected path to file as a literal string")
                        })
                        .and_then(|t| syn::parse2::<LitStr>(t.into_token_stream()))?;

                    // parse optional `ignore_no_mangle = true`
                    let ignore_no_mangle = if let Some(tokens) = iter.next() {
                        match tokens {
                            proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' => {
                                let ident = iter
                                    .next()
                                    .ok_or_else(|| syn::Error::new(ident.span(), "expected ident"))
                                    .and_then(|t| syn::parse2::<Ident>(t.to_token_stream()))?;
                                if ident != "ignore_no_mangle" {
                                    return Err(syn::Error::new(ident.span(), "unexpected input"));
                                }

                                iter.next()
                                    .ok_or_else(|| syn::Error::new(ident.span(), "expected ="))
                                    .and_then(|t| syn::parse2::<token::Eq>(t.to_token_stream()))?;

                                let val = iter
                                    .next()
                                    .ok_or_else(|| {
                                        syn::Error::new(ident.span(), "expected true or false")
                                    })
                                    .and_then(|t| syn::parse2::<LitBool>(t.to_token_stream()))?;
                                val.value()
                            }
                            other => {
                                return Err(syn::Error::new(other.span(), "expected comma"));
                            }
                        }
                    } else {
                        false
                    };

                    // read from file
                    let functions = read_functions_from_file(file_name, ignore_no_mangle)?;
                    for (f, span) in functions {
                        let f = gen_hot_module_function_for(f, span, None)?;
                        items.push(Item::Fn(f));
                    }
                }

                Item::Macro(ItemMacro {
                    mac: Macro { path, tokens, .. },
                    ..
                }) if path.is_ident("inline_hot_file") => {
                    let span = path.span();
                    let mut iter = tokens.into_iter();

                    // get filename
                    let file_name = iter
                        .next()
                        .ok_or_else(|| {
                            syn::Error::new(span, "expected path to file as a literal string")
                        })
                        .and_then(|t| syn::parse2::<LitStr>(t.into_token_stream()))?;

                    // let base_path = proc_macro::Span::call_site()
                    //     .local_file()
                    //     .ok_or(Error::new(
                    //         span,
                    //         format!("Could not find local file. Sorry bruv."),
                    //     ))?
                    //     .parent()
                    //     .ok_or(Error::new(span, format!("File should have a parent.")))?;

                    let path: PathBuf = file_name.value().into();
                    let content = std::fs::read_to_string(&path).map_err(|err| {
                        Error::new(span, format!("Error reading file {path:?}: {err}"))
                    })?;

                    let mut ast: syn::File = syn::parse_file(&content)?;

                    for item in ast.items.drain(..) {
                        match item {
                            syn::Item::Fn(fun) => {
                                fn cfg_no_mangle<'a>(
                                    mut cfg_items: impl Iterator<Item = &'a syn::Meta>,
                                ) -> bool {
                                    let _predicate = cfg_items.next();
                                    // TODO: return false if predicate is false
                                    // false positives are unlikely, but can still compile error
                                    cfg_items.any(|meta| match meta {
                                        syn::Meta::Path(path) => path.is_ident("no_mangle"),
                                        syn::Meta::List(list) => {
                                            let mut found_no_mangle = false;
                                            if let Err(_) = list.parse_nested_meta(|meta| {
                                                if meta.path.is_ident("no_mangle") {
                                                    found_no_mangle = true;
                                                }
                                                Ok(())
                                            }) {
                                                return false;
                                            }
                                            found_no_mangle
                                        }

                                        _ => false,
                                    })
                                }

                                fn is_no_mangle<'a>(
                                    mut attrs: impl Iterator<Item = &'a syn::Attribute>,
                                ) -> bool {
                                    attrs.any(|attr| {
                                        let ident = match attr.path().get_ident() {
                                            Some(i) => i,
                                            None => return false,
                                        };
                                        if *ident == "no_mangle" {
                                            true
                                        } else if *ident == "unsafe" {
                                            let mut found_no_mangle = false;
                                            if let Err(_) = attr.parse_nested_meta(|meta| {
                                                if meta.path.is_ident("no_mangle") {
                                                    found_no_mangle = true;
                                                }
                                                Ok(())
                                            }) {
                                                return false;
                                            }
                                            found_no_mangle
                                        } else if *ident == "cfg_attr" {
                                            let nested = match attr.parse_args_with(
                                                syn::punctuated::Punctuated::<
                                                    syn::Meta,
                                                    syn::Token![,],
                                                >::parse_terminated,
                                            ) {
                                                Ok(nested) => nested,
                                                _ => return false,
                                            };
                                            cfg_no_mangle(nested.iter())
                                        } else if *ident == "hot_function" {
                                            true
                                        } else {
                                            false
                                        }
                                    })
                                }

                                if !is_no_mangle(fun.attrs.iter()) {
                                    items.push(Item::Fn(fun));
                                    continue;
                                };

                                let hot_fun = ForeignItemFn {
                                    attrs: Vec::new(),
                                    vis: fun.vis.clone(),
                                    sig: fun.sig.clone(),
                                    semi_token: syn::token::Semi(span),
                                };

                                let new_fn_name = format!("__{}__", fun.sig.ident.clone());

                                let mut fun = fun.clone();
                                fun.sig.ident = Ident::new(&new_fn_name, fun.sig.ident.span());

                                items.push(Item::Fn(gen_hot_module_function_for(
                                    hot_fun,
                                    file_name.span(),
                                    Some(&fun.sig.ident),
                                )?));

                                items.push(Item::Fn(fun));
                            }
                            _ => items.push(item),
                        }
                    }
                }

                // parses and code gens
                // #[lib_change_subscription]
                // pub fn subscribe() -> ... {}
                syn::Item::Fn(func)
                    if func
                        .attrs
                        .iter()
                        .any(|attr| attr.path().is_ident("lib_change_subscription")) =>
                {
                    let span = func.span();
                    let f = ForeignItemFn {
                        attrs: Vec::new(),
                        vis: func.vis,
                        sig: func.sig,
                        semi_token: token::Semi::default(),
                    };
                    let f = gen_lib_change_subscription_function(f, span)?;
                    items.push(Item::Fn(f));
                }

                // parses and code gens
                // #[lib_version]
                // pub fn version() -> usize {}
                syn::Item::Fn(func)
                    if func
                        .attrs
                        .iter()
                        .any(|attr| attr.path().is_ident("lib_version")) =>
                {
                    let span = func.span();
                    let f = ForeignItemFn {
                        attrs: Vec::new(),
                        vis: func.vis,
                        sig: func.sig,
                        semi_token: token::Semi::default(),
                    };
                    let f = gen_lib_version_function(f, span)?;
                    items.push(Item::Fn(f));
                }

                // parses and code gens
                // #[lib_updated]
                // pub fn was_updated() -> bool {}
                syn::Item::Fn(func)
                    if func
                        .attrs
                        .iter()
                        .any(|attr| attr.path().is_ident("lib_updated")) =>
                {
                    let span = func.span();
                    let f = ForeignItemFn {
                        attrs: Vec::new(),
                        vis: func.vis,
                        sig: func.sig,
                        semi_token: token::Semi::default(),
                    };
                    let f = gen_lib_was_updated_function(f, span)?;
                    items.push(Item::Fn(f));
                }

                // parses and code gens
                // #[hot_function]
                // fn do_stuff(arg: &str) -> u32 {}
                syn::Item::Fn(func)
                    if func
                        .attrs
                        .iter()
                        .any(|attr| attr.path().is_ident("hot_function")) =>
                {
                    let span = func.span();
                    let f = ForeignItemFn {
                        attrs: Vec::new(),
                        vis: func.vis,
                        sig: func.sig,
                        semi_token: token::Semi::default(),
                    };
                    let f = gen_hot_module_function_for(f, span, None)?;
                    items.push(Item::Fn(f));
                }

                // parses and code gens
                // #[hot_functions]
                // extern "Rust" {
                //     pub fn do_stuff(arg: &str) -> u32;
                // }
                syn::Item::ForeignMod(foreign_mod)
                    if foreign_mod
                        .attrs
                        .iter()
                        .any(|attr| attr.path().is_ident("hot_functions")) =>
                {
                    for item in foreign_mod.items {
                        match item {
                            syn::ForeignItem::Fn(f) => {
                                let span = f.span();
                                let f = gen_hot_module_function_for(f, span, None)?;
                                items.push(Item::Fn(f));
                            }
                            _ => {
                                eprintln!(
                                    "[warn] hot_functions extern block includes unexpected items"
                                );
                            }
                        }
                    }
                }

                // otherwise just use the item as is
                item => items.push(item),
            };
        }

        Ok(Self {
            ident,
            vis,
            items,
            attributes,
            hot_module_args: None,
        })
    }
}

impl quote::ToTokens for HotModule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self {
            vis,
            ident,
            items,
            hot_module_args,
            ..
        } = self;

        let HotModuleAttribute {
            lib_name,
            lib_dir,
            file_watch_debounce_ms,
            crate_name,
            loaded_lib_name_template,
        } = match hot_module_args {
            None => panic!("Expected to have macro attributes"),
            Some(attributes) => attributes,
        };

        let lib_loader = generate_lib_loader_items(
            lib_dir,
            lib_name,
            file_watch_debounce_ms,
            crate_name,
            loaded_lib_name_template,
            tokens.span(),
        )
        .expect("error generating hot lib loader helpers");

        let module_def = quote::quote! {
            #vis mod #ident {
                #( #items )*

                #lib_loader
            }
        };

        proc_macro2::TokenStream::extend(tokens, module_def);
    }
}
