use std::{collections::HashMap, iter};

use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_quote, Arm, Error as SynError, Expr, ExprLit, Lit, LitChar, Pat, PatLit, PatOr, PatRange,
    Token,
};

/// A trie based data structure
///
/// `K=key,C=condition,V=Value`
#[derive(Clone, Debug)]
pub(crate) struct Trie<K, C: Condition, V>(HashMap<K, (C, Trie<K, C, V>)>, Option<(C, V)>);

impl<K, C: Condition, V> Trie<K, C, V> {
    fn new() -> Self {
        Self(HashMap::new(), None)
    }

    fn is_leaf(&self) -> bool {
        self.0.is_empty()
    }
}

pub(crate) trait Condition {
    fn empty() -> Self;
    fn merge(self, other: Self) -> Self;
}

#[derive(Clone, Debug)]
pub(crate) struct CFGAttributes(Vec<TokenStream>);

impl Condition for CFGAttributes {
    fn empty() -> Self {
        Self(Vec::new())
    }

    fn merge(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }
}

impl CFGAttributes {
    fn into_attr(self) -> Option<TokenStream> {
        if self.0.is_empty() {
            None
        } else {
            let tokens = self.0;
            Some(quote! { #[cfg(any(#(#tokens),*))] })
        }
    }
}

impl From<Option<TokenStream>> for CFGAttributes {
    fn from(value: Option<TokenStream>) -> Self {
        Self(value.into_iter().collect())
    }
}

pub(super) fn expand_trie(
    trie: &Trie<char, CFGAttributes, Expr>,
    arms: &mut Vec<Arm>,
    states: &mut Vec<Ident>,
    prev_state: &Ident,
) {
    let mut count: u8 = 0;
    for (key, (cfg_attrs, sub_trie)) in trie.0.iter() {
        let chr = LitChar::new(*key, Span::call_site().into());
        if sub_trie.is_leaf() {
            if let Some((cfg_attrs, value)) = &sub_trie.1 {
                let cfg_attrs_tokens = cfg_attrs.clone().into_attr();
                let arm: Arm = parse_quote! {
                    #cfg_attrs_tokens
                    (States::#prev_state, #chr) => ::derive_finite_automaton::GetNextResult::Result {
                        result: #value,
                        ate_character: true
                    },
                };
                arms.push(arm);
            } else {
                unreachable!()
            }
        } else {
            let new_state_name_ident = {
                let as_string = prev_state.to_string();
                count += 1;
                let state_name = (count + 96) as char;
                // Creating state names:
                if as_string.is_empty() || as_string == crate::NO_STATE_NAME {
                    Ident::new(&state_name.to_string(), Span::call_site().into())
                } else {
                    let mut string = as_string.clone();
                    string.push(state_name);
                    Ident::new(&string, Span::call_site().into())
                }
            };
            states.push(new_state_name_ident.clone());

            let cfg_attrs_tokens = cfg_attrs.clone().into_attr();

            let arm: Arm = parse_quote! {
                #cfg_attrs_tokens
                (States::#prev_state, #chr) => ::derive_finite_automaton::GetNextResult::NewState(States::#new_state_name_ident),
            };
            arms.push(arm);

            expand_trie(sub_trie, arms, states, &new_state_name_ident);

            if let Some((cfg_attrs, value)) = &sub_trie.1 {
                let cfg_attrs_tokens = cfg_attrs.clone().into_attr();
                let result = quote! {
                    ::derive_finite_automaton::GetNextResult::Result { result: #value, ate_character: false, }
                };
                let arm: Arm = parse_quote! {
                    #cfg_attrs_tokens
                    (States::#new_state_name_ident, _) => #result,
                };
                arms.push(arm);
            }
        }
    }

    // Add expected case
    if trie.1.is_none() {
        let expected = trie
            .0
            .keys()
            .map(|key| LitChar::new(*key, Span::call_site().into()));

        let result = quote! {
            ::derive_finite_automaton::GetNextResult::InvalidCharacter(
                ::derive_finite_automaton::InvalidCharacter {
                    received: chr,
                    expected: &[ #(#expected),* ]
                }
            )
        };
        let arm: Arm = parse_quote! {
            (States::#prev_state, chr) => #result,
        };
        arms.push(arm);
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum Matcher {
    Single(char),
    Range { from: char, to: char },
    Or(Vec<char>),
}

/// A comma separated list of `"*sequence*" => *output*`
pub(super) struct Mappings(
    syn::punctuated::Punctuated<(Vec<Matcher>, Expr, Option<TokenStream>), syn::token::Comma>,
);

impl syn::parse::Parse for Mappings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input
            .parse_terminated::<_, Token![,]>(|input| {
                let mut cfg_tokens = None;
                if input.peek(Token![#]) {
                    let attributes = syn::Attribute::parse_outer(input)?;
                    for attribute in attributes {
                        if attribute.path.is_ident("cfg") {
                            if let Some(TokenTree::Group(group)) =
                                attribute.tokens.into_iter().next()
                            {
                                cfg_tokens = Some(group.stream());
                            } else {
                                return Err(SynError::new(
                                    Span::call_site(),
                                    "Invalid cfg attribute",
                                ));
                            }
                        }
                        // Skips doc comments
                        else if attribute.path.is_ident("doc") {
                            return Err(SynError::new(Span::call_site(), "Unknown attribute"));
                        }
                    }
                }
                let pat: Pat = input.parse()?;
                let matchers: Vec<Matcher> = if let Pat::Slice(slice) = pat {
                    let mut matchers = Vec::new();
                    for pat in slice.elems.iter() {
                        let matcher = match pat {
                            Pat::Lit(PatLit { expr, .. }) => {
                                if let Expr::Lit(ExprLit {
                                    lit: Lit::Char(chr),
                                    ..
                                }) = &**expr
                                {
                                    Matcher::Single(chr.value())
                                } else {
                                    return Err(SynError::new(
                                        Span::call_site(),
                                        "Expected character matcher",
                                    ));
                                }
                            }
                            Pat::Or(PatOr { cases, .. }) => {
                                let mut char_cases = Vec::new();
                                for case in cases {
                                    if let Pat::Lit(PatLit { expr, .. }) = case {
                                        if let Expr::Lit(ExprLit {
                                            lit: Lit::Char(chr),
                                            ..
                                        }) = &**expr
                                        {
                                            char_cases.push(chr.value());
                                        } else {
                                            return Err(SynError::new(
                                                Span::call_site(),
                                                "Expected character matcher",
                                            ));
                                        }
                                    } else {
                                        return Err(SynError::new(
                                            Span::call_site(),
                                            "Expected character matcher",
                                        ));
                                    }
                                }
                                Matcher::Or(char_cases)
                            }
                            Pat::Range(PatRange { lo, hi, .. }) => {
                                if let (
                                    Expr::Lit(ExprLit {
                                        lit: Lit::Char(chr_lo),
                                        ..
                                    }),
                                    Expr::Lit(ExprLit {
                                        lit: Lit::Char(chr_hi),
                                        ..
                                    }),
                                ) = (&**lo, &**hi)
                                {
                                    Matcher::Range {
                                        from: chr_lo.value(),
                                        to: chr_hi.value(),
                                    }
                                } else {
                                    return Err(SynError::new(
                                        Span::call_site(),
                                        "Expected character matcher",
                                    ));
                                }
                            }
                            _ => {
                                return Err(SynError::new(
                                    Span::call_site(),
                                    "Expected character matcher",
                                ));
                            }
                        };

                        matchers.push(matcher);
                    }
                    matchers
                } else if let Pat::Lit(PatLit { expr, .. }) = pat {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(string),
                        ..
                    }) = &*expr
                    {
                        string.value().chars().map(Matcher::Single).collect()
                    } else {
                        return Err(SynError::new(
                            Span::call_site(),
                            "Expected character matcher",
                        ));
                    }
                } else {
                    return Err(SynError::new(
                        Span::call_site(),
                        "Expected slice pattern or string literal",
                    ));
                };
                input.parse::<Token![=>]>()?;
                let expr: Expr = input.parse()?;
                Ok((matchers, expr, cfg_tokens))
            })
            .map(Self)
    }
}

impl Into<Trie<char, CFGAttributes, Expr>> for Mappings {
    fn into(self) -> Trie<char, CFGAttributes, Expr> {
        fn add_item<K, KC, C, V, I>(
            node: &mut Trie<K, C, V>,
            mut key_chain: KC,
            condition: C,
            value: V,
        ) where
            K: std::hash::Hash + PartialEq + Eq + Clone + Copy,
            KC: Iterator<Item = I> + Clone,
            C: Condition + Clone,
            V: Clone,
            I: IntoIterator<Item = K>,
        {
            if let Some(keys) = key_chain.next() {
                for key in keys {
                    if node.0.get(&key).is_none() {
                        node.0.insert(key, (condition.clone(), Trie::new()));
                    }
                    add_item(
                        &mut node.0.get_mut(&key).unwrap().1,
                        key_chain.clone(),
                        condition.clone(),
                        value.clone(),
                    )
                }
            } else {
                node.1 = Some((condition, value));
            }
        }

        let mut trie = Trie::new();

        for (matches, value, cfg_attributes) in self.0 {
            let cfg_attributes: CFGAttributes = cfg_attributes.into();

            // Have to allocate the matches ahead of time because of splitting
            let collect = matches
                .into_iter()
                .map(|matcher| match matcher {
                    Matcher::Single(chr) => either_n::Either3::One(iter::once(chr)),
                    Matcher::Range { from, to } => either_n::Either3::Two(from..to),
                    Matcher::Or(chars) => either_n::Either3::Three(chars.into_iter()),
                })
                .collect::<Vec<_>>();
            let key_chain = collect.iter().cloned();

            add_item(&mut trie, key_chain, cfg_attributes, value)
        }

        trie
    }
}

#[cfg(test)]
mod tests {
    use crate::trie::Trie;

    use super::Mappings;
    use syn::{self, parse_quote};

    #[test]
    fn test_mappings() {
        let attr: syn::Attribute = parse_quote! {
            #[mappings(
                "abc" => 4,
                #[cfg(feature = "x")]
                "abcd" => 5,
            )]
        };

        let mappings: Mappings = attr.parse_args().unwrap();
        let trie: Trie<_, _, _> = mappings.into();

        assert!(trie.1.is_none());
        let a = trie.0.get(&'a').unwrap();
        let b = a.1 .0.get(&'b').unwrap();
        let c = b.1 .0.get(&'c').unwrap();
        assert!(c.1 .1.is_some());
        let d = c.1 .0.get(&'d').unwrap();
        assert!(d.1.is_leaf());
    }
}
