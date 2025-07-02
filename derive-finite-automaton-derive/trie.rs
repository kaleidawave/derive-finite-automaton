use std::{collections::HashMap, iter};

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_quote, Arm, Error as SynError, Expr, ExprLit, Lit, LitByte, LitChar, Pat, PatLit, PatOr,
    PatRange, Token,
};

/// TODO other items
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Item {
    Char(char),
    Byte(u8),
}

impl Item {
    pub fn from_lit(lit: &Lit) -> Result<Self, SynError> {
        match lit {
            Lit::Byte(item) => Ok(Item::Byte(item.value())),
            Lit::Char(item) => Ok(Item::Char(item.value())),
            _ => Err(SynError::new(
                Span::call_site(),
                "Expected item or byte matcher",
            )),
        }
    }

    pub fn as_pattern(&self) -> Pat {
        match self {
            Item::Char(key) => ExprLit {
                attrs: Default::default(),
                lit: LitChar::new(*key, Span::call_site()).into(),
            }
            .into(),
            Item::Byte(byte) => ExprLit {
                attrs: Default::default(),
                lit: LitByte::new(*byte, Span::call_site()).into(),
            }
            .into(),
        }
    }
}

/// A trie based data structure
#[derive(Clone, Debug)]
pub(crate) struct Trie<K, V>(HashMap<K, Trie<K, V>>, Option<V>);

impl<K, V> Trie<K, V> {
    fn new() -> Self {
        Self(HashMap::new(), None)
    }

    fn is_leaf(&self) -> bool {
        self.0.is_empty()
    }
}

pub(super) fn expand_trie(
    trie: &Trie<Item, Expr>,
    arms: &mut Vec<Arm>,
    states: &mut Vec<Ident>,
    prev_state: &Ident,
    states_ident: &Ident,
) {
    let mut count: u8 = 0;
    for (key, sub_trie) in trie.0.iter() {
        let item = key.as_pattern();
        if sub_trie.is_leaf() {
            if let Some(value) = &sub_trie.1 {
                let arm: Arm = parse_quote! {
                    (#states_ident::#prev_state, #item) => ::derive_finite_automaton::GetNextResult::Result {
                        result: #value,
                        ate_item: true
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
                let state_name = (count + b'A') as char;
                // Creating state names:
                if as_string.is_empty() || as_string == crate::NO_STATE_NAME {
                    Ident::new(&state_name.to_string(), Span::call_site())
                } else {
                    let mut string = as_string.clone();
                    string.push(state_name);
                    Ident::new(&string, Span::call_site())
                }
            };
            states.push(new_state_name_ident.clone());

            let arm: Arm = parse_quote! {
                (#states_ident::#prev_state, #item) => ::derive_finite_automaton::GetNextResult::NewState(#states_ident::#new_state_name_ident),
            };
            arms.push(arm);

            expand_trie(sub_trie, arms, states, &new_state_name_ident, states_ident);

            if let Some(value) = &sub_trie.1 {
                let result = quote! {
                    ::derive_finite_automaton::GetNextResult::Result { result: #value, ate_item: false, }
                };
                let arm: Arm = parse_quote! {
                    (#states_ident::#new_state_name_ident, _) => #result,
                };
                arms.push(arm);
            }
        }
    }

    // Add expected case
    if trie.1.is_none() {
        let expected = trie.0.keys().map(|key| key.as_pattern());

        let result = quote! {
            ::derive_finite_automaton::GetNextResult::InvalidItem(
                ::derive_finite_automaton::InvalidItem {
                    received: item,
                    expected: &[ #(#expected),* ]
                }
            )
        };
        let arm: Arm = parse_quote! {
            (#states_ident::#prev_state, item) => #result,
        };
        arms.push(arm);
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum Matcher {
    Single(Item),
    Range { from: Item, to: Item },
    Or(Vec<Item>),
}

/// A comma separated list of `"*sequence*" => *output*`
pub(super) struct Mappings(syn::punctuated::Punctuated<(Vec<Matcher>, Expr), syn::token::Comma>);

impl syn::parse::Parse for Mappings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input
            .parse_terminated(parse_item_to_matchers_and_expression, Token![,])
            .map(Self)
    }
}

impl Mappings {
    pub(crate) fn extend(&mut self, iter: Self) {
        self.0.extend(iter.0)
    }
}

fn parse_item_to_matchers_and_expression(
    input: &syn::parse::ParseBuffer<'_>,
) -> Result<(Vec<Matcher>, Expr), SynError> {
    let pat: Pat = Pat::parse_multi(input)?;
    let matchers: Vec<Matcher> = if let Pat::Slice(slice) = pat {
        let mut matchers = Vec::new();
        for pat in slice.elems.iter() {
            let matcher = match pat {
                Pat::Lit(PatLit { lit, .. }) => Matcher::Single(Item::from_lit(lit)?),
                Pat::Or(PatOr { cases, .. }) => {
                    let mut char_cases = Vec::new();
                    for case in cases {
                        if let Pat::Lit(PatLit { lit, .. }) = case {
                            char_cases.push(Item::from_lit(lit)?);
                        } else {
                            return Err(SynError::new(Span::call_site(), "Expected item matcher"));
                        }
                    }
                    Matcher::Or(char_cases)
                }
                Pat::Range(PatRange {
                    start: Some(start),
                    end: Some(end),
                    ..
                }) => {
                    if let (
                        Expr::Lit(ExprLit { lit: lit_from, .. }),
                        Expr::Lit(ExprLit { lit: lit_to, .. }),
                    ) = (&**start, &**end)
                    {
                        Matcher::Range {
                            from: Item::from_lit(lit_from)?,
                            to: Item::from_lit(lit_to)?,
                        }
                    } else {
                        return Err(SynError::new(Span::call_site(), "Expected item matcher"));
                    }
                }
                _ => {
                    return Err(SynError::new(Span::call_site(), "Expected item matcher"));
                }
            };

            matchers.push(matcher);
        }
        matchers
    } else if let Pat::Lit(PatLit { lit, .. }) = pat {
        if let Lit::Str(string) = lit {
            string
                .value()
                .chars()
                .map(Item::Char)
                .map(Matcher::Single)
                .collect()
        } else if let Lit::ByteStr(bstring) = lit {
            bstring
                .value()
                .into_iter()
                .map(Item::Byte)
                .map(Matcher::Single)
                .collect()
        } else {
            return Err(SynError::new(Span::call_site(), "Expected item matcher"));
        }
    } else {
        return Err(SynError::new(
            Span::call_site(),
            "Expected slice pattern or string literal",
        ));
    };
    input.parse::<Token![=>]>()?;
    let expr: Expr = input.parse()?;
    Ok((matchers, expr))
}

impl From<Mappings> for Trie<Item, Expr> {
    fn from(mappings: Mappings) -> Trie<Item, Expr> {
        fn add_item<K, KC, V, I>(node: &mut Trie<K, V>, mut key_chain: KC, value: V)
        where
            K: std::hash::Hash + PartialEq + Eq + Clone,
            KC: Iterator<Item = I> + Clone,
            V: Clone,
            I: IntoIterator<Item = K>,
        {
            if let Some(keys) = key_chain.next() {
                for key in keys {
                    if !node.0.contains_key(&key) {
                        node.0.insert(key.clone(), Trie::new());
                    }
                    add_item(
                        node.0.get_mut(&key).unwrap(),
                        key_chain.clone(),
                        value.clone(),
                    )
                }
            } else {
                node.1 = Some(value);
            }
        }

        let mut trie = Trie::new();

        for (matches, value) in mappings.0 {
            // Have to allocate the matches ahead of time because of splitting
            let collect = matches
                .into_iter()
                .map(|matcher| match matcher {
                    Matcher::Single(item) => either_n::Either4::One(iter::once(item)),
                    Matcher::Or(items) => either_n::Either4::Two(items.into_iter()),
                    Matcher::Range {
                        from: Item::Char(from),
                        to: Item::Char(to),
                    } => either_n::Either4::Three((from..to).map(Item::Char)),
                    Matcher::Range {
                        from: Item::Byte(from),
                        to: Item::Byte(to),
                    } => either_n::Either4::Four((from..to).map(Item::Byte)),
                    Matcher::Range { .. } => panic!("Cannot make range out of patter"),
                })
                .collect::<Vec<_>>();

            let key_chain = collect.iter().cloned();
            add_item(&mut trie, key_chain, value)
        }

        trie
    }
}

#[cfg(test)]
mod tests {
    use crate::trie::{Item, Trie};

    use super::Mappings;
    use syn::{self, parse_quote};

    #[test]
    fn test_char_mappings() {
        let attr: syn::Attribute = parse_quote! {
            #[mappings(
                "abc" => 4,
                "abcd" => 5,
            )]
        };

        let mappings: Mappings = attr.parse_args().unwrap();
        let trie: Trie<_, _> = mappings.into();

        assert!(trie.1.is_none());
        let a = trie.0.get(&Item::Char('a')).unwrap();
        let b = a.0.get(&Item::Char('b')).unwrap();
        let c = b.0.get(&Item::Char('c')).unwrap();
        assert!(c.1.is_some());
        let d = c.0.get(&Item::Char('d')).unwrap();
        assert!(d.is_leaf());
    }
}
