use proc_macro::{Span, TokenStream};
use quote::quote;
use std::collections::HashMap;
use syn::{
    parse::Parse, parse_macro_input, parse_quote, Arm, DeriveInput, Expr, Ident, LitChar, LitStr,
    Token,
};

/// A trie based data structure
#[derive(Clone)]
struct Trie<K, V>(HashMap<K, Trie<K, V>>, Option<V>);

impl<K, V> Trie<K, V> {
    fn is_leaf(&self) -> bool {
        self.0.is_empty()
    }
}

const NO_STATE_NAME: &str = "None";

fn expand_trie(
    trie: &Trie<char, Expr>,
    arms: &mut Vec<Arm>,
    states: &mut Vec<Ident>,
    prev_state: &Ident,
) {
    let mut count: u8 = 0;
    for (key, sub_trie) in trie.0.iter() {
        let chr = LitChar::new(*key, Span::call_site().into());
        if sub_trie.is_leaf() {
            if let Some(value) = &sub_trie.1 {
                let arm: Arm = parse_quote! {
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
            let new_state = {
                let as_string = prev_state.to_string();
                count += 1;
                // Creating state names:
                if as_string.is_empty() || as_string == NO_STATE_NAME {
                    let mut string = String::new();
                    string.push((count + 96) as char);
                    Ident::new(&string, Span::call_site().into())
                } else {
                    let mut string = as_string.clone();
                    string.push((count + 96) as char);
                    Ident::new(&string, Span::call_site().into())
                }
            };
            states.push(new_state.clone());
            let arm: Arm = parse_quote! {
                (States::#prev_state, #chr) => ::derive_finite_automaton::GetNextResult::NewState(States::#new_state),
            };
            arms.push(arm);
            expand_trie(sub_trie, arms, states, &new_state);
            if let Some(value) = &sub_trie.1 {
                let arm: Arm = parse_quote! {
                    (States::#new_state, _) => ::derive_finite_automaton::GetNextResult::Result {
                        result: #value,
                        ate_character: false,
                    },
                };
                arms.push(arm);
            }
        }
    }
    if trie.1.is_none() {
        let expected = trie
            .0
            .keys()
            .map(|key| LitChar::new(*key, Span::call_site().into()));
        let arm: Arm = parse_quote! {
            (States::#prev_state, chr) => ::derive_finite_automaton::GetNextResult::InvalidCharacter(::derive_finite_automaton::InvalidCharacter {
                received: chr,
                expected: &[ #(#expected),* ]
            }),
        };
        arms.push(arm);
    }
}

/// A comma separated list of `"*sequence*" => *output*`
struct Mappings(syn::punctuated::Punctuated<(Vec<char>, syn::Expr), syn::token::Comma>);

impl Parse for Mappings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content = input
            .parse_terminated::<(LitStr, Expr), Token![,]>(|input| {
                if input.peek(Token![#]) {
                    let _ = syn::Attribute::parse_outer(input)?;
                }
                let string: LitStr = input.parse()?;
                input.parse::<Token![=>]>()?;
                let expr: Expr = input.parse()?;
                Ok((string, expr))
            })
            .unwrap();
        Ok(Self(content))
    }
}

impl Into<Trie<char, Expr>> for Mappings {
    fn into(self) -> Trie<char, Expr> {
        let mut trie: Trie<char, Expr> = Trie(HashMap::new(), None);

        for (string, value) in self.0 {
            let mut node = &mut trie;
            for chr in string.value().chars() {
                if node.0.get(&chr).is_none() {
                    node.0.insert(chr, Trie(HashMap::new(), None));
                }
                node = node.0.get_mut(&chr).unwrap();
            }
            node.1 = Some(value);
        }

        trie
    }
}

#[proc_macro_derive(FiniteAutomataConstructor, attributes(automaton_mappings))]
pub fn stateful_trie_constructor(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let mappings = input
        .attrs
        .iter()
        .find(|attr| attr.path.is_ident("automaton_mappings"))
        .unwrap();

    let trie: Trie<char, Expr> = mappings.parse_args::<Mappings>().unwrap().into();

    let mut states: Vec<Ident> = Vec::new();
    let mut arms: Vec<Arm> = Vec::new();

    let no_state_ident = Ident::new(NO_STATE_NAME, Span::call_site().into());

    expand_trie(&trie, &mut arms, &mut states, &no_state_ident);

    // Wrap in this const thingy to not pollute global namespace but stay accessible
    let wrapper = Ident::new(
        &format!("_DERIVE_stateful_trie_constructor_for_{}", name),
        Span::call_site().into(),
    );

    let output = quote! {
        const #wrapper: () = {
            #[derive(PartialEq, Eq, Clone, Debug)]
            pub enum States {
                #no_state_ident,
                #( #states ),*
            }

            impl ::derive_finite_automaton::FiniteAutomataConstructor for #name {
                type FiniteAutomata = States;

                fn new_automaton() -> States {
                    States::#no_state_ident
                }
            }

            impl ::derive_finite_automaton::FiniteAutomata<#name> for States {
                type State = States;

                fn get_next(self, chr: char) -> ::derive_finite_automaton::GetNextResult<#name, States> {
                    match (&self, chr) {
                        #( #arms )*
                    }
                }
            }
        };
    };

    output.into()
}
