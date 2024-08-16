mod trie;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Arm, DeriveInput, Expr, Ident};
use trie::Trie;

pub(crate) const NO_STATE_NAME: &str = "None";

#[proc_macro_derive(
    FiniteAutomataConstructor,
    attributes(automaton_mappings, automaton_item_type)
)]
pub fn stateful_trie_constructor(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let mut mappings = None::<trie::Mappings>;

    let automaton_mappings = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("automaton_mappings"));

    for mapping in automaton_mappings {
        let m = mapping.parse_args::<trie::Mappings>().unwrap();
        if let Some(em) = mappings.as_mut() {
            em.extend(m);
        } else {
            mappings = Some(m);
        }
    }

    // TODO unwrap here
    let item_type: Ident = input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("automaton_item_type"))
        .map(|attr| attr.parse_args().unwrap())
        .unwrap_or_else(|| Ident::new("char", Span::call_site()));

    let trie: Trie<crate::trie::Item, Expr> = mappings.unwrap().into();

    let (mut states, mut arms): (Vec<Ident>, Vec<Arm>) = Default::default();

    let no_state_ident = Ident::new(NO_STATE_NAME, Span::call_site());

    trie::expand_trie(&trie, &mut arms, &mut states, &no_state_ident);

    // Wrap in this const thingy to not pollute global namespace but stay accessible
    // TODO this comes up in errors sometimes
    let wrapper = format_ident!(
        "_DERIVE_STATEFUL_TRIE_CONSTRUCTOR_FOR_{}",
        name.to_string().to_uppercase()
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
                type Item = #item_type;

                fn get_next(self, item: Self::Item) -> ::derive_finite_automaton::GetNextResult<#name, States> {
                    match (&self, item) {
                        #( #arms )*
                    }
                }
            }
        };
    };

    output.into()
}
