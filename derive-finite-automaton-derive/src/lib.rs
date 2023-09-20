mod trie;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Arm, DeriveInput, Expr, Ident};
use trie::Trie;

pub(crate) const NO_STATE_NAME: &str = "None";

#[proc_macro_derive(FiniteAutomataConstructor, attributes(automaton_mappings))]
pub fn stateful_trie_constructor(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let mut mappings = None::<trie::Mappings>;

    let filter = input
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("automaton_mappings"));

    for attribute in filter {
        let m = attribute.parse_args::<trie::Mappings>().unwrap();
        if let Some(em) = mappings.as_mut() {
            em.extend(m);
        } else {
            mappings = Some(m);
        }
    }

    let trie: Trie<char, Expr> = mappings.unwrap().into();

    let (mut states, mut arms): (Vec<Ident>, Vec<Arm>) = Default::default();

    let no_state_ident = Ident::new(NO_STATE_NAME, Span::call_site());

    trie::expand_trie(&trie, &mut arms, &mut states, &no_state_ident);

    // Wrap in this const thingy to not pollute global namespace but stay accessible
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
