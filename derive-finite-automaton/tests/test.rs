use std::collections::HashSet;

use derive_finite_automaton::{FiniteAutomata, FiniteAutomataConstructor, GetNextResult};

#[derive(FiniteAutomataConstructor, Debug, PartialEq)]
#[automaton_mappings(
    "{" => Tokens::OpenBrace,
    "}" => Tokens::CloseBrace,
    "=" => Tokens::Assign,
    "=>" => Tokens::ArrowFunction,
    "==" => Tokens::Equal,
    "===" => Tokens::StrictEqual,
    "." => Tokens::Dot,
    "..." => Tokens::Spread,
)]
pub enum Tokens {
    OpenBrace,
    CloseBrace,
    ArrowFunction,
    Equal,
    StrictEqual,
    Assign,
    Dot,
    Spread,
}

macro_rules! assert_is {
    ($e:expr, $p:path) => {
        match $e {
            $p(res) => res,
            v => panic!("Expected {}, found {:?}", stringify!($p), v),
        }
    };
}

#[test]
fn test() {
    let automata = Tokens::new_automaton();
    assert_eq!(
        automata.get_next('{'),
        GetNextResult::Result {
            result: Tokens::OpenBrace,
            ate_item: true
        }
    );

    let automata = Tokens::new_automaton();
    assert_eq!(
        automata.get_next('}'),
        GetNextResult::Result {
            result: Tokens::CloseBrace,
            ate_item: true
        }
    );

    let automata = Tokens::new_automaton();
    let new_automaton = assert_is!(automata.get_next('='), GetNextResult::NewState);
    assert_eq!(
        new_automaton.get_next('t'),
        GetNextResult::Result {
            result: Tokens::Assign,
            ate_item: false
        }
    );

    let automata = Tokens::new_automaton();
    let new_automaton = assert_is!(automata.get_next('='), GetNextResult::NewState);
    assert_eq!(
        new_automaton.get_next('>'),
        GetNextResult::Result {
            result: Tokens::ArrowFunction,
            ate_item: true
        }
    );

    let automata = Tokens::new_automaton();
    let res = automata.get_next('#');
    let new_automaton = assert_is!(res, GetNextResult::InvalidItem);
    assert_eq!(new_automaton.received, '#');
    assert_eq!(
        new_automaton
            .expected
            .iter()
            .cloned()
            .collect::<HashSet<char>>(),
        ['{', '}', '=', '.']
            .iter()
            .cloned()
            .collect::<HashSet<char>>()
    );
}
