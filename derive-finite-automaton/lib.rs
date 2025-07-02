#![doc = include_str!("./README.md")]

use std::fmt::{self, Display};

pub use derive_finite_automaton_derive::FiniteAutomataConstructor;

pub trait FiniteAutomata<T>: Sized {
    /// A data type which holds state under the trie
    type State;
    type Item: std::fmt::Debug + PartialEq + Eq + 'static;

    fn get_next(self, c: Self::Item) -> GetNextResult<T, Self>;
}

/// A type for which a stateful trie can be built
pub trait FiniteAutomataConstructor: Sized {
    type FiniteAutomata: FiniteAutomata<Self>;

    fn new_automaton() -> Self::FiniteAutomata;
}

/// Where 'T' is the type the stateful trie returns
/// TODO the result should be only on the first result not the second ...
#[derive(Debug, PartialEq, Eq)]
pub enum GetNextResult<T, FA: FiniteAutomata<T>> {
    Result {
        result: T,
        /// Whether the item was consumed by the action
        ate_item: bool,
    },
    NewState(FA),
    InvalidItem(InvalidItem<FA::Item>),
}

/// Helper for getting the state item used in the automata
pub type GetAutomataStateForValue<T> =
    <<T as FiniteAutomataConstructor>::FiniteAutomata as FiniteAutomata<T>>::State;

/// Item found initially which does that have transition
#[derive(Debug, PartialEq, Eq)]
pub struct InvalidItem<T>
where
    T: std::fmt::Debug + PartialEq + Eq + 'static,
{
    pub received: T,
    pub expected: &'static [T],
}

impl<T> Display for InvalidItem<T>
where
    T: std::fmt::Debug + PartialEq + Eq,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Expected ")?;
        match self.expected {
            [] => unreachable!(),
            [a] => f.write_fmt(format_args!("{:?}", a)),
            // [a, b] => f.write_fmt(format_args!("{:?} or {:?}", a, b)),
            [head @ .., end] => f.write_fmt(format_args!(
                "{} or {:?}",
                head.iter()
                    .map(|chr| format!("{:?}", chr))
                    .reduce(|mut a, b| {
                        a.push_str(", ");
                        a.push_str(&b);
                        a
                    })
                    .unwrap(),
                end
            )),
        }?;
        write!(f, " found {:?}", self.received)
    }
}

impl<T> std::error::Error for InvalidItem<T> where T: std::fmt::Debug + PartialEq + Eq + 'static {}
