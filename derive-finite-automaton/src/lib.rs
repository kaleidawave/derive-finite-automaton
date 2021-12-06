use std::fmt::{self, Display};

pub use derive_finite_automaton_derive::FiniteAutomataConstructor;

pub trait FiniteAutomata<T>: Sized {
    /// A data type which holds state under the trie
    type State;
    fn get_next(self, c: char) -> GetNextResult<T, Self>;
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
        /// Whether the character was consumed by the action
        ate_character: bool,
    },
    NewState(FA),
    InvalidCharacter(InvalidCharacter),
}

pub type GetAutomataStateForValue<T> =
    <<T as FiniteAutomataConstructor>::FiniteAutomata as FiniteAutomata<T>>::State;

/// Character found initially which does that have transition
#[derive(Debug, PartialEq, Eq)]
pub struct InvalidCharacter {
    pub received: char,
    pub expected: &'static [char],
}

impl Display for InvalidCharacter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Expected ")?;
        match self.expected {
            [] => unreachable!(),
            [a] => f.write_fmt(format_args!("{:?}", a)),
            [a, b] => f.write_fmt(format_args!("{:?} or {:?}", a, b)),
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

impl std::error::Error for InvalidCharacter {}
