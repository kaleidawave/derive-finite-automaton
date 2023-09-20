use derive_finite_automaton::FiniteAutomataConstructor;

#[derive(Debug, FiniteAutomataConstructor)]
#[automaton_mappings(
    "{" => Tokens::OpenBrace,
    "}" => Tokens::CloseBrace,
    "=>" => Tokens::ArrowFunction,
    "==" => Tokens::Equal,
    "===" => Tokens::StrictEqual,
    "=" => Tokens::Assign,
    // Some mapping
    "." => Tokens::Dot,
)]
#[cfg_attr(feature = "special", automaton_mappings(
    ".?." => Tokens::Magic,
))]
pub enum Tokens {
    OpenBrace,
    CloseBrace,
    ArrowFunction,
    Equal,
    StrictEqual,
    Assign,
    Dot,
    #[cfg(feature = "special")]
    Magic,
}

fn main() {}
