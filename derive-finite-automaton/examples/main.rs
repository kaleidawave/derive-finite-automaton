use derive_finite_automaton::{
    FiniteAutomata, FiniteAutomataConstructor, GetNextResult, GetAutomataStateForValue,
};

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
    Literal(String),
}

#[derive(PartialEq)]
enum LexingState {
    Symbol(GetAutomataStateForValue<Tokens>),
    Literal,
    None,
}

fn main() {
    let source = "{} {} == = === => =={as==}}a . ...a".to_owned();
    println!("Source:\n\"{}\"", source);

    let mut state = LexingState::None;
    let mut tokens = Vec::new();
    let mut start: usize = 0;

    for (idx, chr) in source.char_indices() {
        match state {
            LexingState::Literal => {
                if !matches!(chr, 'a'..='z' | 'A'..='Z') {
                    tokens.push(Tokens::Literal(source[start..idx].to_owned()));
                    start = 0;
                    state = LexingState::None;
                }
            }
            LexingState::Symbol(symbol_state) => match symbol_state.get_next(chr) {
                GetNextResult::Result {
                    result,
                    ate_character,
                } => {
                    tokens.push(result);
                    state = LexingState::None;
                    if ate_character {
                        continue;
                    }
                }
                GetNextResult::NewState(new_state) => {
                    state = LexingState::Symbol(new_state);
                }
                GetNextResult::InvalidCharacter(err) => {
                    panic!("{}", err)
                }
            },
            LexingState::None => {}
        }
        if state == LexingState::None {
            match chr {
                'a'..='z' | 'A'..='Z' => {
                    start = idx;
                    state = LexingState::Literal;
                }
                chr if chr.is_whitespace() => {}
                chr => {
                    let automaton = Tokens::new_automaton();
                    match automaton.get_next(chr) {
                        GetNextResult::Result {
                            result,
                            ate_character: _, // Should always be true
                        } => {
                            tokens.push(result);
                        }
                        GetNextResult::NewState(new_state) => {
                            state = LexingState::Symbol(new_state);
                        }
                        GetNextResult::InvalidCharacter(err) => {
                            panic!("{}", err)
                        }
                    }
                }
            }
        }
    }
    // Trailing state
    match state {
        LexingState::Literal => {
            tokens.push(Tokens::Literal(source[start..].to_owned()));
        }
        LexingState::Symbol(symbol_state) => match symbol_state.get_next(0 as char) {
            GetNextResult::Result {
                result,
                ate_character: _, // Should always be true
            } => {
                tokens.push(result);
            }
            GetNextResult::NewState(_new_state) => unreachable!(),
            GetNextResult::InvalidCharacter(err) => {
                panic!("{}", err)
            }
        },
        LexingState::None => {}
    }

    println!("Resulting tokens:\n{:#?}", tokens);
}
