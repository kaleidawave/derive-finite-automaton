use derive_finite_automaton::{
    FiniteAutomata, FiniteAutomataConstructor, GetAutomataStateForValue, GetNextResult,
};

#[derive(Debug, FiniteAutomataConstructor)]
#[automaton_mappings(
    "{" => Tokens::OpenBrace,
    "}" => Tokens::CloseBrace,
    "<" => Tokens::LessThan,
    ">" => Tokens::GreaterThan,
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
    LessThan,
    GreaterThan,
    Literal(String),
}

#[derive(PartialEq)]
enum LexingState {
    Symbol(GetAutomataStateForValue<Tokens>),
    Literal,
    None,
}

fn main() {
    let now = std::time::Instant::now();

    let source = if let Ok(source) = std::fs::read_to_string("./private/tokens.txt") {
        source
    } else {
        eprintln!("using internal source");
        "{} {} == = === => =={as==}}a . ...a".to_owned()
    };

    let mut state = LexingState::None;
    let mut tokens = Vec::new();
    let mut start: usize = 0;

    for (idx, chr) in source.char_indices() {
        match state {
            LexingState::Literal => {
                if !chr.is_ascii_alphabetic() {
                    tokens.push(Tokens::Literal(source[start..idx].to_owned()));
                    start = 0;
                    state = LexingState::None;
                }
            }
            LexingState::Symbol(symbol_state) => match symbol_state.get_next(chr) {
                GetNextResult::Result { result, ate_item } => {
                    tokens.push(result);
                    state = LexingState::None;
                    if ate_item {
                        continue;
                    }
                }
                GetNextResult::NewState(new_state) => {
                    state = LexingState::Symbol(new_state);
                }
                GetNextResult::InvalidItem(err) => {
                    panic!("{} @ {}", err, idx)
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
                            ate_item: _, // Should always be true
                        } => {
                            tokens.push(result);
                        }
                        GetNextResult::NewState(new_state) => {
                            state = LexingState::Symbol(new_state);
                        }
                        GetNextResult::InvalidItem(err) => {
                            panic!("{} @ {}", err, idx)
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
                ate_item: _, // Should always be true
            } => {
                tokens.push(result);
            }
            GetNextResult::NewState(_new_state) => unreachable!(),
            GetNextResult::InvalidItem(err) => {
                panic!("{}", err)
            }
        },
        LexingState::None => {}
    }

    eprintln!("{} tokens in {:?}", tokens.len(), now.elapsed());
}
