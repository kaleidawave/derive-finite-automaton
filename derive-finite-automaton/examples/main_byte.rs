use derive_finite_automaton::{
    FiniteAutomata, FiniteAutomataConstructor, GetAutomataStateForValue, GetNextResult,
};

#[derive(Debug, FiniteAutomataConstructor)]
#[automaton_item_type(u8)]
#[automaton_mappings(
    b"{" => Tokens::OpenBrace,
    b"}" => Tokens::CloseBrace,
    b"<" => Tokens::LessThan,
    b">" => Tokens::GreaterThan,
    b"=>" => Tokens::ArrowFunction,
    b"==" => Tokens::Equal,
    b"===" => Tokens::StrictEqual,
    b"=" => Tokens::Assign,
    // Some mapping
    b"." => Tokens::Dot,
    b"..." => Tokens::Spread,
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
    Literal(Vec<u8>),
}

#[derive(PartialEq)]
enum LexingState {
    Symbol(GetAutomataStateForValue<Tokens>),
    Literal,
    None,
}

fn main() {
    let now = std::time::Instant::now();

    let source = if let Ok(source) = std::fs::read("./private/tokens.txt") {
        source
    } else {
        eprintln!("using internal source");
        b"{} {} == = === => =={as==}}a . ...a".to_vec()
    };

    let mut state = LexingState::None;
    let mut tokens = Vec::new();
    let mut start: usize = 0;

    for (idx, byte) in source.clone().into_iter().enumerate() {
        match state {
            LexingState::Literal => {
                if !matches!(byte, b'a'..=b'z' | b'A'..=b'Z') {
                    tokens.push(Tokens::Literal(source[start..idx].to_vec()));
                    start = 0;
                    state = LexingState::None;
                }
            }
            LexingState::Symbol(symbol_state) => match symbol_state.get_next(byte) {
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
            match byte {
                b'a'..=b'z' | b'A'..=b'Z' => {
                    start = idx;
                    state = LexingState::Literal;
                }
                b' ' => {}
                byte => {
                    let automaton = Tokens::new_automaton();
                    match automaton.get_next(byte) {
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
            tokens.push(Tokens::Literal(source[start..].to_vec()));
        }
        LexingState::Symbol(symbol_state) => match symbol_state.get_next(0) {
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
