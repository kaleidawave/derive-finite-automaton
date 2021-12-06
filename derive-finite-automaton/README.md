# derive-finite-automaton

[![Crates](https://img.shields.io/crates/v/derive-finite-automaton.svg)](https://crates.io/crates/derive-finite-automaton)

A procedural macro for building a finite automaton. Main use is for lexing multiple character wide tokens see [`derive-finite-automaton/examples/main.rs`](derive-finite-automaton/examples/main.rs).

Run example:
```
cd derive-finite-automaton
cargo run --example main
```

View example macro expansion (requires [cargo-expand](https://github.com/dtolnay/cargo-expand)):
```
cd derive-finite-automaton
cargo expand --example main
```

### Example

```rust
use derive_finite_automaton::{FiniteAutomata, FiniteAutomataConstructor};

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
```