# derive finite automaton

[![crates.io badge](https://img.shields.io/crates/v/derive-finite-automaton?style=flat-square)](https://crates.io/crates/derive-finite-automaton)
[![docs.rs badge](https://img.shields.io/docsrs/derive-finite-automaton?style=flat-square)](https://docs.rs/derive-finite-automaton/latest)

A procedural macro for building a finite automaton. Main use is for lexing multiple item wide tokens see [`derive-finite-automaton/examples/main.rs`](derive-finite-automaton/examples/main.rs).

Run example:

```shell
cd derive-finite-automaton
cargo run --example main
```

View example macro expansion (requires [cargo-expand](https://github.com/dtolnay/cargo-expand)):

```shell
cd derive-finite-automaton
cargo expand --example main
```

## Example

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

You can add conditional mappings with the following

```rust
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
```
