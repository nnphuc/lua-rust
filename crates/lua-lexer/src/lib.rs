//! `lua-lexer` — converts raw Lua source text into a stream of [`Token`]s.

pub mod lexer;
pub mod token;

pub use lexer::Lexer;
pub use token::{Token, TokenKind};
