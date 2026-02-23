//! `lua-parser` — builds an AST from a token stream produced by `lua-lexer`.

pub mod ast;
pub mod parser;

pub use ast::Block;
pub use parser::Parser;
