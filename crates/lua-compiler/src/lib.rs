//! `lua-compiler` — compiles a Lua AST to bytecode chunks.

pub mod chunk;
pub mod compiler;

pub use chunk::{Chunk, ProtoBuilder};
pub use compiler::Compiler;
