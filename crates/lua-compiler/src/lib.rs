//! `lua-compiler` — compiles a Lua AST to bytecode chunks.

pub mod chunk;
pub mod compiler;
pub mod decode;
pub mod disasm;
pub mod encode;

pub use chunk::{Chunk, ProtoBuilder};
pub use compiler::Compiler;
pub use decode::decode_chunk;
pub use disasm::disassemble;
pub use encode::{encode_chunk, MAGIC};
