//! `lua-core` — foundational types shared across the entire lua-rust workspace.
//!
//! This crate defines:
//! - [`LuaValue`]: the dynamically-typed Lua value enum
//! - [`LuaError`]: the unified error type
//! - Bytecode [`OpCode`] definitions
//! - Key traits (`Vm`, `Compile`)

pub mod closure;
pub mod error;
pub mod opcode;
pub mod value;

pub use closure::{LuaClosure, Proto, Upvalue, UpvalueDesc, UpvalueInner};
pub use error::LuaError;
pub use opcode::OpCode;
pub use value::LuaValue;
