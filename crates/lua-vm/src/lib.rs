//! `lua-vm` — register-based virtual machine that executes `Chunk` bytecode.

pub mod stdlib;
pub mod vm;

pub use vm::Vm;
