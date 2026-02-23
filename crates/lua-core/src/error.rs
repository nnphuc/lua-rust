use thiserror::Error;

/// All errors that can occur within the lua-rust runtime.
#[derive(Debug, Error, PartialEq)]
pub enum LuaError {
    /// A syntax / parse error.
    #[error("syntax error at line {line}: {message}")]
    Syntax { line: u32, message: String },

    /// A runtime error (equivalent to Lua's `error()` function).
    #[error("runtime error: {0}")]
    Runtime(String),

    /// Wrong type used for an operation.
    #[error("type error: expected {expected}, got {got}")]
    TypeError {
        expected: &'static str,
        got: &'static str,
    },

    /// Stack overflow.
    #[error("stack overflow")]
    StackOverflow,

    /// Internal implementation bug — should never surface to users.
    #[error("internal error: {0}")]
    Internal(String),

    /// Internal control-flow signal used by coroutine.yield.
    #[error("yield")]
    Yield(Vec<crate::value::LuaValue>),
}
