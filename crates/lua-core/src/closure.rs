//! Function prototype and closure types.

use crate::opcode::OpCode;
use crate::value::LuaValue;
use std::sync::{Arc, RwLock};

// ── Function prototype ────────────────────────────────────────────────────────

/// A compiled function prototype — immutable after compilation.
///
/// Every Lua function (top-level chunk or nested lambda) compiles to a `Proto`.
/// Closures share a `Proto` via `Arc`; upvalue cells are per-instance.
#[derive(Debug)]
pub struct Proto {
    /// Bytecode instructions.
    pub instructions: Vec<OpCode>,
    /// Constant pool (integers, floats, strings).
    pub constants: Vec<LuaValue>,
    /// Global variable name strings.
    pub names: Vec<String>,
    /// Nested function prototypes referenced by `Closure` opcodes.
    pub protos: Vec<Arc<Proto>>,
    /// How to obtain each upvalue when the closure is instantiated.
    pub upvalue_descs: Vec<UpvalueDesc>,
    /// Number of fixed parameters.
    pub param_count: u8,
    /// Whether the function accepts varargs (`...`).
    pub is_vararg: bool,
    /// Debug: source name.
    pub source: String,
}

impl Proto {
    /// Create an empty proto with the given source label.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            names: Vec::new(),
            protos: Vec::new(),
            upvalue_descs: Vec::new(),
            param_count: 0,
            is_vararg: false,
            source: source.into(),
        }
    }
}

// ── Upvalue descriptors ───────────────────────────────────────────────────────

/// Describes where to find an upvalue at closure instantiation time.
#[derive(Debug, Clone)]
pub enum UpvalueDesc {
    /// Capture the local at register `reg` in the **immediately enclosing** call frame.
    Stack(u8),
    /// Re-use upvalue at index `idx` from the **enclosing** closure.
    Upvalue(u8),
}

// ── Upvalue cells ─────────────────────────────────────────────────────────────

/// A shared, mutable upvalue cell.
///
/// While the captured local is still on the stack (`Open`), the cell holds its
/// register number. When the local goes out of scope (`CloseUpvalues`), the
/// runtime copies the value into the cell (`Closed`).
#[derive(Debug, Clone)]
pub struct Upvalue(pub Arc<RwLock<UpvalueInner>>);

impl Upvalue {
    /// Create an open upvalue pointing at `reg` in the current frame.
    pub fn open(reg: u8) -> Self {
        Self(Arc::new(RwLock::new(UpvalueInner::Open(reg))))
    }

    /// Create a closed (heap-allocated) upvalue with the given initial value.
    pub fn closed(val: LuaValue) -> Self {
        Self(Arc::new(RwLock::new(UpvalueInner::Closed(val))))
    }
}

/// Interior state of an upvalue cell.
#[derive(Debug, Clone)]
pub enum UpvalueInner {
    /// The value is still alive in the enclosing frame's register `reg`.
    Open(u8),
    /// The enclosing frame exited; the value was migrated here.
    Closed(LuaValue),
}

// ── Lua closure ───────────────────────────────────────────────────────────────

/// A runtime closure: a `Proto` paired with its captured upvalue cells.
#[derive(Debug)]
pub struct LuaClosure {
    /// The compiled function body.
    pub proto: Arc<Proto>,
    /// Upvalue cells, one per `proto.upvalue_descs` entry.
    pub upvalues: Vec<Upvalue>,
}

impl LuaClosure {
    pub fn new(proto: Arc<Proto>, upvalues: Vec<Upvalue>) -> Self {
        Self { proto, upvalues }
    }
}
