use lua_core::{LuaValue, OpCode, Proto};
use std::sync::Arc;

/// A compiled top-level chunk — thin wrapper around the root [`Proto`].
///
/// This is the artifact produced by [`Compiler::compile`] and consumed by the VM.
#[derive(Debug)]
pub struct Chunk {
    /// The root function prototype.
    pub proto: Arc<Proto>,
}

impl Chunk {
    pub fn new(proto: Proto) -> Self {
        Self {
            proto: Arc::new(proto),
        }
    }
}

// ── Proto builder helpers (used by the compiler) ─────────────────────────────

/// Mutable builder for a [`Proto`] during compilation.
#[derive(Debug, Default)]
pub struct ProtoBuilder {
    pub instructions: Vec<OpCode>,
    pub constants: Vec<LuaValue>,
    pub names: Vec<String>,
    pub protos: Vec<Arc<Proto>>,
    pub upvalue_descs: Vec<lua_core::UpvalueDesc>,
    pub param_count: u8,
    pub is_vararg: bool,
    pub source: String,
}

impl ProtoBuilder {
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            ..Default::default()
        }
    }

    pub fn emit(&mut self, op: OpCode) {
        self.instructions.push(op);
    }

    pub fn add_constant(&mut self, value: LuaValue) -> u16 {
        // deduplicate — avoid adding the same constant twice
        if let Some(idx) = self.constants.iter().position(|c| c == &value) {
            return idx as u16;
        }
        let idx = self.constants.len() as u16;
        self.constants.push(value);
        idx
    }

    pub fn add_name(&mut self, name: impl Into<String>) -> u16 {
        let name = name.into();
        if let Some(idx) = self.names.iter().position(|n| n == &name) {
            return idx as u16;
        }
        let idx = self.names.len() as u16;
        self.names.push(name);
        idx
    }

    pub fn add_proto(&mut self, proto: Arc<Proto>) -> u16 {
        let idx = self.protos.len() as u16;
        self.protos.push(proto);
        idx
    }

    pub fn finish(self) -> Proto {
        Proto {
            instructions: self.instructions,
            constants: self.constants,
            names: self.names,
            protos: self.protos,
            upvalue_descs: self.upvalue_descs,
            param_count: self.param_count,
            is_vararg: self.is_vararg,
            source: self.source,
        }
    }
}
