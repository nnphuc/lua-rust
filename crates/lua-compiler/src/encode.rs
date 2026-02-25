//! Binary encoder: `Chunk` → `Vec<u8>`.
//!
//! Format: MAGIC (6 bytes) followed by a recursive Proto encoding.

use lua_core::{LuaValue, OpCode, Proto, UpvalueDesc};

use crate::chunk::Chunk;

/// Magic bytes that identify a compiled lua-rust bytecode file.
pub const MAGIC: &[u8] = b"\x1bLuaR\x01";

// ── Low-level write helpers ────────────────────────────────────────────────

fn push_u8(buf: &mut Vec<u8>, v: u8) {
    buf.push(v);
}

fn push_u16_le(buf: &mut Vec<u8>, v: u16) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn push_u32_le(buf: &mut Vec<u8>, v: u32) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn push_i16_le(buf: &mut Vec<u8>, v: i16) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn push_i64_le(buf: &mut Vec<u8>, v: i64) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn push_f64_le(buf: &mut Vec<u8>, v: f64) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn push_str(buf: &mut Vec<u8>, s: &str) {
    let bytes = s.as_bytes();
    push_u16_le(buf, bytes.len() as u16);
    buf.extend_from_slice(bytes);
}

// ── Proto encoder ──────────────────────────────────────────────────────────

fn encode_proto(proto: &Proto, buf: &mut Vec<u8>) {
    // source
    push_str(buf, &proto.source);

    // param_count, is_vararg
    push_u8(buf, proto.param_count);
    push_u8(buf, proto.is_vararg as u8);

    // constants
    push_u16_le(buf, proto.constants.len() as u16);
    for c in &proto.constants {
        encode_constant(c, buf);
    }

    // names
    push_u16_le(buf, proto.names.len() as u16);
    for name in &proto.names {
        push_str(buf, name);
    }

    // upvalue_descs
    push_u16_le(buf, proto.upvalue_descs.len() as u16);
    for desc in &proto.upvalue_descs {
        match desc {
            UpvalueDesc::Stack(reg) => {
                push_u8(buf, 0);
                push_u8(buf, *reg);
            }
            UpvalueDesc::Upvalue(idx) => {
                push_u8(buf, 1);
                push_u8(buf, *idx);
            }
        }
    }

    // nested protos
    push_u16_le(buf, proto.protos.len() as u16);
    for p in &proto.protos {
        encode_proto(p, buf);
    }

    // instructions
    push_u32_le(buf, proto.instructions.len() as u32);
    for op in &proto.instructions {
        encode_opcode(op, buf);
    }
}

fn encode_constant(val: &LuaValue, buf: &mut Vec<u8>) {
    match val {
        LuaValue::Nil => {
            push_u8(buf, 0);
        }
        LuaValue::Boolean(b) => {
            push_u8(buf, 1);
            push_u8(buf, *b as u8);
        }
        LuaValue::Integer(n) => {
            push_u8(buf, 2);
            push_i64_le(buf, *n);
        }
        LuaValue::Float(f) => {
            push_u8(buf, 3);
            push_f64_le(buf, *f);
        }
        LuaValue::LuaString(s) => {
            push_u8(buf, 4);
            push_str(buf, s);
        }
        // Non-serialisable types (tables, closures, etc.) are never stored as
        // compile-time constants, but we emit a placeholder to avoid panicking.
        _ => {
            push_u8(buf, 0); // treat as Nil
        }
    }
}

fn encode_opcode(op: &OpCode, buf: &mut Vec<u8>) {
    match op {
        OpCode::LoadConst { dst, const_idx } => {
            push_u8(buf, 0);
            push_u8(buf, *dst);
            push_u16_le(buf, *const_idx);
        }
        OpCode::LoadNil { dst } => {
            push_u8(buf, 1);
            push_u8(buf, *dst);
        }
        OpCode::LoadBool { dst, value, skip } => {
            push_u8(buf, 2);
            push_u8(buf, *dst);
            push_u8(buf, *value as u8);
            push_u8(buf, *skip as u8);
        }
        OpCode::Move { dst, src } => {
            push_u8(buf, 3);
            push_u8(buf, *dst);
            push_u8(buf, *src);
        }
        OpCode::Add { dst, lhs, rhs } => {
            push_u8(buf, 4);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Sub { dst, lhs, rhs } => {
            push_u8(buf, 5);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Mul { dst, lhs, rhs } => {
            push_u8(buf, 6);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Div { dst, lhs, rhs } => {
            push_u8(buf, 7);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Mod { dst, lhs, rhs } => {
            push_u8(buf, 8);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Pow { dst, lhs, rhs } => {
            push_u8(buf, 9);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::IDiv { dst, lhs, rhs } => {
            push_u8(buf, 10);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Unm { dst, src } => {
            push_u8(buf, 11);
            push_u8(buf, *dst);
            push_u8(buf, *src);
        }
        OpCode::Eq { dst, lhs, rhs } => {
            push_u8(buf, 12);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Lt { dst, lhs, rhs } => {
            push_u8(buf, 13);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Le { dst, lhs, rhs } => {
            push_u8(buf, 14);
            push_u8(buf, *dst);
            push_u8(buf, *lhs);
            push_u8(buf, *rhs);
        }
        OpCode::Not { dst, src } => {
            push_u8(buf, 15);
            push_u8(buf, *dst);
            push_u8(buf, *src);
        }
        OpCode::Jump { offset } => {
            push_u8(buf, 16);
            push_i16_le(buf, *offset);
        }
        OpCode::JumpIfFalse { src, offset } => {
            push_u8(buf, 17);
            push_u8(buf, *src);
            push_i16_le(buf, *offset);
        }
        OpCode::JumpIfTrue { src, offset } => {
            push_u8(buf, 18);
            push_u8(buf, *src);
            push_i16_le(buf, *offset);
        }
        OpCode::Concat { dst, start, end } => {
            push_u8(buf, 19);
            push_u8(buf, *dst);
            push_u8(buf, *start);
            push_u8(buf, *end);
        }
        OpCode::Len { dst, src } => {
            push_u8(buf, 20);
            push_u8(buf, *dst);
            push_u8(buf, *src);
        }
        OpCode::Call { func, num_args, num_results } => {
            push_u8(buf, 21);
            push_u8(buf, *func);
            push_u8(buf, *num_args);
            push_u8(buf, *num_results);
        }
        OpCode::Return { src, num_results } => {
            push_u8(buf, 22);
            push_u8(buf, *src);
            push_u8(buf, *num_results);
        }
        OpCode::GetGlobal { dst, name_idx } => {
            push_u8(buf, 23);
            push_u8(buf, *dst);
            push_u16_le(buf, *name_idx);
        }
        OpCode::SetGlobal { src, name_idx } => {
            push_u8(buf, 24);
            push_u8(buf, *src);
            push_u16_le(buf, *name_idx);
        }
        OpCode::Closure { dst, proto_idx } => {
            push_u8(buf, 25);
            push_u8(buf, *dst);
            push_u16_le(buf, *proto_idx);
        }
        OpCode::GetUpvalue { dst, upval_idx } => {
            push_u8(buf, 26);
            push_u8(buf, *dst);
            push_u8(buf, *upval_idx);
        }
        OpCode::SetUpvalue { src, upval_idx } => {
            push_u8(buf, 27);
            push_u8(buf, *src);
            push_u8(buf, *upval_idx);
        }
        OpCode::CloseUpvalues { from_reg } => {
            push_u8(buf, 28);
            push_u8(buf, *from_reg);
        }
        OpCode::NewTable { dst } => {
            push_u8(buf, 29);
            push_u8(buf, *dst);
        }
        OpCode::GetTable { dst, table, key } => {
            push_u8(buf, 30);
            push_u8(buf, *dst);
            push_u8(buf, *table);
            push_u8(buf, *key);
        }
        OpCode::SetTable { table, key, val } => {
            push_u8(buf, 31);
            push_u8(buf, *table);
            push_u8(buf, *key);
            push_u8(buf, *val);
        }
        OpCode::GetField { dst, table, name_idx } => {
            push_u8(buf, 32);
            push_u8(buf, *dst);
            push_u8(buf, *table);
            push_u16_le(buf, *name_idx);
        }
        OpCode::SetField { table, name_idx, val } => {
            push_u8(buf, 33);
            push_u8(buf, *table);
            push_u16_le(buf, *name_idx);
            push_u8(buf, *val);
        }
        OpCode::SetList { table, src, count } => {
            push_u8(buf, 34);
            push_u8(buf, *table);
            push_u8(buf, *src);
            push_u8(buf, *count);
        }
        OpCode::VarArg { dst, count } => {
            push_u8(buf, 35);
            push_u8(buf, *dst);
            push_u8(buf, *count);
        }
        // Safety net for any future opcodes added with #[non_exhaustive]
        _ => {
            // Emit a no-op LoadNil to register 0 as a placeholder
            push_u8(buf, 1);
            push_u8(buf, 0);
        }
    }
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Encode a compiled `Chunk` to a byte vector suitable for writing to a `.luac` file.
pub fn encode_chunk(chunk: &Chunk) -> Vec<u8> {
    let mut buf = MAGIC.to_vec();
    encode_proto(&chunk.proto, &mut buf);
    buf
}
