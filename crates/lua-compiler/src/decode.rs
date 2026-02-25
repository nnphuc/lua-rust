//! Binary decoder: `&[u8]` → `Chunk`.
//!
//! Mirrors the encoding in `encode.rs` exactly.

use std::sync::Arc;

use lua_core::{LuaValue, OpCode, Proto, UpvalueDesc};

use crate::chunk::Chunk;
use crate::encode::MAGIC;

// ── Cursor reader ─────────────────────────────────────────────────────────────

struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn remaining(&self) -> usize {
        self.data.len() - self.pos
    }

    fn read_bytes(&mut self, n: usize) -> Result<&'a [u8], String> {
        if self.remaining() < n {
            return Err(format!(
                "unexpected end of data: need {n} bytes at offset {}",
                self.pos
            ));
        }
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    fn read_u8(&mut self) -> Result<u8, String> {
        let b = self.read_bytes(1)?;
        Ok(b[0])
    }

    fn read_u16_le(&mut self) -> Result<u16, String> {
        let b = self.read_bytes(2)?;
        Ok(u16::from_le_bytes([b[0], b[1]]))
    }

    fn read_u32_le(&mut self) -> Result<u32, String> {
        let b = self.read_bytes(4)?;
        Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }

    fn read_i16_le(&mut self) -> Result<i16, String> {
        let b = self.read_bytes(2)?;
        Ok(i16::from_le_bytes([b[0], b[1]]))
    }

    fn read_i64_le(&mut self) -> Result<i64, String> {
        let b = self.read_bytes(8)?;
        Ok(i64::from_le_bytes(b.try_into().unwrap()))
    }

    fn read_f64_le(&mut self) -> Result<f64, String> {
        let b = self.read_bytes(8)?;
        Ok(f64::from_le_bytes(b.try_into().unwrap()))
    }

    fn read_str(&mut self) -> Result<String, String> {
        let len = self.read_u16_le()? as usize;
        let bytes = self.read_bytes(len)?;
        String::from_utf8(bytes.to_vec())
            .map_err(|e| format!("invalid UTF-8 in string: {e}"))
    }
}

// ── Proto decoder ─────────────────────────────────────────────────────────────

fn decode_proto(r: &mut Reader<'_>) -> Result<Proto, String> {
    let source = r.read_str()?;
    let param_count = r.read_u8()?;
    let is_vararg = r.read_u8()? != 0;

    // constants
    let const_count = r.read_u16_le()? as usize;
    let mut constants = Vec::with_capacity(const_count);
    for _ in 0..const_count {
        constants.push(decode_constant(r)?);
    }

    // names
    let name_count = r.read_u16_le()? as usize;
    let mut names = Vec::with_capacity(name_count);
    for _ in 0..name_count {
        names.push(r.read_str()?);
    }

    // upvalue_descs
    let upval_count = r.read_u16_le()? as usize;
    let mut upvalue_descs = Vec::with_capacity(upval_count);
    for _ in 0..upval_count {
        let tag = r.read_u8()?;
        let val = r.read_u8()?;
        let desc = match tag {
            0 => UpvalueDesc::Stack(val),
            1 => UpvalueDesc::Upvalue(val),
            t => return Err(format!("unknown upvalue tag: {t}")),
        };
        upvalue_descs.push(desc);
    }

    // nested protos
    let proto_count = r.read_u16_le()? as usize;
    let mut protos = Vec::with_capacity(proto_count);
    for _ in 0..proto_count {
        protos.push(Arc::new(decode_proto(r)?));
    }

    // instructions
    let instr_count = r.read_u32_le()? as usize;
    let mut instructions = Vec::with_capacity(instr_count);
    for _ in 0..instr_count {
        instructions.push(decode_opcode(r)?);
    }

    Ok(Proto {
        instructions,
        constants,
        names,
        protos,
        upvalue_descs,
        param_count,
        is_vararg,
        source,
    })
}

fn decode_constant(r: &mut Reader<'_>) -> Result<LuaValue, String> {
    let tag = r.read_u8()?;
    match tag {
        0 => Ok(LuaValue::Nil),
        1 => Ok(LuaValue::Boolean(r.read_u8()? != 0)),
        2 => Ok(LuaValue::Integer(r.read_i64_le()?)),
        3 => Ok(LuaValue::Float(r.read_f64_le()?)),
        4 => Ok(LuaValue::LuaString(r.read_str()?)),
        t => Err(format!("unknown constant tag: {t}")),
    }
}

fn decode_opcode(r: &mut Reader<'_>) -> Result<OpCode, String> {
    let tag = r.read_u8()?;
    match tag {
        0 => Ok(OpCode::LoadConst {
            dst: r.read_u8()?,
            const_idx: r.read_u16_le()?,
        }),
        1 => Ok(OpCode::LoadNil {
            dst: r.read_u8()?,
        }),
        2 => Ok(OpCode::LoadBool {
            dst: r.read_u8()?,
            value: r.read_u8()? != 0,
            skip: r.read_u8()? != 0,
        }),
        3 => Ok(OpCode::Move {
            dst: r.read_u8()?,
            src: r.read_u8()?,
        }),
        4 => Ok(OpCode::Add {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        5 => Ok(OpCode::Sub {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        6 => Ok(OpCode::Mul {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        7 => Ok(OpCode::Div {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        8 => Ok(OpCode::Mod {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        9 => Ok(OpCode::Pow {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        10 => Ok(OpCode::IDiv {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        11 => Ok(OpCode::Unm {
            dst: r.read_u8()?,
            src: r.read_u8()?,
        }),
        12 => Ok(OpCode::Eq {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        13 => Ok(OpCode::Lt {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        14 => Ok(OpCode::Le {
            dst: r.read_u8()?,
            lhs: r.read_u8()?,
            rhs: r.read_u8()?,
        }),
        15 => Ok(OpCode::Not {
            dst: r.read_u8()?,
            src: r.read_u8()?,
        }),
        16 => Ok(OpCode::Jump {
            offset: r.read_i16_le()?,
        }),
        17 => Ok(OpCode::JumpIfFalse {
            src: r.read_u8()?,
            offset: r.read_i16_le()?,
        }),
        18 => Ok(OpCode::JumpIfTrue {
            src: r.read_u8()?,
            offset: r.read_i16_le()?,
        }),
        19 => Ok(OpCode::Concat {
            dst: r.read_u8()?,
            start: r.read_u8()?,
            end: r.read_u8()?,
        }),
        20 => Ok(OpCode::Len {
            dst: r.read_u8()?,
            src: r.read_u8()?,
        }),
        21 => Ok(OpCode::Call {
            func: r.read_u8()?,
            num_args: r.read_u8()?,
            num_results: r.read_u8()?,
        }),
        22 => Ok(OpCode::Return {
            src: r.read_u8()?,
            num_results: r.read_u8()?,
        }),
        23 => Ok(OpCode::GetGlobal {
            dst: r.read_u8()?,
            name_idx: r.read_u16_le()?,
        }),
        24 => Ok(OpCode::SetGlobal {
            src: r.read_u8()?,
            name_idx: r.read_u16_le()?,
        }),
        25 => Ok(OpCode::Closure {
            dst: r.read_u8()?,
            proto_idx: r.read_u16_le()?,
        }),
        26 => Ok(OpCode::GetUpvalue {
            dst: r.read_u8()?,
            upval_idx: r.read_u8()?,
        }),
        27 => Ok(OpCode::SetUpvalue {
            src: r.read_u8()?,
            upval_idx: r.read_u8()?,
        }),
        28 => Ok(OpCode::CloseUpvalues {
            from_reg: r.read_u8()?,
        }),
        29 => Ok(OpCode::NewTable {
            dst: r.read_u8()?,
        }),
        30 => Ok(OpCode::GetTable {
            dst: r.read_u8()?,
            table: r.read_u8()?,
            key: r.read_u8()?,
        }),
        31 => Ok(OpCode::SetTable {
            table: r.read_u8()?,
            key: r.read_u8()?,
            val: r.read_u8()?,
        }),
        32 => Ok(OpCode::GetField {
            dst: r.read_u8()?,
            table: r.read_u8()?,
            name_idx: r.read_u16_le()?,
        }),
        33 => Ok(OpCode::SetField {
            table: r.read_u8()?,
            name_idx: r.read_u16_le()?,
            val: r.read_u8()?,
        }),
        34 => Ok(OpCode::SetList {
            table: r.read_u8()?,
            src: r.read_u8()?,
            count: r.read_u8()?,
        }),
        35 => Ok(OpCode::VarArg {
            dst: r.read_u8()?,
            count: r.read_u8()?,
        }),
        t => Err(format!("unknown opcode tag: {t}")),
    }
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Decode a byte slice (previously produced by `encode_chunk`) back into a `Chunk`.
///
/// Returns `Err(String)` if the magic bytes are missing or the data is malformed.
pub fn decode_chunk(bytes: &[u8]) -> Result<Chunk, String> {
    if !bytes.starts_with(MAGIC) {
        return Err("not a lua-rust bytecode file (bad magic)".to_string());
    }
    let mut r = Reader::new(&bytes[MAGIC.len()..]);
    let proto = decode_proto(&mut r)?;
    Ok(Chunk::new(proto))
}
