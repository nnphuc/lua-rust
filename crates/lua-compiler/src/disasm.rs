use lua_core::{OpCode, Proto};

/// Disassemble a [`Proto`] into a human-readable string.
///
/// Recursively disassembles any nested `protos[]` so you see the full picture.
pub fn disassemble(proto: &Proto) -> String {
    let mut out = String::new();
    disasm_proto(proto, &mut out);
    out
}

fn disasm_proto(proto: &Proto, out: &mut String) {
    // Header
    let name = if proto.source.is_empty() {
        "<?>".to_string()
    } else {
        proto.source.clone()
    };
    out.push_str(&format!(
        "== {} ==  (params={}, vararg={})\n",
        name, proto.param_count, proto.is_vararg
    ));

    // Constants pool
    if !proto.constants.is_empty() {
        out.push_str("constants:\n");
        for (i, c) in proto.constants.iter().enumerate() {
            out.push_str(&format!("  [K{i}]  {}\n", fmt_value(c)));
        }
    }

    // Names (globals)
    if !proto.names.is_empty() {
        out.push_str("names:\n");
        for (i, n) in proto.names.iter().enumerate() {
            out.push_str(&format!("  [N{i}]  {n}\n"));
        }
    }

    // Upvalue descriptors
    if !proto.upvalue_descs.is_empty() {
        out.push_str("upvalues:\n");
        for (i, uv) in proto.upvalue_descs.iter().enumerate() {
            let desc = match uv {
                lua_core::UpvalueDesc::Stack(reg) => format!("stack reg={reg}"),
                lua_core::UpvalueDesc::Upvalue(idx) => format!("upvalue idx={idx}"),
            };
            out.push_str(&format!("  [U{i}]  {desc}\n"));
        }
    }

    // Instructions
    out.push_str("instructions:\n");
    for (i, op) in proto.instructions.iter().enumerate() {
        let line = fmt_instruction(i, op, proto);
        out.push_str(&format!("  {line}\n"));
    }

    // Nested protos
    for (i, sub) in proto.protos.iter().enumerate() {
        out.push('\n');
        out.push_str(&format!("-- sub-proto {i} (of {}) --\n", proto.source));
        disasm_proto(sub, out);
    }
}

fn fmt_value(v: &lua_core::LuaValue) -> String {
    match v {
        lua_core::LuaValue::Nil => "nil".to_string(),
        lua_core::LuaValue::Boolean(b) => b.to_string(),
        lua_core::LuaValue::Integer(n) => n.to_string(),
        lua_core::LuaValue::Float(f) => {
            if f.fract() == 0.0 {
                format!("{f:.1}")
            } else {
                f.to_string()
            }
        }
        lua_core::LuaValue::LuaString(s) => format!("{s:?}"),
        _ => format!("{v:?}"),
    }
}

fn fmt_instruction(idx: usize, op: &OpCode, proto: &Proto) -> String {
    let prefix = format!("{idx:04}");
    match op {
        OpCode::LoadConst { dst, const_idx } => {
            let val = proto
                .constants
                .get(*const_idx as usize)
                .map(fmt_value)
                .unwrap_or_else(|| "?".to_string());
            format!("{prefix}  LoadConst     dst={dst}  K{const_idx}({val})")
        }
        OpCode::LoadNil { dst } => format!("{prefix}  LoadNil       dst={dst}"),
        OpCode::LoadBool { dst, value, skip } => {
            format!("{prefix}  LoadBool      dst={dst}  value={value}  skip={skip}")
        }
        OpCode::Move { dst, src } => format!("{prefix}  Move          dst={dst}  src={src}"),

        OpCode::Add { dst, lhs, rhs } => {
            format!("{prefix}  Add           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Sub { dst, lhs, rhs } => {
            format!("{prefix}  Sub           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Mul { dst, lhs, rhs } => {
            format!("{prefix}  Mul           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Div { dst, lhs, rhs } => {
            format!("{prefix}  Div           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Mod { dst, lhs, rhs } => {
            format!("{prefix}  Mod           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Pow { dst, lhs, rhs } => {
            format!("{prefix}  Pow           dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::IDiv { dst, lhs, rhs } => {
            format!("{prefix}  IDiv          dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Unm { dst, src } => format!("{prefix}  Unm           dst={dst}  src={src}"),

        OpCode::Eq { dst, lhs, rhs } => {
            format!("{prefix}  Eq            dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Lt { dst, lhs, rhs } => {
            format!("{prefix}  Lt            dst={dst}  lhs={lhs}  rhs={rhs}")
        }
        OpCode::Le { dst, lhs, rhs } => {
            format!("{prefix}  Le            dst={dst}  lhs={lhs}  rhs={rhs}")
        }

        OpCode::Not { dst, src } => format!("{prefix}  Not           dst={dst}  src={src}"),

        OpCode::Jump { offset } => format!("{prefix}  Jump          offset={offset:+}"),
        OpCode::JumpIfFalse { src, offset } => {
            format!("{prefix}  JumpIfFalse   src={src}  offset={offset:+}")
        }
        OpCode::JumpIfTrue { src, offset } => {
            format!("{prefix}  JumpIfTrue    src={src}  offset={offset:+}")
        }

        OpCode::Concat { dst, start, end } => {
            format!("{prefix}  Concat        dst={dst}  start={start}  end={end}")
        }
        OpCode::Len { dst, src } => format!("{prefix}  Len           dst={dst}  src={src}"),

        OpCode::Call {
            func,
            num_args,
            num_results,
        } => format!("{prefix}  Call          func={func}  args={num_args}  results={num_results}"),
        OpCode::Return { src, num_results } => {
            format!("{prefix}  Return        src={src}  num={num_results}")
        }

        OpCode::GetGlobal { dst, name_idx } => {
            let name = proto
                .names
                .get(*name_idx as usize)
                .cloned()
                .unwrap_or_else(|| "?".to_string());
            format!("{prefix}  GetGlobal     dst={dst}  N{name_idx}({name})")
        }
        OpCode::SetGlobal { src, name_idx } => {
            let name = proto
                .names
                .get(*name_idx as usize)
                .cloned()
                .unwrap_or_else(|| "?".to_string());
            format!("{prefix}  SetGlobal     src={src}  N{name_idx}({name})")
        }

        OpCode::Closure { dst, proto_idx } => {
            format!("{prefix}  Closure       dst={dst}  proto={proto_idx}")
        }
        OpCode::GetUpvalue { dst, upval_idx } => {
            format!("{prefix}  GetUpvalue    dst={dst}  upval={upval_idx}")
        }
        OpCode::SetUpvalue { src, upval_idx } => {
            format!("{prefix}  SetUpvalue    src={src}  upval={upval_idx}")
        }
        OpCode::CloseUpvalues { from_reg } => {
            format!("{prefix}  CloseUpvalues from={from_reg}")
        }

        OpCode::NewTable { dst } => format!("{prefix}  NewTable      dst={dst}"),
        OpCode::GetTable { dst, table, key } => {
            format!("{prefix}  GetTable      dst={dst}  table={table}  key={key}")
        }
        OpCode::SetTable { table, key, val } => {
            format!("{prefix}  SetTable      table={table}  key={key}  val={val}")
        }
        OpCode::GetField {
            dst,
            table,
            name_idx,
        } => {
            let name = proto
                .names
                .get(*name_idx as usize)
                .cloned()
                .unwrap_or_else(|| "?".to_string());
            format!("{prefix}  GetField      dst={dst}  table={table}  N{name_idx}({name})")
        }
        OpCode::SetField {
            table,
            name_idx,
            val,
        } => {
            let name = proto
                .names
                .get(*name_idx as usize)
                .cloned()
                .unwrap_or_else(|| "?".to_string());
            format!("{prefix}  SetField      table={table}  N{name_idx}({name})  val={val}")
        }
        OpCode::SetList { table, src, count } => {
            format!("{prefix}  SetList       table={table}  src={src}  count={count}")
        }

        OpCode::VarArg { dst, count } => {
            format!("{prefix}  VarArg        dst={dst}  count={count}")
        }

        _ => format!("{prefix}  {op:?}"),
    }
}
