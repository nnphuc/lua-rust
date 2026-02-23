use lua_core::LuaValue;
use std::collections::HashMap;

pub fn register(globals: &mut HashMap<String, LuaValue>) {
    globals.insert("print".into(), LuaValue::NativeFunction(lua_print));
    globals.insert("tostring".into(), LuaValue::NativeFunction(lua_tostring));
    globals.insert("tonumber".into(), LuaValue::NativeFunction(lua_tonumber));
    globals.insert("type".into(), LuaValue::NativeFunction(lua_type));
    globals.insert("assert".into(), LuaValue::NativeFunction(lua_assert));
    globals.insert("error".into(), LuaValue::NativeFunction(lua_error));
}

use lua_core::LuaError;

fn lua_print(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let parts: Vec<String> = args.iter().map(|v| v.to_string()).collect();
    println!("{}", parts.join("\t"));
    Ok(vec![])
}

fn lua_tostring(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    Ok(vec![LuaValue::LuaString(v.to_string())])
}

fn lua_tonumber(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    let result = match v {
        LuaValue::Integer(n) => LuaValue::Integer(n),
        LuaValue::Float(f) => LuaValue::Float(f),
        LuaValue::LuaString(s) => {
            if let Ok(n) = s.trim().parse::<i64>() {
                LuaValue::Integer(n)
            } else if let Ok(f) = s.trim().parse::<f64>() {
                LuaValue::Float(f)
            } else {
                LuaValue::Nil
            }
        }
        _ => LuaValue::Nil,
    };
    Ok(vec![result])
}

fn lua_type(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    Ok(vec![LuaValue::LuaString(v.type_name().into())])
}

fn lua_assert(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let v = it.next().unwrap_or(LuaValue::Nil);
    if v.is_truthy() {
        Ok(vec![v])
    } else {
        let msg = it
            .next()
            .map(|m| m.to_string())
            .unwrap_or_else(|| "assertion failed!".into());
        Err(LuaError::Runtime(msg))
    }
}

fn lua_error(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let msg = args.into_iter().next().unwrap_or(LuaValue::Nil).to_string();
    Err(LuaError::Runtime(msg))
}
