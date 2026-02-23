use lua_core::{LuaError, LuaTable, LuaValue};
use std::collections::HashMap;
use std::io::{self, Write};
use std::sync::OnceLock;
use std::sync::{Arc, RwLock};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

pub fn register(globals: &mut HashMap<String, LuaValue>) {
    globals.insert("print".into(),    LuaValue::NativeFunction(lua_print));
    globals.insert("tostring".into(), LuaValue::NativeFunction(lua_tostring));
    globals.insert("tonumber".into(), LuaValue::NativeFunction(lua_tonumber));
    globals.insert("type".into(),     LuaValue::NativeFunction(lua_type));
    globals.insert("assert".into(),   LuaValue::NativeFunction(lua_assert));
    globals.insert("error".into(),    LuaValue::NativeFunction(lua_error));
    globals.insert("ipairs".into(),   LuaValue::NativeFunction(lua_ipairs));
    globals.insert("pairs".into(),    LuaValue::NativeFunction(lua_pairs));
    globals.insert("select".into(),   LuaValue::NativeFunction(lua_select));
    globals.insert("unpack".into(),   LuaValue::NativeFunction(lua_unpack));
    globals.insert("rawget".into(),   LuaValue::NativeFunction(lua_rawget));
    globals.insert("rawset".into(),   LuaValue::NativeFunction(lua_rawset));
    globals.insert("getmetatable".into(), LuaValue::NativeFunction(lua_getmetatable));
    globals.insert("setmetatable".into(), LuaValue::NativeFunction(lua_setmetatable));

    globals.insert("math".into(),   make_math_lib());
    globals.insert("string".into(), make_string_lib());
    globals.insert("io".into(),     make_io_lib());
    globals.insert("os".into(),     make_os_lib());
    globals.insert("coroutine".into(), make_coroutine_lib());
}

// ── Basic functions ─────────────────────────────────────────────────────────

fn lua_print(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let parts: Vec<String> = args.iter().map(|v| v.to_string()).collect();
    println!("{}", parts.join("\t"));
    Ok(vec![])
}

fn call_callable(callable: LuaValue, args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    match callable {
        LuaValue::NativeFunction(f) => f(args),
        LuaValue::Closure(c) => {
            let resumed = crate::vm::with_current_vm(|vm| {
                let co = vm.coroutine_create(LuaValue::Closure(c.clone()))?;
                vm.coroutine_resume(co, args)
            })??;
            match resumed.first() {
                Some(LuaValue::Boolean(true)) => Ok(resumed.into_iter().skip(1).collect()),
                _ => Err(LuaError::Runtime(
                    resumed.get(1).cloned().unwrap_or(LuaValue::LuaString("metamethod call failed".into())).to_string(),
                )),
            }
        }
        v => Err(LuaError::TypeError { expected: "function", got: v.type_name() }),
    }
}

fn lua_tostring(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    let mm = crate::vm::with_current_vm(|vm| {
        vm.get_metatable(&v)
            .map(|mt| mt.read().unwrap().get(&LuaValue::LuaString("__tostring".into())))
            .unwrap_or(LuaValue::Nil)
    })?;
    if !matches!(mm, LuaValue::Nil) {
        let out = call_callable(mm, vec![v.clone()])?;
        match out.into_iter().next().unwrap_or(LuaValue::Nil) {
            LuaValue::LuaString(s) => return Ok(vec![LuaValue::LuaString(s)]),
            other => {
                return Err(LuaError::Runtime(format!(
                    "'__tostring' must return a string, got {}",
                    other.type_name()
                )))
            }
        }
    }
    Ok(vec![LuaValue::LuaString(v.to_string())])
}

fn lua_tonumber(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    let result = match v {
        LuaValue::Integer(n) => LuaValue::Integer(n),
        LuaValue::Float(f)   => LuaValue::Float(f),
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
        // Return all args (first arg + rest)
        Ok(std::iter::once(v).chain(it).collect())
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

fn lua_select(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    match it.next().unwrap_or(LuaValue::Nil) {
        LuaValue::LuaString(s) if s == "#" => {
            let count = it.count() as i64;
            Ok(vec![LuaValue::Integer(count)])
        }
        LuaValue::Integer(n) => {
            let rest: Vec<_> = it.collect();
            let idx = if n < 0 { (rest.len() as i64 + n).max(0) as usize } else { (n - 1).max(0) as usize };
            Ok(rest.into_iter().skip(idx).collect())
        }
        _ => Err(LuaError::Runtime("bad argument #1 to 'select'".into())),
    }
}

fn lua_unpack(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    match it.next().unwrap_or(LuaValue::Nil) {
        LuaValue::Table(t) => {
            let tbl = t.read().unwrap();
            let i = match it.next() { Some(LuaValue::Integer(n)) => n, _ => 1 };
            let j = match it.next() { Some(LuaValue::Integer(n)) => n, _ => tbl.length() };
            let out: Vec<LuaValue> = (i..=j).map(|k| tbl.get(&LuaValue::Integer(k))).collect();
            Ok(out)
        }
        _ => Err(LuaError::Runtime("bad argument #1 to 'unpack' (table expected)".into())),
    }
}

fn lua_rawget(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let t = it.next().unwrap_or(LuaValue::Nil);
    let k = it.next().unwrap_or(LuaValue::Nil);
    match t {
        LuaValue::Table(tbl) => Ok(vec![tbl.read().unwrap().get(&k)]),
        _ => Err(LuaError::Runtime("table expected".into())),
    }
}

fn lua_rawset(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let t = it.next().unwrap_or(LuaValue::Nil);
    let k = it.next().unwrap_or(LuaValue::Nil);
    let v = it.next().unwrap_or(LuaValue::Nil);
    match &t {
        LuaValue::Table(tbl) => { tbl.write().unwrap().set(k, v); Ok(vec![t]) }
        _ => Err(LuaError::Runtime("table expected".into())),
    }
}

fn lua_getmetatable(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let v = args.into_iter().next().unwrap_or(LuaValue::Nil);
    let mt = crate::vm::with_current_vm(|vm| vm.get_metatable(&v))?;
    if let Some(mt_tbl) = mt {
        let protected = mt_tbl
            .read()
            .unwrap()
            .get(&LuaValue::LuaString("__metatable".into()));
        if !matches!(protected, LuaValue::Nil) {
            return Ok(vec![protected]);
        }
        return Ok(vec![LuaValue::Table(mt_tbl)]);
    }
    Ok(vec![LuaValue::Nil])
}

fn lua_setmetatable(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let value = it.next().unwrap_or(LuaValue::Nil);
    let mt = it.next().unwrap_or(LuaValue::Nil);
    let current_mt = crate::vm::with_current_vm(|vm| vm.get_metatable(&value))?;
    if let Some(cur) = current_mt {
        let protected = cur
            .read()
            .unwrap()
            .get(&LuaValue::LuaString("__metatable".into()));
        if !matches!(protected, LuaValue::Nil) {
            return Err(LuaError::Runtime("cannot change a protected metatable".into()));
        }
    }
    let new_mt = match mt {
        LuaValue::Nil => None,
        LuaValue::Table(mt) => Some(mt),
        v => return Err(LuaError::TypeError { expected: "table", got: v.type_name() }),
    };
    crate::vm::with_current_vm(|vm| vm.set_metatable(&value, new_mt))??;
    Ok(vec![value])
}

// ── io / os libraries ──────────────────────────────────────────────────────

fn make_io_lib() -> LuaValue {
    let t = Arc::new(RwLock::new(LuaTable::new()));
    {
        let mut tbl = t.write().unwrap();
        tbl.set(LuaValue::LuaString("write".into()), LuaValue::NativeFunction(io_write));
        tbl.set(LuaValue::LuaString("flush".into()), LuaValue::NativeFunction(io_flush));
    }
    LuaValue::Table(t)
}

fn make_os_lib() -> LuaValue {
    let t = Arc::new(RwLock::new(LuaTable::new()));
    {
        let mut tbl = t.write().unwrap();
        tbl.set(LuaValue::LuaString("clock".into()), LuaValue::NativeFunction(os_clock));
        tbl.set(LuaValue::LuaString("time".into()), LuaValue::NativeFunction(os_time));
        tbl.set(LuaValue::LuaString("date".into()), LuaValue::NativeFunction(os_date));
    }
    LuaValue::Table(t)
}

fn io_write(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut out = io::stdout().lock();
    for v in args {
        out.write_all(v.to_string().as_bytes())
            .map_err(|e| LuaError::Runtime(format!("io.write failed: {e}")))?;
    }
    out.flush()
        .map_err(|e| LuaError::Runtime(format!("io.write flush failed: {e}")))?;
    Ok(vec![])
}

fn io_flush(_: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    io::stdout()
        .flush()
        .map_err(|e| LuaError::Runtime(format!("io.flush failed: {e}")))?;
    Ok(vec![LuaValue::Boolean(true)])
}

fn os_clock(_: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    static START: OnceLock<Instant> = OnceLock::new();
    let start = START.get_or_init(Instant::now);
    Ok(vec![LuaValue::Float(start.elapsed().as_secs_f64())])
}

fn os_time(_: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| LuaError::Runtime(format!("os.time failed: {e}")))?
        .as_secs() as i64;
    Ok(vec![LuaValue::Integer(secs)])
}

fn os_date(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let t = match args.get(1) {
        Some(LuaValue::Integer(v)) => *v,
        Some(LuaValue::Float(v)) => *v as i64,
        _ => SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| LuaError::Runtime(format!("os.date failed: {e}")))?
            .as_secs() as i64,
    };
    Ok(vec![LuaValue::LuaString(format!("{t}"))])
}

fn make_coroutine_lib() -> LuaValue {
    let t = Arc::new(RwLock::new(LuaTable::new()));
    {
        let mut tbl = t.write().unwrap();
        tbl.set(LuaValue::LuaString("create".into()), LuaValue::NativeFunction(coroutine_create));
        tbl.set(LuaValue::LuaString("resume".into()), LuaValue::NativeFunction(coroutine_resume));
        tbl.set(LuaValue::LuaString("yield".into()), LuaValue::NativeFunction(coroutine_yield));
        tbl.set(LuaValue::LuaString("status".into()), LuaValue::NativeFunction(coroutine_status));
        tbl.set(LuaValue::LuaString("running".into()), LuaValue::NativeFunction(coroutine_running));
    }
    LuaValue::Table(t)
}

fn coroutine_create(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let func = args.into_iter().next().unwrap_or(LuaValue::Nil);
    crate::vm::with_current_vm(|vm| vm.coroutine_create(func))?
        .map(|co| vec![co])
}

fn coroutine_resume(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let co = it.next().unwrap_or(LuaValue::Nil);
    let rest: Vec<LuaValue> = it.collect();
    crate::vm::with_current_vm(|vm| vm.coroutine_resume(co, rest))?
}

fn coroutine_yield(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let in_co = crate::vm::with_current_vm(|vm| vm.is_in_coroutine())?;
    if !in_co {
        return Err(LuaError::Runtime("attempt to yield from outside a coroutine".into()));
    }
    Err(LuaError::Yield(args))
}

fn coroutine_status(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let co = args.first().cloned().unwrap_or(LuaValue::Nil);
    let s = crate::vm::with_current_vm(|vm| vm.coroutine_status(&co))??;
    Ok(vec![LuaValue::LuaString(s.into())])
}

fn coroutine_running(_: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let (thread, is_main) = crate::vm::with_current_vm(|vm| vm.coroutine_running())?;
    Ok(vec![thread, LuaValue::Boolean(is_main)])
}

// ── ipairs / pairs ──────────────────────────────────────────────────────────

/// `ipairs(t)` → returns (iterator_fn, t, 0)
/// The iterator fn is represented as a native function that takes (t, i) → (i+1, t[i+1])
fn lua_ipairs(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let t = args.into_iter().next().unwrap_or(LuaValue::Nil);
    match &t {
        LuaValue::Table(tbl) => {
            let mm = tbl
                .read()
                .unwrap()
                .get_metatable()
                .map(|mt| mt.read().unwrap().get(&LuaValue::LuaString("__ipairs".into())))
                .unwrap_or(LuaValue::Nil);
            if !matches!(mm, LuaValue::Nil) {
                return call_callable(mm, vec![t.clone()]);
            }
        }
        _ => return Err(LuaError::TypeError { expected: "table", got: t.type_name() }),
    }
    Ok(vec![
        LuaValue::NativeFunction(ipairs_iter),
        t,
        LuaValue::Integer(0),
    ])
}

fn ipairs_iter(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let tbl = match it.next().unwrap_or(LuaValue::Nil) {
        LuaValue::Table(t) => t,
        _ => return Ok(vec![LuaValue::Nil]),
    };
    let i = match it.next().unwrap_or(LuaValue::Nil) {
        LuaValue::Integer(n) => n,
        _ => return Ok(vec![LuaValue::Nil]),
    };
    let next_i = i + 1;
    let val = tbl.read().unwrap().get(&LuaValue::Integer(next_i));
    if matches!(val, LuaValue::Nil) {
        Ok(vec![LuaValue::Nil])
    } else {
        Ok(vec![LuaValue::Integer(next_i), val])
    }
}

/// `pairs(t)` → (next_fn, t, nil)
fn lua_pairs(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let t = args.into_iter().next().unwrap_or(LuaValue::Nil);
    match &t {
        LuaValue::Table(tbl) => {
            let mm = tbl
                .read()
                .unwrap()
                .get_metatable()
                .map(|mt| mt.read().unwrap().get(&LuaValue::LuaString("__pairs".into())))
                .unwrap_or(LuaValue::Nil);
            if !matches!(mm, LuaValue::Nil) {
                return call_callable(mm, vec![t.clone()]);
            }
        }
        _ => return Err(LuaError::TypeError { expected: "table", got: t.type_name() }),
    }
    Ok(vec![
        LuaValue::NativeFunction(pairs_next),
        t,
        LuaValue::Nil,
    ])
}

/// Stateless `next(t, k)` — iterate all key/value pairs in the table.
fn pairs_next(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let tbl_val = it.next().unwrap_or(LuaValue::Nil);
    let key_val = it.next().unwrap_or(LuaValue::Nil);
    let tbl = match &tbl_val {
        LuaValue::Table(t) => t.clone(),
        _ => return Ok(vec![LuaValue::Nil]),
    };
    let guard = tbl.read().unwrap();
    // Build an ordered key list: array keys first, then hash keys
    let mut keys: Vec<LuaValue> = (1..=guard.array.len() as i64)
        .map(LuaValue::Integer)
        .collect();
    for hk in guard.hash.keys() {
        match hk {
            lua_core::table::HashKey::Int(n) => keys.push(LuaValue::Integer(*n)),
            lua_core::table::HashKey::Str(s) => keys.push(LuaValue::LuaString(s.clone())),
            lua_core::table::HashKey::Bool(b) => keys.push(LuaValue::Boolean(*b)),
        }
    }
    // Find the position of the current key
    let start = if matches!(key_val, LuaValue::Nil) {
        0
    } else {
        keys.iter().position(|k| k == &key_val).map(|p| p + 1).unwrap_or(keys.len())
    };
    if start >= keys.len() {
        return Ok(vec![LuaValue::Nil]);
    }
    let next_key = &keys[start];
    let next_val = guard.get(next_key);
    Ok(vec![next_key.clone(), next_val])
}

// ── math library ────────────────────────────────────────────────────────────

fn make_math_lib() -> LuaValue {
    let t = Arc::new(RwLock::new(LuaTable::new()));
    {
        let mut tbl = t.write().unwrap();
        macro_rules! set_fn {
            ($name:expr, $fn:expr) => {
                tbl.set(LuaValue::LuaString($name.into()), LuaValue::NativeFunction($fn));
            };
        }
        macro_rules! set_num {
            ($name:expr, $val:expr) => {
                tbl.set(LuaValue::LuaString($name.into()), LuaValue::Float($val));
            };
        }
        set_num!("pi",   std::f64::consts::PI);
        set_num!("huge", f64::INFINITY);
        set_fn!("abs",   math_abs);
        set_fn!("ceil",  math_ceil);
        set_fn!("floor", math_floor);
        set_fn!("sqrt",  math_sqrt);
        set_fn!("max",   math_max);
        set_fn!("min",   math_min);
        set_fn!("fmod",  math_fmod);
        set_fn!("modf",  math_modf);
        set_fn!("exp",   math_exp);
        set_fn!("log",   math_log);
        set_fn!("sin",   math_sin);
        set_fn!("cos",   math_cos);
        set_fn!("tan",   math_tan);
        set_fn!("type",  math_type);
        set_fn!("tointeger", math_tointeger);
    }
    LuaValue::Table(t)
}

fn num1(args: &[LuaValue]) -> Result<f64, LuaError> {
    match args.first().unwrap_or(&LuaValue::Nil) {
        LuaValue::Integer(n) => Ok(*n as f64),
        LuaValue::Float(f)   => Ok(*f),
        v => Err(LuaError::TypeError { expected: "number", got: v.type_name() }),
    }
}

fn math_abs(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![match args.first().unwrap_or(&LuaValue::Nil) {
        LuaValue::Integer(n) => LuaValue::Integer(n.abs()),
        _ => LuaValue::Float(num1(&args)?.abs()),
    }])
}
fn math_ceil(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Integer(num1(&args)?.ceil() as i64)])
}
fn math_floor(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Integer(num1(&args)?.floor() as i64)])
}
fn math_sqrt(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Float(num1(&args)?.sqrt())])
}
fn math_exp(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Float(num1(&args)?.exp())])
}
fn math_log(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let n = num1(&args)?;
    let base = match args.get(1) {
        Some(LuaValue::Integer(b)) => Some(*b as f64),
        Some(LuaValue::Float(b))   => Some(*b),
        _ => None,
    };
    let result = match base {
        None => n.ln(),
        Some(b) => n.log(b),
    };
    Ok(vec![LuaValue::Float(result)])
}
fn math_sin(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Float(num1(&args)?.sin())])
}
fn math_cos(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Float(num1(&args)?.cos())])
}
fn math_tan(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::Float(num1(&args)?.tan())])
}
fn math_max(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let first = it.next().unwrap_or(LuaValue::Nil);
    let mut best = first;
    for v in it {
        let bigger = match (&best, &v) {
            (LuaValue::Integer(a), LuaValue::Integer(b)) => b > a,
            (LuaValue::Float(a), LuaValue::Float(b)) => b > a,
            (LuaValue::Integer(a), LuaValue::Float(b)) => *b > *a as f64,
            (LuaValue::Float(a), LuaValue::Integer(b)) => *b as f64 > *a,
            _ => false,
        };
        if bigger { best = v; }
    }
    Ok(vec![best])
}
fn math_min(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut it = args.into_iter();
    let first = it.next().unwrap_or(LuaValue::Nil);
    let mut best = first;
    for v in it {
        let smaller = match (&best, &v) {
            (LuaValue::Integer(a), LuaValue::Integer(b)) => b < a,
            (LuaValue::Float(a), LuaValue::Float(b)) => b < a,
            (LuaValue::Integer(a), LuaValue::Float(b)) => *b < *a as f64,
            (LuaValue::Float(a), LuaValue::Integer(b)) => (*b as f64) < *a,
            _ => false,
        };
        if smaller { best = v; }
    }
    Ok(vec![best])
}
fn math_fmod(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let x = num1(&args)?;
    let y = match args.get(1) {
        Some(v) => match v { LuaValue::Integer(n) => *n as f64, LuaValue::Float(f) => *f, _ => return Err(LuaError::Runtime("number expected".into())) },
        None => return Err(LuaError::Runtime("math.fmod needs 2 args".into())),
    };
    Ok(vec![LuaValue::Float(x % y)])
}
fn math_modf(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let x = num1(&args)?;
    let trunc = x.trunc();
    let frac  = x.fract();
    Ok(vec![LuaValue::Float(trunc), LuaValue::Float(frac)])
}
fn math_type(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let result = match args.first().unwrap_or(&LuaValue::Nil) {
        LuaValue::Integer(_) => LuaValue::LuaString("integer".into()),
        LuaValue::Float(_)   => LuaValue::LuaString("float".into()),
        _                    => LuaValue::Boolean(false),
    };
    Ok(vec![result])
}
fn math_tointeger(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![match args.first().unwrap_or(&LuaValue::Nil) {
        LuaValue::Integer(n) => LuaValue::Integer(*n),
        LuaValue::Float(f) if f.fract() == 0.0 && f.is_finite() => LuaValue::Integer(*f as i64),
        _ => LuaValue::Nil,
    }])
}

// ── string library ──────────────────────────────────────────────────────────

fn make_string_lib() -> LuaValue {
    let t = Arc::new(RwLock::new(LuaTable::new()));
    {
        let mut tbl = t.write().unwrap();
        macro_rules! set_fn {
            ($name:expr, $fn:expr) => {
                tbl.set(LuaValue::LuaString($name.into()), LuaValue::NativeFunction($fn));
            };
        }
        set_fn!("len",     string_len);
        set_fn!("sub",     string_sub);
        set_fn!("upper",   string_upper);
        set_fn!("lower",   string_lower);
        set_fn!("rep",     string_rep);
        set_fn!("reverse", string_reverse);
        set_fn!("byte",    string_byte);
        set_fn!("char",    string_char);
        set_fn!("find",    string_find);
        set_fn!("format",  string_format);
    }
    LuaValue::Table(t)
}

fn get_str(args: &[LuaValue], idx: usize) -> Result<String, LuaError> {
    match args.get(idx).unwrap_or(&LuaValue::Nil) {
        LuaValue::LuaString(s) => Ok(s.clone()),
        LuaValue::Integer(n)   => Ok(n.to_string()),
        LuaValue::Float(f)     => Ok(f.to_string()),
        v => Err(LuaError::TypeError { expected: "string", got: v.type_name() }),
    }
}

fn lua_pos(len: i64, i: i64) -> usize {
    if i >= 0 { (i - 1).max(0) as usize } else { (len + i).max(0) as usize }
}

fn string_len(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let s = get_str(&args, 0)?;
    Ok(vec![LuaValue::Integer(s.len() as i64)])
}
fn string_sub(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let s = get_str(&args, 0)?;
    let n = s.len() as i64;
    let i = match args.get(1) { Some(LuaValue::Integer(v)) => *v, Some(LuaValue::Float(f)) => *f as i64, _ => 1 };
    let j = match args.get(2) { Some(LuaValue::Integer(v)) => *v, Some(LuaValue::Float(f)) => *f as i64, _ => -1 };
    let start = lua_pos(n, i);
    let end   = if j < 0 { (n + j + 1).max(0) as usize } else { (j as usize).min(s.len()) };
    if start >= end { return Ok(vec![LuaValue::LuaString(String::new())]); }
    Ok(vec![LuaValue::LuaString(s[start..end].to_string())])
}
fn string_upper(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::LuaString(get_str(&args, 0)?.to_uppercase())])
}
fn string_lower(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::LuaString(get_str(&args, 0)?.to_lowercase())])
}
fn string_rep(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let s = get_str(&args, 0)?;
    let n = match args.get(1) { Some(LuaValue::Integer(v)) => *v as usize, _ => 0 };
    let sep = match args.get(2) { Some(LuaValue::LuaString(sep)) => sep.clone(), _ => String::new() };
    let result = if n == 0 { String::new() } else {
        std::iter::repeat(s.as_str()).take(n).collect::<Vec<_>>().join(&sep)
    };
    Ok(vec![LuaValue::LuaString(result)])
}
fn string_reverse(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    Ok(vec![LuaValue::LuaString(get_str(&args, 0)?.chars().rev().collect())])
}
fn string_byte(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let s = get_str(&args, 0)?;
    let i = match args.get(1) { Some(LuaValue::Integer(v)) => *v, _ => 1 };
    let j = match args.get(2) { Some(LuaValue::Integer(v)) => *v, _ => i };
    let n = s.len() as i64;
    let start = lua_pos(n, i);
    let end   = if j < 0 { (n + j + 1).max(0) as usize } else { (j as usize).min(s.len()) };
    let bytes = s.as_bytes();
    let out: Vec<LuaValue> = (start..end).map(|idx| LuaValue::Integer(bytes[idx] as i64)).collect();
    Ok(out)
}
fn string_char(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let mut s = String::new();
    for v in &args {
        let n = match v { LuaValue::Integer(n) => *n, LuaValue::Float(f) => *f as i64, _ => return Err(LuaError::Runtime("number expected".into())) };
        s.push(char::from_u32(n as u32).unwrap_or('\u{FFFD}'));
    }
    Ok(vec![LuaValue::LuaString(s)])
}
fn string_find(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let s = get_str(&args, 0)?;
    let pattern = get_str(&args, 1)?;
    let init = match args.get(2) { Some(LuaValue::Integer(v)) => (*v - 1).max(0) as usize, _ => 0 };
    // plain search only (no regex patterns)
    if let Some(pos) = s[init..].find(&*pattern) {
        let start = (init + pos + 1) as i64;
        let end   = (init + pos + pattern.len()) as i64;
        Ok(vec![LuaValue::Integer(start), LuaValue::Integer(end)])
    } else {
        Ok(vec![LuaValue::Nil])
    }
}

/// `string.format(fmt, ...)` — supports %s %d %i %f %g %q %%
fn string_format(args: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
    let fmt = get_str(&args, 0)?;
    let mut result = String::new();
    let mut arg_idx = 1usize;
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '%' { result.push(c); continue; }
        match chars.next() {
            None => break,
            Some('%') => result.push('%'),
            Some(spec) => {
                let v = args.get(arg_idx).unwrap_or(&LuaValue::Nil);
                arg_idx += 1;
                match spec {
                    'd' | 'i' => {
                        let n = match v { LuaValue::Integer(n) => *n, LuaValue::Float(f) => *f as i64, _ => 0 };
                        result.push_str(&n.to_string());
                    }
                    'f' => {
                        let f = match v { LuaValue::Float(f) => *f, LuaValue::Integer(n) => *n as f64, _ => 0.0 };
                        result.push_str(&format!("{f:.6}"));
                    }
                    'g' => {
                        let f = match v { LuaValue::Float(f) => *f, LuaValue::Integer(n) => *n as f64, _ => 0.0 };
                        result.push_str(&format!("{f}"));
                    }
                    's' => result.push_str(&v.to_string()),
                    'q' => {
                        let s = v.to_string();
                        result.push('"');
                        for ch in s.chars() {
                            match ch { '"' => result.push_str("\\\""), '\\' => result.push_str("\\\\"), '\n' => result.push_str("\\n"), _ => result.push(ch) }
                        }
                        result.push('"');
                    }
                    other => { result.push('%'); result.push(other); }
                }
            }
        }
    }
    Ok(vec![LuaValue::LuaString(result)])
}
