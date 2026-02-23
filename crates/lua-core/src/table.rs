use crate::value::LuaValue;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// A Lua table: an associative array keyed by any non-nil, non-NaN value.
///
/// Stores integer keys 1..n in a compact `array` part for fast sequential access;
/// everything else goes into the `hash` part.
#[derive(Debug, Clone, Default)]
pub struct LuaTable {
    pub array: Vec<LuaValue>,                  // 1-indexed: array[i-1] = t[i]
    pub hash:  HashMap<HashKey, LuaValue>,
    pub metatable: Option<Arc<RwLock<LuaTable>>>,
}

/// Keys that can be stored in the hash part of a table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Int(i64),
    Str(String),
    Bool(bool),
}

impl HashKey {
    pub fn from_value(v: &LuaValue) -> Option<HashKey> {
        match v {
            LuaValue::Integer(n) => Some(HashKey::Int(*n)),
            LuaValue::LuaString(s) => Some(HashKey::Str(s.clone())),
            LuaValue::Boolean(b) => Some(HashKey::Bool(*b)),
            LuaValue::Float(f) => {
                // Only coerce if float is an exact integer
                let n = *f as i64;
                if n as f64 == *f {
                    Some(HashKey::Int(n))
                } else {
                    None // NaN / non-integer floats not usable as keys
                }
            }
            _ => None,
        }
    }
}

impl LuaTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_metatable(&self) -> Option<Arc<RwLock<LuaTable>>> {
        self.metatable.clone()
    }

    pub fn set_metatable(&mut self, mt: Option<Arc<RwLock<LuaTable>>>) {
        self.metatable = mt;
    }

    /// Read `t[key]`. Returns `LuaValue::Nil` for missing keys.
    pub fn get(&self, key: &LuaValue) -> LuaValue {
        // Integer keys 1..array.len() go to the array part
        if let LuaValue::Integer(i) = key {
            let i = *i;
            if i >= 1 && i as usize <= self.array.len() {
                return self.array[(i - 1) as usize].clone();
            }
        }
        if let LuaValue::Float(f) = key {
            let i = *f as i64;
            if i as f64 == *f && i >= 1 && i as usize <= self.array.len() {
                return self.array[(i - 1) as usize].clone();
            }
        }
        HashKey::from_value(key)
            .and_then(|hk| self.hash.get(&hk))
            .cloned()
            .unwrap_or(LuaValue::Nil)
    }

    /// Write `t[key] = val`. Setting to nil deletes the entry.
    pub fn set(&mut self, key: LuaValue, val: LuaValue) {
        if let LuaValue::Integer(i) = &key {
            let i = *i;
            if i >= 1 {
                let idx = (i - 1) as usize;
                if idx < self.array.len() {
                    self.array[idx] = val;
                    return;
                } else if idx == self.array.len() {
                    self.array.push(val);
                    // Drain consecutive integer keys from hash into array
                    self.rehash_sequence();
                    return;
                }
            }
        }
        if let LuaValue::Float(f) = &key {
            let f = *f;
            let i = f as i64;
            if i as f64 == f {
                self.set(LuaValue::Integer(i), val);
                return;
            }
        }
        if let Some(hk) = HashKey::from_value(&key) {
            if matches!(val, LuaValue::Nil) {
                self.hash.remove(&hk);
            } else {
                self.hash.insert(hk, val);
            }
        }
    }

    /// Lua-style length: the border of the array sequence (largest n where t[n] ~= nil).
    pub fn length(&self) -> i64 {
        self.array.len() as i64
    }

    /// Append `val` to the array part (equivalent to `t[#t+1] = val`).
    pub fn push(&mut self, val: LuaValue) {
        self.array.push(val);
    }

    /// After a new integer key extends the array part, pull consecutive keys
    /// from the hash part into the array to keep the invariant.
    fn rehash_sequence(&mut self) {
        loop {
            let next = (self.array.len() + 1) as i64;
            let hk = HashKey::Int(next);
            if let Some(v) = self.hash.remove(&hk) {
                self.array.push(v);
            } else {
                break;
            }
        }
    }
}
