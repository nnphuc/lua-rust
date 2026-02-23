use crate::closure::LuaClosure;
use crate::error::LuaError;
use crate::table::LuaTable;
use std::sync::{Arc, RwLock};

/// All Lua value types, mirroring the Lua 5.4 type system.
#[derive(Clone)]
pub enum LuaValue {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    LuaString(String),
    /// A native Rust function callable from Lua.
    NativeFunction(fn(Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError>),
    /// A Lua closure (compiled function + captured upvalues).
    Closure(Arc<LuaClosure>),
    /// A Lua table (array + hash parts, reference-counted + interior mutability).
    Table(Arc<RwLock<LuaTable>>),
}

impl LuaValue {
    /// Returns the Lua type name string as per the reference manual.
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::Nil => "nil",
            LuaValue::Boolean(_) => "boolean",
            LuaValue::Integer(_) => "number",
            LuaValue::Float(_) => "number",
            LuaValue::LuaString(_) => "string",
            LuaValue::NativeFunction(_) => "function",
            LuaValue::Closure(_) => "function",
            LuaValue::Table(_) => "table",
        }
    }

    /// Returns `true` if the value is truthy in Lua's sense
    /// (everything except `nil` and `false` is truthy).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, LuaValue::Nil | LuaValue::Boolean(false))
    }

    /// Create a new empty table value.
    pub fn new_table() -> Self {
        LuaValue::Table(Arc::new(RwLock::new(LuaTable::new())))
    }
}

// NativeFunction is a plain fn pointer which implements PartialEq via pointer equality.
impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaValue::Nil, LuaValue::Nil) => true,
            (LuaValue::Boolean(a), LuaValue::Boolean(b)) => a == b,
            (LuaValue::Integer(a), LuaValue::Integer(b)) => a == b,
            (LuaValue::Float(a), LuaValue::Float(b)) => a == b,
            (LuaValue::Integer(a), LuaValue::Float(b)) => (*a as f64) == *b,
            (LuaValue::Float(a), LuaValue::Integer(b)) => *a == (*b as f64),
            (LuaValue::LuaString(a), LuaValue::LuaString(b)) => a == b,
            (LuaValue::NativeFunction(a), LuaValue::NativeFunction(b)) => {
                (*a as usize) == (*b as usize)
            }
            // Two closures are equal only if they are the exact same object
            (LuaValue::Closure(a), LuaValue::Closure(b)) => Arc::ptr_eq(a, b),
            // Two tables are equal only if they are the exact same object
            (LuaValue::Table(a), LuaValue::Table(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl std::fmt::Debug for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Nil => write!(f, "LuaValue::Nil"),
            LuaValue::Boolean(b) => write!(f, "LuaValue::Boolean({b})"),
            LuaValue::Integer(n) => write!(f, "LuaValue::Integer({n})"),
            LuaValue::Float(n) => write!(f, "LuaValue::Float({n})"),
            LuaValue::LuaString(s) => write!(f, "LuaValue::LuaString({s:?})"),
            LuaValue::NativeFunction(_) => write!(f, "LuaValue::NativeFunction(<fn>)"),
            LuaValue::Closure(c) => write!(f, "LuaValue::Closure({:p})", Arc::as_ptr(c)),
            LuaValue::Table(t) => write!(f, "LuaValue::Table({:p})", Arc::as_ptr(t)),
        }
    }
}

impl std::fmt::Display for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{b}"),
            LuaValue::Integer(n) => write!(f, "{n}"),
            LuaValue::Float(n) => {
                // Lua displays 1.0 as "1.0", not "1"
                if n.fract() == 0.0 && n.is_finite() {
                    write!(f, "{n:.1}")
                } else {
                    write!(f, "{n}")
                }
            }
            LuaValue::LuaString(s) => write!(f, "{s}"),
            LuaValue::NativeFunction(_) => write!(f, "function: 0x<native>"),
            LuaValue::Closure(c) => write!(f, "function: {:p}", Arc::as_ptr(c)),
            LuaValue::Table(t) => write!(f, "table: {:p}", Arc::as_ptr(t)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nil_is_falsy() {
        assert!(!LuaValue::Nil.is_truthy());
    }

    #[test]
    fn false_is_falsy() {
        assert!(!LuaValue::Boolean(false).is_truthy());
    }

    #[test]
    fn zero_integer_is_truthy() {
        // In Lua, 0 is truthy!
        assert!(LuaValue::Integer(0).is_truthy());
    }

    #[test]
    fn type_names() {
        assert_eq!(LuaValue::Nil.type_name(), "nil");
        assert_eq!(LuaValue::Boolean(true).type_name(), "boolean");
        assert_eq!(LuaValue::Integer(1).type_name(), "number");
        assert_eq!(LuaValue::Float(1.0).type_name(), "number");
        assert_eq!(LuaValue::LuaString("hi".into()).type_name(), "string");
        assert_eq!(LuaValue::new_table().type_name(), "table");
    }

    #[test]
    fn native_function_is_truthy() {
        fn dummy(_: Vec<LuaValue>) -> Result<Vec<LuaValue>, LuaError> {
            Ok(vec![])
        }
        assert!(LuaValue::NativeFunction(dummy).is_truthy());
    }

    #[test]
    fn table_reference_equality() {
        let t1 = LuaValue::new_table();
        let t2 = LuaValue::new_table();
        assert_eq!(t1, t1.clone()); // same Arc → equal
        assert_ne!(t1, t2);         // different Arcs → not equal
    }
}
