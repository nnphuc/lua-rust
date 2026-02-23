use lua_compiler::Chunk;
use lua_core::{LuaClosure, LuaError, LuaTable, LuaValue, OpCode, Proto, Upvalue, UpvalueDesc, UpvalueInner};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

// ── Call frame ────────────────────────────────────────────────────────────────

/// A single entry on the call stack.
struct CallFrame {
    /// The function being executed.
    closure: Arc<LuaClosure>,
    /// Instruction pointer (index into `closure.proto.instructions`).
    ip: usize,
    /// Base register in the shared `regs` Vec (frame's reg 0 = `regs[base]`).
    base: usize,
    /// Where in `regs` the caller wants the result (relative to caller's base).
    /// `None` for the top-level frame.
    result_base: Option<usize>,
    /// How many result registers the caller expects.
    expected_results: u8,
    /// Varargs passed to this frame (args beyond `param_count`).
    varargs: Vec<LuaValue>,
}

// ── VM ────────────────────────────────────────────────────────────────────────

/// Register-based virtual machine that executes Lua bytecode.
pub struct Vm {
    globals: HashMap<String, LuaValue>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Vm {
            globals: HashMap::new(),
        };
        crate::stdlib::register(&mut vm.globals);
        vm
    }

    /// Execute `chunk` and return the value produced by the initial `Return`.
    pub fn execute(&mut self, chunk: &Chunk) -> Result<LuaValue, LuaError> {
        // Wrap the top-level proto in a closure (no upvalues needed)
        let closure = Arc::new(LuaClosure::new(chunk.proto.clone(), vec![]));
        self.run(closure)
    }

    // ── Core interpreter loop ─────────────────────────────────────────────────

    fn run(&mut self, root: Arc<LuaClosure>) -> Result<LuaValue, LuaError> {
        // Shared register file (all frames share one flat Vec, offset by `base`)
        let mut regs: Vec<LuaValue> = vec![LuaValue::Nil; 1024];
        // Open upvalue cells: reg → shared cell (managed across call frames)
        let mut open_upvalues: Vec<(usize, Upvalue)> = Vec::new();

        let mut frames: Vec<CallFrame> = vec![CallFrame {
            closure: root,
            ip: 0,
            base: 0,
            result_base: None,
            expected_results: 1,
            varargs: vec![],
        }];

        loop {
            let frame = frames.last_mut().unwrap();
            let proto = &frame.closure.proto;

            if frame.ip >= proto.instructions.len() {
                // Implicit return nil
                let _ = frame;
                match self.return_from_frame(&mut frames, vec![LuaValue::Nil], &mut regs, &mut open_upvalues) {
                    Some(final_val) => return Ok(final_val),
                    None => continue,
                }
            }

            let ip = frame.ip;
            let base = frame.base;
            frame.ip += 1;

            macro_rules! reg {
                ($r:expr) => {
                    regs[base + $r as usize]
                };
            }

            match &proto.instructions[ip].clone() {
                OpCode::LoadNil { dst } => {
                    reg!(*dst) = LuaValue::Nil;
                }
                OpCode::LoadBool { dst, value, skip } => {
                    reg!(*dst) = LuaValue::Boolean(*value);
                    if *skip {
                        frames.last_mut().unwrap().ip += 1;
                    }
                }
                OpCode::LoadConst { dst, const_idx } => {
                    reg!(*dst) = proto.constants[*const_idx as usize].clone();
                }
                OpCode::Move { dst, src } => {
                    let v = reg!(*src).clone();
                    reg!(*dst) = v;
                }

                // ── Arithmetic ────────────────────────────────────────────────
                OpCode::Add { dst, lhs, rhs } => {
                    let v = arith_add(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Sub { dst, lhs, rhs } => {
                    let v = arith_sub(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Mul { dst, lhs, rhs } => {
                    let v = arith_mul(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Div { dst, lhs, rhs } => {
                    let v = arith_div(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::IDiv { dst, lhs, rhs } => {
                    let v = arith_idiv(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Mod { dst, lhs, rhs } => {
                    let v = arith_mod(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Pow { dst, lhs, rhs } => {
                    let v = arith_pow(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = v;
                }
                OpCode::Unm { dst, src } => {
                    let v = arith_unm(&reg!(*src).clone())?;
                    reg!(*dst) = v;
                }

                // ── Comparison ────────────────────────────────────────────────
                OpCode::Eq { dst, lhs, rhs } => {
                    let v = cmp_eq(&reg!(*lhs).clone(), &reg!(*rhs).clone());
                    reg!(*dst) = LuaValue::Boolean(v);
                }
                OpCode::Lt { dst, lhs, rhs } => {
                    let v = cmp_lt(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = LuaValue::Boolean(v);
                }
                OpCode::Le { dst, lhs, rhs } => {
                    let v = cmp_le(&reg!(*lhs).clone(), &reg!(*rhs).clone())?;
                    reg!(*dst) = LuaValue::Boolean(v);
                }
                OpCode::Not { dst, src } => {
                    let v = !reg!(*src).is_truthy();
                    reg!(*dst) = LuaValue::Boolean(v);
                }

                // ── String ────────────────────────────────────────────────────
                OpCode::Concat { dst, start, end } => {
                    let a = to_string_coerce(&reg!(*start).clone())?;
                    let b = to_string_coerce(&reg!(*end).clone())?;
                    reg!(*dst) = LuaValue::LuaString(a + &b);
                }
                OpCode::Len { dst, src } => {
                    let n = match &reg!(*src).clone() {
                        LuaValue::LuaString(s) => s.len() as i64,
                        LuaValue::Table(t) => t.read().unwrap().length(),
                        v => {
                            return Err(LuaError::TypeError {
                                expected: "string or table",
                                got: v.type_name(),
                            })
                        }
                    };
                    reg!(*dst) = LuaValue::Integer(n);
                }

                // ── Control flow ──────────────────────────────────────────────
                OpCode::Jump { offset } => {
                    let new_ip = frame.ip as i64 + *offset as i64;
                    frames.last_mut().unwrap().ip = new_ip as usize;
                    continue;
                }
                OpCode::JumpIfFalse { src, offset } => {
                    if !reg!(*src).is_truthy() {
                        let new_ip = frame.ip as i64 + *offset as i64;
                        frames.last_mut().unwrap().ip = new_ip as usize;
                        continue;
                    }
                }
                OpCode::JumpIfTrue { src, offset } => {
                    if reg!(*src).is_truthy() {
                        let new_ip = frame.ip as i64 + *offset as i64;
                        frames.last_mut().unwrap().ip = new_ip as usize;
                        continue;
                    }
                }

                // ── Globals ───────────────────────────────────────────────────
                OpCode::GetGlobal { dst, name_idx } => {
                    let name = &proto.names[*name_idx as usize];
                    let v = self.globals.get(name).cloned().unwrap_or(LuaValue::Nil);
                    reg!(*dst) = v;
                }
                OpCode::SetGlobal { src, name_idx } => {
                    let name = proto.names[*name_idx as usize].clone();
                    self.globals.insert(name, reg!(*src).clone());
                }

                // ── Upvalues ──────────────────────────────────────────────────
                OpCode::GetUpvalue { dst, upval_idx } => {
                    let val = self.read_upvalue(&frames.last().unwrap().closure, *upval_idx, &regs);
                    reg!(*dst) = val;
                }
                OpCode::SetUpvalue { src, upval_idx } => {
                    let val = reg!(*src).clone();
                    self.write_upvalue(
                        &frames.last().unwrap().closure.clone(),
                        *upval_idx,
                        val,
                        &mut regs,
                    );
                }
                OpCode::CloseUpvalues { from_reg } => {
                    let abs_from = base + *from_reg as usize;
                    close_upvalues_from(&mut open_upvalues, abs_from, &regs);
                }

                // ── Closures ──────────────────────────────────────────────────
                OpCode::Closure { dst, proto_idx } => {
                    let child_proto = proto.protos[*proto_idx as usize].clone();
                    let upvalues = self.instantiate_upvalues(
                        &child_proto,
                        &frames.last().unwrap().closure.clone(),
                        base,
                        &mut open_upvalues,
                        &regs,
                    );
                    let closure = Arc::new(LuaClosure::new(child_proto, upvalues));
                    reg!(*dst) = LuaValue::Closure(closure);
                }

                // ── Function calls ────────────────────────────────────────────
                OpCode::Call {
                    func,
                    num_args,
                    num_results,
                } => {
                    let func_val = reg!(*func).clone();
                    let func_abs = base + *func as usize;
                    let args: Vec<LuaValue> = (0..*num_args as usize)
                        .map(|i| regs[func_abs + 1 + i].clone())
                        .collect();

                    match func_val {
                        LuaValue::NativeFunction(f) => {
                            let results = f(args)?;
                            // Write results into func slot onwards
                            for i in 0..(*num_results as usize) {
                                regs[func_abs + i] = results.get(i).cloned().unwrap_or(LuaValue::Nil);
                            }
                        }
                        LuaValue::Closure(callee) => {
                            let new_base = func_abs;
                            let param_count = callee.proto.param_count as usize;
                            let n = param_count.min(args.len());
                            regs[new_base..new_base + n].clone_from_slice(&args[..n]);
                            for i in args.len()..param_count {
                                regs[new_base + i] = LuaValue::Nil;
                            }
                            let varargs = if callee.proto.is_vararg && args.len() > param_count {
                                args[param_count..].to_vec()
                            } else {
                                vec![]
                            };
                            frames.push(CallFrame {
                                closure: callee,
                                ip: 0,
                                base: new_base,
                                result_base: Some(func_abs),
                                expected_results: *num_results,
                                varargs,
                            });
                            continue;
                        }
                        other => {
                            return Err(LuaError::TypeError {
                                expected: "function",
                                got: other.type_name(),
                            })
                        }
                    }
                }

                OpCode::Return { src, num_results } => {
                    // Collect return values from this frame
                    let n = *num_results as usize;
                    let vals: Vec<LuaValue> = if n == 0 {
                        vec![LuaValue::Nil]
                    } else {
                        (0..n).map(|i| regs[base + *src as usize + i].clone()).collect()
                    };
                    match self.return_from_frame(&mut frames, vals, &mut regs, &mut open_upvalues) {
                        Some(final_val) => return Ok(final_val),
                        None => continue,
                    }
                }

                // ── Tables ────────────────────────────────────────────────────
                OpCode::NewTable { dst } => {
                    reg!(*dst) = LuaValue::Table(Arc::new(RwLock::new(LuaTable::new())));
                }
                OpCode::GetTable { dst, table, key } => {
                    let val = match &reg!(*table).clone() {
                        LuaValue::Table(t) => t.read().unwrap().get(&reg!(*key).clone()),
                        v => return Err(LuaError::TypeError { expected: "table", got: v.type_name() }),
                    };
                    reg!(*dst) = val;
                }
                OpCode::SetTable { table, key, val } => {
                    let k = reg!(*key).clone();
                    let v = reg!(*val).clone();
                    match reg!(*table).clone() {
                        LuaValue::Table(t) => t.write().unwrap().set(k, v),
                        v => return Err(LuaError::TypeError { expected: "table", got: v.type_name() }),
                    }
                }
                OpCode::GetField { dst, table, name_idx } => {
                    let key = LuaValue::LuaString(proto.names[*name_idx as usize].clone());
                    let val = match &reg!(*table).clone() {
                        LuaValue::Table(t) => t.read().unwrap().get(&key),
                        v => return Err(LuaError::TypeError { expected: "table", got: v.type_name() }),
                    };
                    reg!(*dst) = val;
                }
                OpCode::SetField { table, name_idx, val } => {
                    let key = LuaValue::LuaString(proto.names[*name_idx as usize].clone());
                    let v = reg!(*val).clone();
                    match reg!(*table).clone() {
                        LuaValue::Table(t) => t.write().unwrap().set(key, v),
                        v => return Err(LuaError::TypeError { expected: "table", got: v.type_name() }),
                    }
                }
                OpCode::SetList { .. } => { /* TODO: EmitList not yet used */ }

                // ── Varargs ───────────────────────────────────────────────────
                OpCode::VarArg { dst, count } => {
                    let varargs = frames.last().unwrap().varargs.clone();
                    let n = if *count == 255 { varargs.len() } else { *count as usize };
                    for i in 0..n {
                        reg!(*dst + i as u8) = varargs.get(i).cloned().unwrap_or(LuaValue::Nil);
                    }
                }

                _ => {
                    return Err(LuaError::Internal(format!(
                        "unimplemented opcode: {:?}",
                        &proto.instructions[ip]
                    )));
                }
            }
        }
    }

    // ── Frame management ──────────────────────────────────────────────────────

    /// Pop the top frame and write return values to the caller.
    /// Returns `Some(first_val)` if this was the last frame (program done), else `None`.
    fn return_from_frame(
        &self,
        frames: &mut Vec<CallFrame>,
        vals: Vec<LuaValue>,
        regs: &mut [LuaValue],
        open_upvalues: &mut Vec<(usize, Upvalue)>,
    ) -> Option<LuaValue> {
        let frame = frames.pop().unwrap();
        // Close any upvalues that lived in this frame
        close_upvalues_from(open_upvalues, frame.base, regs);

        if frames.is_empty() {
            return Some(vals.into_iter().next().unwrap_or(LuaValue::Nil));
        }
        // Write return values where the caller expects them
        if let Some(result_base) = frame.result_base {
            let n = frame.expected_results as usize;
            for i in 0..n {
                regs[result_base + i] = vals.get(i).cloned().unwrap_or(LuaValue::Nil);
            }
        }
        None
    }

    // ── Upvalue helpers ───────────────────────────────────────────────────────

    fn read_upvalue(&self, closure: &LuaClosure, idx: u8, regs: &[LuaValue]) -> LuaValue {
        let cell = &closure.upvalues[idx as usize];
        match &*cell.0.read().unwrap() {
            // Open cells store absolute register indices
            UpvalueInner::Open(abs_reg) => regs[*abs_reg as usize].clone(),
            UpvalueInner::Closed(val) => val.clone(),
        }
    }

    fn write_upvalue(&self, closure: &LuaClosure, idx: u8, val: LuaValue, regs: &mut [LuaValue]) {
        let cell = &closure.upvalues[idx as usize];
        let mut inner = cell.0.write().unwrap();
        match &*inner {
            // Open cells store absolute register indices
            UpvalueInner::Open(abs_reg) => {
                regs[*abs_reg as usize] = val;
            }
            UpvalueInner::Closed(_) => {
                *inner = UpvalueInner::Closed(val);
            }
        }
    }

    /// Create upvalue cells for each `UpvalueDesc` in `child_proto`.
    fn instantiate_upvalues(
        &self,
        child_proto: &Proto,
        enclosing: &LuaClosure,
        enclosing_base: usize,
        open_upvalues: &mut Vec<(usize, Upvalue)>,
        _regs: &[LuaValue],
    ) -> Vec<Upvalue> {
        child_proto
            .upvalue_descs
            .iter()
            .map(|desc| {
                match desc {
                    UpvalueDesc::Stack(reg) => {
                        // Absolute register in the shared regs array
                        let abs_reg = enclosing_base + *reg as usize;
                        // Reuse existing open cell if there is one for this abs_reg
                        if let Some(pos) = open_upvalues.iter().position(|(r, _)| *r == abs_reg) {
                            open_upvalues[pos].1.clone()
                        } else {
                            // Store the ABSOLUTE register index in the Open cell
                            let cell = Upvalue::open(abs_reg as u8);
                            open_upvalues.push((abs_reg, cell.clone()));
                            cell
                        }
                    }
                    UpvalueDesc::Upvalue(idx) => {
                        // Inherit the cell from the enclosing closure
                        enclosing.upvalues[*idx as usize].clone()
                    }
                }
            })
            .collect()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

// ── Close upvalues ────────────────────────────────────────────────────────────

/// Migrate all open upvalue cells whose absolute register is >= `from_abs`
/// from stack-based to heap-based (closed).
fn close_upvalues_from(open: &mut Vec<(usize, Upvalue)>, from_abs: usize, regs: &[LuaValue]) {
    for (abs_reg, cell) in open.iter() {
        if *abs_reg >= from_abs {
            let mut inner = cell.0.write().unwrap();
            if let UpvalueInner::Open(_) = *inner {
                // The cell's Open value IS the abs_reg already
                *inner = UpvalueInner::Closed(regs[*abs_reg].clone());
            }
        }
    }
    open.retain(|(abs_reg, _)| *abs_reg < from_abs);
}

// ── Arithmetic helpers ────────────────────────────────────────────────────────

fn arith_add(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => LuaValue::Integer(x.wrapping_add(*y)),
        (LuaValue::Float(x), LuaValue::Float(y)) => LuaValue::Float(x + y),
        (LuaValue::Integer(x), LuaValue::Float(y)) => LuaValue::Float(*x as f64 + y),
        (LuaValue::Float(x), LuaValue::Integer(y)) => LuaValue::Float(x + *y as f64),
        _ => {
            return Err(LuaError::TypeError {
                expected: "number",
                got: a.type_name(),
            })
        }
    })
}

fn arith_sub(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => LuaValue::Integer(x.wrapping_sub(*y)),
        (LuaValue::Float(x), LuaValue::Float(y)) => LuaValue::Float(x - y),
        (LuaValue::Integer(x), LuaValue::Float(y)) => LuaValue::Float(*x as f64 - y),
        (LuaValue::Float(x), LuaValue::Integer(y)) => LuaValue::Float(x - *y as f64),
        _ => {
            return Err(LuaError::TypeError {
                expected: "number",
                got: a.type_name(),
            })
        }
    })
}

fn arith_mul(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => LuaValue::Integer(x.wrapping_mul(*y)),
        (LuaValue::Float(x), LuaValue::Float(y)) => LuaValue::Float(x * y),
        (LuaValue::Integer(x), LuaValue::Float(y)) => LuaValue::Float(*x as f64 * y),
        (LuaValue::Float(x), LuaValue::Integer(y)) => LuaValue::Float(x * *y as f64),
        _ => {
            return Err(LuaError::TypeError {
                expected: "number",
                got: a.type_name(),
            })
        }
    })
}

fn arith_div(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(LuaValue::Float(to_float(a)? / to_float(b)?))
}

fn arith_idiv(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => {
            if *y == 0 {
                return Err(LuaError::Runtime("attempt to perform 'n//0'".into()));
            }
            LuaValue::Integer(x.div_euclid(*y))
        }
        _ => LuaValue::Float((to_float(a)? / to_float(b)?).floor()),
    })
}

fn arith_mod(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => {
            if *y == 0 {
                return Err(LuaError::Runtime("attempt to perform 'n%0'".into()));
            }
            LuaValue::Integer(x.rem_euclid(*y))
        }
        _ => {
            let x = to_float(a)?;
            let y = to_float(b)?;
            LuaValue::Float(x - (x / y).floor() * y)
        }
    })
}

fn arith_pow(a: &LuaValue, b: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(LuaValue::Float(to_float(a)?.powf(to_float(b)?)))
}

fn arith_unm(a: &LuaValue) -> Result<LuaValue, LuaError> {
    Ok(match a {
        LuaValue::Integer(n) => LuaValue::Integer(n.wrapping_neg()),
        LuaValue::Float(f) => LuaValue::Float(-f),
        _ => {
            return Err(LuaError::TypeError {
                expected: "number",
                got: a.type_name(),
            })
        }
    })
}

// ── Comparison helpers ────────────────────────────────────────────────────────

fn cmp_eq(a: &LuaValue, b: &LuaValue) -> bool {
    match (a, b) {
        (LuaValue::Nil, LuaValue::Nil) => true,
        (LuaValue::Boolean(x), LuaValue::Boolean(y)) => x == y,
        (LuaValue::Integer(x), LuaValue::Integer(y)) => x == y,
        (LuaValue::Float(x), LuaValue::Float(y)) => x == y,
        (LuaValue::Integer(x), LuaValue::Float(y)) => (*x as f64) == *y,
        (LuaValue::Float(x), LuaValue::Integer(y)) => *x == (*y as f64),
        (LuaValue::LuaString(x), LuaValue::LuaString(y)) => x == y,
        _ => false,
    }
}

fn cmp_lt(a: &LuaValue, b: &LuaValue) -> Result<bool, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => x < y,
        (LuaValue::Float(x), LuaValue::Float(y)) => x < y,
        (LuaValue::Integer(x), LuaValue::Float(y)) => (*x as f64) < *y,
        (LuaValue::Float(x), LuaValue::Integer(y)) => *x < (*y as f64),
        (LuaValue::LuaString(x), LuaValue::LuaString(y)) => x < y,
        _ => {
            return Err(LuaError::TypeError {
                expected: "number or string",
                got: a.type_name(),
            })
        }
    })
}

fn cmp_le(a: &LuaValue, b: &LuaValue) -> Result<bool, LuaError> {
    Ok(match (a, b) {
        (LuaValue::Integer(x), LuaValue::Integer(y)) => x <= y,
        (LuaValue::Float(x), LuaValue::Float(y)) => x <= y,
        (LuaValue::Integer(x), LuaValue::Float(y)) => (*x as f64) <= *y,
        (LuaValue::Float(x), LuaValue::Integer(y)) => *x <= (*y as f64),
        (LuaValue::LuaString(x), LuaValue::LuaString(y)) => x <= y,
        _ => {
            return Err(LuaError::TypeError {
                expected: "number or string",
                got: a.type_name(),
            })
        }
    })
}

// ── Coercion helpers ──────────────────────────────────────────────────────────

fn to_float(v: &LuaValue) -> Result<f64, LuaError> {
    match v {
        LuaValue::Integer(n) => Ok(*n as f64),
        LuaValue::Float(f) => Ok(*f),
        _ => Err(LuaError::TypeError {
            expected: "number",
            got: v.type_name(),
        }),
    }
}

fn to_string_coerce(v: &LuaValue) -> Result<String, LuaError> {
    match v {
        LuaValue::LuaString(s) => Ok(s.clone()),
        LuaValue::Integer(n) => Ok(n.to_string()),
        LuaValue::Float(f) => Ok(f.to_string()),
        _ => Err(LuaError::TypeError {
            expected: "string or number",
            got: v.type_name(),
        }),
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use lua_compiler::Compiler;
    use lua_parser::Parser;

    fn run(src: &str) -> LuaValue {
        let block = Parser::new(src).unwrap().parse().unwrap();
        let chunk = Compiler::new("<test>").compile(&block).unwrap();
        Vm::new().execute(&chunk).unwrap()
    }

    fn run_err(src: &str) -> LuaError {
        let block = Parser::new(src).unwrap().parse().unwrap();
        let chunk = Compiler::new("<test>").compile(&block).unwrap();
        Vm::new().execute(&chunk).unwrap_err()
    }

    // ── Literals ─────────────────────────────────────────────────────────────
    #[test]
    fn returns_integer() {
        assert_eq!(run("return 42"), LuaValue::Integer(42));
    }
    #[test]
    fn returns_float() {
        assert_eq!(run("return 3.14"), LuaValue::Float(3.14));
    }
    #[test]
    fn returns_string() {
        assert_eq!(
            run(r#"return "hello""#),
            LuaValue::LuaString("hello".into())
        );
    }
    #[test]
    fn returns_nil() {
        assert_eq!(run("return nil"), LuaValue::Nil);
    }
    #[test]
    fn returns_true() {
        assert_eq!(run("return true"), LuaValue::Boolean(true));
    }
    #[test]
    fn returns_false() {
        assert_eq!(run("return false"), LuaValue::Boolean(false));
    }

    // ── Arithmetic ────────────────────────────────────────────────────────────
    #[test]
    fn integer_add() {
        assert_eq!(run("return 1 + 2"), LuaValue::Integer(3));
    }
    #[test]
    fn integer_sub() {
        assert_eq!(run("return 10 - 3"), LuaValue::Integer(7));
    }
    #[test]
    fn integer_mul() {
        assert_eq!(run("return 4 * 5"), LuaValue::Integer(20));
    }
    #[test]
    fn float_div() {
        assert_eq!(run("return 7 / 2"), LuaValue::Float(3.5));
    }
    #[test]
    fn integer_idiv() {
        assert_eq!(run("return 7 // 2"), LuaValue::Integer(3));
    }
    #[test]
    fn integer_mod() {
        assert_eq!(run("return 7 % 3"), LuaValue::Integer(1));
    }
    #[test]
    fn power() {
        assert_eq!(run("return 2 ^ 10"), LuaValue::Float(1024.0));
    }
    #[test]
    fn unary_minus() {
        assert_eq!(run("return -5"), LuaValue::Integer(-5));
    }
    #[test]
    fn precedence_respected() {
        assert_eq!(run("return 2 + 3 * 4"), LuaValue::Integer(14));
    }

    // ── String ────────────────────────────────────────────────────────────────
    #[test]
    fn string_concat() {
        assert_eq!(
            run(r#"return "hello" .. " " .. "world""#),
            LuaValue::LuaString("hello world".into())
        );
    }
    #[test]
    fn string_length() {
        assert_eq!(run(r#"return #"hello""#), LuaValue::Integer(5));
    }

    // ── Comparison ────────────────────────────────────────────────────────────
    #[test]
    fn equal_integers() {
        assert_eq!(run("return 1 == 1"), LuaValue::Boolean(true));
    }
    #[test]
    fn not_equal() {
        assert_eq!(run("return 1 ~= 2"), LuaValue::Boolean(true));
    }
    #[test]
    fn less_than() {
        assert_eq!(run("return 1 < 2"), LuaValue::Boolean(true));
    }

    // ── Logical ───────────────────────────────────────────────────────────────
    #[test]
    fn logical_and_short_circuit() {
        assert_eq!(run("return false and 99"), LuaValue::Boolean(false));
    }
    #[test]
    fn logical_or_short_circuit() {
        assert_eq!(run("return false or 42"), LuaValue::Integer(42));
    }
    #[test]
    fn logical_not() {
        assert_eq!(run("return not false"), LuaValue::Boolean(true));
    }

    // ── Locals ────────────────────────────────────────────────────────────────
    #[test]
    fn local_variable() {
        assert_eq!(run("local x = 10; return x"), LuaValue::Integer(10));
    }
    #[test]
    fn local_arithmetic() {
        assert_eq!(
            run("local x = 3; local y = 4; return x + y"),
            LuaValue::Integer(7)
        );
    }

    // ── Globals ───────────────────────────────────────────────────────────────
    #[test]
    fn global_assign_and_read() {
        assert_eq!(run("x = 99; return x"), LuaValue::Integer(99));
    }

    // ── Control flow ──────────────────────────────────────────────────────────
    #[test]
    fn if_true_branch() {
        assert_eq!(
            run("if true then x = 1 else x = 2 end; return x"),
            LuaValue::Integer(1)
        );
    }
    #[test]
    fn if_false_branch() {
        assert_eq!(
            run("if false then x = 1 else x = 2 end; return x"),
            LuaValue::Integer(2)
        );
    }

    #[test]
    fn while_loop_sum() {
        assert_eq!(
            run("local s = 0; local i = 1; while i <= 5 do s = s + i; i = i + 1 end; return s"),
            LuaValue::Integer(15),
        );
    }

    #[test]
    fn numeric_for_sum() {
        assert_eq!(
            run("local s = 0; for i = 1, 5 do s = s + i end; return s"),
            LuaValue::Integer(15),
        );
    }

    // ── stdlib ────────────────────────────────────────────────────────────────
    #[test]
    fn print_returns_nil() {
        assert_eq!(run("return print(42)"), LuaValue::Nil);
    }

    // ── Error cases ───────────────────────────────────────────────────────────
    #[test]
    fn type_error_on_add_string() {
        assert!(matches!(
            run_err(r#"return "a" + 1"#),
            LuaError::TypeError { .. }
        ));
    }
    #[test]
    fn division_by_zero_idiv_error() {
        assert!(matches!(run_err("return 1 // 0"), LuaError::Runtime(_)));
    }

    // ── First-class functions ─────────────────────────────────────────────────
    #[test]
    fn local_function_call() {
        assert_eq!(
            run("local function add(a, b) return a + b end; return add(3, 4)"),
            LuaValue::Integer(7),
        );
    }

    #[test]
    fn named_function_global() {
        assert_eq!(
            run(r#"function greet(s) return "hi " .. s end; return greet("lua")"#),
            LuaValue::LuaString("hi lua".into()),
        );
    }

    #[test]
    fn anonymous_function_as_value() {
        assert_eq!(
            run("local f = function(x) return x * 2 end; return f(21)"),
            LuaValue::Integer(42),
        );
    }

    #[test]
    fn higher_order_function() {
        assert_eq!(
            run("local function apply(f, x) return f(x) end; return apply(function(n) return n + 1 end, 99)"),
            LuaValue::Integer(100),
        );
    }

    #[test]
    fn recursive_fibonacci() {
        assert_eq!(
            run("local function fib(n) if n <= 1 then return n end; return fib(n-1) + fib(n-2) end; return fib(10)"),
            LuaValue::Integer(55),
        );
    }

    #[test]
    fn closure_captures_upvalue() {
        assert_eq!(
            run("local function make_adder(n) return function(x) return x + n end end; local add5 = make_adder(5); return add5(10)"),
            LuaValue::Integer(15),
        );
    }

    #[test]
    fn closure_mutates_upvalue() {
        assert_eq!(
            run("local function counter() local n = 0; return function() n = n + 1; return n end end; local c = counter(); c(); c(); return c()"),
            LuaValue::Integer(3),
        );
    }

    // ── break ────────────────────────────────────────────────────────────────
    #[test]
    fn break_while_loop() {
        assert_eq!(
            run("local i = 0; while true do i = i + 1; if i == 3 then break end end; return i"),
            LuaValue::Integer(3),
        );
    }

    #[test]
    fn break_numeric_for() {
        assert_eq!(
            run("local r = 0; for i = 1, 10 do r = i; if i == 5 then break end end; return r"),
            LuaValue::Integer(5),
        );
    }

    // ── repeat…until ─────────────────────────────────────────────────────────
    #[test]
    fn repeat_until_basic() {
        assert_eq!(
            run("local i = 0; repeat i = i + 1 until i >= 3; return i"),
            LuaValue::Integer(3),
        );
    }

    // ── multiple return values ────────────────────────────────────────────────
    #[test]
    fn multi_return_assign() {
        assert_eq!(
            run("local function f() return 10, 20, 30 end; local a, b, c = f(); return b"),
            LuaValue::Integer(20),
        );
    }

    #[test]
    fn multi_return_reassign() {
        assert_eq!(
            run("local function minmax(a, b) if a < b then return a, b else return b, a end end; local x, y = 0, 0; x, y = minmax(9, 3); return y"),
            LuaValue::Integer(9),
        );
    }

    // ── tables ────────────────────────────────────────────────────────────────
    #[test]
    fn table_positional() {
        assert_eq!(run("local t = {10, 20, 30}; return t[2]"), LuaValue::Integer(20));
    }

    #[test]
    fn table_named_field() {
        assert_eq!(run("local t = {x = 42}; return t.x"), LuaValue::Integer(42));
    }

    #[test]
    fn table_len() {
        assert_eq!(run("local t = {1, 2, 3}; return #t"), LuaValue::Integer(3));
    }

    #[test]
    fn table_set_and_get() {
        assert_eq!(run("local t = {}; t[1] = 99; return t[1]"), LuaValue::Integer(99));
    }

    #[test]
    fn table_field_assign() {
        assert_eq!(run("local t = {}; t.x = 7; return t.x"), LuaValue::Integer(7));
    }

    // ── ipairs / generic for ─────────────────────────────────────────────────
    #[test]
    fn generic_for_ipairs_sum() {
        assert_eq!(
            run("local s = 0; for i, v in ipairs({1, 2, 3}) do s = s + v end; return s"),
            LuaValue::Integer(6),
        );
    }

    // ── varargs ───────────────────────────────────────────────────────────────
    #[test]
    fn varargs_basic() {
        assert_eq!(
            run("local function sum(a, b, c) return a + b + c end; return sum(1, 2, 3)"),
            LuaValue::Integer(6),
        );
    }

    // ── stdlib: math ─────────────────────────────────────────────────────────
    #[test]
    fn math_floor_test() {
        assert_eq!(run("return math.floor(3.7)"), LuaValue::Integer(3));
    }

    #[test]
    fn math_ceil_test() {
        assert_eq!(run("return math.ceil(3.2)"), LuaValue::Integer(4));
    }

    #[test]
    fn math_abs_test() {
        assert_eq!(run("return math.abs(-5)"), LuaValue::Integer(5));
    }

    #[test]
    fn math_max_test() {
        assert_eq!(run("return math.max(1, 5, 3)"), LuaValue::Integer(5));
    }

    // ── stdlib: string ───────────────────────────────────────────────────────
    #[test]
    fn string_upper_test() {
        assert_eq!(
            run(r#"return string.upper("hello")"#),
            LuaValue::LuaString("HELLO".into()),
        );
    }

    #[test]
    fn string_sub_test() {
        assert_eq!(
            run(r#"return string.sub("hello", 2, 4)"#),
            LuaValue::LuaString("ell".into()),
        );
    }

    #[test]
    fn string_format_test() {
        assert_eq!(
            run(r#"return string.format("x=%d", 42)"#),
            LuaValue::LuaString("x=42".into()),
        );
    }
}
