use crate::chunk::{Chunk, ProtoBuilder};
use lua_core::{LuaError, LuaValue, OpCode, UpvalueDesc};
use lua_parser::ast::{BinOp, Block, CallArgs, Expr, FuncBody, Stmt, UnOp};
use std::sync::Arc;

// ── Register allocator / scope tracker ───────────────────────────────────────

/// Tracks registers and scopes for a single function body.
struct Frame {
    /// `(name, register)` for each live local.
    locals: Vec<(String, u8)>,
    /// Scope checkpoints: `locals.len()` at each `push_scope`.
    scope_stack: Vec<usize>,
    /// Next free register.
    next_reg: u8,
}

impl Frame {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            scope_stack: Vec::new(),
            next_reg: 0,
        }
    }

    fn alloc(&mut self) -> Result<u8, LuaError> {
        if self.next_reg == 255 {
            return Err(LuaError::Internal("register overflow".into()));
        }
        let r = self.next_reg;
        self.next_reg += 1;
        Ok(r)
    }

    /// Allocate the next register and bind it to `name`.
    fn declare_local(&mut self, name: &str) -> Result<u8, LuaError> {
        let r = self.alloc()?;
        self.locals.push((name.to_owned(), r));
        Ok(r)
    }

    /// Return the register for `name`, or `None` if it is not a local.
    fn resolve(&self, name: &str) -> Option<u8> {
        self.locals
            .iter()
            .rposition(|(n, _)| n == name)
            .map(|i| self.locals[i].1)
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(self.locals.len());
    }

    /// Pop a scope, return the base register of the first freed local
    /// (used to know which upvalues to close).
    fn pop_scope(&mut self) -> u8 {
        if let Some(base) = self.scope_stack.pop() {
            let first_freed = if base < self.locals.len() {
                self.locals[base].1
            } else {
                self.next_reg
            };
            // Rewind next_reg to just above the highest surviving local
            self.next_reg = if base == 0 {
                0
            } else {
                self.locals[..base]
                    .iter()
                    .map(|(_, r)| r + 1)
                    .max()
                    .unwrap_or(0)
            };
            self.locals.truncate(base);
            first_freed
        } else {
            0
        }
    }
}

// ── Upvalue descriptor tracking ───────────────────────────────────────────────

/// An upvalue slot as seen by the compiler (not yet an `UpvalueDesc`).
#[derive(Debug, Clone)]
struct UpvalEntry {
    name: String,
    desc: UpvalueDesc,
}

// ── Compiler ─────────────────────────────────────────────────────────────────

/// Compiles a Lua 5.4 AST into register-based [`Chunk`] bytecode.
pub struct Compiler {
    proto: ProtoBuilder,
    frame: Frame,
    /// Upvalues captured from enclosing scopes.
    upvalues: Vec<UpvalEntry>,
    /// Snapshot of the parent compiler's live locals at the time this child
    /// was created.  Used to capture upvalues without needing a live parent ref.
    parent_locals: Vec<(String, u8)>,
    /// Snapshot of the parent's already-captured upvalues (for upvalue-of-upvalue).
    parent_upvalues: Vec<UpvalEntry>,
}

impl Compiler {
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            proto: ProtoBuilder::new(source),
            frame: Frame::new(),
            upvalues: Vec::new(),
            parent_locals: Vec::new(),
            parent_upvalues: Vec::new(),
        }
    }

    fn with_parent(source: impl Into<String>, parent: &Compiler) -> Self {
        Self {
            proto: ProtoBuilder::new(source),
            frame: Frame::new(),
            upvalues: Vec::new(),
            parent_locals: parent.frame.locals.clone(),
            parent_upvalues: parent.upvalues.clone(),
        }
    }

    /// Compile `block` and return the finished [`Chunk`].
    pub fn compile(mut self, block: &Block) -> Result<Chunk, LuaError> {
        self.compile_block(block)?;
        // Implicit `return nil` at end of top-level chunk
        let nil_reg = self.frame.alloc()?;
        self.proto.emit(OpCode::LoadNil { dst: nil_reg });
        self.proto.emit(OpCode::Return {
            src: nil_reg,
            num_results: 0,
        });
        Ok(Chunk::new(self.proto.finish()))
    }

    // ── Block / Statements ────────────────────────────────────────────────────

    fn compile_block(&mut self, block: &Block) -> Result<(), LuaError> {
        self.frame.push_scope();
        for stmt in &block.stmts {
            self.compile_stmt(stmt)?;
        }
        if let Some(ret) = &block.ret {
            if ret.values.is_empty() {
                let r = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadNil { dst: r });
                self.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 0,
                });
            } else {
                let r = self.compile_expr(&ret.values[0])?;
                self.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 1,
                });
            }
        }
        let from_reg = self.frame.pop_scope();
        // Close any upvalues that were pointing at locals now going out of scope
        if self.has_upvalue_for_reg_range(from_reg) {
            self.proto.emit(OpCode::CloseUpvalues { from_reg });
        }
        Ok(())
    }

    /// Returns true if any captured upvalue references a stack register >= `from`.
    fn has_upvalue_for_reg_range(&self, from: u8) -> bool {
        self.upvalues
            .iter()
            .any(|u| matches!(u.desc, UpvalueDesc::Stack(r) if r >= from))
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), LuaError> {
        match stmt {
            Stmt::Local { names, values, .. } => {
                // Evaluate all RHS into temporaries first, then bind locals
                let mut val_regs: Vec<u8> = Vec::new();
                for val in values {
                    val_regs.push(self.compile_expr(val)?);
                }
                for (i, ln) in names.iter().enumerate() {
                    let dst = self.frame.declare_local(&ln.name)?;
                    if let Some(&src) = val_regs.get(i) {
                        self.proto.emit(OpCode::Move { dst, src });
                    } else {
                        self.proto.emit(OpCode::LoadNil { dst });
                    }
                }
            }

            Stmt::Assign {
                targets, values, ..
            } => {
                let mut val_regs: Vec<u8> = Vec::new();
                for val in values {
                    val_regs.push(self.compile_expr(val)?);
                }
                for (i, target) in targets.iter().enumerate() {
                    match target {
                        Expr::Name(name, _) => {
                            let src_opt = val_regs.get(i).copied();
                            self.assign_name(name, src_opt)?;
                        }
                        _ => {
                            return Err(LuaError::Internal(
                                "complex assignment target not yet supported".into(),
                            ))
                        }
                    }
                }
            }

            Stmt::FnCall(expr) => {
                self.compile_expr(expr)?;
            }

            Stmt::Do(block) => {
                self.compile_block(block)?;
            }

            Stmt::While { cond, body, .. } => {
                let loop_start = self.proto.instructions.len() as i16;
                let cond_reg = self.compile_expr(cond)?;
                let jif_idx = self.proto.instructions.len();
                self.proto.emit(OpCode::JumpIfFalse {
                    src: cond_reg,
                    offset: 0,
                });
                self.compile_block(body)?;
                let loop_end = self.proto.instructions.len();
                let back = loop_start - loop_end as i16 - 1;
                self.proto.emit(OpCode::Jump { offset: back });
                let exit = (loop_end as i16 + 1) - (jif_idx as i16 + 1);
                self.patch_jump(jif_idx, exit);
            }

            Stmt::If {
                cond,
                then,
                elseifs,
                else_,
                ..
            } => {
                self.compile_if(cond, then, elseifs, else_.as_ref())?;
            }

            Stmt::NumericFor {
                var,
                start,
                limit,
                step,
                body,
                ..
            } => {
                self.compile_numeric_for(var, start, limit, step.as_ref(), body)?;
            }

            // ── Function definitions ──────────────────────────────────────────
            Stmt::LocalFn { name, body, .. } => {
                // `local function f() end` ≡ `local f; f = function() end`
                // Declare the local first so the function can reference itself.
                let dst = self.frame.declare_local(name)?;
                let closure_reg = self.compile_func_body(body)?;
                self.proto.emit(OpCode::Move {
                    dst,
                    src: closure_reg,
                });
            }

            Stmt::FnDef { name, body, .. } => {
                // `function a.b.c() end` — only simple single-name case for now
                if name.parts.len() == 1 && name.method.is_none() {
                    let closure_reg = self.compile_func_body(body)?;
                    let name_idx = self.proto.add_name(&name.parts[0]);
                    self.proto.emit(OpCode::SetGlobal {
                        src: closure_reg,
                        name_idx,
                    });
                } else {
                    return Err(LuaError::Internal(
                        "dotted/method function names not yet supported".into(),
                    ));
                }
            }

            Stmt::Break(_) => {
                return Err(LuaError::Internal("`break` not yet implemented".into()));
            }
            Stmt::Repeat { .. } => {
                return Err(LuaError::Internal("`repeat` not yet implemented".into()));
            }
            Stmt::GenericFor { .. } => {
                return Err(LuaError::Internal("generic for not yet implemented".into()));
            }
            Stmt::Goto { .. } | Stmt::Label { .. } => {
                return Err(LuaError::Internal("goto/label not yet implemented".into()));
            }
        }
        Ok(())
    }

    // ── Assign helpers ────────────────────────────────────────────────────────

    /// Write `src_opt` (or nil) to the named variable (local / upvalue / global).
    fn assign_name(&mut self, name: &str, src_opt: Option<u8>) -> Result<(), LuaError> {
        if let Some(reg) = self.frame.resolve(name) {
            // Local
            if let Some(src) = src_opt {
                self.proto.emit(OpCode::Move { dst: reg, src });
            } else {
                self.proto.emit(OpCode::LoadNil { dst: reg });
            }
        } else if let Some(upval_idx) = self.resolve_upvalue(name) {
            // Upvalue
            let src = if let Some(s) = src_opt {
                s
            } else {
                let tmp = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadNil { dst: tmp });
                tmp
            };
            self.proto.emit(OpCode::SetUpvalue { src, upval_idx });
        } else {
            // Global
            let src = if let Some(s) = src_opt {
                s
            } else {
                let tmp = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadNil { dst: tmp });
                tmp
            };
            let name_idx = self.proto.add_name(name);
            self.proto.emit(OpCode::SetGlobal { src, name_idx });
        }
        Ok(())
    }

    // ── Control flow helpers ──────────────────────────────────────────────────

    fn compile_if(
        &mut self,
        cond: &Expr,
        then: &Block,
        elseifs: &[(Expr, Block)],
        else_: Option<&Block>,
    ) -> Result<(), LuaError> {
        let cond_reg = self.compile_expr(cond)?;
        let jif_idx = self.proto.instructions.len();
        self.proto.emit(OpCode::JumpIfFalse {
            src: cond_reg,
            offset: 0,
        });

        self.compile_block(then)?;

        let mut end_jumps: Vec<usize> = Vec::new();
        let mut last_jif = jif_idx;

        for (ei_cond, ei_body) in elseifs {
            end_jumps.push(self.proto.instructions.len());
            self.proto.emit(OpCode::Jump { offset: 0 });
            let here = self.proto.instructions.len() as i16;
            self.patch_jump(last_jif, here - (last_jif as i16 + 1));
            let ei_reg = self.compile_expr(ei_cond)?;
            last_jif = self.proto.instructions.len();
            self.proto.emit(OpCode::JumpIfFalse {
                src: ei_reg,
                offset: 0,
            });
            self.compile_block(ei_body)?;
        }

        if let Some(else_block) = else_ {
            end_jumps.push(self.proto.instructions.len());
            self.proto.emit(OpCode::Jump { offset: 0 });
            let here = self.proto.instructions.len() as i16;
            self.patch_jump(last_jif, here - (last_jif as i16 + 1));
            self.compile_block(else_block)?;
        } else {
            let here = self.proto.instructions.len() as i16;
            self.patch_jump(last_jif, here - (last_jif as i16 + 1));
        }

        let end = self.proto.instructions.len() as i16;
        for idx in end_jumps {
            self.patch_jump(idx, end - (idx as i16 + 1));
        }
        Ok(())
    }

    fn compile_numeric_for(
        &mut self,
        var: &str,
        start: &Expr,
        limit: &Expr,
        step: Option<&Expr>,
        body: &Block,
    ) -> Result<(), LuaError> {
        self.frame.push_scope();
        let start_reg = self.compile_expr(start)?;
        let limit_reg = self.compile_expr(limit)?;
        let step_reg = if let Some(s) = step {
            self.compile_expr(s)?
        } else {
            let r = self.frame.alloc()?;
            let idx = self.proto.add_constant(LuaValue::Integer(1));
            self.proto.emit(OpCode::LoadConst {
                dst: r,
                const_idx: idx,
            });
            r
        };
        let var_reg = self.frame.declare_local(var)?;
        self.proto.emit(OpCode::Move {
            dst: var_reg,
            src: start_reg,
        });

        let loop_start = self.proto.instructions.len();
        let cmp_reg = self.frame.alloc()?;
        self.proto.emit(OpCode::Le {
            dst: cmp_reg,
            lhs: var_reg,
            rhs: limit_reg,
        });
        let jif_idx = self.proto.instructions.len();
        self.proto.emit(OpCode::JumpIfFalse {
            src: cmp_reg,
            offset: 0,
        });
        self.compile_block(body)?;
        let tmp = self.frame.alloc()?;
        self.proto.emit(OpCode::Add {
            dst: tmp,
            lhs: var_reg,
            rhs: step_reg,
        });
        self.proto.emit(OpCode::Move {
            dst: var_reg,
            src: tmp,
        });
        let back = loop_start as i16 - self.proto.instructions.len() as i16 - 1;
        self.proto.emit(OpCode::Jump { offset: back });
        let end = self.proto.instructions.len() as i16;
        self.patch_jump(jif_idx, end - (jif_idx as i16 + 1));
        self.frame.pop_scope();
        Ok(())
    }

    fn patch_jump(&mut self, idx: usize, offset: i16) {
        match &mut self.proto.instructions[idx] {
            OpCode::Jump { offset: o } => *o = offset,
            OpCode::JumpIfFalse { offset: o, .. } => *o = offset,
            _ => {}
        }
    }

    // ── Function body compilation ─────────────────────────────────────────────

    /// Compile a function body into a child `Proto`, add it to the current
    /// proto's table, and emit a `Closure` opcode returning the closure in a
    /// fresh register.
    fn compile_func_body(&mut self, body: &FuncBody) -> Result<u8, LuaError> {
        // Build child compiler with a snapshot of the current locals for upvalue capture
        let mut child = Compiler::with_parent(self.proto.source.clone(), self);
        child.proto.param_count = body.params.len() as u8;
        child.proto.is_vararg = body.vararg;

        // Declare parameters as the first locals
        child.frame.push_scope();
        for p in &body.params {
            child.frame.declare_local(p)?;
        }

        // Compile the body
        for stmt in &body.body.stmts {
            child.compile_stmt_with_parent(stmt, self)?;
        }
        if let Some(ret) = &body.body.ret {
            if ret.values.is_empty() {
                let r = child.frame.alloc()?;
                child.proto.emit(OpCode::LoadNil { dst: r });
                child.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 0,
                });
            } else {
                let r = child.compile_expr_with_parent(&ret.values[0], self)?;
                child.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 1,
                });
            }
        }
        child.frame.pop_scope();
        // Implicit return nil
        let nil = child.frame.alloc()?;
        child.proto.emit(OpCode::LoadNil { dst: nil });
        child.proto.emit(OpCode::Return {
            src: nil,
            num_results: 0,
        });

        // Freeze the upvalue descriptors into the child proto
        child.proto.upvalue_descs = child.upvalues.iter().map(|u| u.desc.clone()).collect();

        let child_proto = Arc::new(child.proto.finish());
        let proto_idx = self.proto.add_proto(child_proto);
        let dst = self.frame.alloc()?;
        self.proto.emit(OpCode::Closure { dst, proto_idx });
        Ok(dst)
    }

    // ── Upvalue resolution ────────────────────────────────────────────────────

    /// Find or create an upvalue slot for `name`, returning the slot index.
    /// Checks: (1) already captured, (2) parent locals snapshot, (3) parent upvalues.
    /// Returns `None` if the name should be treated as a global.
    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        // Already captured?
        if let Some(i) = self.upvalues.iter().position(|u| u.name == name) {
            return Some(i as u8);
        }
        // Capture from parent's live locals (Stack upvalue)
        if let Some(pos) = self.parent_locals.iter().rposition(|(n, _)| n == name) {
            let reg = self.parent_locals[pos].1;
            let idx = self.upvalues.len() as u8;
            self.upvalues.push(UpvalEntry {
                name: name.to_owned(),
                desc: UpvalueDesc::Stack(reg),
            });
            return Some(idx);
        }
        // Capture from parent's upvalues (Upvalue-of-upvalue)
        if let Some(pos) = self.parent_upvalues.iter().position(|u| u.name == name) {
            let parent_upval_idx = pos as u8;
            let idx = self.upvalues.len() as u8;
            self.upvalues.push(UpvalEntry {
                name: name.to_owned(),
                desc: UpvalueDesc::Upvalue(parent_upval_idx),
            });
            return Some(idx);
        }
        None
    }

    // ── Expression compilation ────────────────────────────────────────────────

    fn compile_expr(&mut self, expr: &Expr) -> Result<u8, LuaError> {
        self.compile_expr_impl(expr, None::<&mut Compiler>)
    }

    fn compile_expr_with_parent(
        &mut self,
        expr: &Expr,
        parent: &mut Compiler,
    ) -> Result<u8, LuaError> {
        self.compile_expr_impl(expr, Some(parent))
    }

    fn compile_stmt_with_parent(
        &mut self,
        stmt: &Stmt,
        parent: &mut Compiler,
    ) -> Result<(), LuaError> {
        // For most statements, just delegate without parent (no nested fns inside)
        // but for expressions inside statements, we need parent for name resolution.
        // Use a helper that handles expr compilation with parent awareness.
        match stmt {
            Stmt::Local { names, values, .. } => {
                let mut val_regs: Vec<u8> = Vec::new();
                for val in values {
                    val_regs.push(self.compile_expr_with_parent(val, parent)?);
                }
                for (i, ln) in names.iter().enumerate() {
                    let dst = self.frame.declare_local(&ln.name)?;
                    if let Some(&src) = val_regs.get(i) {
                        self.proto.emit(OpCode::Move { dst, src });
                    } else {
                        self.proto.emit(OpCode::LoadNil { dst });
                    }
                }
            }
            Stmt::Assign {
                targets, values, ..
            } => {
                let mut val_regs: Vec<u8> = Vec::new();
                for val in values {
                    val_regs.push(self.compile_expr_with_parent(val, parent)?);
                }
                for (i, target) in targets.iter().enumerate() {
                    match target {
                        Expr::Name(name, _) => {
                            let src_opt = val_regs.get(i).copied();
                            self.assign_name_with_parent(name, src_opt, parent)?;
                        }
                        _ => {
                            return Err(LuaError::Internal(
                                "complex assignment not supported".into(),
                            ))
                        }
                    }
                }
            }
            Stmt::FnCall(expr) => {
                self.compile_expr_with_parent(expr, parent)?;
            }
            Stmt::Do(block) => {
                self.compile_block_with_parent(block, parent)?;
            }
            Stmt::While { cond, body, .. } => {
                let loop_start = self.proto.instructions.len() as i16;
                let cond_reg = self.compile_expr_with_parent(cond, parent)?;
                let jif_idx = self.proto.instructions.len();
                self.proto.emit(OpCode::JumpIfFalse {
                    src: cond_reg,
                    offset: 0,
                });
                self.compile_block_with_parent(body, parent)?;
                let loop_end = self.proto.instructions.len();
                let back = loop_start - loop_end as i16 - 1;
                self.proto.emit(OpCode::Jump { offset: back });
                let exit = (loop_end as i16 + 1) - (jif_idx as i16 + 1);
                self.patch_jump(jif_idx, exit);
            }
            Stmt::If {
                cond,
                then,
                elseifs,
                else_,
                ..
            } => {
                let cond_reg = self.compile_expr_with_parent(cond, parent)?;
                let jif_idx = self.proto.instructions.len();
                self.proto.emit(OpCode::JumpIfFalse {
                    src: cond_reg,
                    offset: 0,
                });
                self.compile_block_with_parent(then, parent)?;
                let mut end_jumps: Vec<usize> = Vec::new();
                let mut last_jif = jif_idx;
                for (ei_cond, ei_body) in elseifs {
                    end_jumps.push(self.proto.instructions.len());
                    self.proto.emit(OpCode::Jump { offset: 0 });
                    let here = self.proto.instructions.len() as i16;
                    self.patch_jump(last_jif, here - (last_jif as i16 + 1));
                    let ei_reg = self.compile_expr_with_parent(ei_cond, parent)?;
                    last_jif = self.proto.instructions.len();
                    self.proto.emit(OpCode::JumpIfFalse {
                        src: ei_reg,
                        offset: 0,
                    });
                    self.compile_block_with_parent(ei_body, parent)?;
                }
                if let Some(else_block) = else_ {
                    end_jumps.push(self.proto.instructions.len());
                    self.proto.emit(OpCode::Jump { offset: 0 });
                    let here = self.proto.instructions.len() as i16;
                    self.patch_jump(last_jif, here - (last_jif as i16 + 1));
                    self.compile_block_with_parent(else_block, parent)?;
                } else {
                    let here = self.proto.instructions.len() as i16;
                    self.patch_jump(last_jif, here - (last_jif as i16 + 1));
                }
                let end = self.proto.instructions.len() as i16;
                for idx in end_jumps {
                    self.patch_jump(idx, end - (idx as i16 + 1));
                }
            }
            Stmt::NumericFor {
                var,
                start,
                limit,
                step,
                body,
                ..
            } => {
                self.frame.push_scope();
                let start_r = self.compile_expr_with_parent(start, parent)?;
                let limit_r = self.compile_expr_with_parent(limit, parent)?;
                let step_r = if let Some(s) = step {
                    self.compile_expr_with_parent(s, parent)?
                } else {
                    let r = self.frame.alloc()?;
                    let idx = self.proto.add_constant(LuaValue::Integer(1));
                    self.proto.emit(OpCode::LoadConst {
                        dst: r,
                        const_idx: idx,
                    });
                    r
                };
                let var_reg = self.frame.declare_local(var)?;
                self.proto.emit(OpCode::Move {
                    dst: var_reg,
                    src: start_r,
                });
                let loop_start = self.proto.instructions.len();
                let cmp_reg = self.frame.alloc()?;
                self.proto.emit(OpCode::Le {
                    dst: cmp_reg,
                    lhs: var_reg,
                    rhs: limit_r,
                });
                let jif_idx = self.proto.instructions.len();
                self.proto.emit(OpCode::JumpIfFalse {
                    src: cmp_reg,
                    offset: 0,
                });
                self.compile_block_with_parent(body, parent)?;
                let tmp = self.frame.alloc()?;
                self.proto.emit(OpCode::Add {
                    dst: tmp,
                    lhs: var_reg,
                    rhs: step_r,
                });
                self.proto.emit(OpCode::Move {
                    dst: var_reg,
                    src: tmp,
                });
                let back = loop_start as i16 - self.proto.instructions.len() as i16 - 1;
                self.proto.emit(OpCode::Jump { offset: back });
                let end = self.proto.instructions.len() as i16;
                self.patch_jump(jif_idx, end - (jif_idx as i16 + 1));
                self.frame.pop_scope();
            }
            Stmt::LocalFn { name, body, .. } => {
                let dst = self.frame.declare_local(name)?;
                let closure_reg = self.compile_func_body_with_parent(body, parent)?;
                self.proto.emit(OpCode::Move {
                    dst,
                    src: closure_reg,
                });
            }
            Stmt::FnDef { name, body, .. } => {
                if name.parts.len() == 1 && name.method.is_none() {
                    let closure_reg = self.compile_func_body_with_parent(body, parent)?;
                    let name_idx = self.proto.add_name(&name.parts[0]);
                    self.proto.emit(OpCode::SetGlobal {
                        src: closure_reg,
                        name_idx,
                    });
                } else {
                    return Err(LuaError::Internal("dotted fn names not supported".into()));
                }
            }
            _ => {
                self.compile_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn compile_block_with_parent(
        &mut self,
        block: &Block,
        parent: &mut Compiler,
    ) -> Result<(), LuaError> {
        self.frame.push_scope();
        for stmt in &block.stmts {
            self.compile_stmt_with_parent(stmt, parent)?;
        }
        if let Some(ret) = &block.ret {
            if ret.values.is_empty() {
                let r = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadNil { dst: r });
                self.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 0,
                });
            } else {
                let r = self.compile_expr_with_parent(&ret.values[0], parent)?;
                self.proto.emit(OpCode::Return {
                    src: r,
                    num_results: 1,
                });
            }
        }
        let from_reg = self.frame.pop_scope();
        if self.has_upvalue_for_reg_range(from_reg) {
            self.proto.emit(OpCode::CloseUpvalues { from_reg });
        }
        Ok(())
    }

    fn assign_name_with_parent(
        &mut self,
        name: &str,
        src_opt: Option<u8>,
        parent: &mut Compiler,
    ) -> Result<(), LuaError> {
        if let Some(reg) = self.frame.resolve(name) {
            if let Some(src) = src_opt {
                self.proto.emit(OpCode::Move { dst: reg, src });
            } else {
                self.proto.emit(OpCode::LoadNil { dst: reg });
            }
        } else if let Some(upval_idx) = self.resolve_upvalue_with_parent(name, parent) {
            let src = match src_opt {
                Some(s) => s,
                None => {
                    let tmp = self.frame.alloc()?;
                    self.proto.emit(OpCode::LoadNil { dst: tmp });
                    tmp
                }
            };
            self.proto.emit(OpCode::SetUpvalue { src, upval_idx });
        } else {
            let src = match src_opt {
                Some(s) => s,
                None => {
                    let tmp = self.frame.alloc()?;
                    self.proto.emit(OpCode::LoadNil { dst: tmp });
                    tmp
                }
            };
            let name_idx = self.proto.add_name(name);
            self.proto.emit(OpCode::SetGlobal { src, name_idx });
        }
        Ok(())
    }

    /// Try to resolve `name` as an upvalue, capturing from `parent` if needed.
    fn resolve_upvalue_with_parent(&mut self, name: &str, parent: &mut Compiler) -> Option<u8> {
        // Already captured?
        if let Some(i) = self.upvalues.iter().position(|u| u.name == name) {
            return Some(i as u8);
        }
        // Try to capture from parent's locals
        if let Some(reg) = parent.frame.resolve(name) {
            let idx = self.upvalues.len() as u8;
            self.upvalues.push(UpvalEntry {
                name: name.to_owned(),
                desc: UpvalueDesc::Stack(reg),
            });
            return Some(idx);
        }
        // Try to capture from parent's upvalues (upvalue-of-upvalue)
        if let Some(upval_idx) = parent.resolve_upvalue(name) {
            let idx = self.upvalues.len() as u8;
            self.upvalues.push(UpvalEntry {
                name: name.to_owned(),
                desc: UpvalueDesc::Upvalue(upval_idx),
            });
            return Some(idx);
        }
        None
    }

    fn compile_func_body_with_parent(
        &mut self,
        body: &FuncBody,
        _parent: &mut Compiler,
    ) -> Result<u8, LuaError> {
        // Grandchild compiler: child's parent is `self`, self's parent is `parent`.
        // For simplicity, build the child using self as its effective parent.
        self.compile_func_body(body)
    }

    fn compile_expr_impl(
        &mut self,
        expr: &Expr,
        _parent: Option<&mut Compiler>,
    ) -> Result<u8, LuaError> {
        match expr {
            Expr::Nil(_) => {
                let dst = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadNil { dst });
                Ok(dst)
            }
            Expr::True(_) => {
                let dst = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadBool {
                    dst,
                    value: true,
                    skip: false,
                });
                Ok(dst)
            }
            Expr::False(_) => {
                let dst = self.frame.alloc()?;
                self.proto.emit(OpCode::LoadBool {
                    dst,
                    value: false,
                    skip: false,
                });
                Ok(dst)
            }
            Expr::Integer(n, _) => {
                let dst = self.frame.alloc()?;
                let idx = self.proto.add_constant(LuaValue::Integer(*n));
                self.proto.emit(OpCode::LoadConst {
                    dst,
                    const_idx: idx,
                });
                Ok(dst)
            }
            Expr::Float(f, _) => {
                let dst = self.frame.alloc()?;
                let idx = self.proto.add_constant(LuaValue::Float(*f));
                self.proto.emit(OpCode::LoadConst {
                    dst,
                    const_idx: idx,
                });
                Ok(dst)
            }
            Expr::LuaString(s, _) => {
                let dst = self.frame.alloc()?;
                let idx = self.proto.add_constant(LuaValue::LuaString(s.clone()));
                self.proto.emit(OpCode::LoadConst {
                    dst,
                    const_idx: idx,
                });
                Ok(dst)
            }

            Expr::Name(name, _) => {
                let dst = self.frame.alloc()?;
                if let Some(src) = self.frame.resolve(name) {
                    // Local variable in current scope
                    self.proto.emit(OpCode::Move { dst, src });
                } else if let Some(upval_idx) = self.resolve_upvalue(name) {
                    // Already-captured upvalue
                    self.proto.emit(OpCode::GetUpvalue { dst, upval_idx });
                } else {
                    // Global variable
                    let name_idx = self.proto.add_name(name);
                    self.proto.emit(OpCode::GetGlobal { dst, name_idx });
                }
                Ok(dst)
            }

            Expr::BinOp { op, lhs, rhs, .. } => {
                let l = self.compile_expr(lhs)?;
                let r = self.compile_expr(rhs)?;
                let dst = self.frame.alloc()?;
                let opcode = match op {
                    BinOp::Add => OpCode::Add {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Sub => OpCode::Sub {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Mul => OpCode::Mul {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Div => OpCode::Div {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::IDiv => OpCode::IDiv {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Mod => OpCode::Mod {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Pow => OpCode::Pow {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Concat => OpCode::Concat {
                        dst,
                        start: l,
                        end: r,
                    },
                    BinOp::Eq => OpCode::Eq {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::NotEq => {
                        self.proto.emit(OpCode::Eq {
                            dst,
                            lhs: l,
                            rhs: r,
                        });
                        OpCode::Not { dst, src: dst }
                    }
                    BinOp::Lt => OpCode::Lt {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::LtEq => OpCode::Le {
                        dst,
                        lhs: l,
                        rhs: r,
                    },
                    BinOp::Gt => OpCode::Lt {
                        dst,
                        lhs: r,
                        rhs: l,
                    },
                    BinOp::GtEq => OpCode::Le {
                        dst,
                        lhs: r,
                        rhs: l,
                    },
                    BinOp::And => {
                        self.proto.emit(OpCode::Move { dst, src: l });
                        let jif = self.proto.instructions.len();
                        self.proto.emit(OpCode::JumpIfFalse {
                            src: dst,
                            offset: 0,
                        });
                        self.proto.emit(OpCode::Move { dst, src: r });
                        let here = self.proto.instructions.len() as i16;
                        self.patch_jump(jif, here - (jif as i16 + 1));
                        return Ok(dst);
                    }
                    BinOp::Or => {
                        self.proto.emit(OpCode::Move { dst, src: l });
                        let jit = self.proto.instructions.len();
                        self.proto.emit(OpCode::JumpIfFalse {
                            src: dst,
                            offset: 0,
                        });
                        let skip = self.proto.instructions.len();
                        self.proto.emit(OpCode::Jump { offset: 0 });
                        let after = self.proto.instructions.len() as i16;
                        self.patch_jump(jit, after - (jit as i16 + 1));
                        self.proto.emit(OpCode::Move { dst, src: r });
                        let end = self.proto.instructions.len() as i16;
                        self.patch_jump(skip, end - (skip as i16 + 1));
                        return Ok(dst);
                    }
                    _ => {
                        return Err(LuaError::Internal(format!(
                            "binop {:?} not implemented",
                            op
                        )))
                    }
                };
                self.proto.emit(opcode);
                Ok(dst)
            }

            Expr::UnOp { op, operand, .. } => {
                let src = self.compile_expr_impl(operand, None)?;
                let dst = self.frame.alloc()?;
                let opcode = match op {
                    UnOp::Neg => OpCode::Unm { dst, src },
                    UnOp::Not => OpCode::Not { dst, src },
                    UnOp::Len => OpCode::Len { dst, src },
                    UnOp::BitNot => {
                        return Err(LuaError::Internal("bitwise not not yet implemented".into()))
                    }
                };
                self.proto.emit(opcode);
                Ok(dst)
            }

            Expr::FnDef(body) => {
                // Anonymous function expression
                self.compile_func_body(body)
            }

            Expr::FnCall { func, args, .. } => {
                let func_reg = self.compile_expr_impl(func, None)?;
                let num_args = self.compile_call_args(args, func_reg + 1)?;
                self.proto.emit(OpCode::Call {
                    func: func_reg,
                    num_args,
                    num_results: 1,
                });
                Ok(func_reg)
            }

            _ => Err(LuaError::Internal(format!(
                "expression not yet supported: {:?}",
                expr
            ))),
        }
    }

    fn compile_call_args(&mut self, args: &CallArgs, first_arg_reg: u8) -> Result<u8, LuaError> {
        match args {
            CallArgs::Exprs(exprs) => {
                for (i, e) in exprs.iter().enumerate() {
                    let reg = self.compile_expr(e)?;
                    let target = first_arg_reg + i as u8;
                    if reg != target {
                        self.proto.emit(OpCode::Move {
                            dst: target,
                            src: reg,
                        });
                    }
                }
                Ok(exprs.len() as u8)
            }
            CallArgs::String(s) => {
                let idx = self.proto.add_constant(LuaValue::LuaString(s.clone()));
                self.proto.emit(OpCode::LoadConst {
                    dst: first_arg_reg,
                    const_idx: idx,
                });
                Ok(1)
            }
            CallArgs::Table(_) => Err(LuaError::Internal(
                "table call args not yet supported".into(),
            )),
        }
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use lua_parser::Parser;

    fn compile(src: &str) -> Chunk {
        let block = Parser::new(src).unwrap().parse().unwrap();
        Compiler::new("<test>").compile(&block).unwrap()
    }

    #[allow(dead_code)]
    fn compile_err(src: &str) -> LuaError {
        let block = Parser::new(src).unwrap().parse().unwrap();
        Compiler::new("<test>").compile(&block).unwrap_err()
    }

    fn opcodes(chunk: &Chunk) -> Vec<String> {
        chunk
            .proto
            .instructions
            .iter()
            .map(|op| format!("{op:?}"))
            .collect()
    }

    // ── Constants ──────────────────────────────────────────────────────────────
    #[test]
    fn compile_integer_constant() {
        let chunk = compile("return 42");
        assert!(chunk.proto.constants.contains(&LuaValue::Integer(42)));
        assert!(opcodes(&chunk).iter().any(|s| s.contains("LoadConst")));
    }

    #[test]
    fn compile_float_constant() {
        let chunk = compile("return 3.14");
        assert!(chunk.proto.constants.contains(&LuaValue::Float(3.14)));
    }

    #[test]
    fn compile_string_constant() {
        let chunk = compile(r#"return "hello""#);
        assert!(chunk
            .proto
            .constants
            .contains(&LuaValue::LuaString("hello".into())));
    }

    #[test]
    fn compile_nil() {
        let chunk = compile("return nil");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("LoadNil")));
    }

    #[test]
    fn compile_true_false() {
        let chunk = compile("return true");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("LoadBool")));
    }

    // ── Arithmetic ────────────────────────────────────────────────────────────
    #[test]
    fn compile_add() {
        let chunk = compile("return 1 + 2");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("Add")));
    }

    #[test]
    fn compile_mul_and_add() {
        let chunk = compile("return 1 + 2 * 3");
        let ops = opcodes(&chunk);
        assert!(ops.iter().any(|s| s.contains("Mul")));
        assert!(ops.iter().any(|s| s.contains("Add")));
    }

    // ── Locals ────────────────────────────────────────────────────────────────
    #[test]
    fn compile_local_assignment() {
        let chunk = compile("local x = 10");
        assert!(chunk.proto.constants.contains(&LuaValue::Integer(10)));
    }

    #[test]
    fn compile_local_read() {
        let chunk = compile("local x = 5; return x");
        let ops = opcodes(&chunk);
        assert!(ops.iter().any(|s| s.contains("Move")));
    }

    // ── Globals ───────────────────────────────────────────────────────────────
    #[test]
    fn compile_get_global() {
        let chunk = compile("return _G");
        assert!(chunk.proto.names.contains(&"_G".to_string()));
        assert!(opcodes(&chunk).iter().any(|s| s.contains("GetGlobal")));
    }

    #[test]
    fn compile_set_global() {
        let chunk = compile("x = 1");
        assert!(chunk.proto.names.contains(&"x".to_string()));
        assert!(opcodes(&chunk).iter().any(|s| s.contains("SetGlobal")));
    }

    // ── Control flow ──────────────────────────────────────────────────────────
    #[test]
    fn compile_while() {
        let chunk = compile("while false do end");
        let ops = opcodes(&chunk);
        assert!(ops.iter().any(|s| s.contains("JumpIfFalse")));
        assert!(ops.iter().any(|s| s.contains("Jump")));
    }

    #[test]
    fn compile_if() {
        let chunk = compile("if true then end");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("JumpIfFalse")));
    }

    // ── Function calls ────────────────────────────────────────────────────────
    #[test]
    fn compile_fn_call() {
        let chunk = compile("print(1)");
        assert!(chunk.proto.names.contains(&"print".to_string()));
        assert!(opcodes(&chunk).iter().any(|s| s.contains("Call")));
    }

    // ── Closure compilation ───────────────────────────────────────────────────
    #[test]
    fn compile_local_function() {
        let chunk = compile("local function f(x) return x end");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("Closure")));
    }

    #[test]
    fn compile_anonymous_function() {
        let chunk = compile("local f = function(x) return x * 2 end");
        assert!(opcodes(&chunk).iter().any(|s| s.contains("Closure")));
    }

    // ── Error cases ───────────────────────────────────────────────────────────
    #[test]
    fn break_not_yet_supported() {
        let block = lua_parser::Parser::new("while true do break end")
            .unwrap()
            .parse()
            .unwrap();
        assert!(Compiler::new("<test>").compile(&block).is_err());
    }
}
