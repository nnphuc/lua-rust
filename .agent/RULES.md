# Agent Rules — Lua → Rust Rewrite

## Project Goal
Faithfully rewrite the Lua 5.x programming language runtime in Rust.
The output must be **correct**, **well-tested**, and **architecturally clean**.

---

## Core Principles

### 1. Test-Driven Development (TDD)
- **Write tests first**, then implement.
- Every module MUST have a corresponding `tests/` file or inline `#[cfg(test)]` block.
- All public APIs must have unit tests.
- Integration tests live in `tests/` at the crate root.
- No feature is considered done until tests pass.
- Use `cargo test` to run the full suite; never skip failing tests.

### 2. Clean Architecture
Follow a strict layered architecture. Dependencies only point **inward**:

```
┌────────────────────────────────────┐
│         CLI / REPL (bin)           │  ← outermost: IO, user interface
├────────────────────────────────────┤
│         VM / Interpreter           │  ← orchestration, execution loop
├────────────────────────────────────┤
│   Compiler (Parser → AST → BC)     │  ← lexer → parser → code-gen
├────────────────────────────────────┤
│         Core Types & Traits        │  ← LuaValue, LuaError, opcodes
└────────────────────────────────────┘
```

Layers communicate through **traits and interfaces**, never through concrete cross-layer imports.

### 3. Clear Code Standards
- Use idiomatic Rust (`clippy`, `rustfmt` must pass with zero warnings).
- Prefer **strong types** over `Any` or raw enums where possible.
- Name things precisely: `lex_token()` not `process()`, `OpCode::Add` not `Op::A`.
- Avoid `unwrap()` in library code — use `?` and proper `Result<_, LuaError>`.
- Document every public item with `///` doc comments.
- Keep functions small (< 40 lines is a guideline).

---

## Crate Structure

```
lua-rust/
├── Cargo.toml           # workspace root
├── crates/
│   ├── lua-core/        # LuaValue, LuaError, Bytecode opcodes, traits
│   ├── lua-lexer/       # Tokenizer
│   ├── lua-parser/      # AST types + recursive-descent parser
│   ├── lua-compiler/    # AST → Bytecode
│   ├── lua-vm/          # Bytecode virtual machine + standard library
│   └── lua-repl/        # CLI / REPL binary (uses lua-vm)
└── tests/               # integration tests (runs real .lua scripts)
```

Each crate MUST:
- Have its own `README.md` describing its purpose.
- Define only the types it owns.
- Export a clean public API (all impl details are `pub(crate)` or private).

---

## Workflow Rules

1. **Implement one Lua feature at a time** (e.g., arithmetic, strings, tables, closures…).
2. Before implementing a feature:
   - Read the Lua 5.4 reference manual section for that feature.
   - Write failing tests that capture the expected Lua behavior.
3. After implementing:
   - Run `cargo test --workspace`.
   - Run `cargo clippy --workspace -- -D warnings`.
   - Run `cargo fmt --check`.
4. Commit message format: `feat(crate-name): short description`.
5. Breaking changes across crates require updating all dependents in the same commit.

---

## Error Handling Policy

- All errors use the `lua-core` crate's `LuaError` type.
- Runtime errors → `LuaError::Runtime(String)`.
- Parse errors → `LuaError::Syntax { line, message }`.
- Type errors → `LuaError::TypeError { expected, got }`.
- Use `thiserror` for deriving error traits.

---

## Forbidden Patterns

| ❌ Avoid | ✅ Prefer |
|---|---|
| `unwrap()` / `expect()` in library code | `?` operator with `LuaError` |
| Concrete cross-layer imports | Trait objects / generics |
| Magic numbers | Named constants |
| Commented-out code | Delete or use feature flags |
| `println!` for debug output | `tracing` or `eprintln!` with `#[cfg(debug_assertions)]` |
| Monolithic files > 300 lines | Split into focused modules |

---

## Reference

- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Crafting Interpreters](https://craftinginterpreters.com/) — implementation inspiration
- Rust edition: **2021**
- MSRV: **1.75.0**
