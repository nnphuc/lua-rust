---
description: how to implement a new Lua language feature in Rust
---

# Implement a Lua Feature — TDD Workflow

Follow these steps exactly for every new Lua language feature (e.g., arithmetic ops, string library, tables, closures).

## 1. Research
- [ ] Read the relevant section of the [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/).
- [ ] Identify which crate(s) are affected (`lua-core`, `lua-lexer`, `lua-parser`, `lua-compiler`, `lua-vm`).
- [ ] Note any edge cases (e.g., integer overflow, metatable fallback, coercion rules).

## 2. Write Failing Tests First (TDD)
- [ ] Create or open the test file for the affected crate.
  - Unit tests: inline `#[cfg(test)]` module inside the source file.
  - Integration tests: `tests/<feature_name>.rs` at the **workspace root** level for end-to-end `.lua` script tests.
- [ ] Write tests that cover:
  - Happy path (basic usage)
  - Edge cases and error conditions
  - Lua spec-compliant behavior
- [ ] Confirm tests **fail** before writing implementation:
// turbo
```
cargo test --workspace 2>&1 | tail -20
```

## 3. Implement
- [ ] Implement the feature in the appropriate crate layer (see RULES.md for layer diagram).
- [ ] Follow the Error Handling Policy in RULES.md (use `LuaError`, never `unwrap()`).
- [ ] Keep functions under 40 lines; split into private helpers if needed.
- [ ] Add `///` doc comments to every public item.

## 4. Run Tests
// turbo
```
cargo test --workspace
```
- All tests must pass. Fix any failures before proceeding.

## 5. Lint & Format
// turbo
```
cargo clippy --workspace -- -D warnings
```
// turbo
```
cargo fmt --check
```
- Fix all warnings and formatting issues.

## 6. Commit
```
git add -A
git commit -m "feat(<crate-name>): <short description of feature>"
```
Replace `<crate-name>` with the primary crate changed (e.g., `lua-vm`) and provide a concise description.
