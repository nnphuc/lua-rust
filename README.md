# lua-rust

A Lua 5.4 runtime implemented in Rust — lexer, parser, bytecode compiler, and register-based VM, all from scratch.

## Quick start

```sh
# Interactive REPL
cargo run -p lua-repl

# Run a script
cargo run -p lua-repl -- script.lua
```

No system Lua installation required.

## What works

| Feature | Status |
|---|---|
| All primitive types (nil, bool, integer, float, string) | ✅ |
| Arithmetic (`+` `-` `*` `/` `//` `%` `^`), unary `-` | ✅ |
| String concatenation `..`, length `#` | ✅ |
| Comparison (`==` `~=` `<` `<=` `>` `>=`) | ✅ |
| Logical (`and`, `or`, `not`) with short-circuit | ✅ |
| `local` variables, multiple assignment | ✅ |
| Global variables | ✅ |
| `if / elseif / else / end` | ✅ |
| `while` loops | ✅ |
| Numeric `for` loops | ✅ |
| `do … end` blocks | ✅ |
| First-class functions (`function`, anonymous `function() end`) | ✅ |
| Closures with upvalue capture and mutation | ✅ |
| Recursion | ✅ |
| Higher-order functions | ✅ |
| `print()` built-in | ✅ |

## Example

```lua
-- closures
local function make_adder(n)
  return function(x) return x + n end
end
local add5 = make_adder(5)
print(add5(10))   --> 15

-- recursion
local function fib(n)
  if n <= 1 then return n end
  return fib(n-1) + fib(n-2)
end
print(fib(10))    --> 55

-- counter with mutable upvalue
local function counter()
  local n = 0
  return function()
    n = n + 1
    return n
  end
end
local c = counter()
print(c(), c(), c())   --> 1  2  3
```

## Architecture

```
lua-rust/
├── crates/
│   ├── lua-core/       -- LuaValue, LuaError, OpCode, Proto, LuaClosure, Upvalue
│   ├── lua-lexer/      -- hand-written lexer → Token stream
│   ├── lua-parser/     -- recursive-descent parser → AST
│   ├── lua-compiler/   -- AST → register-based bytecode (Chunk / Proto)
│   ├── lua-vm/         -- call-frame-stack VM with upvalue support
│   └── lua-repl/       -- `lua` binary: interactive REPL + script runner
```

### VM design notes

- **Register-based** (like Lua 5.x, unlike stack VMs)
- **Call-frame stack** — all frames share one flat `Vec<LuaValue>` register file, each frame has a `base` offset
- **Upvalue cells** — `Arc<RwLock<Open(abs_reg) | Closed(LuaValue)>>` — open while the enclosing frame is alive, migrated to heap on `CloseUpvalues`
- **Closures** — `Proto` (immutable, `Arc`-shared) + per-instance `Vec<Upvalue>`

## Running tests

```sh
cargo test --workspace
# 120 tests, 0 failures
```

## Linting

```sh
cargo clippy --workspace -- -D warnings
cargo fmt --check
```

## Not yet implemented

- Tables and metatables
- String library (`string.format`, `string.sub`, …)
- `math` / `io` / `os` libraries
- Varargs (`...`)
- Multiple return values
- `repeat … until`, generic `for`, `goto` / labels
- `coroutine`
