# lua-rust

A Lua 5.4 runtime implemented in Rust — lexer, parser, bytecode compiler, and register-based VM, all from scratch.

## Quick start

```sh
# Interactive REPL (with readline history, arrow keys, Ctrl-R search)
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
| `repeat … until` loops | ✅ |
| `break` inside any loop | ✅ |
| `goto` / labels (`::name::`, `goto name`) | ✅ |
| Numeric `for` loops | ✅ |
| Generic `for` loops (`for k, v in ipairs(t) do`) | ✅ |
| `do … end` blocks | ✅ |
| First-class functions (`function`, anonymous `function() end`) | ✅ |
| Closures with upvalue capture and mutation | ✅ |
| Recursion | ✅ |
| Higher-order functions | ✅ |
| Multiple return values (`return a, b, c`) | ✅ |
| Multiple assignment from function call (`local a, b = f()`) | ✅ |
| Varargs (`...`) — single-value expansion | ✅ |
| Tables (`{}`, `t.x`, `t[k]`, `#t`) | ✅ |
| Table constructors (positional, named, indexed fields) | ✅ |
| Method calls `obj:method(args)` | ✅ |
| `print`, `tostring`, `tonumber`, `type`, `assert`, `error` | ✅ |
| `ipairs`, `pairs`, `select`, `unpack`, `rawget`, `rawset` | ✅ |
| `io` library (basic: `write`, `flush`) | ✅ |
| `os` library (basic: `clock`, `time`, `date`) | ✅ |
| Metatables (basic `getmetatable` / `setmetatable`, table `__index` / `__newindex` / `__call` / `__len`) | ✅ |
| `coroutine` library (basic: `create`, `resume`, `yield`, `status`, `running`) | ✅ |
| `math` library (`floor`, `ceil`, `abs`, `sqrt`, `max`, `min`, `sin`, `cos`, `tan`, `log`, `exp`, `modf`, `fmod`, `pi`, `huge`, `type`, `tointeger`) | ✅ |
| `string` library (`sub`, `len`, `upper`, `lower`, `rep`, `reverse`, `byte`, `char`, `find`, `format`) | ✅ |
| REPL readline: history, arrow keys, Ctrl-R search | ✅ |

## Example

```lua
-- tables + ipairs
local fruits = {"apple", "banana", "cherry"}
for i, v in ipairs(fruits) do
  print(i, v)
end

-- multiple return values
local function minmax(t)
  local lo, hi = t[1], t[1]
  for _, v in ipairs(t) do
    if v < lo then lo = v end
    if v > hi then hi = v end
  end
  return lo, hi
end
local lo, hi = minmax({3, 1, 4, 1, 5, 9})
print(lo, hi)   --> 1  9

-- closures + upvalues
local function make_adder(n)
  return function(x) return x + n end
end
local add5 = make_adder(5)
print(add5(10))   --> 15

-- math & string stdlib
print(math.floor(3.7))          --> 3
print(string.format("pi=%.4f", math.pi))  --> pi=3.1416
```

## Architecture

```
lua-rust/
├── crates/
│   ├── lua-core/       -- LuaValue, LuaError, OpCode, LuaTable, Proto, LuaClosure, Upvalue
│   ├── lua-lexer/      -- hand-written lexer → Token stream
│   ├── lua-parser/     -- recursive-descent parser → AST
│   ├── lua-compiler/   -- AST → register-based bytecode (Chunk / Proto)
│   ├── lua-vm/         -- call-frame-stack VM with upvalue + table support
│   └── lua-repl/       -- `lua` binary: interactive REPL + script runner
```

### VM design notes

- **Register-based** (like Lua 5.x, unlike stack VMs)
- **Call-frame stack** — all frames share one flat `Vec<LuaValue>` register file, each frame has a `base` offset
- **Upvalue cells** — `Arc<RwLock<Open(abs_reg) | Closed(LuaValue)>>` — open while the enclosing frame is alive, migrated to heap on `CloseUpvalues`
- **Closures** — `Proto` (immutable, `Arc`-shared) + per-instance `Vec<Upvalue>`
- **Tables** — `Arc<RwLock<LuaTable>>` with compact array part (1-indexed) + hash part; reference equality

## Running tests

```sh
cargo test --workspace
# 159 tests, 0 failures
```

## Linting

```sh
cargo clippy --workspace -- -D warnings
cargo fmt --check
```

## Not yet implemented

- Additional metamethods: `__eq`, `__lt`, `__le`, arithmetic (`__add`/`__sub`/`__mul`/`__div`/`__mod`/`__pow`), `__concat`, `__tostring`
- Iteration/metatable hooks: `__pairs`, `__ipairs`
- Full Lua metamethod semantics for non-table values
