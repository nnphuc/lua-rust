use std::io::{self, BufRead, Write};

use lua_compiler::Compiler;
use lua_core::LuaValue;
use lua_parser::Parser;
use lua_vm::Vm;

// ── Helpers ───────────────────────────────────────────────────────────────────

fn is_incomplete(err: &str) -> bool {
    err.contains("Eof") || err.contains("unexpected end")
}

fn try_parse(src: &str) -> Result<lua_parser::ast::Block, String> {
    Parser::new(src)
        .map_err(|e| format!("{e}"))?
        .parse()
        .map_err(|e| format!("{e}"))
}

fn exec(src: &str, vm: &mut Vm) -> Result<LuaValue, String> {
    let block = try_parse(src)?;
    let chunk = Compiler::new("<stdin>")
        .compile(&block)
        .map_err(|e| format!("compile error: {e}"))?;
    vm.execute(&chunk).map_err(|e| format!("runtime error: {e}"))
}

// ── Script runner ─────────────────────────────────────────────────────────────

fn run_file(path: &str) {
    let src = std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("error: cannot read '{path}': {e}");
        std::process::exit(1);
    });
    let mut vm = Vm::new();
    if let Err(e) = exec(&src, &mut vm) {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

// ── Interactive REPL ──────────────────────────────────────────────────────────

fn repl() {
    println!("lua-rust 0.1.0  (type 'exit' or Ctrl-D to quit)");
    let stdin = io::stdin();
    let mut vm = Vm::new();
    let mut buf = String::new();

    loop {
        let prompt = if buf.is_empty() { "> " } else { ">> " };
        print!("{prompt}");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                if !buf.is_empty() {
                    eprintln!("<incomplete input>");
                }
                println!();
                break;
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("read error: {e}");
                break;
            }
        }

        let trimmed = line.trim_end_matches('\n');
        if buf.is_empty() && (trimmed == "exit" || trimmed == ".exit") {
            break;
        }

        if !buf.is_empty() {
            buf.push('\n');
        }
        buf.push_str(trimmed);

        // --- Attempt to evaluate the accumulated buffer ---

        // Strategy:
        // 1. Try `return <buf>` — works for bare expressions (prints result).
        // 2. Try `<buf>` as statements — works for assignments, function defs, etc.
        //    For `local function f …`, hoist it to a global: replace leading `local`
        //    when used inside REPL so the binding persists across lines.
        // 3. If both fail with "incomplete", show `>>` and keep accumulating.
        // 4. Real parse error → report and reset.

        // Hoist `local function` → `function` so the name survives as a global
        let global_buf = hoist_locals(&buf);

        let src_ret = format!("return {buf}");

        // Parse attempts (in priority order)
        let result: Option<(&str, bool)> = if try_parse(&src_ret).is_ok() {
            Some((&src_ret, true))   // bare expression
        } else if try_parse(&global_buf).is_ok() {
            Some((&global_buf, false)) // statement (hoisted)
        } else if try_parse(&buf).is_ok() {
            Some((&buf, false))        // statement (original)
        } else {
            None
        };

        match result {
            Some((src, is_expr)) => {
                let src_owned = src.to_owned();
                buf.clear();
                match exec(&src_owned, &mut vm) {
                    Ok(LuaValue::Nil) if is_expr => {} // void call — don't spam nil
                    Ok(LuaValue::Nil) => {}
                    Ok(v) => println!("{v}"),
                    Err(e) => eprintln!("{e}"),
                }
            }
            None => {
                // Check if it's incomplete vs. a real error
                let err = try_parse(&buf).unwrap_err();
                if is_incomplete(&err) {
                    continue; // wait for more input
                }
                eprintln!("parse error: {err}");
                buf.clear();
            }
        }
    }
}

/// In REPL mode, `local function f` → `function f` so `f` is a global and
/// survives across REPL lines. Only applies when the buffer starts with `local`.
fn hoist_locals(src: &str) -> String {
    // Replace `local function` at the start of each statement with `function`
    // Simple approach: strip a leading `local ` prefix.
    let s = src.trim_start();
    if s.starts_with("local function ") {
        s.trim_start_matches("local ").to_string()
    } else if s.starts_with("local ") {
        // `local x = expr` → `x = expr`  (makes it a global assignment)
        s.trim_start_matches("local ").to_string()
    } else {
        src.to_string()
    }
}

// ── Entry point ───────────────────────────────────────────────────────────────

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("usage: lua [script.lua]");
            std::process::exit(1);
        }
    }
}
