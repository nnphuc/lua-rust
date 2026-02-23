use std::io::{self, BufRead, Write};

use lua_compiler::Compiler;
use lua_core::LuaValue;
use lua_parser::Parser;
use lua_vm::Vm;

fn eval(src: &str, vm: &mut Vm) -> Result<LuaValue, String> {
    let parser = Parser::new(src).map_err(|e| format!("lexer error: {e}"))?;
    let block = parser.parse().map_err(|e| format!("parse error: {e}"))?;
    let chunk = Compiler::new("<stdin>")
        .compile(&block)
        .map_err(|e| format!("compile error: {e}"))?;
    vm.execute(&chunk).map_err(|e| format!("runtime error: {e}"))
}

fn run_file(path: &str) {
    let src = std::fs::read_to_string(path)
        .unwrap_or_else(|e| { eprintln!("error: cannot read '{path}': {e}"); std::process::exit(1); });
    let mut vm = Vm::new();
    if let Err(e) = eval(&src, &mut vm) {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn repl() {
    println!("lua-rust 0.1.0 -- type `.exit` or Ctrl-D to quit");
    let stdin = io::stdin();
    let mut vm = Vm::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => { println!(); break; } // EOF (Ctrl-D)
            Ok(_) => {}
            Err(e) => { eprintln!("read error: {e}"); break; }
        }

        let trimmed = line.trim();
        if trimmed == ".exit" || trimmed == "exit" { break; }
        if trimmed.is_empty() { continue; }

        // Wrap bare expressions in `return` so the value is surfaced.
        // Try with `return` first; fall back to treating it as a statement.
        let src_return = format!("return {trimmed}");
        let src_use = if Parser::new(&src_return)
            .and_then(|p| p.parse())
            .is_ok()
        {
            src_return.as_str()
        } else {
            trimmed
        };

        match eval(src_use, &mut vm) {
            Ok(LuaValue::Nil) => {}                       // don't print nil for void calls
            Ok(v)             => println!("{v}"),
            Err(e)            => eprintln!("{e}"),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1    => repl(),
        2    => run_file(&args[1]),
        _    => {
            eprintln!("usage: lua [script.lua]");
            std::process::exit(1);
        }
    }
}
