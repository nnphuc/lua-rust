//! Lua 5.4 Abstract Syntax Tree types.
//!
//! A source file compiles to a top-level [`Block`].

// ── Shared span info ─────────────────────────────────────────────────────────

/// 1-based source line number attached to nodes that need it for error messages.
pub type Line = u32;

// ── Blocks & Statements ──────────────────────────────────────────────────────

/// A sequence of statements, optionally terminated by an explicit `return`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Option<ReturnStmt>,
    pub line: Line,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub values: Vec<Expr>,
    pub line: Line,
}

/// All statement forms in Lua 5.4.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// `varlist = explist`
    Assign {
        targets: Vec<Expr>,
        values: Vec<Expr>,
        line: Line,
    },

    /// `local namelist [= explist]`
    Local {
        names: Vec<LocalName>,
        values: Vec<Expr>,
        line: Line,
    },

    /// A bare function-call statement: `f(args)` or `o:method(args)`
    FnCall(Expr),

    /// `do block end`
    Do(Block),

    /// `while exp do block end`
    While { cond: Expr, body: Block, line: Line },

    /// `repeat block until exp`
    Repeat { body: Block, cond: Expr, line: Line },

    /// `if exp then block {elseif exp then block} [else block] end`
    If {
        cond: Expr,
        then: Block,
        elseifs: Vec<(Expr, Block)>,
        else_: Option<Block>,
        line: Line,
    },

    /// Numeric for: `for Name = exp, exp [, exp] do block end`
    NumericFor {
        var: String,
        start: Expr,
        limit: Expr,
        step: Option<Expr>,
        body: Block,
        line: Line,
    },

    /// Generic for: `for namelist in explist do block end`
    GenericFor {
        vars: Vec<String>,
        iterators: Vec<Expr>,
        body: Block,
        line: Line,
    },

    /// `function funcname funcbody`  (top-level named function)
    FnDef {
        name: FuncName,
        body: FuncBody,
        line: Line,
    },

    /// `local function Name funcbody`
    LocalFn {
        name: String,
        body: FuncBody,
        line: Line,
    },

    /// `goto Name`
    Goto { label: String, line: Line },

    /// `::Name::`
    Label { name: String, line: Line },

    /// `break`
    Break(Line),
}

/// A local variable declaration item with an optional attribute (`<close>`, `<const>`).
#[derive(Debug, Clone, PartialEq)]
pub struct LocalName {
    pub name: String,
    pub attrib: Option<String>, // "close" | "const"
}

/// dotted function name, e.g. `a.b.c:method`
#[derive(Debug, Clone, PartialEq)]
pub struct FuncName {
    pub parts: Vec<String>,
    pub method: Option<String>,
}

/// Function parameters + body block.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncBody {
    pub params: Vec<String>,
    pub vararg: bool,
    pub body: Block,
    pub line: Line,
}

// ── Expressions ──────────────────────────────────────────────────────────────

/// All expression forms in Lua 5.4.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Nil(Line),
    True(Line),
    False(Line),
    Integer(i64, Line),
    Float(f64, Line),
    LuaString(String, Line),
    Vararg(Line),

    /// Variable / name reference
    Name(String, Line),

    /// `lhs op rhs`
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        line: Line,
    },

    /// `op operand`
    UnOp {
        op: UnOp,
        operand: Box<Expr>,
        line: Line,
    },

    /// `table[key]`
    Index {
        table: Box<Expr>,
        key: Box<Expr>,
        line: Line,
    },

    /// `table.field`
    Field {
        table: Box<Expr>,
        field: String,
        line: Line,
    },

    /// `func(args)` or `func{table}` or `func"string"`
    FnCall {
        func: Box<Expr>,
        args: CallArgs,
        line: Line,
    },

    /// `obj:method(args)` — method call
    MethodCall {
        obj: Box<Expr>,
        method: String,
        args: CallArgs,
        line: Line,
    },

    /// `function(params) body end`  — anonymous function
    FnDef(FuncBody),

    /// `{ field, field, ... }` — table constructor
    Table(Vec<Field>, Line),
}

impl Expr {
    pub fn line(&self) -> Line {
        match self {
            Expr::Nil(l) | Expr::True(l) | Expr::False(l) | Expr::Vararg(l) => *l,
            Expr::Integer(_, l) | Expr::Float(_, l) | Expr::LuaString(_, l) => *l,
            Expr::Name(_, l) => *l,
            Expr::BinOp { line, .. } | Expr::UnOp { line, .. } => *line,
            Expr::Index { line, .. } | Expr::Field { line, .. } => *line,
            Expr::FnCall { line, .. } | Expr::MethodCall { line, .. } => *line,
            Expr::FnDef(fb) => fb.line,
            Expr::Table(_, l) => *l,
        }
    }
}

/// Arguments passed to a function call.
#[derive(Debug, Clone, PartialEq)]
pub enum CallArgs {
    /// `(explist)`
    Exprs(Vec<Expr>),
    /// `{table}`
    Table(Vec<Field>),
    /// `"string"` — single string arg, no parens
    String(String),
}

/// A single field in a table constructor.
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    /// `[expr] = expr`
    Index(Expr, Expr),
    /// `Name = expr`
    Named(String, Expr),
    /// `expr`  (positional)
    Positional(Expr),
}

// ── Operators ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    // String
    Concat,
    // Comparison
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Logical
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    Neg,    // -
    Not,    // not
    Len,    // #
    BitNot, // ~
}
