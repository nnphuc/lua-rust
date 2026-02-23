use crate::ast::*;
use lua_core::LuaError;
use lua_lexer::{Lexer, Token, TokenKind};

/// Recursive-descent parser for Lua 5.4.
///
/// Usage:
/// ```ignore
/// let block = Parser::new("local x = 1 + 2").parse()?;
/// ```
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

// ── Constructor ───────────────────────────────────────────────────────────────

impl Parser {
    /// Lex `src` and create a parser over the token stream.
    pub fn new(src: &str) -> Result<Self, LuaError> {
        let tokens = Lexer::new(src).tokenize()?;
        Ok(Self { tokens, pos: 0 })
    }

    /// Parse the entire source as a [`Block`] and return it.
    pub fn parse(mut self) -> Result<Block, LuaError> {
        let block = self.parse_block()?;
        self.expect(TokenKind::Eof)?;
        Ok(block)
    }
}

// ── Token navigation ──────────────────────────────────────────────────────────

impl Parser {
    fn peek(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn peek_line(&self) -> Line {
        self.tokens[self.pos].line
    }

    /// Advance past the current token and return it.
    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if self.pos + 1 < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    /// Advance if the current token matches `kind`.
    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.peek() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advance and return `Ok(token)`, or error if the current token does not
    /// match `expected`.
    fn expect(&mut self, expected: TokenKind) -> Result<&Token, LuaError> {
        let line = self.peek_line();
        if *self.peek() == expected {
            Ok(self.advance())
        } else {
            Err(LuaError::Syntax {
                line,
                message: format!("expected {:?}, got {:?}", expected, self.peek()),
            })
        }
    }

    /// Expect a `Name` token and return the identifier string.
    fn expect_name(&mut self) -> Result<String, LuaError> {
        let line = self.peek_line();
        match self.peek().clone() {
            TokenKind::Name(s) => {
                self.advance();
                Ok(s)
            }
            other => Err(LuaError::Syntax {
                line,
                message: format!("expected Name, got {:?}", other),
            }),
        }
    }

    #[allow(dead_code)]
    fn is_at_eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }
}

// ── Block & Statements ────────────────────────────────────────────────────────

impl Parser {
    /// `block ::= {stat} [retstat]`
    fn parse_block(&mut self) -> Result<Block, LuaError> {
        let line = self.peek_line();
        let mut stmts = Vec::new();
        loop {
            // Skip optional semicolons between statements
            while self.eat(&TokenKind::Semicolon) {}

            let ret = self.try_parse_return()?;
            if ret.is_some() {
                // Optional trailing semicolon after return
                self.eat(&TokenKind::Semicolon);
                return Ok(Block { stmts, ret, line });
            }

            if self.is_block_end() {
                break;
            }

            match self.parse_stmt()? {
                Some(s) => stmts.push(s),
                None => break,
            }
        }
        Ok(Block {
            stmts,
            ret: None,
            line,
        })
    }

    /// Returns true when the current token signals the end of a block.
    fn is_block_end(&self) -> bool {
        matches!(
            self.peek(),
            TokenKind::Eof
                | TokenKind::End
                | TokenKind::Else
                | TokenKind::Elseif
                | TokenKind::Until
        )
    }

    fn try_parse_return(&mut self) -> Result<Option<ReturnStmt>, LuaError> {
        if !matches!(self.peek(), TokenKind::Return) {
            return Ok(None);
        }
        let line = self.peek_line();
        self.advance(); // return

        let values = if self.is_block_end()
            || matches!(self.peek(), TokenKind::Semicolon | TokenKind::Eof)
        {
            vec![]
        } else {
            self.parse_expr_list()?
        };
        Ok(Some(ReturnStmt { values, line }))
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, LuaError> {
        let line = self.peek_line();
        let stmt = match self.peek().clone() {
            TokenKind::If => self.parse_if()?,
            TokenKind::While => self.parse_while()?,
            TokenKind::Do => {
                self.advance();
                let b = self.parse_block()?;
                self.expect(TokenKind::End)?;
                Stmt::Do(b)
            }
            TokenKind::For => self.parse_for()?,
            TokenKind::Repeat => self.parse_repeat()?,
            TokenKind::Function => self.parse_fn_def()?,
            TokenKind::Local => self.parse_local()?,
            TokenKind::Goto => {
                self.advance();
                let lbl = self.expect_name()?;
                Stmt::Goto { label: lbl, line }
            }
            TokenKind::Break => {
                self.advance();
                Stmt::Break(line)
            }
            TokenKind::ColonColon => self.parse_label()?,
            TokenKind::Return
            | TokenKind::End
            | TokenKind::Else
            | TokenKind::Elseif
            | TokenKind::Until
            | TokenKind::Eof => {
                return Ok(None);
            }
            _ => self.parse_expr_stat()?,
        };
        Ok(Some(stmt))
    }

    // ── Control flow ─────────────────────────────────────────────────────────

    fn parse_if(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::If)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::Then)?;
        let then = self.parse_block()?;

        let mut elseifs = Vec::new();
        let mut else_ = None;
        loop {
            if self.eat(&TokenKind::Elseif) {
                let ec = self.parse_expr()?;
                self.expect(TokenKind::Then)?;
                let eb = self.parse_block()?;
                elseifs.push((ec, eb));
            } else if self.eat(&TokenKind::Else) {
                else_ = Some(self.parse_block()?);
                break;
            } else {
                break;
            }
        }
        self.expect(TokenKind::End)?;
        Ok(Stmt::If {
            cond,
            then,
            elseifs,
            else_,
            line,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::While)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::Do)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::End)?;
        Ok(Stmt::While { cond, body, line })
    }

    fn parse_repeat(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::Repeat)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::Until)?;
        let cond = self.parse_expr()?;
        Ok(Stmt::Repeat { body, cond, line })
    }

    fn parse_for(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::For)?;
        let first_name = self.expect_name()?;

        if self.eat(&TokenKind::Assign) {
            // Numeric for
            let start = self.parse_expr()?;
            self.expect(TokenKind::Comma)?;
            let limit = self.parse_expr()?;
            let step = if self.eat(&TokenKind::Comma) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(TokenKind::Do)?;
            let body = self.parse_block()?;
            self.expect(TokenKind::End)?;
            Ok(Stmt::NumericFor {
                var: first_name,
                start,
                limit,
                step,
                body,
                line,
            })
        } else {
            // Generic for
            let mut vars = vec![first_name];
            while self.eat(&TokenKind::Comma) {
                vars.push(self.expect_name()?);
            }
            self.expect(TokenKind::In)?;
            let iterators = self.parse_expr_list()?;
            self.expect(TokenKind::Do)?;
            let body = self.parse_block()?;
            self.expect(TokenKind::End)?;
            Ok(Stmt::GenericFor {
                vars,
                iterators,
                body,
                line,
            })
        }
    }

    fn parse_fn_def(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::Function)?;
        let name = self.parse_func_name()?;
        let is_method = name.method.is_some();
        let body = self.parse_func_body(is_method, line)?;
        Ok(Stmt::FnDef { name, body, line })
    }

    fn parse_local(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::Local)?;
        if self.eat(&TokenKind::Function) {
            let name = self.expect_name()?;
            let body = self.parse_func_body(false, line)?;
            return Ok(Stmt::LocalFn { name, body, line });
        }
        // Local variable list
        let mut names = Vec::new();
        loop {
            let n = self.expect_name()?;
            let attrib = if self.eat(&TokenKind::Lt) {
                let a = self.expect_name()?;
                self.expect(TokenKind::Gt)?;
                Some(a)
            } else {
                None
            };
            names.push(LocalName { name: n, attrib });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        let values = if self.eat(&TokenKind::Assign) {
            self.parse_expr_list()?
        } else {
            vec![]
        };
        Ok(Stmt::Local {
            names,
            values,
            line,
        })
    }

    /// `::label::`
    fn parse_label(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        self.expect(TokenKind::ColonColon)?;
        let name = self.expect_name()?;
        self.expect(TokenKind::ColonColon)?;
        Ok(Stmt::Label { name, line })
    }

    /// Assignment or bare function-call statement.
    fn parse_expr_stat(&mut self) -> Result<Stmt, LuaError> {
        let line = self.peek_line();
        let expr = self.parse_suffixed_expr()?;

        // If next token is `=` or `,` this is an assignment
        if matches!(self.peek(), TokenKind::Assign | TokenKind::Comma) {
            let mut targets = vec![expr];
            while self.eat(&TokenKind::Comma) {
                targets.push(self.parse_suffixed_expr()?);
            }
            self.expect(TokenKind::Assign)?;
            let values = self.parse_expr_list()?;
            return Ok(Stmt::Assign {
                targets,
                values,
                line,
            });
        }

        // Otherwise it must be a function call
        match &expr {
            Expr::FnCall { .. } | Expr::MethodCall { .. } => Ok(Stmt::FnCall(expr)),
            _ => Err(LuaError::Syntax {
                line,
                message: "syntax error: expected assignment or function call".into(),
            }),
        }
    }

    // ── Functions ─────────────────────────────────────────────────────────────

    /// `funcname ::= Name {'.' Name} [':' Name]`
    fn parse_func_name(&mut self) -> Result<FuncName, LuaError> {
        let mut parts = vec![self.expect_name()?];
        while self.eat(&TokenKind::Dot) {
            parts.push(self.expect_name()?);
        }
        let method = if self.eat(&TokenKind::Colon) {
            Some(self.expect_name()?)
        } else {
            None
        };
        Ok(FuncName { parts, method })
    }

    /// `funcbody ::= '(' [parlist] ')' block end`
    fn parse_func_body(&mut self, implicit_self: bool, line: Line) -> Result<FuncBody, LuaError> {
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        let mut vararg = false;

        if implicit_self {
            params.push("self".to_string());
        }

        if !matches!(self.peek(), TokenKind::RParen) {
            if self.eat(&TokenKind::DotDotDot) {
                vararg = true;
            } else {
                params.push(self.expect_name()?);
                while self.eat(&TokenKind::Comma) {
                    if self.eat(&TokenKind::DotDotDot) {
                        vararg = true;
                        break;
                    }
                    params.push(self.expect_name()?);
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::End)?;
        Ok(FuncBody {
            params,
            vararg,
            body,
            line,
        })
    }

    // ── Expression list ───────────────────────────────────────────────────────

    fn parse_expr_list(&mut self) -> Result<Vec<Expr>, LuaError> {
        let mut list = vec![self.parse_expr()?];
        while self.eat(&TokenKind::Comma) {
            list.push(self.parse_expr()?);
        }
        Ok(list)
    }

    // ── Expressions (Pratt / precedence climbing) ─────────────────────────────

    pub fn parse_expr(&mut self) -> Result<Expr, LuaError> {
        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Result<Expr, LuaError> {
        let line = self.peek_line();
        let mut lhs = self.parse_unary_expr()?;

        while let Some((left_prec, right_prec, op)) = self.peek_binop() {
            if left_prec < min_prec {
                break;
            }
            self.advance(); // consume the operator

            let rhs = self.parse_expr_prec(right_prec)?;
            lhs = Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                line,
            };
        }
        Ok(lhs)
    }

    /// Returns `(left_prec, right_prec, BinOp)` for the current token if it is
    /// a binary operator, or `None` otherwise.
    fn peek_binop(&self) -> Option<(u8, u8, BinOp)> {
        // (left_priority, right_priority) following Lua 5.4 manual §3.4.8
        // right-associative ops have right_priority < left_priority
        let (l, r, op) = match self.peek() {
            TokenKind::Or => (1, 1, BinOp::Or),
            TokenKind::And => (2, 2, BinOp::And),
            TokenKind::Lt => (3, 3, BinOp::Lt),
            TokenKind::Gt => (3, 3, BinOp::Gt),
            TokenKind::LtEq => (3, 3, BinOp::LtEq),
            TokenKind::GtEq => (3, 3, BinOp::GtEq),
            TokenKind::Eq => (3, 3, BinOp::Eq),
            TokenKind::NotEq => (3, 3, BinOp::NotEq),
            TokenKind::Pipe => (4, 4, BinOp::BitOr),
            TokenKind::Tilde => (5, 5, BinOp::BitXor),
            TokenKind::Ampersand => (6, 6, BinOp::BitAnd),
            TokenKind::ShiftLeft => (7, 7, BinOp::Shl),
            TokenKind::ShiftRight => (7, 7, BinOp::Shr),
            TokenKind::DotDot => (8, 7, BinOp::Concat), // right-assoc
            TokenKind::Plus => (9, 9, BinOp::Add),
            TokenKind::Minus => (9, 9, BinOp::Sub),
            TokenKind::Star => (10, 10, BinOp::Mul),
            TokenKind::Slash => (10, 10, BinOp::Div),
            TokenKind::SlashSlash => (10, 10, BinOp::IDiv),
            TokenKind::Percent => (10, 10, BinOp::Mod),
            TokenKind::Caret => (12, 11, BinOp::Pow), // right-assoc
            _ => return None,
        };
        Some((l, r, op))
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, LuaError> {
        let line = self.peek_line();
        let op = match self.peek() {
            TokenKind::Not => Some(UnOp::Not),
            TokenKind::Minus => Some(UnOp::Neg),
            TokenKind::Hash => Some(UnOp::Len),
            TokenKind::Tilde => Some(UnOp::BitNot),
            _ => None,
        };
        if let Some(op) = op {
            self.advance();
            // unary has priority 11 (higher than all binary ops except ^)
            let operand = self.parse_expr_prec(11)?;
            return Ok(Expr::UnOp {
                op,
                operand: Box::new(operand),
                line,
            });
        }
        self.parse_simple_expr()
    }

    fn parse_simple_expr(&mut self) -> Result<Expr, LuaError> {
        let line = self.peek_line();
        match self.peek().clone() {
            TokenKind::Integer(n) => {
                self.advance();
                Ok(Expr::Integer(n, line))
            }
            TokenKind::Float(f) => {
                self.advance();
                Ok(Expr::Float(f, line))
            }
            TokenKind::LuaString(s) => {
                self.advance();
                Ok(Expr::LuaString(s, line))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expr::Nil(line))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::True(line))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::False(line))
            }
            TokenKind::DotDotDot => {
                self.advance();
                Ok(Expr::Vararg(line))
            }
            TokenKind::Function => {
                self.advance();
                let body = self.parse_func_body(false, line)?;
                Ok(Expr::FnDef(body))
            }
            TokenKind::LBrace => self.parse_table_constructor(),
            _ => self.parse_suffixed_expr(),
        }
    }

    /// Primary + chain of `.field`, `[key]`, `(args)`, `:method(args)` suffixes.
    fn parse_suffixed_expr(&mut self) -> Result<Expr, LuaError> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            let line = self.peek_line();
            match self.peek().clone() {
                TokenKind::Dot => {
                    self.advance();
                    let field = self.expect_name()?;
                    expr = Expr::Field {
                        table: Box::new(expr),
                        field,
                        line,
                    };
                }
                TokenKind::LBracket => {
                    self.advance();
                    let key = self.parse_expr()?;
                    self.expect(TokenKind::RBracket)?;
                    expr = Expr::Index {
                        table: Box::new(expr),
                        key: Box::new(key),
                        line,
                    };
                }
                TokenKind::Colon => {
                    self.advance();
                    let method = self.expect_name()?;
                    let args = self.parse_call_args()?;
                    expr = Expr::MethodCall {
                        obj: Box::new(expr),
                        method,
                        args,
                        line,
                    };
                }
                TokenKind::LParen | TokenKind::LBrace | TokenKind::LuaString(_) => {
                    let args = self.parse_call_args()?;
                    expr = Expr::FnCall {
                        func: Box::new(expr),
                        args,
                        line,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, LuaError> {
        let line = self.peek_line();
        match self.peek().clone() {
            TokenKind::Name(n) => {
                self.advance();
                Ok(Expr::Name(n, line))
            }
            TokenKind::LParen => {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(e)
            }
            other => Err(LuaError::Syntax {
                line,
                message: format!("unexpected symbol {:?}", other),
            }),
        }
    }

    // ── Call arguments ────────────────────────────────────────────────────────

    fn parse_call_args(&mut self) -> Result<CallArgs, LuaError> {
        match self.peek().clone() {
            TokenKind::LParen => {
                self.advance();
                if self.eat(&TokenKind::RParen) {
                    Ok(CallArgs::Exprs(vec![]))
                } else {
                    let exprs = self.parse_expr_list()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(CallArgs::Exprs(exprs))
                }
            }
            TokenKind::LBrace => {
                let fields = self.parse_table_fields()?;
                Ok(CallArgs::Table(fields))
            }
            TokenKind::LuaString(s) => {
                self.advance();
                Ok(CallArgs::String(s))
            }
            other => Err(LuaError::Syntax {
                line: self.peek_line(),
                message: format!("expected function arguments, got {:?}", other),
            }),
        }
    }

    // ── Table constructors ────────────────────────────────────────────────────

    fn parse_table_constructor(&mut self) -> Result<Expr, LuaError> {
        let line = self.peek_line();
        let fields = self.parse_table_fields()?;
        Ok(Expr::Table(fields, line))
    }

    fn parse_table_fields(&mut self) -> Result<Vec<Field>, LuaError> {
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace) {
            fields.push(self.parse_field()?);
            if !self.eat(&TokenKind::Comma) && !self.eat(&TokenKind::Semicolon) {
                break;
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(fields)
    }

    fn parse_field(&mut self) -> Result<Field, LuaError> {
        match self.peek().clone() {
            TokenKind::LBracket => {
                // [expr] = expr
                self.advance();
                let key = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                self.expect(TokenKind::Assign)?;
                let val = self.parse_expr()?;
                Ok(Field::Index(key, val))
            }
            TokenKind::Name(n)
                if {
                    // Peek ahead: if next is `=` this is `Name = expr`
                    let next = self.tokens.get(self.pos + 1).map(|t| &t.kind);
                    matches!(next, Some(TokenKind::Assign))
                } =>
            {
                self.advance(); // Name
                self.advance(); // =
                let val = self.parse_expr()?;
                Ok(Field::Named(n, val))
            }
            _ => {
                let val = self.parse_expr()?;
                Ok(Field::Positional(val))
            }
        }
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> Block {
        Parser::new(src).unwrap().parse().unwrap()
    }

    fn parse_expr(src: &str) -> Expr {
        // Wrap in `return` to get an expression out
        let b = parse(&format!("return {src}"));
        b.ret.unwrap().values.into_iter().next().unwrap()
    }

    fn parse_err(src: &str) -> LuaError {
        Parser::new(src).unwrap().parse().unwrap_err()
    }

    // ── Empty / trivial ──────────────────────────────────────────────────────
    #[test]
    fn empty_block() {
        let b = parse("");
        assert!(b.stmts.is_empty());
        assert!(b.ret.is_none());
    }

    #[test]
    fn bare_return() {
        let b = parse("return");
        assert!(b.ret.is_some());
        assert!(b.ret.unwrap().values.is_empty());
    }

    // ── Literals ─────────────────────────────────────────────────────────────
    #[test]
    fn integer_expr() {
        assert!(matches!(parse_expr("42"), Expr::Integer(42, _)));
    }

    #[test]
    fn float_expr() {
        assert!(matches!(parse_expr("3.14"), Expr::Float(_, _)));
    }

    #[test]
    fn string_expr() {
        assert!(matches!(parse_expr(r#""hello""#), Expr::LuaString(s, _) if s == "hello"));
    }

    #[test]
    fn nil_expr() {
        assert!(matches!(parse_expr("nil"), Expr::Nil(_)));
    }
    #[test]
    fn true_expr() {
        assert!(matches!(parse_expr("true"), Expr::True(_)));
    }
    #[test]
    fn false_expr() {
        assert!(matches!(parse_expr("false"), Expr::False(_)));
    }

    // ── Binary ops ───────────────────────────────────────────────────────────
    #[test]
    fn add_expr() {
        let e = parse_expr("1 + 2");
        assert!(matches!(e, Expr::BinOp { op: BinOp::Add, .. }));
    }

    #[test]
    fn precedence_mul_over_add() {
        // 1 + 2 * 3  should parse as  1 + (2 * 3)
        let e = parse_expr("1 + 2 * 3");
        match e {
            Expr::BinOp {
                op: BinOp::Add,
                lhs,
                rhs,
                ..
            } => {
                assert!(matches!(*lhs, Expr::Integer(1, _)));
                assert!(matches!(*rhs, Expr::BinOp { op: BinOp::Mul, .. }));
            }
            _ => panic!("expected Add at top level"),
        }
    }

    #[test]
    fn concat_is_right_associative() {
        // "a" .. "b" .. "c"  should parse as  "a" .. ("b" .. "c")
        let e = parse_expr(r#""a" .. "b" .. "c""#);
        match e {
            Expr::BinOp {
                op: BinOp::Concat,
                rhs,
                ..
            } => {
                assert!(matches!(
                    *rhs,
                    Expr::BinOp {
                        op: BinOp::Concat,
                        ..
                    }
                ));
            }
            _ => panic!("expected Concat at top level"),
        }
    }

    #[test]
    fn unary_neg() {
        let e = parse_expr("-5");
        assert!(matches!(e, Expr::UnOp { op: UnOp::Neg, .. }));
    }

    #[test]
    fn not_expr() {
        let e = parse_expr("not true");
        assert!(matches!(e, Expr::UnOp { op: UnOp::Not, .. }));
    }

    // ── Statements ───────────────────────────────────────────────────────────
    #[test]
    fn local_assign() {
        let b = parse("local x = 1");
        assert_eq!(b.stmts.len(), 1);
        assert!(matches!(&b.stmts[0], Stmt::Local { names, .. } if names[0].name == "x"));
    }

    #[test]
    fn global_assign() {
        let b = parse("x = 42");
        assert!(matches!(&b.stmts[0], Stmt::Assign { .. }));
    }

    #[test]
    fn while_stmt() {
        let b = parse("while true do end");
        assert!(matches!(&b.stmts[0], Stmt::While { .. }));
    }

    #[test]
    fn if_else_stmt() {
        let b = parse("if x then end");
        assert!(matches!(&b.stmts[0], Stmt::If { else_: None, .. }));
    }

    #[test]
    fn numeric_for() {
        let b = parse("for i = 1, 10 do end");
        assert!(matches!(&b.stmts[0], Stmt::NumericFor { var, .. } if var == "i"));
    }

    #[test]
    fn generic_for() {
        let b = parse("for k, v in pairs(t) do end");
        assert!(matches!(&b.stmts[0], Stmt::GenericFor { vars, .. } if vars.len() == 2));
    }

    #[test]
    fn fn_call_stmt() {
        let b = parse("print(1)");
        assert!(matches!(&b.stmts[0], Stmt::FnCall(Expr::FnCall { .. })));
    }

    #[test]
    fn function_def() {
        let b = parse("function foo() end");
        assert!(matches!(&b.stmts[0], Stmt::FnDef { .. }));
    }

    #[test]
    fn local_function() {
        let b = parse("local function bar() end");
        assert!(matches!(&b.stmts[0], Stmt::LocalFn { name, .. } if name == "bar"));
    }

    #[test]
    fn break_stmt() {
        let b = parse("while true do break end");
        assert!(
            matches!(&b.stmts[0], Stmt::While { body, .. } if matches!(&body.stmts[0], Stmt::Break(_)))
        );
    }

    #[test]
    fn label_and_goto() {
        let b = parse("::start:: goto start");
        assert!(matches!(&b.stmts[0], Stmt::Label { name, .. } if name == "start"));
        assert!(matches!(&b.stmts[1], Stmt::Goto { label, .. } if label == "start"));
    }

    // ── Tables ───────────────────────────────────────────────────────────────
    #[test]
    fn table_constructor_positional() {
        let e = parse_expr("{1, 2, 3}");
        assert!(matches!(e, Expr::Table(fields, _) if fields.len() == 3));
    }

    #[test]
    fn table_constructor_named() {
        let e = parse_expr("{x = 1}");
        assert!(matches!(e,
            Expr::Table(fields, _)
            if matches!(&fields[0], Field::Named(n, _) if n == "x")
        ));
    }

    // ── Error cases ──────────────────────────────────────────────────────────
    #[test]
    fn missing_then_is_error() {
        assert!(matches!(parse_err("if true end"), LuaError::Syntax { .. }));
    }

    #[test]
    fn missing_end_is_error() {
        assert!(matches!(
            parse_err("while true do"),
            LuaError::Syntax { .. }
        ));
    }
}
