use crate::token::{Token, TokenKind};
use lua_core::LuaError;

/// Tokenises a Lua 5.4 source string into a flat list of [`Token`]s.
pub struct Lexer<'src> {
    src: &'src [u8],
    pos: usize,
    line: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src: src.as_bytes(),
            pos: 0,
            line: 1,
        }
    }

    /// Consume the entire source and return all tokens, including a final `Eof`.
    pub fn tokenize(mut self) -> Result<Vec<Token>, LuaError> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            let done = tok.kind == TokenKind::Eof;
            tokens.push(tok);
            if done {
                break;
            }
        }
        Ok(tokens)
    }

    // ── internal helpers ────────────────────────────────────────────────────

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let ch = self.src.get(self.pos).copied()?;
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
        }
        Some(ch)
    }

    fn eat_if(&mut self, expected: u8) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn current_str(&self, start: usize) -> &str {
        std::str::from_utf8(&self.src[start..self.pos]).unwrap_or("")
    }

    // ── whitespace & comments ───────────────────────────────────────────────

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LuaError> {
        loop {
            // Skip whitespace
            while matches!(self.peek(), Some(b' ' | b'\t' | b'\r' | b'\n')) {
                self.advance();
            }

            // Check for comment
            if self.peek() == Some(b'-') && self.peek2() == Some(b'-') {
                self.advance(); // -
                self.advance(); // -

                // Long comment?
                if self.peek() == Some(b'[') {
                    let level = self.count_long_bracket_level();
                    if level >= 0 {
                        self.read_long_string(level as usize, true)?;
                        continue;
                    }
                }

                // Short comment: skip to end of line
                while !matches!(self.peek(), Some(b'\n') | None) {
                    self.advance();
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Peek ahead from `self.pos` and count `=` signs in a long-bracket opening
    /// of the form `[===[`.  The cursor must be positioned at the FIRST `[`.
    /// Returns -1 if this is not a valid long bracket start.
    fn count_long_bracket_level(&self) -> i32 {
        self.count_long_bracket_level_at(self.pos)
    }

    /// Same as [`count_long_bracket_level`] but starting from an arbitrary byte
    /// offset into `self.src`.  The byte at `offset` should be the first `[`.
    fn count_long_bracket_level_at(&self, offset: usize) -> i32 {
        let mut i = offset;
        if self.src.get(i) != Some(&b'[') {
            return -1;
        }
        i += 1;
        let mut level = 0i32;
        while self.src.get(i) == Some(&b'=') {
            level += 1;
            i += 1;
        }
        if self.src.get(i) == Some(&b'[') {
            level
        } else {
            -1
        }
    }

    /// Like `count_long_bracket_level` but used when the FIRST `[` has already
    /// been consumed (cursor is one past it).  Looks for `=`*N followed by `[`.
    fn count_long_bracket_level_after_first_bracket(&self) -> i32 {
        let mut i = self.pos;
        let mut level = 0i32;
        while self.src.get(i) == Some(&b'=') {
            level += 1;
            i += 1;
        }
        if self.src.get(i) == Some(&b'[') {
            level
        } else {
            -1
        }
    }

    /// Consume a long string `[===[…]===]` starting from `self.pos` which
    /// must be pointing at the FIRST `[`.  Used by block comments.
    fn read_long_string(&mut self, level: usize, discard: bool) -> Result<String, LuaError> {
        self.advance(); // first [
        for _ in 0..level {
            self.advance();
        } // =…=
        self.advance(); // second [
        self.read_long_string_body(level, discard)
    }

    /// Read the body of a long string after BOTH opening brackets have already
    /// been consumed.  Handles the leading-newline skip and closing bracket.
    fn read_long_string_body(&mut self, level: usize, discard: bool) -> Result<String, LuaError> {
        // Skip an immediate newline after the opening bracket (Lua spec)
        if self.peek() == Some(b'\n') {
            self.advance();
        } else if self.peek() == Some(b'\r') {
            self.advance();
            if self.peek() == Some(b'\n') {
                self.advance();
            }
        }

        let mut buf = String::new();
        let start_line = self.line;
        loop {
            match self.advance() {
                None => {
                    return Err(LuaError::Syntax {
                        line: start_line,
                        message: "unfinished long string".into(),
                    })
                }
                Some(b']') => {
                    let mut eq = 0usize;
                    while self.peek() == Some(b'=') {
                        self.advance();
                        eq += 1;
                    }
                    if eq == level && self.peek() == Some(b']') {
                        self.advance(); // closing ]
                        return Ok(if discard { String::new() } else { buf });
                    } else if !discard {
                        buf.push(']');
                        for _ in 0..eq {
                            buf.push('=');
                        }
                    }
                }
                Some(ch) => {
                    if !discard {
                        buf.push(ch as char);
                    }
                }
            }
        }
    }

    // ── string literals ─────────────────────────────────────────────────────

    fn read_short_string(&mut self, delim: u8) -> Result<String, LuaError> {
        let start_line = self.line;
        let mut buf = String::new();
        loop {
            match self.advance() {
                None | Some(b'\n') | Some(b'\r') => {
                    return Err(LuaError::Syntax {
                        line: start_line,
                        message: "unfinished string".into(),
                    });
                }
                Some(b'\\') => {
                    let ch = self.advance().ok_or_else(|| LuaError::Syntax {
                        line: self.line,
                        message: "unfinished escape sequence".into(),
                    })?;
                    let esc = match ch {
                        b'a' => '\x07',
                        b'b' => '\x08',
                        b'f' => '\x0C',
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        b'v' => '\x0B',
                        b'\\' => '\\',
                        b'\'' => '\'',
                        b'"' => '"',
                        b'\n' | b'\r' => '\n',
                        b'x' => {
                            // \xHH
                            let h1 = self
                                .advance()
                                .and_then(|c| (c as char).to_digit(16))
                                .ok_or_else(|| LuaError::Syntax {
                                    line: self.line,
                                    message: "invalid hex escape".into(),
                                })? as u8;
                            let h2 = self
                                .advance()
                                .and_then(|c| (c as char).to_digit(16))
                                .ok_or_else(|| LuaError::Syntax {
                                    line: self.line,
                                    message: "invalid hex escape".into(),
                                })? as u8;
                            (h1 << 4 | h2) as char
                        }
                        b'u' => {
                            // \u{XXXX}
                            if self.advance() != Some(b'{') {
                                return Err(LuaError::Syntax {
                                    line: self.line,
                                    message: "missing '{' in \\u{xxxx}".into(),
                                });
                            }
                            let mut code = 0u32;
                            while self.peek() != Some(b'}') {
                                let d = self
                                    .advance()
                                    .and_then(|c| (c as char).to_digit(16))
                                    .ok_or_else(|| LuaError::Syntax {
                                        line: self.line,
                                        message: "invalid unicode escape".into(),
                                    })?;
                                code = code * 16 + d;
                            }
                            self.advance(); // }
                            char::from_u32(code).ok_or_else(|| LuaError::Syntax {
                                line: self.line,
                                message: "invalid unicode codepoint".into(),
                            })?
                        }
                        b'z' => {
                            // \z skips following whitespace
                            while matches!(self.peek(), Some(b' ' | b'\t' | b'\n' | b'\r')) {
                                self.advance();
                            }
                            continue;
                        }
                        d if d.is_ascii_digit() => {
                            // \ddd decimal escape
                            let mut val = (d - b'0') as u32;
                            for _ in 0..2 {
                                if self.peek().is_some_and(|c| c.is_ascii_digit()) {
                                    val = val * 10 + (self.advance().unwrap() - b'0') as u32;
                                }
                            }
                            if val > 255 {
                                return Err(LuaError::Syntax {
                                    line: self.line,
                                    message: "decimal escape too large".into(),
                                });
                            }
                            val as u8 as char
                        }
                        _ => {
                            return Err(LuaError::Syntax {
                                line: self.line,
                                message: format!("invalid escape '\\{}'", ch as char),
                            })
                        }
                    };
                    buf.push(esc);
                }
                Some(ch) if ch == delim => break,
                Some(ch) => buf.push(ch as char),
            }
        }
        Ok(buf)
    }

    // ── number literals ─────────────────────────────────────────────────────

    fn read_number(&mut self, first: u8) -> Result<TokenKind, LuaError> {
        let start = self.pos - 1;

        // Hex?
        if first == b'0' && matches!(self.peek(), Some(b'x' | b'X')) {
            self.advance(); // x/X
            let hex_start = self.pos;
            while self
                .peek()
                .is_some_and(|c| c.is_ascii_hexdigit() || c == b'_')
            {
                self.advance();
            }
            let has_dot = self.peek() == Some(b'.');
            if has_dot {
                self.advance();
                while self
                    .peek()
                    .is_some_and(|c| c.is_ascii_hexdigit() || c == b'_')
                {
                    self.advance();
                }
            }
            let has_exp = matches!(self.peek(), Some(b'p' | b'P'));
            if has_exp {
                self.advance();
                if matches!(self.peek(), Some(b'+' | b'-')) {
                    self.advance();
                }
                while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    self.advance();
                }
            }
            let raw = self.current_str(start).replace('_', "");
            if has_dot || has_exp {
                return raw
                    .parse::<f64>()
                    .map(TokenKind::Float)
                    .map_err(|_| LuaError::Syntax {
                        line: self.line,
                        message: format!("malformed number: {raw}"),
                    });
            } else {
                let digits = std::str::from_utf8(&self.src[hex_start..self.pos])
                    .unwrap_or("")
                    .replace('_', "");
                return i64::from_str_radix(&digits, 16)
                    .map(TokenKind::Integer)
                    .or_else(|_| {
                        u64::from_str_radix(&digits, 16).map(|u| TokenKind::Integer(u as i64))
                    })
                    .map_err(|_| LuaError::Syntax {
                        line: self.line,
                        message: format!("malformed hex integer: {digits}"),
                    });
            }
        }

        // Decimal
        while self.peek().is_some_and(|c| c.is_ascii_digit() || c == b'_') {
            self.advance();
        }
        let has_dot = self.peek() == Some(b'.') && (self.peek2() != Some(b'.'));
        if has_dot {
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit() || c == b'_') {
                self.advance();
            }
        }
        let has_exp = matches!(self.peek(), Some(b'e' | b'E'));
        if has_exp {
            self.advance();
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.advance();
            }
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }
        let raw = self.current_str(start).replace('_', "");
        if has_dot || has_exp {
            raw.parse::<f64>()
                .map(TokenKind::Float)
                .map_err(|_| LuaError::Syntax {
                    line: self.line,
                    message: format!("malformed float: {raw}"),
                })
        } else {
            raw.parse::<i64>()
                .map(TokenKind::Integer)
                .map_err(|_| LuaError::Syntax {
                    line: self.line,
                    message: format!("malformed integer: {raw}"),
                })
        }
    }

    // ── main dispatch ────────────────────────────────────────────────────────

    fn next_token(&mut self) -> Result<Token, LuaError> {
        self.skip_whitespace_and_comments()?;
        let line = self.line;

        let ch = match self.advance() {
            None => return Ok(Token::new(TokenKind::Eof, line)),
            Some(c) => c,
        };

        let kind = match ch {
            // Numbers
            b'0'..=b'9' => self.read_number(ch)?,
            b'.' if self.peek().is_some_and(|c| c.is_ascii_digit()) => {
                // .5 style float
                let start = self.pos - 1;
                while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    self.advance();
                }
                if matches!(self.peek(), Some(b'e' | b'E')) {
                    self.advance();
                    if matches!(self.peek(), Some(b'+' | b'-')) {
                        self.advance();
                    }
                    while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                        self.advance();
                    }
                }
                self.current_str(start)
                    .parse::<f64>()
                    .map(TokenKind::Float)
                    .map_err(|_| LuaError::Syntax {
                        line,
                        message: "malformed float".into(),
                    })?
            }

            // Strings
            b'"' | b'\'' => {
                let s = self.read_short_string(ch)?;
                TokenKind::LuaString(s)
            }

            // Names / keywords
            b'_' | b'a'..=b'z' | b'A'..=b'Z' => {
                let start = self.pos - 1;
                while self
                    .peek()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_')
                {
                    self.advance();
                }
                let s = self.current_str(start);
                TokenKind::keyword(s).unwrap_or_else(|| TokenKind::Name(s.to_owned()))
            }

            // Long strings — the first `[` was already consumed by advance().
            // We now look at self.pos which points at the character right after
            // that `[`.  A valid long string continues with `=`*N then `[`.
            b'[' => {
                // Count `=` signs starting at current pos (first `[` consumed).
                let level = self.count_long_bracket_level_after_first_bracket();
                if level >= 0 {
                    // Consume `=`*level + `[` (the first `[` is already gone).
                    for _ in 0..level {
                        self.advance();
                    } // =…=
                    self.advance(); // closing `[`
                                    // Now read the body (skip the usual opening-bracket consume
                                    // inside read_long_string by calling the body helper directly).
                    let s = self.read_long_string_body(level as usize, false)?;
                    TokenKind::LuaString(s)
                } else {
                    TokenKind::LBracket
                }
            }

            // Operators and punctuation
            b'+' => TokenKind::Plus,
            b'*' => TokenKind::Star,
            b'%' => TokenKind::Percent,
            b'^' => TokenKind::Caret,
            b'&' => TokenKind::Ampersand,
            b'|' => TokenKind::Pipe,
            b'#' => TokenKind::Hash,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b']' => TokenKind::RBracket,
            b';' => TokenKind::Semicolon,
            b',' => TokenKind::Comma,

            b'-' => TokenKind::Minus,

            b'/' => {
                if self.eat_if(b'/') {
                    TokenKind::SlashSlash
                } else {
                    TokenKind::Slash
                }
            }
            b'~' => {
                if self.eat_if(b'=') {
                    TokenKind::NotEq
                } else {
                    TokenKind::Tilde
                }
            }
            b'<' => {
                if self.eat_if(b'<') {
                    TokenKind::ShiftLeft
                } else if self.eat_if(b'=') {
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            b'>' => {
                if self.eat_if(b'>') {
                    TokenKind::ShiftRight
                } else if self.eat_if(b'=') {
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            b'=' => {
                if self.eat_if(b'=') {
                    TokenKind::Eq
                } else {
                    TokenKind::Assign
                }
            }
            b':' => {
                if self.eat_if(b':') {
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            b'.' => {
                if self.eat_if(b'.') {
                    if self.eat_if(b'.') {
                        TokenKind::DotDotDot
                    } else {
                        TokenKind::DotDot
                    }
                } else {
                    TokenKind::Dot
                }
            }

            other => {
                return Err(LuaError::Syntax {
                    line,
                    message: format!("unexpected character '{}'", other as char),
                })
            }
        };

        Ok(Token::new(kind, line))
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(src: &str) -> Vec<TokenKind> {
        Lexer::new(src)
            .tokenize()
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    fn lex_err(src: &str) -> LuaError {
        Lexer::new(src).tokenize().unwrap_err()
    }

    // ── EOF / whitespace ────────────────────────────────────────
    #[test]
    fn empty_source_yields_eof() {
        assert_eq!(lex(""), vec![TokenKind::Eof]);
    }

    #[test]
    fn whitespace_only_yields_eof() {
        assert_eq!(lex("   \t\n  "), vec![TokenKind::Eof]);
    }

    // ── Comments ────────────────────────────────────────────────
    #[test]
    fn line_comment_skipped() {
        assert_eq!(lex("-- this is a comment\n"), vec![TokenKind::Eof]);
    }

    #[test]
    fn block_comment_skipped() {
        assert_eq!(lex("--[[ block comment ]]"), vec![TokenKind::Eof]);
    }

    #[test]
    fn block_comment_level2_skipped() {
        assert_eq!(lex("--[==[ block comment ]==]"), vec![TokenKind::Eof]);
    }

    // ── Integers ────────────────────────────────────────────────
    #[test]
    fn decimal_integer() {
        assert_eq!(lex("42"), vec![TokenKind::Integer(42), TokenKind::Eof]);
    }

    #[test]
    fn hex_integer() {
        assert_eq!(lex("0xFF"), vec![TokenKind::Integer(255), TokenKind::Eof]);
    }

    #[test]
    fn hex_integer_uppercase() {
        assert_eq!(lex("0XFF"), vec![TokenKind::Integer(255), TokenKind::Eof]);
    }

    // ── Floats ──────────────────────────────────────────────────
    #[test]
    fn simple_float() {
        assert_eq!(lex("3.14"), vec![TokenKind::Float(3.14), TokenKind::Eof]);
    }

    #[test]
    fn scientific_float() {
        assert_eq!(lex("1e3"), vec![TokenKind::Float(1000.0), TokenKind::Eof]);
    }

    #[test]
    fn dot_float() {
        assert_eq!(lex(".5"), vec![TokenKind::Float(0.5), TokenKind::Eof]);
    }

    // ── Strings ─────────────────────────────────────────────────
    #[test]
    fn double_quoted_string() {
        assert_eq!(
            lex(r#""hello""#),
            vec![TokenKind::LuaString("hello".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn single_quoted_string() {
        assert_eq!(
            lex("'world'"),
            vec![TokenKind::LuaString("world".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn escape_sequences() {
        assert_eq!(
            lex(r#""\n\t\\""#),
            vec![TokenKind::LuaString("\n\t\\".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn hex_escape() {
        assert_eq!(
            lex(r#""\x41""#),
            vec![TokenKind::LuaString("A".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn long_string() {
        assert_eq!(
            lex("[[hello]]"),
            vec![TokenKind::LuaString("hello".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn long_string_level1() {
        assert_eq!(
            lex("[=[hi]=]"),
            vec![TokenKind::LuaString("hi".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn long_string_skips_leading_newline() {
        assert_eq!(
            lex("[[\nhello]]"),
            vec![TokenKind::LuaString("hello".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn unterminated_string_is_error() {
        assert!(matches!(lex_err(r#""no close"#), LuaError::Syntax { .. }));
    }

    // ── Keywords ────────────────────────────────────────────────
    #[test]
    fn keywords() {
        assert_eq!(
            lex("if then else end"),
            vec![
                TokenKind::If,
                TokenKind::Then,
                TokenKind::Else,
                TokenKind::End,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn true_false_nil() {
        assert_eq!(
            lex("true false nil"),
            vec![
                TokenKind::True,
                TokenKind::False,
                TokenKind::Nil,
                TokenKind::Eof
            ]
        );
    }

    // ── Identifiers ─────────────────────────────────────────────
    #[test]
    fn identifier() {
        assert_eq!(
            lex("myVar"),
            vec![TokenKind::Name("myVar".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn underscore_identifier() {
        assert_eq!(
            lex("_G"),
            vec![TokenKind::Name("_G".into()), TokenKind::Eof]
        );
    }

    // ── Operators ───────────────────────────────────────────────
    #[test]
    fn arithmetic_ops() {
        assert_eq!(
            lex("+ - * / // % ^"),
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::SlashSlash,
                TokenKind::Percent,
                TokenKind::Caret,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn comparison_ops() {
        assert_eq!(
            lex("== ~= < <= > >="),
            vec![
                TokenKind::Eq,
                TokenKind::NotEq,
                TokenKind::Lt,
                TokenKind::LtEq,
                TokenKind::Gt,
                TokenKind::GtEq,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn dot_ops() {
        assert_eq!(
            lex(". .. ..."),
            vec![
                TokenKind::Dot,
                TokenKind::DotDot,
                TokenKind::DotDotDot,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn colon_ops() {
        assert_eq!(
            lex(": ::"),
            vec![TokenKind::Colon, TokenKind::ColonColon, TokenKind::Eof]
        );
    }

    // ── Punctuation ─────────────────────────────────────────────
    #[test]
    fn brackets() {
        assert_eq!(
            lex("( ) { } ] ;"),
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::RBracket,
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    // ── Line tracking ────────────────────────────────────────────
    #[test]
    fn line_numbers_tracked() {
        let tokens = Lexer::new("a\nb\nc").tokenize().unwrap();
        let lines: Vec<u32> = tokens.iter().map(|t| t.line).collect();
        assert_eq!(lines, vec![1, 2, 3, 3]); // a, b, c, EOF
    }

    // ── A real program ───────────────────────────────────────────
    #[test]
    fn simple_assignment_expression() {
        let kinds = lex("local x = 10 + 2");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Local,
                TokenKind::Name("x".into()),
                TokenKind::Assign,
                TokenKind::Integer(10),
                TokenKind::Plus,
                TokenKind::Integer(2),
                TokenKind::Eof,
            ]
        );
    }
}
