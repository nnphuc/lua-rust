/// A single lexical token produced by the [`crate::Lexer`].
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    /// 1-based source line where this token starts.
    pub line: u32,
}

impl Token {
    pub fn new(kind: TokenKind, line: u32) -> Self {
        Self { kind, line }
    }
}

/// Every token kind in the Lua 5.4 grammar.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ── Literals ───────────────────────────────────────────────────────────
    Integer(i64),
    Float(f64),
    LuaString(String),
    Name(String),

    // ── Keywords ───────────────────────────────────────────────────────────
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    // ── Arithmetic / bitwise operators ─────────────────────────────────────
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    SlashSlash, // //
    Percent,    // %
    Caret,      // ^
    Ampersand,  // &
    Tilde,      // ~
    Pipe,       // |
    ShiftLeft,  // <<
    ShiftRight, // >>

    // ── Comparison / misc operators ────────────────────────────────────────
    Hash,  // #
    Eq,    // ==
    NotEq, // ~=
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=

    // ── Assignment ─────────────────────────────────────────────────────────
    Assign, // =

    // ── Delimiters / punctuation ───────────────────────────────────────────
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    ColonColon, // ::
    Semicolon,  // ;
    Colon,      // :
    Comma,      // ,
    Dot,        // .
    DotDot,     // ..
    DotDotDot,  // ...

    // ── End-of-file ────────────────────────────────────────────────────────
    Eof,
}

impl TokenKind {
    /// Map a keyword string to its `TokenKind`, or return `None` if it is not
    /// a keyword (caller should treat it as a [`TokenKind::Name`]).
    pub fn keyword(s: &str) -> Option<TokenKind> {
        Some(match s {
            "and" => TokenKind::And,
            "break" => TokenKind::Break,
            "do" => TokenKind::Do,
            "else" => TokenKind::Else,
            "elseif" => TokenKind::Elseif,
            "end" => TokenKind::End,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "function" => TokenKind::Function,
            "goto" => TokenKind::Goto,
            "if" => TokenKind::If,
            "in" => TokenKind::In,
            "local" => TokenKind::Local,
            "nil" => TokenKind::Nil,
            "not" => TokenKind::Not,
            "or" => TokenKind::Or,
            "repeat" => TokenKind::Repeat,
            "return" => TokenKind::Return,
            "then" => TokenKind::Then,
            "true" => TokenKind::True,
            "until" => TokenKind::Until,
            "while" => TokenKind::While,
            _ => return None,
        })
    }
}
