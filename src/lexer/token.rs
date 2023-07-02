#[derive(Debug, Eq, PartialEq)]
pub enum TokenValue {
    Char,
    Int,
    Real,
    Bool,
    String,
    If,
    Then,
    Else,
    Loop,
    Input,
    Output,
    Return,
    Comma,
    SemiColon,
    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    Assign,
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Percent,
    LessThan,
    GreaterThan,
    VBar,
    Tilde,
    LessOrEqual,
    GreaterOrEqual,
    Equals,
    Bang,
    Differs,
    Identifier,
    Error,
    SOI,
    EOF,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
    Literal,
    Identifier,
    Special,
    Operator,
    Error,
    SOI,
    EOF,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    token: Option<Vec<u8>>,
    token_value: TokenValue,
    token_type: TokenKind,
}

impl Token {

    pub fn is_eof(&mut self) -> bool {
        self.token_value == TokenValue::EOF
    }

    pub fn make_soi() -> Token {
        Token {
            token: None,
            token_value: TokenValue::SOI,
            token_type: TokenKind::SOI,
        }
    }

    pub fn make_eof() -> Token {
        Token {
            token: None,
            token_value: TokenValue::EOF,
            token_type: TokenKind::EOF,
        }
    }

    pub fn make_special(keyword: TokenValue) -> Token {
        Token {
            token: None,
            token_value: keyword,
            token_type: TokenKind::Special,
        }
    }

    pub fn make_identifier(ident: Vec<u8>) -> Token {
        Token {
            token: Option::Some(ident),
            token_value: TokenValue::Identifier,
            token_type: TokenKind::Identifier,
        }
    }

    pub fn make_operator(operator: TokenValue) -> Token {
        Token {
            token: None,
            token_value: operator,
            token_type: TokenKind::Operator,
        }
    }

    pub fn make_literal(literal: Vec<u8>, literal_type: TokenValue) -> Token {
        Token {
            token: Option::Some(literal),
            token_value: literal_type,
            token_type: TokenKind::Literal,
        }
    }

    pub fn make_error(token: Vec<u8>) -> Token {
        Token {
            token: Option::Some(token),
            token_value: TokenValue::Error,
            token_type: TokenKind::Error,
        }
    }

    pub fn is_valid_escape_character(character: u8) -> bool {
        match character {
            b'a'..=b'z' | b'A'..=b'Z' | b'\\' | b'\'' | b'"' | b'0' => true,
            _ => false,
        }
    }
}
