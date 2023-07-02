use super::token::Token;
use super::token::TokenValue;

macro_rules! either {
    ($x:expr => $true_expr:expr; $false_expr:expr) => {
        if $x {
            $true_expr
        } else {
            $false_expr
        }
    };
}

pub struct Lexer {
    input: Vec<u8>,
    read_position: usize,
    prev_character: u8,
    character: u8,
    peeked_character: u8,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut input: Vec<u8> = input.bytes().map(|c| c as u8).collect();
        input.push(b'\0');
        let character = input[0];
        let peeked_character = either!(input.len() > 1 => input[1]; b'\0');
        Lexer {
            input,
            read_position: 0,
            prev_character: b'\0',
            character,
            peeked_character,
            line: 0,
            col: 0,
        }
    }

    fn advance(&mut self) {
        if self.read_position >= self.input.len() {
            // TODO: Fix this with proper error handling?
            panic!("Reached EOF while reading a token");
        } else {
            self.prev_character =
                either!(self.read_position > 1 => self.input[self.read_position - 1]; b'\0');
            self.character = self.input[self.read_position];
            self.peeked_character = either!(self.read_position + 1 < self.input.len() => self.input[self.read_position + 1]; b'\0');
            self.read_position += 1;
            self.col += 1;
        }

        if self.character == b'\n' {
            self.line += 1;
            self.col = 0;
        }
    }

    fn skip_spaces(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> Vec<u8> {
        let mut identifier = Vec::<u8>::new();
        identifier.push(self.character);

        while self.peeked_character.is_ascii_alphanumeric()
            || self.peeked_character == b'_'
            || self.peeked_character == b'.'
        {
            self.advance();
            identifier.push(self.character);
        }
        return identifier;
    }

    fn make_number(&mut self) -> Token {
        let mut number_literal = Vec::<u8>::new();
        let mut number_kind = TokenValue::Int;
        number_literal.push(self.character);

        while self.peeked_character.is_ascii_digit() {
            self.advance();
            number_literal.push(self.character);
        }

        if self.peeked_character == b'.' {
            self.advance();
            number_literal.push(self.character);
            number_kind = TokenValue::Real;
            while self.peeked_character.is_ascii_digit() {
                self.advance();
                number_literal.push(self.character);
            }
        }

        return Token::make_literal(number_literal, number_kind);
    }

    fn make_char(&mut self) -> Token {
        let mut char_literal = Vec::<u8>::new();
        char_literal.push(self.character);
        self.advance();

        if self.character != b'\\' && self.character != b'\'' {
            char_literal.push(self.character);
            self.advance();
        } else if Token::is_valid_escape_character(self.peeked_character) {
            char_literal.push(self.character);
            self.advance();
            char_literal.push(self.character);
            self.advance()
        }

        if self.character == b'\'' {
            char_literal.push(self.character);
        } else {
            return Token::make_error(char_literal);
        }
        return Token::make_literal(char_literal, TokenValue::Char);
    }

    fn make_string(&mut self) -> Token {
        let mut string_literal = Vec::<u8>::new();
        string_literal.push(self.character);

        while self.peeked_character != b'"' || self.character == b'\\' {
            self.advance();
            string_literal.push(self.character);
        }
        self.advance();
        string_literal.push(self.character);

        return Token::make_literal(string_literal, TokenValue::String);
    }

    fn ignore_comment(&mut self) -> Token {
        if self.peeked_character != b'\\' {
            panic!("Invalid token '\\'");
        }

        self.advance();
        if self.peeked_character == b'\\' {
            while self.prev_character != b'/'
                || self.character != b'/'
                || self.peeked_character != b'/'
            {
                self.advance();
            }
            self.advance();
            self.advance();
        } else {
            while self.character != b'\n' && self.character != b'\0' {
                self.advance();
            }
        }

        return self.read_token();
    }

    fn read_token(&mut self) -> Token {
        self.skip_spaces();

        match self.character {
            b'\0' => Token::make_eof(),
            b',' => Token::make_special(TokenValue::Comma),
            b';' => Token::make_special(TokenValue::SemiColon),
            b'(' => Token::make_special(TokenValue::OpenParenthesis),
            b')' => Token::make_special(TokenValue::CloseParenthesis),
            b'[' => Token::make_special(TokenValue::OpenBracket),
            b']' => Token::make_special(TokenValue::CloseBracket),
            b'{' => Token::make_special(TokenValue::OpenCurlyBracket),
            b'}' => Token::make_special(TokenValue::CloseCurlyBracket),
            b'/' => Token::make_special(TokenValue::ForwardSlash),
            b'\\' => self.ignore_comment(),
            b'\'' => self.make_char(),
            b'"' => self.make_string(),
            b'0'..=b'9' => self.make_number(),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let identifier = self.read_identifier();
                match identifier.as_slice() {
                    b"char" => Token::make_special(TokenValue::Char),
                    b"int" => Token::make_special(TokenValue::Int),
                    b"real" => Token::make_special(TokenValue::Real),
                    b"bool" => Token::make_special(TokenValue::Bool),
                    b"string" => Token::make_special(TokenValue::String),
                    b"if" => Token::make_special(TokenValue::If),
                    b"then" => Token::make_special(TokenValue::Then),
                    b"else" => Token::make_special(TokenValue::Else),
                    b"loop" => Token::make_special(TokenValue::Loop),
                    b"input" => Token::make_special(TokenValue::Input),
                    b"output" => Token::make_special(TokenValue::Output),
                    b"return" => Token::make_special(TokenValue::Return),
                    b"true" | b"false" => Token::make_literal(identifier, TokenValue::Bool),
                    _ => Token::make_identifier(identifier),
                }
            }
            b'-' => {
                either!(self.peeked_character.is_ascii_digit() => self.make_number(); Token::make_operator(TokenValue::Minus))
            }
            b'<' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(); TokenValue::LessOrEqual}; TokenValue::LessThan),
            ),
            b'>' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(); TokenValue::GreaterOrEqual}; TokenValue::GreaterThan),
            ),
            b'!' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(); TokenValue::Differs}; TokenValue::Bang),
            ),
            b'=' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(); TokenValue::Equals}; TokenValue::Assign),
            ),
            b'+' => Token::make_operator(TokenValue::Plus),
            b'*' => Token::make_operator(TokenValue::Star),
            b'%' => Token::make_operator(TokenValue::Percent),
            b'|' => Token::make_operator(TokenValue::VBar),
            b'~' => Token::make_operator(TokenValue::Tilde),
            _ => panic!("Unrecognized token: {}", self.character as char),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        let mut last_token = Token::make_soi();
        while !last_token.is_eof() {
            tokens.push(last_token);
            self.advance();
            last_token = self.read_token();
            println!("Last token: {:?}", last_token);
        }
        tokens.push(last_token);
        return tokens;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn as_byte_vec(token: &str) -> Vec<u8> {
        return token.bytes().map(|c| c as u8).collect();
    }

    #[test]
    fn should_create_empty_program() {
        let mut lexer = Lexer::new("");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![Token::make_soi(), Token::make_eof()];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_int_literals() {
        let mut lexer = Lexer::new("420");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("420"), TokenValue::Int),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_int_literals_negative() {
        let mut lexer = Lexer::new("-69");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("-69"), TokenValue::Int),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_real_literals() {
        let mut lexer = Lexer::new("123.45");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("123.45"), TokenValue::Real),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_real_literals_negative() {
        let mut lexer = Lexer::new("-1.23");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("-1.23"), TokenValue::Real),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_char_literals() {
        let mut lexer = Lexer::new("'a'");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("'a'"), TokenValue::Char),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_char_literals_escaped() {
        let mut lexer = Lexer::new("'\\''");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("'\\''"), TokenValue::Char),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_char_literals_empty() {
        let mut lexer = Lexer::new("''");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("''"), TokenValue::Char),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_string_literals() {
        let mut lexer = Lexer::new("\"a string literal\"");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("\"a string literal\""), TokenValue::String),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_string_literals_with_escaped_quotes() {
        let mut lexer = Lexer::new("\"a string literal with escaped \\\" quotes \\\"\"");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(
                as_byte_vec("\"a string literal with escaped \\\" quotes \\\"\""),
                TokenValue::String,
            ),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_string_literals_with_escaped_newline() {
        let mut lexer = Lexer::new("\"a string literal with escaped \\\\n newline\"");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(
                as_byte_vec("\"a string literal with escaped \\\\n newline\""),
                TokenValue::String,
            ),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_string_literals_empty() {
        let mut lexer = Lexer::new("\"\"");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("\"\""), TokenValue::String),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_bool_literals() {
        let mut lexer = Lexer::new("true false");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_literal(as_byte_vec("true"), TokenValue::Bool),
            Token::make_literal(as_byte_vec("false"), TokenValue::Bool),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_keywords() {
        let mut lexer =
            Lexer::new("char int real bool string if then else loop input output return");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_special(TokenValue::Int),
            Token::make_special(TokenValue::Real),
            Token::make_special(TokenValue::Bool),
            Token::make_special(TokenValue::String),
            Token::make_special(TokenValue::If),
            Token::make_special(TokenValue::Then),
            Token::make_special(TokenValue::Else),
            Token::make_special(TokenValue::Loop),
            Token::make_special(TokenValue::Input),
            Token::make_special(TokenValue::Output),
            Token::make_special(TokenValue::Return),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_special_characters() {
        let mut lexer = Lexer::new(",;()[]{}/");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Comma),
            Token::make_special(TokenValue::SemiColon),
            Token::make_special(TokenValue::OpenParenthesis),
            Token::make_special(TokenValue::CloseParenthesis),
            Token::make_special(TokenValue::OpenBracket),
            Token::make_special(TokenValue::CloseBracket),
            Token::make_special(TokenValue::OpenCurlyBracket),
            Token::make_special(TokenValue::CloseCurlyBracket),
            Token::make_special(TokenValue::ForwardSlash),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_operators() {
        let mut lexer = Lexer::new("=+-*%<>|~<= >= == ! !=");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_operator(TokenValue::Assign),
            Token::make_operator(TokenValue::Plus),
            Token::make_operator(TokenValue::Minus),
            Token::make_operator(TokenValue::Star),
            Token::make_operator(TokenValue::Percent),
            Token::make_operator(TokenValue::LessThan),
            Token::make_operator(TokenValue::GreaterThan),
            Token::make_operator(TokenValue::VBar),
            Token::make_operator(TokenValue::Tilde),
            Token::make_operator(TokenValue::LessOrEqual),
            Token::make_operator(TokenValue::GreaterOrEqual),
            Token::make_operator(TokenValue::Equals),
            Token::make_operator(TokenValue::Bang),
            Token::make_operator(TokenValue::Differs),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_identifiers() {
        let mut lexer = Lexer::new("identifier_1 identifier.2 id_3.0");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_identifier(as_byte_vec("identifier_1")),
            Token::make_identifier(as_byte_vec("identifier.2")),
            Token::make_identifier(as_byte_vec("id_3.0")),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_full_program() {
        let source = std::fs::read_to_string("tests/factorial.pr").expect("File did not exist");
        let mut lexer = Lexer::new(&source);
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Int),
            Token::make_identifier(as_byte_vec("factorial_recursive")),
            Token::make_special(TokenValue::OpenParenthesis),
            Token::make_special(TokenValue::Int),
            Token::make_identifier(as_byte_vec("number")),
            Token::make_special(TokenValue::CloseParenthesis),
            Token::make_special(TokenValue::OpenCurlyBracket),
            Token::make_special(TokenValue::If),
            Token::make_identifier(as_byte_vec("number")),
            Token::make_operator(TokenValue::LessOrEqual),
            Token::make_literal(as_byte_vec("0"), TokenValue::Int),
            Token::make_special(TokenValue::Then),
            Token::make_special(TokenValue::OpenCurlyBracket),
            Token::make_special(TokenValue::Return),
            Token::make_literal(as_byte_vec("0"), TokenValue::Int),
            Token::make_special(TokenValue::SemiColon),
            Token::make_special(TokenValue::CloseCurlyBracket),
            Token::make_special(TokenValue::If),
            Token::make_identifier(as_byte_vec("number")),
            Token::make_operator(TokenValue::Equals),
            Token::make_literal(as_byte_vec("1"), TokenValue::Int),
            Token::make_special(TokenValue::Then),
            Token::make_special(TokenValue::OpenCurlyBracket),
            Token::make_special(TokenValue::Return),
            Token::make_literal(as_byte_vec("1"), TokenValue::Int),
            Token::make_special(TokenValue::SemiColon),
            Token::make_special(TokenValue::CloseCurlyBracket),
            Token::make_special(TokenValue::Else),
            Token::make_special(TokenValue::OpenCurlyBracket),
            Token::make_special(TokenValue::Return),
            Token::make_identifier(as_byte_vec("number")),
            Token::make_operator(TokenValue::Star),
            Token::make_identifier(as_byte_vec("factorial_recursive")),
            Token::make_special(TokenValue::OpenParenthesis),
            Token::make_identifier(as_byte_vec("number")),
            Token::make_operator(TokenValue::Minus),
            Token::make_literal(as_byte_vec("1"), TokenValue::Int),
            Token::make_special(TokenValue::CloseParenthesis),
            Token::make_special(TokenValue::SemiColon),
            Token::make_special(TokenValue::CloseCurlyBracket),
            Token::make_special(TokenValue::CloseCurlyBracket),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }
}
