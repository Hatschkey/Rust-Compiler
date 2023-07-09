use super::lexerror::LexerError;
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

fn as_byte_vec(token: &str) -> Vec<u8> {
    return token.bytes().map(|c| c as u8).collect();
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut input: Vec<u8> = as_byte_vec(input);
        input.push(b'\0');
        let character = input[0];
        let peeked_character = either!(input.len() > 1 => input[1]; b'\0');
        Lexer {
            input,
            read_position: 0,
            prev_character: b'\0',
            character,
            peeked_character,
            line: 1,
            col: 0,
        }
    }

    fn advance(&mut self, times: usize) {
        for _ in 1..=times {
            if self.read_position >= self.input.len() {
                self.character = b'\0';
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
    }

    fn advance_and_push(&mut self, token: &mut Vec<u8>, times: usize) {
        for _ in 1..=times {
            self.advance(1);
            if self.character != b'\0' && self.character != b'\n' {
                token.push(self.character);
            }
        }
    }

    fn skip_spaces(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.advance(1);
        }
    }

    fn read_identifier(&mut self) -> Vec<u8> {
        let mut identifier = Vec::<u8>::new();
        identifier.push(self.character);

        while self.peeked_character.is_ascii_alphanumeric()
            || self.peeked_character == b'_'
            || self.peeked_character == b'.'
        {
            self.advance_and_push(&mut identifier, 1);
        }
        return identifier;
    }

    fn make_number(&mut self) -> Token {
        let mut number_literal = Vec::<u8>::new();
        let mut number_kind = TokenValue::Int;
        let start_line = self.line;
        let start_col = self.col;
        number_literal.push(self.character);

        while self.peeked_character.is_ascii_digit() {
            self.advance_and_push(&mut number_literal, 1);
        }

        if self.peeked_character == b'.' {
            number_kind = TokenValue::Real;
            self.advance_and_push(&mut number_literal, 1);

            while self.peeked_character.is_ascii_digit() {
                self.advance_and_push(&mut number_literal, 1);
            }

            if !self.character.is_ascii_digit() {
                return Token::make_error(as_byte_vec(
                    LexerError::UnterminatedReal
                        .format_message(start_line, start_col, number_literal)
                        .as_str(),
                ));
            }
        }

        return Token::make_literal(number_literal, number_kind);
    }

    fn make_char(&mut self) -> Token {
        let mut char_literal = Vec::<u8>::new();
        let start_line = self.line;
        let start_col = self.col;

        char_literal.push(self.character);
        match self.peeked_character {
            b'\\' => self.advance_and_push(&mut char_literal, 3),
            b'\0' => self.advance(1),
            _ => self.advance_and_push(&mut char_literal, 1),
        };

        if self.character != b'\0' && self.peeked_character == b'\'' {
            self.advance_and_push(&mut char_literal, 1);
        }

        if self.character == b'\'' {
            return Token::make_literal(char_literal, TokenValue::Char);
        }

        while self.peeked_character != b'\0' && self.peeked_character != b'\'' {
            self.advance_and_push(&mut char_literal, 1);
        }

        match self.peeked_character {
            b'\0' => Token::make_error(as_byte_vec(
                LexerError::UnterminatedChar
                    .format_message(start_line, start_col, char_literal)
                    .as_str(),
            )),
            _ => {
                self.advance_and_push(&mut char_literal, 1);
                if self.prev_character == b'\0' {
                    Token::make_error(as_byte_vec(
                        LexerError::MalformedChar
                            .format_message(start_line, start_col, char_literal)
                            .as_str(),
                    ))
                } else {
                    Token::make_error(as_byte_vec(
                        LexerError::TooBigChar
                            .format_message(start_line, start_col, char_literal)
                            .as_str(),
                    ))
                }
            }
        }
    }

    fn make_string(&mut self) -> Token {
        let mut string_literal = Vec::<u8>::new();
        let start_line = self.line;
        let start_col = self.col;
        string_literal.push(self.character);

        while self.peeked_character != b'\n'
            && self.peeked_character != b'\0'
            && (self.peeked_character != b'"' || self.character == b'\\')
        {
            self.advance_and_push(&mut string_literal, 1);
        }
        self.advance_and_push(&mut string_literal, 1);

        match self.character {
            b'\n' | b'\0' => Token::make_error(as_byte_vec(
                LexerError::UnterminatedString
                    .format_message(start_line, start_col, string_literal)
                    .as_str(),
            )),
            _ => Token::make_literal(string_literal, TokenValue::String),
        }
    }

    fn ignore_comment(&mut self) -> Token {
        let start_line = self.line;
        let start_col = self.col;
        self.advance(1);

        match self.character {
            b'\\' => {
                if self.peeked_character != b'\\' {
                    while self.character != b'\n' && self.character != b'\0' {
                        self.advance(1);
                    }
                } else {
                    while self.peeked_character != b'\0'
                        && (self.prev_character != b'/'
                            || self.character != b'/'
                            || self.peeked_character != b'/')
                    {
                        self.advance(1);
                    }
                    if self.peeked_character != b'\0' {
                        self.advance(2);
                    } else {
                        return Token::make_error(as_byte_vec(
                            LexerError::UnterminatedComment
                                .format_message(start_line, start_col, vec![])
                                .as_str(),
                        ));
                    }
                }
                return self.read_token();
            }
            _ => Token::make_error(as_byte_vec(
                LexerError::InvalidCharacter
                    .format_message(self.line, self.col, vec![self.prev_character as u8])
                    .as_str(),
            )),
        }
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
                either!(self.peeked_character == b'=' => {self.advance(1); TokenValue::LessOrEqual}; TokenValue::LessThan),
            ),
            b'>' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(1); TokenValue::GreaterOrEqual}; TokenValue::GreaterThan),
            ),
            b'!' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(1); TokenValue::Differs}; TokenValue::Bang),
            ),
            b'=' => Token::make_operator(
                either!(self.peeked_character == b'=' => {self.advance(1); TokenValue::Equals}; TokenValue::Assign),
            ),
            b'+' => Token::make_operator(TokenValue::Plus),
            b'*' => Token::make_operator(TokenValue::Star),
            b'%' => Token::make_operator(TokenValue::Percent),
            b'|' => Token::make_operator(TokenValue::VBar),
            b'~' => Token::make_operator(TokenValue::Tilde),
            _ => Token::make_error(as_byte_vec(
                LexerError::InvalidCharacter
                    .format_message(self.line, self.col, vec![self.character as u8])
                    .as_str(),
            )),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        let mut last_token = Token::make_soi();
        while !last_token.is_eof() {
            tokens.push(last_token);
            self.advance(1);
            last_token = self.read_token();
        }
        tokens.push(last_token);
        return tokens;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn should_ignore_comments() {
        let mut lexer = Lexer::new("\\\\\\multiline comment///");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![Token::make_soi(), Token::make_eof()];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_read_full_program() {
        let mut lexer = Lexer::new(
            "
            int factorial_recursive(int number) {
                if number <= 0 then { \\\\ single line comment that should be ignored
                    return 0;
                }
                if number == 1 then {
                    return 1; 
                    \\\\\\ multi-line comment 
                    that should also be ignored
                    ///
                } else {
                    return number * factorial_recursive(number - 1);
                }
            }
            \\\\ End comment",
        );
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

    #[test]
    fn should_error_on_invalid_token() {
        let mut lexer = Lexer::new("int ten = 10;\n$");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Int),
            Token::make_identifier(as_byte_vec("ten")),
            Token::make_operator(TokenValue::Assign),
            Token::make_literal(as_byte_vec("10"), TokenValue::Int),
            Token::make_special(TokenValue::SemiColon),
            Token::make_error(as_byte_vec("(At l: 2, c: 1): Invalid token: $")),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_big_char() {
        let mut lexer = Lexer::new("char big = 'ab';\nchar bigger = 'ab\ncd';");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("big")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec("(At l: 1, c: 12): Char literal too big: 'ab'")),
            Token::make_special(TokenValue::SemiColon),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("bigger")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 2, c: 15): Char literal too big: 'abcd'",
            )),
            Token::make_special(TokenValue::SemiColon),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unterm_char() {
        let mut lexer = Lexer::new("char unterm = 'b");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("unterm")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 15): Unterminated char literal: 'b",
            )),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unstarted_char() {
        let mut lexer = Lexer::new("char unterm = '");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("unterm")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 15): Unterminated char literal: '",
            )),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unterm_escaped_char() {
        let mut lexer = Lexer::new("char unterm = '\\'");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("unterm")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 15): Unterminated char literal: '\\'",
            )),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_malformed_escape_char() {
        let mut lexer = Lexer::new("char malformed = '\0'");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("malformed")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec("(At l: 1, c: 18): Malformed char literal: ''")),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unterm_string() {
        let mut lexer = Lexer::new(
            "string unterm = \"unterminated strin\nstring unterm2 = \"another unterminated strin",
        );
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::String),
            Token::make_identifier(as_byte_vec("unterm")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 17): Unterminated string literal: \"unterminated strin",
            )),
            Token::make_special(TokenValue::String),
            Token::make_identifier(as_byte_vec("unterm2")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 2, c: 18): Unterminated string literal: \"another unterminated strin",
            )),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unterm_comment() {
        let mut lexer = Lexer::new("char x; \\\\\\ unterminated\nmultiline\ncomme");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Char),
            Token::make_identifier(as_byte_vec("x")),
            Token::make_special(TokenValue::SemiColon),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 9): Unterminated multi-line comment",
            )),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }

    #[test]
    fn should_error_on_unterm_real() {
        let mut lexer = Lexer::new("real x = 12.;");
        let actual_tokens = lexer.tokenize();
        let expected_tokens = vec![
            Token::make_soi(),
            Token::make_special(TokenValue::Real),
            Token::make_identifier(as_byte_vec("x")),
            Token::make_operator(TokenValue::Assign),
            Token::make_error(as_byte_vec(
                "(At l: 1, c: 10): Unterminated real literal: 12.",
            )),
            Token::make_special(TokenValue::SemiColon),
            Token::make_eof(),
        ];
        assert!(actual_tokens == expected_tokens);
    }
}
