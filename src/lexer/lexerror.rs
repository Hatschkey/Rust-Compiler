pub enum LexerError {
    UnterminatedComment,
    UnterminatedString,
    UnterminatedChar,
    UnterminatedReal,
    MalformedChar,
    TooBigChar,
    InvalidCharacter,
}

impl LexerError {
    pub fn format_message(&self, line: usize, column: usize, token: Vec<u8>) -> String {
        let stringified_token = String::from_utf8(token).unwrap();
        let bad_token_message = format!("Invalid token: {}", stringified_token);
        let big_char_message = format!("Char literal too big: {}", stringified_token);
        let unterm_string_message = format!("Unterminated string literal: {}", stringified_token);
        let unterm_char_message = format!("Unterminated char literal: {}", stringified_token);
        let unterm_real_message = format!("Unterminated real literal: {}", stringified_token);
        let malformed_char_message = format!("Malformed char literal: {}", stringified_token);
        let error_content = match *self {
            LexerError::UnterminatedComment => "Unterminated multi-line comment",
            LexerError::UnterminatedString => unterm_string_message.as_str(),
            LexerError::UnterminatedChar => unterm_char_message.as_str(),
            LexerError::UnterminatedReal => unterm_real_message.as_str(),
            LexerError::TooBigChar => big_char_message.as_str(),
            LexerError::InvalidCharacter => bad_token_message.as_str(),
            LexerError::MalformedChar => malformed_char_message.as_str(),
        };

        return format!("(At l: {}, c: {}): {}", line, column, error_content);
    }
}
