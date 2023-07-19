use crate::tokens::Token;

pub enum ParserError {
    BadTopLevelStatement,
}

impl ParserError {
    pub fn format_message(&self, expected: Vec<Token>, found: Token) -> String {
        let leading_error: &str = match *self {
            ParserError::BadTopLevelStatement => "Malformed top level statement",
        };

        return format!(
            "(At l: {}, c: {}): {}.\nExpected {} {:?} but found {}",
            "0", // TODO: Store position in token
            "0", // TODO: Store position in token
            if expected.len() > 1 { "one of" } else { "" },
            leading_error,
            expected,
            found.to_string()
        );
    }
}
