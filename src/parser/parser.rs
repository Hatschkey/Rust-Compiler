use super::{ast::AstNode, stack::Stack, statement::Statement, ProgramContext};
use crate::tokens::{Token, TokenValue};

pub struct Parser {
    tokens: Vec<Token>,
    token_context: Stack<Token>,
    read_position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            token_context: Stack::new(),
            read_position: 0,
        }
    }

    pub fn get_next_token(&mut self) -> Option<&Token> {
        let mut token = Option::None;
        if self.read_position < self.tokens.len() {
            self.read_position += 1;
            token = Option::Some(&self.tokens[self.read_position - 1]);
        }
        return token;
    }

    pub fn parse(&mut self) -> ProgramContext {
        todo!();
    }

    pub fn make_top_level_stmnt(&mut self) -> AstNode<'_> {
        todo!();
    }
}
