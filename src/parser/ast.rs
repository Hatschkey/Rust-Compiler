use super::{statement::Statement, language::LanguageType};
use crate::tokens::Token;

pub struct AstNode<'a> {
    statement: Statement,
    return_type: LanguageType,
    lexem: &'a Token,
    children: Vec<Box<AstNode<'a>>>,
    next_node: Option<Box<AstNode<'a>>>,
}

impl<'a> AstNode<'a> {
    pub fn new(statement: Statement, return_type: LanguageType, lexem: &'a Token) -> AstNode<'a> {
        AstNode {
            statement,
            return_type,
            lexem,
            children: Vec::new(),
            next_node: Option::None,
        }
    }

    pub fn add_next(&mut self, next: AstNode<'a>) {
        self.next_node = Option::Some(Box::new(next));
    }

    pub fn get_next(&self) -> Option<&'a AstNode> {
        match &self.next_node {
            None => Option::None,
            Some(node) => Option::Some(node.as_ref()),
        }
    }
}
