use super::{ast::AstNode, statement::Statement, symbol::SymbolTable, language::LanguageType};
use crate::tokens::Token;

struct ScopeNode<'a> {
    table: SymbolTable,
    parent: Option<&'a ScopeNode<'a>>,
    children: Vec<SymbolTable>,
}

pub struct ProgramContext<'a> {
    ast_root: AstNode<'a>,
    scopes: ScopeNode<'a>,
    current_scope: &'a ScopeNode<'a>,
}

impl<'a> ProgramContext<'a> {
    pub fn new(soi_token: &'a Token) -> ProgramContext<'a> {
        todo!();
    }

    pub fn begin_scope() {}

    pub fn end_scope() {}
}
