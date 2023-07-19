use super::language::LanguageType;
use crate::tokens::Token;
use std::collections::hash_map::HashMap;

enum SymbolKind {
    KindFunction,
    KindIdentifier,
    KindParamater,
    KindArrayIdentifier,
}

pub struct Symbol {
    token: Token,
    name: String,
    symbol_type: SymbolKind,
    language_type: LanguageType,
    declare_line: u32,
    declare_colummn: u32,
    parameters: Vec<Symbol>,
    memory_address: usize,
}

pub struct SymbolTable {
    base_address: usize,
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            base_address: 0,
            table: HashMap::new(),
        }
    }

    pub fn lookup(&self, symbol: &str) -> Option<&Symbol> {
        return self.table.get(symbol);
    }
}
