use crate::ast_node::*;
use crate::symbol_table::*;
use crate::tree::*;

#[derive(Debug)]
pub enum SymbolTableError {
    ClassNotFound(String),
    ParentClassNotFound(String, String),
    CircularClassDependency(Vec<String>),
    FunctionDefDoesNotMatchDecl(String),
    FunctionNotFound(String, String),
    DuplicateIdentifier(String),
}

#[derive(Clone)]
pub enum SemanticWarning {
    ShadowParentVariable(String, String),
    ShadowParentFunction(String, String),
}

impl Tree<NodeElement, SymbolTableArena> {
    pub fn get_name(&self, node_index: usize, child_index: usize) -> String {
        self.get_element(self.get_children(node_index)[child_index])
            .clone()
            .data
            .unwrap()
    }

    pub fn check_duplicate(
        &self,
        table_index: usize,
        entry_index: usize,
    ) -> Result<(), SymbolTableError> {
        let name = &self
            .symbol_table_arena
            .get_symbol_table_entry(entry_index)
            .name;
        for entry in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            if self.symbol_table_arena.get_symbol_table_entry(*entry).name == *name {
                return Err(SymbolTableError::DuplicateIdentifier(name.to_string()));
            }
        }

        Ok(())
    }

    pub fn add_entry_to_table(
        &mut self,
        table_index: usize,
        entry_index: usize,
    ) -> Result<(), SymbolTableError> {
        self.check_duplicate(table_index, entry_index)?;
        self.symbol_table_arena.add_entry(table_index, entry_index);

        Ok(())
    }

    pub fn is_member_variable(&self, name: &str) -> Option<(usize, usize)> {
        None
    }
}
