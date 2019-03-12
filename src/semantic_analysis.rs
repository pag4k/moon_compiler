use crate::ast_node::*;
use crate::symbol_table::*;
use crate::tree::*;

#[derive(Debug)]
pub enum SemanticError {
    ClassNotFound(String),
    ParentClassNotFound(String, String),
    CircularClassDependency(Vec<String>),
    FunctionDefDoesNotMatchDecl(String),
    FunctionNotFound(String, String),
    MemberFunctionDeclHasNotDef(String, String),
    DuplicateIdentifier(String),
}

#[derive(Debug, Clone)]
pub enum SemanticWarning {
    ShadowInheritedMemberVariable(String, String, String),
    ShadowInheritedMemberFunction(String, String, String),
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
    ) -> Result<(), SemanticError> {
        let name = &self
            .symbol_table_arena
            .get_symbol_table_entry(entry_index)
            .name;
        for entry in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            if self.symbol_table_arena.get_symbol_table_entry(*entry).name == *name {
                return Err(SemanticError::DuplicateIdentifier(name.to_string()));
            }
        }

        Ok(())
    }

    pub fn add_entry_to_table(
        &mut self,
        table_index: usize,
        entry_index: usize,
    ) -> Result<(), SemanticError> {
        self.check_duplicate(table_index, entry_index)?;
        self.symbol_table_arena.add_entry(table_index, entry_index);

        Ok(())
    }

    pub fn get_class_tables_in_table(&self, table_index: usize) -> Vec<usize> {
        let mut class_table_indices = Vec::new();

        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if let SymbolKind::Class = symbol_entry.kind {
                if let Some(class_table_index) = symbol_entry.link {
                    class_table_indices.push(class_table_index);
                }
            }
        }
        class_table_indices
    }

    pub fn find_variable_in_class(&self, table_index: usize, name: &str) -> Option<usize> {
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if symbol_entry.name == name {
                if let SymbolKind::Variable(_) = symbol_entry.kind {
                    return Some(*entry_index);
                }
            }
        }

        None
    }

    pub fn find_function_in_class(&self, table_index: usize, name: &str) -> Option<usize> {
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if symbol_entry.name == name {
                if let SymbolKind::Function(_, _) = symbol_entry.kind {
                    return Some(*entry_index);
                }
            }
        }

        None
    }

    pub fn is_member_variable(&self, table_index: usize, name: &str) -> Option<usize> {
        let result = self.find_variable_in_class(table_index, name);
        if result.is_some() {
            return Some(table_index);
        }
        for inherited_class_index in self.get_class_tables_in_table(table_index) {
            let result = self.is_member_function(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn is_member_function(&self, table_index: usize, name: &str) -> Option<usize> {
        let result = self.find_function_in_class(table_index, name);
        if result.is_some() {
            return Some(table_index);
        }
        for inherited_class_index in self.get_class_tables_in_table(table_index) {
            let result = self.is_member_function(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }
}
