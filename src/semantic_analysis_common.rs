use crate::ast_node::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

impl AST {
    pub fn check_duplicate(
        &self,
        table_index: usize,
        node_index: usize,
    ) -> Result<(), SemanticError> {
        let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
        let name = &self
            .symbol_table_arena
            .get_symbol_table_entry(entry_index)
            .name;
        for entry in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            if self.symbol_table_arena.get_symbol_table_entry(*entry).name == *name {
                return Err(SemanticError::DuplicateIdentifier(
                    self.get_leftmost_token(node_index),
                    self.symbol_table_arena
                        .get_symbol_table(table_index)
                        .name
                        .clone(),
                    name.clone(),
                ));
            }
        }

        Ok(())
    }

    pub fn add_entry_to_table(
        &mut self,
        table_index: usize,
        node_index: usize,
    ) -> Result<(), SemanticError> {
        self.check_duplicate(table_index, node_index)?;
        let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
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

    pub fn find_free_function(&self, name: &str) -> Option<usize> {
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(self.symbol_table_arena.root.unwrap())
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

    pub fn find_function_in_table(&self, table_index: usize, name: &str) -> Option<usize> {
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

    pub fn is_member_variable(&self, class_table_index: usize, name: &str) -> Option<usize> {
        let result = self.get_variable_entry_in_class_table(class_table_index, name);
        if result.is_some() {
            return result;
        }
        for inherited_class_index in self.get_class_tables_in_table(class_table_index) {
            let result = self.is_member_variable(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn is_member_function(&self, class_table_index: usize, name: &str) -> Option<usize> {
        let result = self.find_function_in_table(class_table_index, name);
        if result.is_some() {
            return result;
        }
        for inherited_class_index in self.get_class_tables_in_table(class_table_index) {
            let result = self.is_member_function(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn find_class_symbol_table(&self, name: &str) -> Option<usize> {
        for class_table_index in
            self.get_class_tables_in_table(self.symbol_table_arena.root.unwrap())
        {
            let class_name = self
                .symbol_table_arena
                .get_symbol_table(class_table_index)
                .name
                .clone();
            if name == class_name {
                return Some(class_table_index);
            }
        }
        None
    }

    pub fn get_node_index_with_entry_index(
        &self,
        node_index: usize,
        entry_index: usize,
    ) -> Option<usize> {
        if let Some(current_entry_index) = self.get_element(node_index).symbol_table_entry {
            if current_entry_index == entry_index {
                return Some(node_index);
            }
        }
        for node_child_index in self.get_children(node_index) {
            let node_index = self.get_node_index_with_entry_index(node_child_index, entry_index);
            if node_index.is_some() {
                return node_index;
            }
        }
        None
    }

    pub fn get_node_symbol_type(&self, node_index: usize) -> Option<SymbolType> {
        self.get_element(node_index).data_type.clone()
    }

    pub fn get_variable_entry_in_class_table(
        &self,
        table_index: usize,
        name: &str,
    ) -> Option<usize> {
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if symbol_entry.name == name {
                if let SymbolKind::Variable(_) = symbol_entry.kind {
                    return Some(*entry_index);
                } else if let SymbolKind::Parameter(_) = symbol_entry.kind {
                    unreachable!();
                }
            }
        }

        None
    }
}
