use crate::ast_node::*;
use crate::symbol_table::*;

impl AST {
    pub fn get_class_tables_in_table(&self, class_table_index: usize) -> Vec<usize> {
        self.symbol_table_arena
            .get_table_entries(class_table_index)
            .iter()
            .map(|entry_index| self.symbol_table_arena.get_table_entry(*entry_index))
            .filter_map(|symbol_entry| {
                if let SymbolKind::Class = symbol_entry.get_kind() {
                    symbol_entry.get_link()
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn find_function_in_table(&self, table_index: usize, name: &str) -> Option<usize> {
        for entry_index in self.symbol_table_arena.get_table_entries(table_index) {
            let symbol_entry = self.symbol_table_arena.get_table_entry(*entry_index);
            if symbol_entry.is_function() && symbol_entry.has_name(name) {
                return Some(*entry_index);
            }
        }

        None
    }

    pub fn is_member_variable(
        &self,
        class_table_index: usize,
        name: &str,
    ) -> Option<(usize, usize)> {
        let member_entry_index = self.get_variable_entry_in_class_table(class_table_index, name);
        if let Some(member_entry_index) = member_entry_index {
            return Some((class_table_index, member_entry_index));
        }
        for inherited_class_index in self.get_class_tables_in_table(class_table_index) {
            let result = self.is_member_variable(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn is_member_function(
        &self,
        class_table_index: usize,
        name: &str,
    ) -> Option<(usize, usize)> {
        let member_entry_index = self.find_function_in_table(class_table_index, name);
        if let Some(member_entry_index) = member_entry_index {
            return Some((class_table_index, member_entry_index));
        }
        for inherited_class_index in self.get_class_tables_in_table(class_table_index) {
            let result = self.is_member_function(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn get_variable_entry_in_class_table(
        &self,
        table_index: usize,
        name: &str,
    ) -> Option<usize> {
        for entry_index in self.symbol_table_arena.get_table_entries(table_index) {
            let symbol_entry = self.symbol_table_arena.get_table_entry(*entry_index);
            if symbol_entry.has_name(name) {
                if symbol_entry.is_variable() {
                    return Some(*entry_index);
                } else if symbol_entry.is_parameter() {
                    unreachable!();
                }
            }
        }

        None
    }
}

pub fn get_class_table_index_from_name(ast: &AST, class_name: &str) -> Option<usize> {
    ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap())
        .into_iter()
        .find(|&class_table_index| {
            ast.symbol_table_arena
                .get_table(class_table_index)
                .has_name(class_name)
        })
}
