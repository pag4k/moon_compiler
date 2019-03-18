use crate::ast_node::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

impl Tree<NodeElement, SymbolTableArena> {
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

    pub fn find_variable_in_table(&self, table_index: usize, name: &str) -> Option<usize> {
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            //dbg!(&symbol_entry.name);
            if symbol_entry.name == name {
                if let SymbolKind::Variable(_) | SymbolKind::Parameter(_) = symbol_entry.kind {
                    return Some(*entry_index);
                }
            }
        }

        None
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

    pub fn is_member_variable(&self, table_index: usize, name: &str) -> Option<usize> {
        let result = self.find_variable_in_table(table_index, name);
        if result.is_some() {
            return result;
        }
        for inherited_class_index in self.get_class_tables_in_table(table_index) {
            let result = self.is_member_variable(inherited_class_index, name);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub fn is_member_function(&self, table_index: usize, name: &str) -> Option<usize> {
        let result = self.find_function_in_table(table_index, name);
        if result.is_some() {
            return result;
        }
        for inherited_class_index in self.get_class_tables_in_table(table_index) {
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

    pub fn get_valid_class_table_from_type(
        &self,
        symbol_type: &SymbolType,
    ) -> Result<(SymbolType, Option<usize>), String> {
        match symbol_type {
            SymbolType::Class(type_name, _) => match self.find_class_symbol_table(type_name) {
                Some(type_entry_index) => Ok((symbol_type.clone(), Some(type_entry_index))),
                None => Err(type_name.clone()),
            },
            _ => Ok((symbol_type.clone(), None)),
        }
    }

    // pub fn get_valid_class_table_from_name(
    //     &self,
    //     type_name: &String,
    // ) -> Result<Option<usize>, String> {
    //     if KeywordType::from_str(&type_name).is_err() {
    //         match self.find_class_symbol_table(type_name) {
    //             Some(entry_index) => Ok(Some(entry_index)),
    //             None => Err(type_name.clone()),
    //         }
    //     } else {
    //         Ok(None)
    //     }
    // }

    pub fn get_valid_class_table_from_entry(
        &self,
        entry_index: usize,
    ) -> Result<(SymbolType, Option<usize>), String> {
        use SymbolKind::*;
        match self
            .symbol_table_arena
            .get_symbol_table_entry(entry_index)
            .kind
            .clone()
        {
            Variable(symbol_type) | Parameter(symbol_type) => {
                self.get_valid_class_table_from_type(&symbol_type)
            }
            Function(symbol_type, _) => match symbol_type {
                Some(symbol_type) => self.get_valid_class_table_from_type(&symbol_type),
                None => unreachable!(), // This should only happen for the main function.
            },
            Class => unreachable!(),
        }
    }

    pub fn is_array_type(
        &self,
        symbol_entry_index: usize,
        data_member_node: usize,
    ) -> Result<bool, SemanticError> {
        use SymbolKind::*;

        let symbol_entry = self
            .symbol_table_arena
            .get_symbol_table_entry(symbol_entry_index);
        let dimension_list_len = match symbol_entry.clone().kind {
            Variable(symbol_type) | Parameter(symbol_type) => {
                symbol_type.get_dimension_list().len()
            }
            _ => unreachable!(),
        };

        let index_list_len = self.get_children_of_child(data_member_node, 1).len();
        match (dimension_list_len == 0, index_list_len == 0) {
            (true, true) => Ok(false),
            (false, true) => Ok(true),
            (true, false) => Err(SemanticError::MismatchedNumberOfDimension(
                self.get_leftmost_token(data_member_node),
                dimension_list_len,
                index_list_len,
            )),
            (false, false) => Ok(false),
        }
    }

    pub fn check_number_of_dimensions(
        &self,
        symbol_entry_index: usize,
        data_member_node: usize,
    ) -> Result<(), SemanticError> {
        use SymbolKind::*;

        let symbol_entry = self
            .symbol_table_arena
            .get_symbol_table_entry(symbol_entry_index);
        let dimension_list_len = match symbol_entry.clone().kind {
            Variable(symbol_type) | Parameter(symbol_type) => {
                symbol_type.get_dimension_list().len()
            }
            _ => unreachable!(),
        };

        let index_list_len = self.get_children_of_child(data_member_node, 1).len();
        if dimension_list_len != index_list_len {
            return Err(SemanticError::MismatchedNumberOfDimension(
                self.get_leftmost_token(data_member_node),
                dimension_list_len,
                index_list_len,
            ));
        }

        Ok(())
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
            let node_index = self.get_node_index_with_entry_index(*node_child_index, entry_index);
            if node_index.is_some() {
                return node_index;
            }
        }
        None
    }
}
