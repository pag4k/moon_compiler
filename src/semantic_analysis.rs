use crate::ast_node::*;
use crate::language::*;
use crate::lexical_analyzer::*;
use crate::symbol_table::*;
use crate::tree::*;

use std::str::FromStr;

#[derive(Debug)]
pub enum SemanticError {
    //Class errors
    ClassNotFound(String),
    ParentClassNotFound(String, String),
    CircularClassDependency(Vec<String>),
    NumericalValuesHaveNoMember(String),
    //Member function errors
    MemberFunctionDefDoesNotMatchDecl(String),
    MemberFunctionDefDoesNotHaveDecl(String, String),
    MemberFunctionDeclHasNotDef(String, String),
    //Identifier errors
    DuplicateIdentifier(String),
    VariableUsedBeforeBeingDeclared(String),
    UndefinedLocalVariable(String),
    UndefinedFunction(String),
    UndefinedFreeFunction(String),
    UndefinedMemberVariable(String),
    UndefinedMemberFunction(String),
    UndefinedClass(String),
    DifferentNumberOfDimension(String),
    InvalidIndex(String),
    //Type error
    InvalidAddOperation,
    InvalidMultOperation,
    InvalidRelOperation,
    CannotAssignFloatToInteger,
    CannotAssignNumericalValueToClass(String),
    CannotAssignClassToNumericalValue(String),
    CannotAssignClasseToADifferentOne(String, String),
    ShouldNotReturnFromMain,
    ReturnTypeDoesNotMatchFuctionDeclaration(SymbolType, SymbolType),
}

#[derive(Debug, Clone)]
pub enum SemanticWarning {
    ShadowInheritedMemberVariable(String, String, String),
    ShadowInheritedMemberFunction(String, String, String),
}

impl Tree<NodeElement, SymbolTableArena> {
    pub fn get_token(&self, node_index: usize) -> Token {
        self.get_element(node_index).token.clone().unwrap()
    }

    pub fn get_lexeme(&self, node_index: usize) -> String {
        self.get_element(node_index)
            .token
            .clone()
            .unwrap()
            .lexeme
            .unwrap()
    }

    pub fn get_child_lexeme(&self, node_index: usize, child_index: usize) -> String {
        self.get_lexeme(self.get_children(node_index)[child_index])
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

    pub fn get_valid_class_table_from_name(
        &self,
        type_name: &String,
    ) -> Result<Option<usize>, String> {
        if KeywordType::from_str(&type_name).is_err() {
            match self.find_class_symbol_table(type_name) {
                Some(entry_index) => Ok(Some(entry_index)),
                None => Err(type_name.clone()),
            }
        } else {
            Ok(None)
        }
    }

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

    // pub fn check_indices(
    //     &self,
    //     variable_declaration_entry_index: usize,
    //     data_member_node: usize,
    // ) -> Result<(), SemanticError> {
    //     use SymbolKind::*;

    //     let symbol_entry = self
    //         .symbol_table_arena
    //         .get_symbol_table_entry(variable_declaration_entry_index);
    //     dbg!(&symbol_entry);
    //     let dimension_list = match symbol_entry.clone().kind {
    //         Variable(symbol_type) | Parameter(symbol_type) => match symbol_type {
    //             SymbolType::Integer(dimension_list) => dimension_list,
    //             SymbolType::Float(dimension_list) => dimension_list,
    //             SymbolType::Class(_, dimension_list) => dimension_list,
    //         },
    //         _ => unreachable!(),
    //     };
    //     dbg!(&self
    //         .get_children_of_child(data_member_node, 1)
    //         .iter()
    //         .map(|&node_index| self
    //             .get_element(node_index)
    //             .clone()
    //             .data
    //             .unwrap()
    //             .parse::<usize>()));
    //     let index_list: Vec<usize> = self
    //         .get_children_of_child(data_member_node, 1)
    //         .iter()
    //         .map(|&node_index| {
    //             self.get_element(node_index)
    //                 .clone()
    //                 .data
    //                 .expect("Node has no data.")
    //                 .parse::<usize>()
    //                 .expect("Index is not an integer.")
    //         })
    //         .collect();
    //     dbg!(&index_list);
    //     if dimension_list.len() != index_list.len() {
    //         return Err(SemanticError::DifferentNumberOfDimension(
    //             symbol_entry.name.clone(),
    //         ));
    //     }

    //     if !dimension_list
    //         .iter()
    //         .zip(index_list.iter())
    //         .all(|(dimension, index)| index < dimension)
    //     {
    //         return Err(SemanticError::InvalidIndex(symbol_entry.name.clone()));
    //     }

    //     Ok(())
    // }
}
