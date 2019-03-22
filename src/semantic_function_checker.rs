use crate::ast_node::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

use std::collections::HashMap;

impl AST {
    pub fn semantic_function_checker(&mut self) -> Vec<SemanticError> {
        let mut semantic_errors: Vec<SemanticError> = Vec::new();

        self.check_function(&mut semantic_errors, self.root.unwrap());
        semantic_errors
    }

    fn check_function(&mut self, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.check_function(semantic_errors, child_index);
        }

        match self.get_note_type(node_index) {
            Prog => {
                let table_index = self
                    .get_element(self.get_children(node_index)[2])
                    .symbol_table
                    .unwrap();
                let mut variable_map: HashMap<String, bool> = self
                    .get_variable_names_in_table(table_index)
                    .iter()
                    .map(|variable| (variable.clone(), false))
                    .collect();
                self.check_statement_block(
                    semantic_errors,
                    self.get_children(node_index)[2],
                    &mut variable_map,
                    None,
                    table_index,
                    None,
                );
            }
            FuncDef => {
                let table_index = self.get_element(node_index).symbol_table.unwrap();

                // Prepare map to verify if variable have been declared before being used.
                let mut variable_map: HashMap<String, bool> = HashMap::new();
                for parameter_name in self.get_parameter_names_in_table(table_index) {
                    variable_map.insert(parameter_name, true);
                }
                for variable_name in self.get_variable_names_in_table(table_index) {
                    variable_map.insert(variable_name, false);
                }
                let scope_element = self.get_element(self.get_children(node_index)[1]);
                let scope_table_index = match scope_element.node_type {
                    // Here, we assume that we have already checked that the parent is valid
                    NodeType::Id => self.find_class_symbol_table(
                        &scope_element.token.clone().unwrap().lexeme.unwrap(),
                    ),
                    _ => None,
                };

                self.check_statement_block(
                    semantic_errors,
                    self.get_children(node_index)[4],
                    &mut variable_map,
                    scope_table_index,
                    table_index,
                    None,
                );
            }
            _ => {}
        }
    }

    fn check_statement_block(
        &mut self,
        semantic_errors: &mut Vec<SemanticError>,
        node_index: usize,
        variable_map: &mut HashMap<String, bool>,
        scope_index: Option<usize>,
        function_table_index: usize,
        for_variable_entry: Option<usize>,
    ) {
        use NodeType::*;

        let mut for_variable_entry = for_variable_entry;

        match self.get_note_type(node_index) {
            ForStat => {
                for_variable_entry = Some(
                    self.symbol_table_arena.get_symbol_table_entries(
                        self.get_element(node_index).symbol_table.unwrap(),
                    )[0],
                );
            }
            VarDecl => {
                let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                let symbol_entry = self
                    .symbol_table_arena
                    .get_symbol_table_entry(entry_index)
                    .clone();
                variable_map.insert(symbol_entry.name, true);
            }
            VarElementList => {
                //Check first variable in list
                let first_child_index = self.get_children(node_index)[0];

                //Assign first token to the list (need to do to it before other borrows).
                self.get_mut_element(node_index).token =
                    Some(self.get_child_token(first_child_index, 0).clone());

                let first_child_element = self.get_element(first_child_index);
                let id_index = self.get_children(first_child_index)[0];
                let symbol_name = self.get_lexeme(id_index);

                let (mut previous_type, mut previous_table_index) = match first_child_element
                    .node_type
                {
                    NodeType::DataMember => {
                        let variable_name = symbol_name;

                        let is_for_variable = match for_variable_entry {
                            Some(for_variable_entry) => {
                                let for_variable_name = self
                                    .symbol_table_arena
                                    .get_symbol_table_entry(for_variable_entry)
                                    .clone()
                                    .name;
                                variable_name == for_variable_name
                            }
                            None => false,
                        };

                        if is_for_variable {
                            // We have the for variable.
                            self.get_mut_element(first_child_index).symbol_table_entry =
                                Some(for_variable_entry.unwrap());
                            match self
                                .symbol_table_arena
                                .get_symbol_table_entry(for_variable_entry.unwrap())
                                .clone()
                                .kind
                            {
                                SymbolKind::Variable(symbol_type) => match symbol_type {
                                    SymbolType::Integer(dimensions) => {
                                        (SymbolType::Integer(dimensions), None)
                                    }
                                    SymbolType::Float(dimensions) => {
                                        (SymbolType::Float(dimensions), None)
                                    }
                                    SymbolType::Class(class_name, dimensions) => (
                                        SymbolType::Class(class_name.clone(), dimensions),
                                        self.find_class_symbol_table(&class_name),
                                    ),
                                },
                                _ => unreachable!(),
                            }
                        } else {
                            let is_local_variable = match scope_index {
                                Some(table_index) => {
                                    match self.is_member_variable(table_index, &variable_name) {
                                        Some(entry_index) => {
                                            self.get_mut_element(first_child_index)
                                                .symbol_table_entry = Some(entry_index);
                                            false
                                        }
                                        None => true,
                                    }
                                }
                                None => true,
                            };

                            if is_local_variable {
                                if variable_map.contains_key(&variable_name) {
                                    if !variable_map[&variable_name] {
                                        semantic_errors.push(
                                            SemanticError::VariableUsedBeforeBeingDeclared(
                                                self.get_leftmost_token(first_child_index),
                                                self.symbol_table_arena
                                                    .get_symbol_table(function_table_index)
                                                    .name
                                                    .clone(),
                                            ),
                                        );
                                        return;
                                    } else {
                                        let entry_index = self
                                            .find_variable_in_table(
                                                function_table_index,
                                                &variable_name,
                                            )
                                            .unwrap();
                                        self.get_mut_element(first_child_index)
                                            .symbol_table_entry = Some(entry_index);
                                        self.get_valid_class_table_from_entry(entry_index).unwrap()
                                    }
                                } else {
                                    semantic_errors.push(SemanticError::UndefinedLocalVariable(
                                        self.get_leftmost_token(first_child_index),
                                        self.symbol_table_arena
                                            .get_symbol_table(function_table_index)
                                            .name
                                            .clone(),
                                    ));
                                    return;
                                }
                            } else {
                                self.get_valid_class_table_from_entry(
                                    self.is_member_variable(scope_index.unwrap(), &variable_name)
                                        .unwrap(),
                                )
                                .unwrap()
                            }
                        }
                    }
                    NodeType::FunctionCall => {
                        let function_name = symbol_name;
                        match scope_index {
                            Some(table_index) => {
                                match self.is_member_function(table_index, &function_name) {
                                    Some(entry_index) => {
                                        self.get_mut_element(first_child_index)
                                            .symbol_table_entry = Some(entry_index);
                                        self.get_valid_class_table_from_entry(entry_index).unwrap()
                                    }
                                    None => match self.find_free_function(&function_name) {
                                        Some(entry_index) => {
                                            self.get_mut_element(first_child_index)
                                                .symbol_table_entry = Some(entry_index);
                                            self.get_valid_class_table_from_entry(entry_index)
                                                .unwrap()
                                        }
                                        None => {
                                            semantic_errors.push(SemanticError::UndefinedFunction(
                                                self.get_leftmost_token(first_child_index),
                                            ));
                                            return;
                                        }
                                    },
                                }
                            }
                            None => match self.find_free_function(&function_name) {
                                Some(entry_index) => {
                                    self.get_mut_element(first_child_index).symbol_table_entry =
                                        Some(entry_index);
                                    self.get_valid_class_table_from_entry(entry_index).unwrap()
                                }
                                None => {
                                    semantic_errors.push(SemanticError::UndefinedFreeFunction(
                                        self.get_leftmost_token(first_child_index),
                                    ));
                                    return;
                                }
                            },
                        }
                    }
                    _ => unreachable!(),
                };

                // Here, we have succesfully identified the first element of the list.
                // And we know it has a valid type.
                for child_index in self.get_children(node_index).to_vec().iter().skip(1) {
                    let current_symbol_name = self.get_child_lexeme(*child_index, 0);

                    let previous_element = match previous_table_index {
                        Some(previous_table_index) => match self.get_note_type(*child_index) {
                            NodeType::DataMember => {
                                let variable_name = current_symbol_name;
                                match self.is_member_variable(previous_table_index, &variable_name)
                                {
                                    Some(entry_index) => {
                                        self.get_mut_element(*child_index).symbol_table_entry =
                                            Some(entry_index);
                                        self.get_valid_class_table_from_entry(entry_index).unwrap()
                                    }
                                    None => {
                                        semantic_errors.push(
                                            SemanticError::UndefinedMemberVariable(
                                                self.get_leftmost_token(first_child_index),
                                                previous_type,
                                            ),
                                        );
                                        return;
                                    }
                                }
                            }
                            NodeType::FunctionCall => {
                                let function_name = current_symbol_name;
                                match self.is_member_function(previous_table_index, &function_name)
                                {
                                    Some(entry_index) => {
                                        self.get_mut_element(*child_index).symbol_table_entry =
                                            Some(entry_index);
                                        self.get_valid_class_table_from_entry(entry_index).unwrap()
                                    }
                                    None => {
                                        semantic_errors.push(
                                            SemanticError::UndefinedMemberFunction(
                                                self.get_leftmost_token(*child_index),
                                                previous_type,
                                            ),
                                        );
                                        return;
                                    }
                                }
                            }
                            _ => unreachable!(),
                        },
                        None => {
                            semantic_errors.push(SemanticError::DotOperatorWithInvalidClass(
                                self.get_leftmost_token(*child_index),
                                previous_type,
                            ));
                            return;
                        }
                    };
                    previous_type = previous_element.0;
                    previous_table_index = previous_element.1;
                }
            }
            _ => {}
        }

        for child_index in self.get_children(node_index).to_vec() {
            self.check_statement_block(
                semantic_errors,
                child_index,
                variable_map,
                scope_index,
                function_table_index,
                for_variable_entry,
            );
        }
    }

    fn get_variable_names_in_table(&self, table_index: usize) -> Vec<String> {
        use SymbolKind::*;

        let mut variable_names: Vec<String> = Vec::new();
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if let Variable(_) = symbol_entry.kind {
                variable_names.push(symbol_entry.name.clone());
            }
        }
        variable_names
    }

    fn get_parameter_names_in_table(&self, table_index: usize) -> Vec<String> {
        use SymbolKind::*;

        let mut variable_names: Vec<String> = Vec::new();
        for entry_index in self
            .symbol_table_arena
            .get_symbol_table_entries(table_index)
        {
            let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
            if let Parameter(_) = symbol_entry.kind {
                variable_names.push(symbol_entry.name.clone());
            }
        }
        variable_names
    }

    // // Return None if type is integer or float.
    // fn get_class_type_from_entry(&self, entry_index: usize) -> Option<String> {
    //     let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(entry_index);
    //     if let SymbolKind::Variable(symbol_type) = symbol_entry.kind {
    //         if let SymbolType::Class(type_name, _) = symbol_type {
    //             Some(type_name.clone())
    //         } else {
    //             None
    //         }
    //     } else {
    //         None
    //     }
    //     //None
    //     //
    // }
}
