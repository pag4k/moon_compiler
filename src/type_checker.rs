use crate::ast_node::*;
use crate::language::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

impl AST {
    pub fn type_checker(&mut self) -> Vec<SemanticError> {
        let mut semantic_errors: Vec<SemanticError> = Vec::new();
        self.check_type(&mut semantic_errors, self.root.unwrap());
        semantic_errors
    }

    pub fn check_type(&mut self, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.check_type(semantic_errors, child_index);
        }

        match self.get_note_type(node_index) {
            Prog => {
                let table_index = self
                    .get_element(self.get_children(node_index)[2])
                    .symbol_table
                    .unwrap();
                self.check_statement_block_type(
                    semantic_errors,
                    self.get_children(node_index)[2],
                    None,
                    None,
                    table_index,
                    None,
                );
            }
            FuncDef => {
                let table_index = self.get_element(node_index).symbol_table.unwrap();
                let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                let scope_element = self.get_element(self.get_children(node_index)[1]);
                let scope_table_index = match scope_element.node_type {
                    // Here, we assume that we have already checked that the parent is valid
                    NodeType::Id => self.find_class_symbol_table(
                        &scope_element.token.clone().unwrap().lexeme.unwrap(),
                    ),
                    _ => None,
                };
                self.check_statement_block_type(
                    semantic_errors,
                    node_index,
                    scope_table_index,
                    Some(entry_index),
                    table_index,
                    None,
                );
            }
            _ => {}
        }
    }

    pub fn check_statement_block_type(
        &mut self,
        semantic_errors: &mut Vec<SemanticError>,
        node_index: usize,
        scope_index: Option<usize>,
        function_def_entry_index: Option<usize>,
        function_table_index: usize,
        for_variable_entry: Option<usize>,
    ) {
        use NodeType::*;
        use SymbolType::*;

        let for_variable_entry = match self.get_note_type(node_index) {
            ForStat => Some(
                self.symbol_table_arena
                    .get_symbol_table_entries(self.get_element(node_index).symbol_table.unwrap())
                    [0],
            ),
            _ => for_variable_entry,
        };

        for child_index in self.get_children(node_index).to_vec() {
            self.check_statement_block_type(
                semantic_errors,
                child_index,
                scope_index,
                function_def_entry_index,
                function_table_index,
                for_variable_entry,
            );
        }

        match self.get_note_type(node_index) {
            AssignStat | AssignStati => {
                let lhs = self.get_child_data_type(node_index, 0);
                let rhs = self.get_child_data_type(node_index, 1);
                let lhs_dimension_list = lhs.get_dimension_list();
                let rhs_dimension_list = rhs.get_dimension_list();

                if lhs_dimension_list != rhs_dimension_list {
                    semantic_errors.push(SemanticError::MismatchedTypeDimensions(
                        self.get_child_token(node_index, 0),
                        lhs,
                        rhs,
                    ));
                    return;
                }

                // Check if types match.
                // The only mismatch allowed is integer to float.
                if !match (lhs.clone(), rhs.clone()) {
                    (Integer(_), Integer(_)) => true,
                    (Float(_), Float(_)) => true,
                    (Float(_), Integer(_)) => true,
                    (Class(lhs_class_name, _), Class(rhs_class_name, _)) => {
                        lhs_class_name == rhs_class_name
                    }
                    _ => false,
                } {
                    semantic_errors.push(SemanticError::MismatchedTypes(
                        self.get_leftmost_token(node_index),
                        lhs,
                        rhs,
                    ));
                }
            }
            VarElementList => {
                //Check first variable in list
                let first_child_index = self.get_children(node_index)[0];
                let first_child_element = self.get_element(first_child_index);
                let id_index = self.get_children(first_child_index)[0];
                let symbol_name = self.get_lexeme(id_index);

                let mut previous_table_index = match first_child_element.node_type {
                    NodeType::DataMember => {
                        let variable_name = symbol_name;

                        if self.is_for_variable(for_variable_entry, &variable_name) {
                            // We have the for variable.
                            self.check_for_variable(node_index, for_variable_entry.unwrap())
                        } else {
                            let entry_index = self.get_variable_entry(
                                scope_index,
                                Some(function_table_index),
                                &variable_name,
                            );

                            match self.check_data_member(
                                node_index,
                                first_child_index,
                                entry_index,
                                0,
                            ) {
                                Ok(previous_table_index) => previous_table_index,
                                Err(error) => {
                                    semantic_errors.push(error);
                                    self.get_mut_element(node_index).data_type =
                                        Some(SymbolType::Integer(Vec::new()));
                                    return;
                                }
                            }
                        }
                    }
                    NodeType::FunctionCall => {
                        let function_name = symbol_name;
                        let entry_index = match scope_index {
                            Some(table_index) => {
                                match self.is_member_function(table_index, &function_name) {
                                    Some(entry_index) => entry_index,
                                    None => self.find_free_function(&function_name).unwrap(),
                                }
                            }
                            None => self.find_free_function(&function_name).unwrap(),
                        };
                        self.check_function_call(node_index, entry_index, 0)
                    }
                    _ => unreachable!(),
                };

                for (element_position, child_node) in self
                    .get_children(node_index)
                    .to_vec()
                    .iter()
                    .enumerate()
                    .skip(1)
                {
                    let current_symbol_name = self.get_child_lexeme(*child_node, 0);

                    previous_table_index = {
                        match self.get_note_type(*child_node) {
                            NodeType::DataMember => {
                                let variable_name = current_symbol_name;
                                let entry_index = self.get_variable_entry(
                                    previous_table_index,
                                    None,
                                    &variable_name,
                                );

                                match self.check_data_member(
                                    node_index,
                                    *child_node,
                                    entry_index,
                                    element_position,
                                ) {
                                    Ok(previous_table_index) => previous_table_index,
                                    Err(error) => {
                                        semantic_errors.push(error);
                                        self.get_mut_element(node_index).data_type =
                                            Some(SymbolType::Integer(Vec::new()));
                                        return;
                                    }
                                }
                            }
                            NodeType::FunctionCall => {
                                let function_name = current_symbol_name;
                                let entry_index = self
                                    .is_member_function(
                                        previous_table_index.unwrap(),
                                        &function_name,
                                    )
                                    .unwrap();
                                self.check_function_call(node_index, entry_index, element_position)
                            }
                            _ => unreachable!(),
                        }
                    };
                }
                if self.get_mut_element(node_index).data_type.is_none() {
                    dbg!(&self.get_leftmost_token(node_index));
                }
            }

            RelExpr => {
                //dbg!(self.g(node_index));
                let left_type = self.get_child_data_type(node_index, 0);
                let right_type = self.get_child_data_type(node_index, 2);
                let symbol_type = match (left_type.clone(), right_type.clone()) {
                    (Integer(_), Integer(_)) => SymbolType::Integer(Vec::new()),
                    (Float(_), Float(_)) => SymbolType::Integer(Vec::new()),
                    (Float(_), Integer(_)) => SymbolType::Integer(Vec::new()),
                    (Integer(_), Float(_)) => SymbolType::Integer(Vec::new()),
                    _ => {
                        semantic_errors.push(SemanticError::InvalidRelOperation(
                            self.get_leftmost_token(node_index),
                            left_type.clone(),
                            right_type,
                        ));
                        self.get_mut_element(node_index).data_type = Some(left_type);
                        return;
                    }
                };
                self.get_mut_element(node_index).data_type = Some(symbol_type);
            }
            AddOp => {
                let left_type = self.get_child_data_type(node_index, 0);
                let right_type = self.get_child_data_type(node_index, 1);
                let symbol_type = match (left_type.clone(), right_type.clone()) {
                    (Integer(_), Integer(_)) => left_type,
                    (Float(_), Float(_)) => left_type,
                    (Float(_), Integer(_)) => left_type,
                    (Integer(_), Float(_)) => right_type,
                    _ => {
                        semantic_errors.push(SemanticError::InvalidAddOperation(
                            self.get_token(node_index),
                            left_type.clone(),
                            right_type,
                        ));
                        self.get_mut_element(node_index).data_type = Some(left_type);
                        return;
                    }
                };
                self.get_mut_element(node_index).data_type = Some(symbol_type);
            }
            MultOp => {
                let left_type = self.get_child_data_type(node_index, 0);
                let right_type = self.get_child_data_type(node_index, 1);
                let symbol_type = match (left_type.clone(), right_type.clone()) {
                    (Integer(_), Integer(_)) => left_type,
                    (Float(_), Float(_)) => left_type,
                    (Float(_), Integer(_)) => left_type,
                    (Integer(_), Float(_)) => right_type,
                    _ => {
                        semantic_errors.push(SemanticError::InvalidMultOperation(
                            self.get_token(node_index),
                            left_type.clone(),
                            right_type,
                        ));
                        self.get_mut_element(node_index).data_type = Some(left_type);
                        return;
                    }
                };
                self.get_mut_element(node_index).data_type = Some(symbol_type);
            }

            Not | Sign => {
                let child_type = self.get_child_data_type(node_index, 0);
                self.get_mut_element(node_index).data_type = Some(child_type);
            }
            Num => {
                let symbol_type = match self.get_token_type(node_index) {
                    TokenType::IntNum => SymbolType::Integer(Vec::new()),
                    TokenType::FloatNum => SymbolType::Float(Vec::new()),
                    _ => unreachable!(),
                };
                self.get_mut_element(node_index).data_type = Some(symbol_type);
            }
            DataMember => {
                let invalid_index_position: Vec<(usize, SymbolType)> = self
                    .get_children_of_child(node_index, 1)
                    .iter()
                    .enumerate()
                    .map(|(index, child_index)| {
                        (
                            index,
                            self.get_element(*child_index).clone().data_type.unwrap(),
                        )
                    })
                    .filter(|(_, data_type)| match data_type {
                        SymbolType::Integer(_) => false,
                        _ => true,
                    })
                    .collect();
                for (index, _) in invalid_index_position.clone() {
                    semantic_errors.push(SemanticError::ArrayIndiceMustBeInteger(
                        self.get_leftmost_token(node_index),
                        invalid_index_position[index].clone(),
                    ));
                }
            }
            FunctionCall => {
                let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                let function_parameter_list = match self
                    .symbol_table_arena
                    .get_symbol_table_entry(entry_index)
                    .kind
                    .clone()
                {
                    SymbolKind::Function(_, function_parameter_list) => function_parameter_list,
                    _ => unreachable!(),
                };
                let argument_node_list = self.get_children_of_child(node_index, 1);
                let argument_parameter_list: Vec<SymbolType> = argument_node_list
                    .iter()
                    .map(|&child_index| self.get_element(child_index).data_type.clone().unwrap())
                    .collect();

                if function_parameter_list.len() != argument_parameter_list.len() {
                    semantic_errors.push(SemanticError::WrongNumberOfArguments(
                        self.get_child_token(node_index, 0),
                        function_parameter_list.len(),
                        argument_parameter_list.len(),
                    ));
                    return;
                }

                let invalid_arguments: Vec<(usize, SymbolType, SymbolType)> =
                    function_parameter_list
                        .iter()
                        .enumerate()
                        .filter_map(|(position, function_parameter)| {
                            let argument_parameter = argument_parameter_list[position].clone();
                            let function_parameter = function_parameter.clone();
                            if argument_parameter == function_parameter {
                                None
                            } else {
                                Some((position, argument_parameter, function_parameter))
                            }
                        })
                        .collect();

                for (index, _, _) in invalid_arguments.clone() {
                    semantic_errors.push(SemanticError::InvalidArgument(
                        self.get_child_token(node_index, 0),
                        invalid_arguments[index].clone(),
                    ));
                }
            }
            ReturnStat => {
                // dbg!(&self
                //     .symbol_table_arena
                //     .get_symbol_table_entry(function_def_entry_index.unwrap()));
                match function_def_entry_index {
                    Some(function_def_entry_index) => {
                        let function_return_type = match self
                            .symbol_table_arena
                            .get_symbol_table_entry(function_def_entry_index)
                            .kind
                            .clone()
                        {
                            SymbolKind::Function(return_type, _) => return_type,
                            _ => unreachable!(),
                        };

                        let return_type = self.get_child_data_type(node_index, 0);

                        match function_return_type {
                            Some(function_return_type) => {
                                if function_return_type != return_type {
                                    semantic_errors.push(
                                        SemanticError::ReturnTypeDoesNotMatchFuctionDeclaration(
                                            self.get_leftmost_token(node_index),
                                            function_return_type,
                                            return_type,
                                        ),
                                    );
                                }
                            }
                            // There will always be a return type since the main function is handled differently.
                            None => unreachable!(),
                        }
                    }
                    None => {
                        semantic_errors.push(SemanticError::ShouldNotReturnFromMain(
                            self.get_leftmost_token(node_index),
                        ));
                    }
                }
            }
            _ => {}
        }
    }

    fn get_token_type(&self, node_index: usize) -> TokenType {
        self.get_element(node_index)
            .clone()
            .token
            .unwrap()
            .token_type
    }

    fn check_data_member(
        &mut self,
        node_index: usize,
        child_node: usize,
        entry_index: usize,
        element_position: usize,
    ) -> Result<Option<usize>, SemanticError> {
        let (previous_symbol_type, previous_table_index) =
            self.get_valid_class_table_from_entry(entry_index).unwrap();
        if element_position + 1 == self.get_children(node_index).len() {
            if !self.is_array_type(entry_index, child_node)? {
                self.check_number_of_dimensions(entry_index, child_node)?;
                // Set data type of the whole list.
                self.get_mut_element(node_index).data_type =
                    Some(previous_symbol_type.clone().remove_dimensions());
            } else {
                // Set data type of the whole list.
                self.get_mut_element(node_index).data_type = Some(previous_symbol_type.clone());
            }
        } else {
            self.check_number_of_dimensions(entry_index, child_node)?;
        }

        Ok(previous_table_index)
    }

    fn get_variable_entry(
        &self,
        previous_table_index: Option<usize>,
        function_table_index: Option<usize>,
        variable_name: &str,
    ) -> usize {
        if match previous_table_index {
            Some(table_index) => self
                .is_member_variable(table_index, variable_name)
                .is_none(),
            None => true,
        } {
            self.find_variable_in_table(function_table_index.unwrap(), &variable_name)
                .unwrap()
        } else {
            self.is_member_variable(previous_table_index.unwrap(), &variable_name)
                .unwrap()
        }
    }

    fn is_for_variable(&self, for_variable_entry: Option<usize>, variable_name: &str) -> bool {
        match for_variable_entry {
            Some(for_variable_entry) => {
                let for_variable_name = self
                    .symbol_table_arena
                    .get_symbol_table_entry(for_variable_entry)
                    .clone()
                    .name;
                variable_name == for_variable_name
            }
            None => false,
        }
    }

    fn check_for_variable(&mut self, node_index: usize, entry_index: usize) -> Option<usize> {
        let (previous_symbol_type, previous_table_index) = match self
            .symbol_table_arena
            .get_symbol_table_entry(entry_index)
            .clone()
            .kind
        {
            SymbolKind::Variable(symbol_type) => match symbol_type {
                SymbolType::Integer(_) => (SymbolType::Integer(Vec::new()), None),
                SymbolType::Float(_) => (SymbolType::Float(Vec::new()), None),
                SymbolType::Class(class_name, _) => (
                    SymbolType::Class(class_name.clone(), Vec::new()),
                    self.find_class_symbol_table(&class_name),
                ),
            },
            _ => unreachable!(),
        };
        if self.get_children(node_index).len() == 1 {
            // Set data type of the whole list.
            self.get_mut_element(node_index).data_type =
                Some(previous_symbol_type.clone().remove_dimensions());
        }
        previous_table_index
    }

    fn check_function_call(
        &mut self,
        node_index: usize,
        entry_index: usize,
        element_position: usize,
    ) -> Option<usize> {
        let (previous_symbol_type, previous_table_index) =
            self.get_valid_class_table_from_entry(entry_index).unwrap();
        if element_position + 1 == self.get_children(node_index).len() {
            // Set data type of the whole list.
            self.get_mut_element(node_index).data_type =
                Some(previous_symbol_type.clone().remove_dimensions());
        }
        previous_table_index
    }
}
