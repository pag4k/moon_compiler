use crate::ast_node::*;
use crate::language::*;
use crate::semantic_analysis::*;
use crate::symbol_table::*;
use crate::tree::*;

impl Tree<NodeElement, SymbolTableArena> {
    pub fn type_checker(&mut self) -> Result<(), SemanticError> {
        //FIXME: Check if there is a root
        self.check_type(self.root.unwrap())
    }

    pub fn check_type(&mut self, node_index: usize) -> Result<(), SemanticError> {
        use NodeType::*;
        use SymbolType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.check_type(child_index)?;
        }

        match self.get_element(node_index).node_type {
            Prog => {
                let table_index = self
                    .get_element(self.get_children(node_index)[2])
                    .symbol_table
                    .unwrap();
                self.check_statement_block_type(
                    self.get_children(node_index)[2],
                    None,
                    None,
                    table_index,
                    None,
                )?;
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
                    node_index,
                    scope_table_index,
                    Some(entry_index),
                    table_index,
                    None,
                )?;
            }
            ForStat => {
                //TODO!
                // for_variable = Some((
                //     self.get_child_lexeme(node_index, 0),
                //     self.get_child_lexeme(node_index, 1),
                // ));
            }
            AssignStat | AssignStati => {
                let lhs = self.get_child_data_type(node_index, 0);
                let rhs = self.get_child_data_type(node_index, 1);
                match (lhs.clone(), rhs.clone()) {
                    (Integer(_), Integer(_)) => {}
                    (Float(_), Float(_)) => {}
                    (Float(_), Integer(_)) => {}
                    (Integer(_), Float(_)) => {
                        return Err(SemanticError::CannotAssignFloatToInteger);
                    }
                    (Class(lhs_class_name, _), Class(rhs_class_name, _)) => {
                        if lhs_class_name != rhs_class_name {
                            return Err(SemanticError::CannotAssignClasseToADifferentOne(
                                lhs_class_name,
                                rhs_class_name,
                            ));
                        }
                    }
                    (Class(lhs_class_name, _), Integer(_))
                    | (Class(lhs_class_name, _), Float(_)) => {
                        return Err(SemanticError::CannotAssignNumericalValueToClass(
                            lhs_class_name,
                        ));
                    }
                    (Integer(_), Class(rhs_class_name, _))
                    | (Float(_), Class(rhs_class_name, _)) => {
                        return Err(SemanticError::CannotAssignClassToNumericalValue(
                            rhs_class_name,
                        ));
                    }
                }
            }
            RelExpr => {
                let left_type = self.get_child_data_type(node_index, 0);
                let right_type = self.get_child_data_type(node_index, 2);
                let symbol_type = match (left_type, right_type) {
                    (Integer(_), Integer(_)) => SymbolType::Integer(Vec::new()),
                    (Float(_), Float(_)) => SymbolType::Integer(Vec::new()),
                    (Float(_), Integer(_)) => SymbolType::Integer(Vec::new()),
                    (Integer(_), Float(_)) => SymbolType::Integer(Vec::new()),
                    _ => {
                        return Err(SemanticError::InvalidRelOperation);
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
                        return Err(SemanticError::InvalidAddOperation);
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
                        return Err(SemanticError::InvalidMultOperation);
                    }
                };
                self.get_mut_element(node_index).data_type = Some(symbol_type);
            }
            VarElementList => {}
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
            _ => {}
        }

        Ok(())
    }

    pub fn check_statement_block_type(
        &mut self,
        node_index: usize,
        scope_index: Option<usize>,
        function_def_entry_index: Option<usize>,
        function_table_index: usize,
        for_variable_entry: Option<usize>,
    ) -> Result<(), SemanticError> {
        use NodeType::*;

        let mut for_variable_entry = for_variable_entry;

        match self.get_element(node_index).node_type {
            ForStat => {
                for_variable_entry = Some(
                    self.symbol_table_arena.get_symbol_table_entries(
                        self.get_element(node_index).symbol_table.unwrap(),
                    )[0],
                );
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
                let argument_parameter_list: Vec<SymbolType> = self
                    .get_children_of_child(node_index, 1)
                    .iter()
                    .map(|&child_index| self.get_element(child_index).data_type.clone().unwrap())
                    .collect();

                //if function_parameter_list.iter().zip().all(|(parameter,argument)| )
            }
            ReturnStat => {
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
                                    return Err(
                                        SemanticError::ReturnTypeDoesNotMatchFuctionDeclaration(
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
                        return Err(SemanticError::ShouldNotReturnFromMain);
                    }
                }
            }
            _ => {}
        }

        for child_index in self.get_children(node_index).to_vec() {
            self.check_statement_block_type(
                child_index,
                scope_index,
                function_def_entry_index,
                function_table_index,
                for_variable_entry,
            )?;
        }

        Ok(())
    }

    fn get_token_type(&self, node_index: usize) -> TokenType {
        self.get_element(node_index)
            .clone()
            .token
            .unwrap()
            .token_type
    }

    pub fn get_child_data_type(&self, node_index: usize, child_index: usize) -> SymbolType {
        self.get_element(self.get_children(node_index)[child_index])
            .clone()
            .data_type
            .unwrap()
    }
}
