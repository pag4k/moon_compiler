use crate::ast_node::*;
use crate::language::*;
use crate::semantic_analysis::*;
use crate::symbol_table::*;
use crate::tree::*;

use std::collections::HashMap;
use std::str::FromStr;

impl Tree<NodeElement, SymbolTableArena> {
    pub fn semantic_function_checker(&mut self) -> Result<(), SemanticError> {
        //FIXME: Verify if there is a root node.
        self.check_function(self.root.unwrap())?;
        Ok(())
    }

    fn check_function(&mut self, node_index: usize) -> Result<(), SemanticError> {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.check_function(child_index)?;
        }

        match self.get_element(node_index).node_type {
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
                    self.get_children(node_index)[2],
                    &mut variable_map,
                    None,
                    table_index,
                    None,
                )?;
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
                    self.get_children(node_index)[4],
                    &mut variable_map,
                    scope_table_index,
                    table_index,
                    None,
                )?;
            }
            VarDecl => {
                //let entry_index = self.symbol_table_arena.get
                //let type_node_index = self.get_children(node_index)[0];
                let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                if let Err(type_name) = self.get_valid_class_table_from_entry(entry_index) {
                    return Err(SemanticError::UndefinedClass(type_name));
                }
            }
            VarElementList => {}
            _ => {}
        }
        Ok(())
    }

    fn check_statement_block(
        &mut self,
        node_index: usize,
        variable_map: &mut HashMap<String, bool>,
        scope_index: Option<usize>,
        function_index: usize,
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
                let first_child_element = self.get_element(first_child_index);
                let id_index = self.get_children(first_child_index)[0];
                let symbol_name = self.get_lexeme(id_index);

                let (mut previous_symbol_type, mut previous_table_index) = match first_child_element
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
                                    SymbolType::Class(class_name, dimensions) => {
                                        match self.find_class_symbol_table(&class_name) {
                                            Some(entry_index) => (
                                                SymbolType::Class(class_name.clone(), dimensions),
                                                Some(entry_index),
                                            ),
                                            None => {
                                                return Err(SemanticError::UndefinedClass(
                                                    class_name.clone(),
                                                ));
                                            }
                                        }
                                    }
                                },
                                _ => unreachable!(),
                            }
                        } else {
                            let is_local_variable = match scope_index {
                                Some(table_index) => match self
                                    .is_member_variable(table_index, &variable_name)
                                {
                                    Some(entry_index) => {
                                        match self.get_valid_class_table_from_entry(entry_index) {
                                            Ok(_) => {
                                                self.get_mut_element(first_child_index)
                                                    .symbol_table_entry = Some(entry_index);
                                                false
                                            }
                                            Err(type_name) => {
                                                return Err(SemanticError::UndefinedClass(
                                                    type_name,
                                                ));
                                            }
                                        }
                                    }
                                    None => true,
                                },
                                None => true,
                            };

                            if is_local_variable {
                                if variable_map.contains_key(&variable_name) {
                                    if !variable_map[&variable_name] {
                                        return Err(SemanticError::VariableUsedBeforeBeingDeclared(
                                            variable_name,
                                        ));
                                    } else {
                                        match self
                                            .find_variable_in_table(function_index, &variable_name)
                                        {
                                            Some(entry_index) => {
                                                match self
                                                    .get_valid_class_table_from_entry(entry_index)
                                                {
                                                    Ok(result) => {
                                                        self.get_mut_element(first_child_index)
                                                            .symbol_table_entry = Some(entry_index);
                                                        result
                                                    }
                                                    Err(type_name) => {
                                                        return Err(SemanticError::UndefinedClass(
                                                            type_name.clone(),
                                                        ));
                                                    }
                                                }
                                            }
                                            None => unreachable!(),
                                        }
                                    }
                                } else {
                                    return Err(SemanticError::UndefinedLocalVariable(
                                        variable_name,
                                    ));
                                }
                            } else {
                                // Unwrap everything since it was already checked above.
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
                                        match self.get_valid_class_table_from_entry(entry_index) {
                                            Ok(result) => {
                                                self.get_mut_element(first_child_index)
                                                    .symbol_table_entry = Some(entry_index);
                                                result
                                            }
                                            Err(type_name) => {
                                                // FIXME: Error about return type
                                                return Err(SemanticError::UndefinedClass(
                                                    type_name.clone(),
                                                ));
                                            }
                                        }
                                    }
                                    None => match self.find_free_function(&function_name) {
                                        Some(entry_index) => {
                                            match self.get_valid_class_table_from_entry(entry_index)
                                            {
                                                Ok(result) => {
                                                    self.get_mut_element(first_child_index)
                                                        .symbol_table_entry = Some(entry_index);
                                                    result
                                                }
                                                Err(type_name) => {
                                                    // FIXME: Error about return type
                                                    return Err(SemanticError::UndefinedClass(
                                                        type_name.clone(),
                                                    ));
                                                }
                                            }
                                        }
                                        None => {
                                            return Err(SemanticError::UndefinedFunction(
                                                function_name,
                                            ));
                                        }
                                    },
                                }
                            }
                            None => match self.find_free_function(&function_name) {
                                Some(entry_index) => {
                                    match self.get_valid_class_table_from_entry(entry_index) {
                                        Ok(result) => {
                                            self.get_mut_element(first_child_index)
                                                .symbol_table_entry = Some(entry_index);
                                            result
                                        }
                                        Err(type_name) => {
                                            // FIXME: Error about return type
                                            return Err(SemanticError::UndefinedClass(
                                                type_name.clone(),
                                            ));
                                        }
                                    }
                                }
                                None => {
                                    return Err(SemanticError::UndefinedFreeFunction(function_name));
                                }
                            },
                        }
                    }
                    _ => unreachable!(),
                };

                // If None, we have a integer or a float.

                // Here, we have succesfully identified the first element of the list.
                // And we know it has a valid type.
                for child_index in self.get_children(node_index).to_vec().iter().skip(1) {
                    let current_symbol_name = self.get_child_lexeme(*child_index, 0);

                    let previous_element = match previous_table_index {
                        Some(previous_table_index) => {
                            //FIXME: I'm not sure if I checked all possible class types here.
                            //Case 1: Member variable: Need to check at var declaration. Not sure if this is done before here.
                            // I THINK IT IS CHECKED ABOVE SINCE I CHECK CLASSES BEFORE FUNCTIONS AND PROG
                            //Case 2: Return variable of function. Again, not sure if it is properly checked here.
                            // DONE DURING GENERATION
                            match self.get_element(*child_index).node_type {
                                NodeType::DataMember => {
                                    let variable_name = current_symbol_name;
                                    match self
                                        .is_member_variable(previous_table_index, &variable_name)
                                    {
                                        Some(entry_index) => {
                                            match self.get_valid_class_table_from_entry(entry_index)
                                            {
                                                Ok(result) => {
                                                    self.get_mut_element(*child_index)
                                                        .symbol_table_entry = Some(entry_index);
                                                    result
                                                }
                                                Err(type_name) => {
                                                    return Err(SemanticError::UndefinedClass(
                                                        type_name.clone(),
                                                    ));
                                                }
                                            }
                                        }
                                        None => {
                                            return Err(SemanticError::UndefinedMemberVariable(
                                                variable_name,
                                            ));
                                        }
                                    }
                                }
                                NodeType::FunctionCall => {
                                    let function_name = current_symbol_name;
                                    match self
                                        .is_member_function(previous_table_index, &function_name)
                                    {
                                        Some(entry_index) => {
                                            // dbg!(&self
                                            //     .symbol_table_arena
                                            //     .get_symbol_table_entry(entry_index));
                                            match self.get_valid_class_table_from_entry(entry_index)
                                            {
                                                Ok(result) => {
                                                    self.get_mut_element(*child_index)
                                                        .symbol_table_entry = Some(entry_index);
                                                    result
                                                }
                                                Err(type_name) => {
                                                    return Err(SemanticError::UndefinedClass(
                                                        type_name.clone(),
                                                    ));
                                                }
                                            }
                                        }
                                        None => {
                                            return Err(SemanticError::UndefinedMemberFunction(
                                                function_name,
                                            ));
                                        }
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }
                        None => {
                            //Trying to access members of float or integer.
                            //FIXME: I think I'll need te previous element to output error message.
                            return Err(SemanticError::NumericalValuesHaveNoMember(
                                current_symbol_name,
                            ));
                        }
                    };
                    previous_symbol_type = previous_element.0;
                    previous_table_index = previous_element.1;
                }
                self.get_mut_element(node_index).data_type = Some(previous_symbol_type);
            }
            _ => {}
        }

        for child_index in self.get_children(node_index).to_vec() {
            self.check_statement_block(
                child_index,
                variable_map,
                scope_index,
                function_index,
                for_variable_entry,
            )?;
        }

        Ok(())
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
