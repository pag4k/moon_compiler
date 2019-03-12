use crate::ast_node::*;
use crate::semantic_analysis::*;
use crate::symbol_table::*;
use crate::tree::*;

use std::collections::HashMap;

impl Tree<NodeElement, SymbolTableArena> {
    pub fn generate_symbol_table(&mut self) -> Result<Vec<SemanticWarning>, SemanticError> {
        //FIXME: Verify if there is a root node.
        let mut semantic_warnings: Vec<SemanticWarning> = Vec::new();
        self.create_symbol_table(&mut semantic_warnings, self.root.unwrap())?;
        Ok(semantic_warnings.to_vec())
    }

    fn create_symbol_table(
        &mut self,
        semantic_warnings: &mut Vec<SemanticWarning>,
        node_index: usize,
    ) -> Result<(), SemanticError> {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.create_symbol_table(semantic_warnings, child_index)?;
        }

        match self.get_element(node_index).node_type {
            Prog => {
                let table_index = self
                    .symbol_table_arena
                    .new_symbol_table("Global".to_string());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                // Set root.
                self.symbol_table_arena.root = Some(table_index);

                let mut class_table_map: HashMap<String, usize> = HashMap::new();

                // Add classes.
                for node_index in self.get_children_of_child(node_index, 0) {
                    let class_table_index = self.get_element(node_index).symbol_table.unwrap();
                    let class_name = self
                        .symbol_table_arena
                        .get_symbol_table(class_table_index)
                        .name
                        .clone();
                    class_table_map.insert(class_name, class_table_index);
                    let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                    self.add_entry_to_table(table_index, entry_index)?;
                }

                // Add functions.
                for node_index in self.get_children_of_child(node_index, 1) {
                    let function_definition_table_index =
                        self.get_element(node_index).symbol_table.unwrap();
                    let function_definition_table_entry_index =
                        self.get_element(node_index).symbol_table_entry.unwrap();
                    let function_name = self
                        .symbol_table_arena
                        .get_symbol_table(function_definition_table_index)
                        .name
                        .clone();
                    match self
                        .get_element(self.get_children(node_index)[1])
                        .data
                        .clone()
                    {
                        // If scope, link it in its class.
                        Some(class_name) => match class_table_map.get(&class_name) {
                            Some(class_table_index) => {
                                match self
                                    .find_function_in_class(*class_table_index, &function_name)
                                {
                                    Some(function_declaration_entry_index) => {
                                        let mut function_definition_entry =
                                            (*self.symbol_table_arena.get_symbol_table_entry(
                                                function_declaration_entry_index,
                                            ))
                                            .clone();
                                        function_definition_entry.link = None;
                                        let function_declaration_entry =
                                            (*self.symbol_table_arena.get_symbol_table_entry(
                                                function_declaration_entry_index,
                                            ))
                                            .clone();
                                        // Compare declaration and definition, assumin neither has a link.
                                        if function_declaration_entry == function_definition_entry {
                                            self.symbol_table_arena
                                                .get_mut_symbol_table_entry(
                                                    function_declaration_entry_index,
                                                )
                                                .link = Some(function_definition_table_index);
                                        } else {
                                            return Err(SemanticError::FunctionDefDoesNotMatchDecl(
                                                function_name,
                                            ));
                                        }
                                    }
                                    None => {
                                        return Err(SemanticError::FunctionNotFound(
                                            class_name,
                                            function_name,
                                        ));
                                    }
                                }
                            }
                            None => {
                                return Err(SemanticError::ClassNotFound(class_name));
                            }
                        },
                        // If no scope, add to global.
                        None => {
                            self.add_entry_to_table(
                                table_index,
                                function_definition_table_entry_index,
                            )?;
                        }
                    }
                }

                // Add program on StatBlock. SPECIAL CASE!
                let program_table_index = self
                    .symbol_table_arena
                    .new_symbol_table("Program".to_string());

                let block_node_index = self.get_children(node_index)[2];
                self.get_mut_element(block_node_index).symbol_table = Some(program_table_index);

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    "Program".to_string(),
                    SymbolKind::Function(None, Vec::new()),
                    Some(program_table_index),
                );
                self.add_entry_to_table(table_index, entry_index)?;
                self.get_mut_element(block_node_index).symbol_table_entry = Some(entry_index);

                // Get variables
                for variable_index in self.get_children(block_node_index).to_vec() {
                    if let Some(entry_index) =
                        self.get_mut_element(variable_index).symbol_table_entry
                    {
                        self.add_entry_to_table(program_table_index, entry_index)?;
                    }
                }
            }
            ClassDecl => {
                let name = self.get_name(node_index, 0);

                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name.clone(),
                    SymbolKind::Class,
                    Some(table_index),
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);

                // Get variables
                for variable_index in self.get_children_of_child(node_index, 2) {
                    if let Some(entry_index) =
                        self.get_mut_element(variable_index).symbol_table_entry
                    {
                        self.add_entry_to_table(table_index, entry_index)?;
                    }
                }
            }
            FuncDef => {
                let name = self.get_name(node_index, 2);

                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                let return_type = self.get_type(node_index, 0, Vec::new());

                // Get parameters
                let mut parameters = Vec::new();
                for parameter_index in self.get_children_of_child(node_index, 3) {
                    let entry_index = self
                        .get_mut_element(parameter_index)
                        .symbol_table_entry
                        .unwrap();
                    let parameter_type = match self
                        .symbol_table_arena
                        .get_symbol_table_entry(entry_index)
                        .kind
                        .clone()
                    {
                        SymbolKind::Parameter(parameter_type) => parameter_type,
                        _ => unreachable!(),
                    };

                    parameters.push(parameter_type.clone());
                    self.add_entry_to_table(table_index, entry_index)?;
                }

                // Get variables
                for variable_index in self.get_children_of_child(node_index, 4) {
                    if let Some(entry_index) =
                        self.get_mut_element(variable_index).symbol_table_entry
                    {
                        self.add_entry_to_table(table_index, entry_index)?;
                    }
                }

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Function(Some(return_type), parameters),
                    Some(table_index),
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            FuncDecl => {
                let name = self.get_name(node_index, 1);
                let return_type = self.get_type(node_index, 0, Vec::new());

                // Get parameters
                let mut parameters = Vec::new();
                for parameter_index in self.get_children_of_child(node_index, 2) {
                    let entry_index = self
                        .get_mut_element(parameter_index)
                        .symbol_table_entry
                        .unwrap();
                    let parameter_type = match self
                        .symbol_table_arena
                        .get_symbol_table_entry(entry_index)
                        .kind
                        .clone()
                    {
                        SymbolKind::Parameter(parameter_type) => parameter_type,
                        _ => unreachable!(),
                    };

                    parameters.push(parameter_type.clone());
                }

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Function(Some(return_type), parameters),
                    None,
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            VarDecl => {
                let name = self.get_name(node_index, 1);
                let mut indices = Vec::new();
                for index in self.get_children_of_child(node_index, 2) {
                    indices.push(self.get_index(index));
                }
                let parameter_type = self.get_type(node_index, 0, indices);
                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Variable(parameter_type),
                    None,
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            FParam => {
                let name = self.get_name(node_index, 1);
                let mut indices = Vec::new();
                for index in self.get_children_of_child(node_index, 2) {
                    indices.push(self.get_index(index));
                }
                let parameter_type = self.get_type(node_index, 0, indices);
                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Parameter(parameter_type),
                    None,
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            _ => {}
        };
        Ok(())
    }

    fn get_type(&self, node_index: usize, child_index: usize, indices: Vec<usize>) -> SymbolType {
        SymbolType::new(
            &self
                .get_element(self.get_children(node_index)[child_index])
                .clone()
                .data
                .unwrap(),
            indices,
        )
    }

    fn get_index(&self, node_index: usize) -> usize {
        self.get_element(node_index)
            .clone()
            .data
            .unwrap()
            .parse::<usize>()
            .unwrap()
    }
}
