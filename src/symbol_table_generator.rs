use crate::ast_node::*;
use crate::semantic_analysis::*;
use crate::symbol_table::*;
use crate::tree::*;

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

                // Add classes.
                for node_index in self.get_children_of_child(node_index, 0) {
                    let class_table_index = self.get_element(node_index).symbol_table.unwrap();
                    let class_name = self
                        .symbol_table_arena
                        .get_symbol_table(class_table_index)
                        .name
                        .clone();
                    let entry_index = self.get_element(node_index).symbol_table_entry.unwrap();
                    self.add_entry_to_table(table_index, entry_index)?;
                }

                // Add functions.
                // Here we assume that the list of classes is complete to validate types.
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
                        .token
                        .clone()
                    {
                        // If scope, link it in its class.
                        Some(token) => {
                            let class_name = token.lexeme.unwrap();
                            match self.find_class_symbol_table(&class_name) {
                                Some(class_table_index) => {
                                    match self
                                        .find_function_in_table(class_table_index, &function_name)
                                    {
                                        Some(function_declaration_entry_index) => {
                                            // Get a clone of the definition entry and remore link.
                                            let mut function_definition_entry =
                                                (*self.symbol_table_arena.get_symbol_table_entry(
                                                    function_declaration_entry_index,
                                                ))
                                                .clone();
                                            function_definition_entry.link = None;
                                            // Get a clone of the definition entry.
                                            let function_declaration_entry =
                                                (*self.symbol_table_arena.get_symbol_table_entry(
                                                    function_declaration_entry_index,
                                                ))
                                                .clone();
                                            // Compare declaration and definition, assumin neither has a link.
                                            if function_declaration_entry
                                                == function_definition_entry
                                            {
                                                self.symbol_table_arena
                                                    .get_mut_symbol_table_entry(
                                                        function_declaration_entry_index,
                                                    )
                                                    .link = Some(function_definition_table_index);
                                            } else {
                                                return Err(
                                                SemanticError::MemberFunctionDefDoesNotMatchDecl(
                                                    function_name,
                                                ),
                                            );
                                            }
                                        }
                                        None => {
                                            return Err(
                                                SemanticError::MemberFunctionDefDoesNotHaveDecl(
                                                    class_name,
                                                    function_name,
                                                ),
                                            );
                                        }
                                    }
                                }
                                None => {
                                    return Err(SemanticError::ClassNotFound(class_name));
                                }
                            }
                        }
                        // If no scope, add to global.
                        None => {
                            self.add_entry_to_table(
                                table_index,
                                function_definition_table_entry_index,
                            )?;
                        }
                    }

                    // Check if return value and parameters are valid
                    let function_symbol_enty = self
                        .symbol_table_arena
                        .get_symbol_table_entry(function_definition_table_entry_index);
                    //dbg!(&function_symbol_enty);
                    match function_symbol_enty.kind.clone() {
                        SymbolKind::Function(return_type, parameter_types) => {
                            if let Some(return_symbole_type) = return_type {
                                if let Err(class_name) =
                                    self.get_valid_class_table_from_type(&return_symbole_type)
                                {
                                    // FIXME: ADD ERROR ABOUT RETURN TYPE
                                    return Err(SemanticError::ClassNotFound(class_name));
                                }
                            }
                            for parameter_type in parameter_types {
                                if let Err(class_name) =
                                    self.get_valid_class_table_from_type(&parameter_type)
                                {
                                    // FIXME: ADD ERROR ABOUT PARAMETER TYPE
                                    return Err(SemanticError::ClassNotFound(class_name));
                                }
                            }
                        }
                        _ => unreachable!(),
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
                let name = self.get_child_lexeme(node_index, 0);

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
                // Function Definition has an entry and a table.
                let name = self.get_child_lexeme(node_index, 2);

                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                let return_type = self.make_type_from_child(node_index, 0, Vec::new());

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
                // Function Declaration just has an entry.
                let name = self.get_child_lexeme(node_index, 1);
                let return_type = self.make_type_from_child(node_index, 0, Vec::new());

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
                let name = self.get_child_lexeme(node_index, 1);
                let mut indices = Vec::new();
                for index in self.get_children_of_child(node_index, 2) {
                    indices.push(self.get_index(index));
                }
                let parameter_type = self.make_type_from_child(node_index, 0, indices);
                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Variable(parameter_type),
                    None,
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            FParam => {
                let name = self.get_child_lexeme(node_index, 1);
                let mut indices = Vec::new();
                for index in self.get_children_of_child(node_index, 2) {
                    indices.push(self.get_index(index));
                }
                let parameter_type = self.make_type_from_child(node_index, 0, indices);
                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Parameter(parameter_type),
                    None,
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            ForStat => {
                let symbol_type = self.make_type_from_child(node_index, 0, Vec::new());
                let name = self.get_child_lexeme(node_index, 1);
                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Variable(symbol_type),
                    None,
                );
                self.add_entry_to_table(table_index, entry_index)?;
                self.get_mut_element(node_index).symbol_table = Some(table_index);
            }
            _ => {}
        };
        Ok(())
    }

    fn make_type_from_child(
        &self,
        node_index: usize,
        child_index: usize,
        indices: Vec<usize>,
    ) -> SymbolType {
        SymbolType::new(
            &self
                .get_element(self.get_children(node_index)[child_index])
                .clone()
                .token
                .unwrap()
                .lexeme
                .unwrap(),
            indices,
        )
    }

    fn get_index(&self, node_index: usize) -> usize {
        self.get_element(node_index)
            .clone()
            .token
            .unwrap()
            .lexeme
            .unwrap()
            .parse::<usize>()
            .unwrap()
    }
}
