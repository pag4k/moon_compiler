use crate::ast_node::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

impl AST {
    pub fn generate_symbol_table(&mut self) -> Vec<SemanticError> {
        let mut semantic_errors: Vec<SemanticError> = Vec::new();
        self.create_symbol_table(&mut semantic_errors, self.root.unwrap());
        semantic_errors
    }

    fn create_symbol_table(&mut self, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.create_symbol_table(semantic_errors, child_index);
        }

        match self.get_note_type(node_index) {
            Prog => {
                let table_index = self
                    .symbol_table_arena
                    .new_symbol_table("Global".to_string());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                // Set root.
                self.symbol_table_arena.root = Some(table_index);

                // Add classes.
                for node_index in self.get_children_of_child(node_index, 0) {
                    if let Err(error) = self.add_entry_to_table(table_index, node_index) {
                        semantic_errors.push(error);
                    }
                }

                // Add functions.
                // Here we assume that the list of classes is complete to validate types.
                for function_node_index in self.get_children_of_child(node_index, 1) {
                    if let Err(error) = self.add_function(function_node_index, table_index) {
                        semantic_errors.push(error);
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
                self.get_mut_element(block_node_index).symbol_table_entry = Some(entry_index);
                if let Err(error) = self.add_entry_to_table(table_index, block_node_index) {
                    semantic_errors.push(error);
                }

                // Get variables
                for variable_index in self.get_children(block_node_index).to_vec() {
                    if self
                        .get_mut_element(variable_index)
                        .symbol_table_entry
                        .is_some()
                    {
                        if let Err(error) =
                            self.add_entry_to_table(program_table_index, variable_index)
                        {
                            semantic_errors.push(error);
                        }
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

                // Get members
                for member_index in self.get_children_of_child(node_index, 2) {
                    if self.get_element(member_index).symbol_table_entry.is_some() {
                        if let Err(error) = self.add_entry_to_table(table_index, member_index) {
                            semantic_errors.push(error);
                        }
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
                    if let Err(error) = self.add_entry_to_table(table_index, parameter_index) {
                        semantic_errors.push(error);
                    }
                }

                // Get variables
                for variable_index in self.get_children_of_child(node_index, 4) {
                    if self
                        .get_mut_element(variable_index)
                        .symbol_table_entry
                        .is_some()
                    {
                        if let Err(error) = self.add_entry_to_table(table_index, variable_index) {
                            semantic_errors.push(error);
                        }
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
                let first_child_index = self.get_children(node_index)[0];

                self.get_mut_element(first_child_index).symbol_table_entry = Some(entry_index);
                if let Err(error) = self.add_entry_to_table(table_index, first_child_index) {
                    semantic_errors.push(error);
                }
                self.get_mut_element(node_index).symbol_table = Some(table_index);
            }
            _ => {}
        };
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

    fn add_function(
        &mut self,
        function_node_index: usize,
        table_index: usize,
    ) -> Result<(), SemanticError> {
        let function_definition_table_index =
            self.get_element(function_node_index).symbol_table.unwrap();
        let function_definition_table_entry_index = self
            .get_element(function_node_index)
            .symbol_table_entry
            .unwrap();
        let function_name = self
            .symbol_table_arena
            .get_symbol_table(function_definition_table_index)
            .name
            .clone();

        match self
            .get_element(self.get_children(function_node_index)[1])
            .token
            .clone()
        {
            // If scope, link it in its class.
            Some(token) => {
                let class_name = token.lexeme.clone().unwrap();
                let class_table_index = match self.find_class_symbol_table(&class_name) {
                    Some(class_table_index) => class_table_index,
                    None => {
                        return Err(SemanticError::UndefinedClass(
                            self.get_token(self.get_children(function_node_index)[1]),
                        ));
                    }
                };
                match self.find_function_in_table(class_table_index, &function_name) {
                    Some(function_declaration_entry_index) => {
                        // Get a clone of the definition entry and remore link.
                        let mut function_definition_entry = (*self
                            .symbol_table_arena
                            .get_symbol_table_entry(function_definition_table_entry_index))
                        .clone();
                        function_definition_entry.link = None;
                        // Get a clone of the definition entry.
                        let function_declaration_entry = (*self
                            .symbol_table_arena
                            .get_symbol_table_entry(function_declaration_entry_index))
                        .clone();
                        // Compare declaration and definition, assumin neither has a link.
                        if function_declaration_entry == function_definition_entry {
                            self.symbol_table_arena
                                .get_mut_symbol_table_entry(function_declaration_entry_index)
                                .link = Some(function_definition_table_index);
                            return Ok(());
                        } else {
                            return Err(SemanticError::MismatchMemberFunctionDeclAndDef(
                                self.get_leftmost_token(function_node_index),
                                function_name.clone(),
                                function_declaration_entry.kind,
                                function_definition_entry.kind,
                            ));
                        }
                    }
                    None => {
                        return Err(SemanticError::MemberFunctionDefDoesNotHaveDecl(
                            self.get_leftmost_token(function_node_index),
                            function_name.clone(),
                        ));
                    }
                }
            }
            // If no scope, add to global.
            None => {
                self.add_entry_to_table(table_index, function_node_index)?;
            }
        }
        Ok(())
    }
}
