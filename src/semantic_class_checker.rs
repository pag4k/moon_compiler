use crate::ast_node::*;
use crate::semantic_error::*;
use crate::symbol_table::*;
use crate::tree::*;

impl AST {
    pub fn semantic_class_checker(&mut self) -> (Vec<SemanticWarning>, Vec<SemanticError>) {
        let mut semantic_warnings: Vec<SemanticWarning> = Vec::new();
        let mut semantic_errors: Vec<SemanticError> = Vec::new();
        self.check_class(
            &mut semantic_warnings,
            &mut semantic_errors,
            self.root.unwrap(),
        );
        (semantic_warnings, semantic_errors)
    }

    fn check_class(
        &mut self,
        semantic_warnings: &mut Vec<SemanticWarning>,
        semantic_errors: &mut Vec<SemanticError>,
        node_index: usize,
    ) {
        use NodeType::*;
        use SymbolKind::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.check_class(semantic_warnings, semantic_errors, child_index);
        }

        match self.get_note_type(node_index) {
            Prog => {
                // Check if all member function are linked.
                for class_table_index in
                    self.get_class_tables_in_table(self.symbol_table_arena.root.unwrap())
                {
                    for member_entry_index in self
                        .symbol_table_arena
                        .get_symbol_table_entries(class_table_index)
                    {
                        let function_symbol_entry = self
                            .symbol_table_arena
                            .get_symbol_table_entry(*member_entry_index);
                        let function_name = function_symbol_entry.name.clone();
                        if let Function(_, _) = function_symbol_entry.kind {
                            if function_symbol_entry.link.is_none() {
                                semantic_errors.push(SemanticError::MemberFunctionDeclHasNoDef(
                                    self.get_leftmost_token(
                                        self.get_node_index_with_entry_index(
                                            self.root.unwrap(),
                                            *member_entry_index,
                                        )
                                        .unwrap(),
                                    ),
                                    function_name,
                                ));
                                return;
                            }
                        }
                    }
                }

                // At this point, all inherited classes should be properly linked, so circular dependancy can be checked.
                if let Err(error) = self.check_circular_dependency() {
                    semantic_errors.push(error);
                    return;
                }

                // At this point, there are no circular dependancy, so shadowed member can be checked.
                for class_table_index in
                    self.get_class_tables_in_table(self.symbol_table_arena.root.unwrap())
                {
                    let base_class_name = self
                        .symbol_table_arena
                        .get_symbol_table(class_table_index)
                        .name
                        .clone();
                    for inherited_class_index in self.get_class_tables_in_table(class_table_index) {
                        for entry_index in self
                            .symbol_table_arena
                            .get_symbol_table_entries(class_table_index)
                        {
                            let symbol_entry =
                                self.symbol_table_arena.get_symbol_table_entry(*entry_index);
                            let member_name = &symbol_entry.name;
                            match symbol_entry.kind {
                                Function(_, _) => {
                                    if let Some(table_index) =
                                        self.is_member_function(inherited_class_index, member_name)
                                    {
                                        let class_name = self
                                            .symbol_table_arena
                                            .get_symbol_table(table_index)
                                            .name
                                            .clone();
                                        semantic_warnings.push(
                                            SemanticWarning::ShadowInheritedMemberFunction(
                                                self.get_leftmost_token(
                                                    self.get_node_index_with_entry_index(
                                                        self.root.unwrap(),
                                                        *entry_index,
                                                    )
                                                    .unwrap(),
                                                ),
                                                base_class_name.clone(),
                                                member_name.clone(),
                                                class_name,
                                            ),
                                        )
                                    }
                                }
                                Variable(_) => {
                                    if let Some(table_index) =
                                        self.is_member_variable(inherited_class_index, member_name)
                                    {
                                        let class_name = self
                                            .symbol_table_arena
                                            .get_symbol_table(table_index)
                                            .name
                                            .clone();
                                        semantic_warnings.push(
                                            SemanticWarning::ShadowInheritedMemberVariable(
                                                self.get_leftmost_token(
                                                    self.get_node_index_with_entry_index(
                                                        self.root.unwrap(),
                                                        *entry_index,
                                                    )
                                                    .unwrap(),
                                                ),
                                                base_class_name.clone(),
                                                member_name.clone(),
                                                class_name,
                                            ),
                                        )
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            ClassDecl => {
                let table_index = self.get_element(node_index).symbol_table.unwrap();

                // Get parents.
                for class_node_index in self.get_children_of_child(node_index, 1) {
                    let parent_name = self.get_lexeme(class_node_index);
                    match self.find_class_symbol_table(&parent_name) {
                        Some(parent_table_index) => {
                            let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                                parent_name,
                                SymbolKind::Class,
                                Some(parent_table_index),
                            );
                            self.symbol_table_arena.add_entry(table_index, entry_index);
                        }
                        None => {
                            semantic_errors.push(SemanticError::UndefinedClass(
                                self.get_leftmost_token(class_node_index),
                            ));
                        }
                    }
                }
            }
            Type => {
                let type_name = self.get_lexeme(node_index);
                if type_name != "integer"
                    && type_name != "float"
                    && self.find_class_symbol_table(&type_name).is_none()
                {
                    semantic_errors.push(SemanticError::UndefinedClass(self.get_token(node_index)));
                }
            }
            _ => {}
        }
    }

    // fn get_class_entries_in_table(&self, table_index: usize) -> Vec<usize> {
    //     let mut class_entry_indices = Vec::new();
    //
    //     for entry_index in self
    //         .symbol_table_arena
    //         .get_symbol_table_entries(table_index)
    //     {
    //         let symbol_entry = self.symbol_table_arena.get_symbol_table_entry(*entry_index);
    //         if let SymbolKind::Class = symbol_entry.kind {
    //             class_entry_indices.push(*entry_index);
    //         }
    //     }
    //     class_entry_indices
    // }

    fn check_circular_dependency(&self) -> Result<(), SemanticError> {
        for class_table_index in
            self.get_class_tables_in_table(self.symbol_table_arena.root.unwrap())
        {
            let mut class_table_index_stack: Vec<usize> = Vec::new();
            self.check_circular_dependency_in_class(
                class_table_index,
                &mut class_table_index_stack,
            )?;
        }

        Ok(())
    }

    fn check_circular_dependency_in_class(
        &self,
        class_table_index: usize,
        class_table_index_stack: &mut Vec<usize>,
    ) -> Result<(), SemanticError> {
        if class_table_index_stack.contains(&class_table_index) {
            class_table_index_stack.push(class_table_index);
            let dependency_list: Vec<String> = class_table_index_stack
                .iter()
                .map(|&index| self.symbol_table_arena.get_symbol_table(index).name.clone())
                .collect();
            return Err(SemanticError::CircularClassDependency(
                self.get_leftmost_token(
                    self.get_node_index_with_entry_index(self.root.unwrap(), class_table_index)
                        .unwrap(),
                ),
                dependency_list,
            ));
        }
        let mut class_table_index_stack = class_table_index_stack.clone();
        class_table_index_stack.push(class_table_index);

        for sub_class_table_index in self.get_class_tables_in_table(class_table_index) {
            self.check_circular_dependency_in_class(
                sub_class_table_index,
                &mut class_table_index_stack.clone(),
            )?;
        }
        Ok(())
    }
}
