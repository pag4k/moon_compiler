use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::semantic_error::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn class_checker_visitor(ast: &mut AST) -> Vec<SemanticError> {
    use NodeType::*;
    let mut semantic_errors: Vec<SemanticError> = Vec::new();
    let mut semantic_actions: SemanticActionMap<SemanticError> = HashMap::new();
    semantic_actions.insert(Prog, prog);
    semantic_actions.insert(ClassDecl, class_decl);
    semantic_actions.insert(Type, node_type);
    //semantic_actions.insert(VarElementList, var_element_list);
    ast_traversal(ast, &mut semantic_errors, &semantic_actions);
    semantic_errors
}

fn prog(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, _node_index: usize) {
    use SymbolKind::*;
    // Check if all member function are linked.
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        for member_entry_index in ast.symbol_table_arena.get_table_entries(class_table_index) {
            let function_symbol_entry = ast.symbol_table_arena.get_table_entry(*member_entry_index);
            let function_name = function_symbol_entry.name.clone();
            if let Function(_, _) = function_symbol_entry.kind {
                if function_symbol_entry.link.is_none() {
                    semantic_errors.push(SemanticError::MemberFunctionDeclHasNoDef(
                        ast.get_leftmost_token(
                            get_node_index_with_entry_index(
                                ast,
                                ast.root.unwrap(),
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
    if let Err(error) = check_circular_dependency(ast) {
        semantic_errors.push(error);
        return;
    }

    // At this point, there are no circular dependancy, so shadowed member can be checked.
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        let base_class_name = ast
            .symbol_table_arena
            .get_table(class_table_index)
            .name
            .clone();
        for inherited_class_index in ast.get_class_tables_in_table(class_table_index) {
            for entry_index in ast.symbol_table_arena.get_table_entries(class_table_index) {
                let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
                let member_name = &symbol_entry.name;
                match symbol_entry.kind {
                    Function(_, _) => {
                        if let Some(table_index) =
                            ast.is_member_function(inherited_class_index, member_name)
                        {
                            let class_name =
                                ast.symbol_table_arena.get_table(table_index).name.clone();
                            semantic_errors.push(SemanticError::ShadowInheritedMemberFunction(
                                ast.get_leftmost_token(
                                    get_node_index_with_entry_index(
                                        ast,
                                        ast.root.unwrap(),
                                        *entry_index,
                                    )
                                    .unwrap(),
                                ),
                                base_class_name.clone(),
                                member_name.clone(),
                                class_name,
                            ))
                        }
                    }
                    Variable(_) => {
                        if let Some(table_index) =
                            ast.is_member_variable(inherited_class_index, member_name)
                        {
                            let class_name =
                                ast.symbol_table_arena.get_table(table_index).name.clone();
                            semantic_errors.push(SemanticError::ShadowInheritedMemberVariable(
                                ast.get_leftmost_token(
                                    get_node_index_with_entry_index(
                                        ast,
                                        ast.root.unwrap(),
                                        *entry_index,
                                    )
                                    .unwrap(),
                                ),
                                base_class_name.clone(),
                                member_name.clone(),
                                class_name,
                            ))
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}
fn class_decl(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let table_index = ast.get_element(node_index).symbol_table.unwrap();

    // Get parents.
    for class_node_index in ast.get_children_of_child(node_index, 1) {
        let parent_name = ast.get_lexeme(class_node_index);
        match ast.find_class_symbol_table(&parent_name) {
            Some(parent_table_index) => {
                let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
                    parent_name,
                    SymbolKind::Class,
                    Some(parent_table_index),
                );
                ast.symbol_table_arena.add_entry(table_index, entry_index);
            }
            None => {
                semantic_errors.push(SemanticError::UndefinedClass(
                    ast.get_leftmost_token(class_node_index),
                ));
            }
        }
    }
}
fn node_type(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let type_name = ast.get_lexeme(node_index);
    if type_name != "integer"
        && type_name != "float"
        && ast.find_class_symbol_table(&type_name).is_none()
    {
        semantic_errors.push(SemanticError::UndefinedClass(ast.get_token(node_index)));
    }
}

fn check_circular_dependency(ast: &AST) -> Result<(), SemanticError> {
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        let mut class_table_index_stack: Vec<usize> = Vec::new();
        check_circular_dependency_in_class(ast, class_table_index, &mut class_table_index_stack)?;
    }

    Ok(())
}

fn check_circular_dependency_in_class(
    ast: &AST,
    class_table_index: usize,
    class_table_index_stack: &mut Vec<usize>,
) -> Result<(), SemanticError> {
    if class_table_index_stack.contains(&class_table_index) {
        class_table_index_stack.push(class_table_index);
        let dependency_list: Vec<String> = class_table_index_stack
            .iter()
            .map(|&index| ast.symbol_table_arena.get_table(index).name.clone())
            .collect();
        return Err(SemanticError::CircularClassDependency(
            ast.get_leftmost_token(
                get_node_index_with_entry_index(ast, ast.root.unwrap(), class_table_index).unwrap(),
            ),
            dependency_list,
        ));
    }
    let mut class_table_index_stack = class_table_index_stack.clone();
    class_table_index_stack.push(class_table_index);

    for sub_class_table_index in ast.get_class_tables_in_table(class_table_index) {
        check_circular_dependency_in_class(
            ast,
            sub_class_table_index,
            &mut class_table_index_stack.clone(),
        )?;
    }
    for sub_class_table_index in get_member_variable_in_table(ast, class_table_index) {
        check_circular_dependency_in_class(
            ast,
            sub_class_table_index,
            &mut class_table_index_stack.clone(),
        )?;
    }
    Ok(())
}

fn get_node_index_with_entry_index(
    ast: &AST,
    node_index: usize,
    entry_index: usize,
) -> Option<usize> {
    if let Some(current_entry_index) = ast.get_element(node_index).symbol_table_entry {
        if current_entry_index == entry_index {
            return Some(node_index);
        }
    }
    for node_child_index in ast.get_children(node_index) {
        let node_index = get_node_index_with_entry_index(ast, node_child_index, entry_index);
        if node_index.is_some() {
            return node_index;
        }
    }
    None
}

fn get_member_variable_in_table(ast: &AST, class_table_index: usize) -> Vec<usize> {
    ast.symbol_table_arena
        .get_table_entries(class_table_index)
        .iter()
        .map(|entry_index| ast.symbol_table_arena.get_table_entry(*entry_index))
        .filter_map(|symbol_entry| {
            if let SymbolKind::Variable(symbol_type) = &symbol_entry.kind {
                Some(symbol_type)
            } else {
                None
            }
        })
        .filter_map(|symbol_type| {
            if let SymbolType::Class(class_name, _) = symbol_type {
                ast.find_class_symbol_table(&class_name)
            } else {
                None
            }
        })
        .collect()
}
