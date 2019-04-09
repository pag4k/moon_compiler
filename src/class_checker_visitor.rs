use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::semantic_analysis_common::*;
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
    ast_traversal(ast, &mut semantic_errors, &semantic_actions);
    semantic_errors
}

fn prog(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, _node_index: usize) {
    // Check if all member function are linked.
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        for member_entry_index in ast.symbol_table_arena.get_table_entries(class_table_index) {
            let function_symbol_entry = ast.symbol_table_arena.get_table_entry(*member_entry_index);
            let function_name = function_symbol_entry.get_name_clone();
            if function_symbol_entry.is_function() && function_symbol_entry.get_link().is_none() {
                semantic_errors.push(SemanticError::MemberFunctionDeclHasNoDef(
                    ast.get_leftmost_token(
                        get_node_index_with_entry_index(
                            ast,
                            ast.root.unwrap(),
                            *member_entry_index,
                        )
                        .unwrap(),
                    )
                    .clone(),
                    function_name,
                ));
            }
        }
    }

    // At this point, all inherited classes should be properly linked, so circular dependancy can be checked.
    while let Err(error) = check_circular_dependency(ast) {
        semantic_errors.push(error);
    }

    // At this point, there are no circular dependancy, so shadowed member can be checked.
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        let base_class_name = ast
            .symbol_table_arena
            .get_table(class_table_index)
            .get_name();
        for inherited_class_index in ast.get_class_tables_in_table(class_table_index) {
            for entry_index in ast.symbol_table_arena.get_table_entries(class_table_index) {
                let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
                let member_name = symbol_entry.get_name();
                if symbol_entry.is_function() {
                    if let Some((class_table_index, _)) =
                        ast.is_member_function(inherited_class_index, member_name)
                    {
                        let class_name = ast
                            .symbol_table_arena
                            .get_table(class_table_index)
                            .get_name_clone();
                        semantic_errors.push(SemanticError::ShadowInheritedMemberFunction(
                            ast.get_leftmost_token(
                                get_node_index_with_entry_index(
                                    ast,
                                    ast.root.unwrap(),
                                    *entry_index,
                                )
                                .unwrap(),
                            )
                            .clone(),
                            base_class_name.clone(),
                            member_name.clone(),
                            class_name,
                        ))
                    }
                } else if symbol_entry.is_variable() {
                    if let Some((class_table_index, _)) =
                        ast.is_member_variable(inherited_class_index, member_name)
                    {
                        let class_name = ast
                            .symbol_table_arena
                            .get_table(class_table_index)
                            .get_name();
                        semantic_errors.push(SemanticError::ShadowInheritedMemberVariable(
                            ast.get_leftmost_token(
                                get_node_index_with_entry_index(
                                    ast,
                                    ast.root.unwrap(),
                                    *entry_index,
                                )
                                .unwrap(),
                            )
                            .clone(),
                            base_class_name.clone(),
                            member_name.clone(),
                            class_name.clone(),
                        ))
                    }
                }
            }
        }
    }
}
fn class_decl(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let table_index = ast.get_element(node_index).symbol_table.unwrap();

    // Get parents.
    for child_index in ast.get_children_of_child(node_index, 1) {
        let parent_name = ast.get_lexeme(child_index);
        match get_class_table_index_from_name(ast, parent_name) {
            Some(parent_table_index) => {
                let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
                    parent_name.clone(),
                    SymbolKind::Class,
                    Some(parent_table_index),
                );
                // This is weird to add an entry on an ID, but it makes the rest easier.
                ast.get_mut_element(child_index).symbol_table_entry = Some(entry_index);
                ast.symbol_table_arena.add_entry(table_index, entry_index);
            }
            None => {
                semantic_errors.push(SemanticError::UndefinedClass(
                    ast.get_leftmost_token(child_index).clone(),
                ));
            }
        }
    }
}

fn node_type(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let type_name = ast.get_lexeme(node_index);
    if type_name != "integer"
        && type_name != "float"
        && get_class_table_index_from_name(ast, type_name).is_none()
    {
        semantic_errors.push(SemanticError::UndefinedClass(
            ast.get_token(node_index).clone(),
        ));
    }
}

fn check_circular_dependency(ast: &mut AST) -> Result<(), SemanticError> {
    for class_table_index in ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap()) {
        let mut class_table_index_stack: Vec<usize> = Vec::new();
        check_circular_dependency_in_class(ast, class_table_index, &mut class_table_index_stack)?;
    }

    Ok(())
}

fn check_circular_dependency_in_class(
    ast: &mut AST,
    new_class_table_index: usize,
    class_table_index_stack: &mut Vec<usize>,
) -> Result<(), SemanticError> {
    // Check if the next table index can be found in the stack.
    if let Some(duplicated_class_position) = class_table_index_stack
        .iter()
        .position(|&table_index| table_index == new_class_table_index)
    {
        // Get last class_index before adding the duplicated one.
        let last_class_table_index = *class_table_index_stack.last().unwrap();
        // Add duplicated class_index on stack.
        class_table_index_stack.push(new_class_table_index);
        let splitted_class_table_index_stack =
            class_table_index_stack.split_off(duplicated_class_position);

        let dependency_list: Vec<String> = splitted_class_table_index_stack
            .iter()
            .map(|&index| ast.symbol_table_arena.get_table(index).get_name_clone())
            .collect();

        // In the last class, remove link to or variable of the duplicated one.
        // This is to remove the dependency and allow the process to continue.
        let duplicated_class_name = ast
            .symbol_table_arena
            .get_table(new_class_table_index)
            .get_name_clone();
        for (position, entry_index) in ast
            .symbol_table_arena
            .get_table_entries(last_class_table_index)
            .to_vec()
            .into_iter()
            .enumerate()
        {
            let symbol_entry = ast.symbol_table_arena.get_mut_table_entry(entry_index);
            if symbol_entry.is_class() {
                if let Some(link_index) = symbol_entry.get_link() {
                    if link_index == new_class_table_index {
                        symbol_entry.set_link(None);
                        break;
                    }
                }
            } else if symbol_entry.is_variable()
                && symbol_entry.has_class_name(&duplicated_class_name)
            {
                ast.symbol_table_arena
                    .remove_entry_from_position(last_class_table_index, position);
                break;
            }
        }
        return Err(SemanticError::CircularClassDependency(
            ast.get_leftmost_token(
                get_node_index_with_table_index(ast, ast.root.unwrap(), new_class_table_index)
                    .unwrap(),
            )
            .clone(),
            dependency_list,
        ));
    }
    let mut class_table_index_stack = class_table_index_stack.clone();
    class_table_index_stack.push(new_class_table_index);

    for sub_class_table_index in ast.get_class_tables_in_table(new_class_table_index) {
        check_circular_dependency_in_class(
            ast,
            sub_class_table_index,
            &mut class_table_index_stack.clone(),
        )?;
    }
    for sub_class_table_index in get_member_variable_in_table(ast, new_class_table_index) {
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

fn get_node_index_with_table_index(
    ast: &AST,
    node_index: usize,
    table_index: usize,
) -> Option<usize> {
    if let Some(current_table_index) = ast.get_element(node_index).symbol_table {
        if current_table_index == table_index {
            return Some(node_index);
        }
    }
    for node_child_index in ast.get_children(node_index) {
        let node_index = get_node_index_with_table_index(ast, node_child_index, table_index);
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
        .map(|&entry_index| ast.symbol_table_arena.get_table_entry(entry_index))
        .filter(|symbol_entry| symbol_entry.is_variable())
        .filter_map(|symbol_entry| {
            if let SymbolType::Class(class_name, _) = symbol_entry.get_symbol_type().unwrap() {
                get_class_table_index_from_name(ast, class_name)
            } else {
                None
            }
        })
        .collect()
}
