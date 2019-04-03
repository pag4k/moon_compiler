use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::semantic_error::*;
use crate::symbol_table::*;

use std::collections::HashMap;

// FIXME: I NEED TO ADD A CHECK FOR THE TYPE OF THE FOR VARIABLE, BUT I'M NOT SURE IF IT CAN BE A SEMANTIC ERROR.

pub fn function_checker_visitor(ast: &mut AST) -> Vec<SemanticError> {
    use NodeType::*;
    let mut semantic_errors: Vec<SemanticError> = Vec::new();
    let mut semantic_actions: SemanticActionMap<SemanticError> = HashMap::new();
    semantic_actions.insert(MainFuncBody, check_return_stat);
    semantic_actions.insert(FuncDef, check_return_stat);
    semantic_actions.insert(DataMember, data_member);
    semantic_actions.insert(FunctionCall, function_call);
    semantic_actions.insert(VarElementList, var_element_list);
    ast_traversal(ast, &mut semantic_errors, &semantic_actions);
    semantic_errors
}

fn check_return_stat(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use NodeType::*;
    let node_type = ast.get_node_type(node_index);
    let mut return_stat_node_indices: Vec<usize> = Vec::new();
    ast.find_node_type(node_index, &mut return_stat_node_indices, &[ReturnStat]);
    match node_type {
        MainFuncBody => {
            for return_stat_node_index in return_stat_node_indices {
                semantic_errors.push(SemanticError::ShouldNotReturnFromMain(
                    ast.get_leftmost_token(return_stat_node_index).clone(),
                ));
            }
        }
        FuncDef => {
            if return_stat_node_indices.is_empty() {
                semantic_errors.push(SemanticError::FunctionHasNoReturnStat(
                    ast.get_token(ast.get_child(node_index, 2)).clone(),
                ));
            }
        }
        _ => unreachable!(),
    }
}

fn data_member(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use NodeType::*;

    // FIXME: I think I'm priorizing the member variable before the local one. It should be the opposite.

    let variable_name = ast.get_child_lexeme(node_index, 0);

    // Check if first element in VarElementList
    let left_element_node_index = ast.get_left_sibling(node_index);

    // Get the variable table entry index.
    let variable_entry_index = match left_element_node_index {
        // If it has a left, node, get the class table index of the previous type.
        Some(left_element_node_index) => {
            match get_symbol_type_and_class_table_from_node(ast, left_element_node_index) {
                Ok(result) => match result {
                    Some((previous_type, class_table_index)) => {
                        match ast.is_member_variable(class_table_index, &variable_name) {
                            Some((_, variable_entry_index)) => variable_entry_index,
                            None => {
                                semantic_errors.push(SemanticError::UndefinedMemberVariable(
                                    ast.get_leftmost_token(node_index).clone(),
                                    previous_type.clone(),
                                ));
                                return;
                            }
                        }
                    }
                    None => {
                        return;
                    }
                },

                Err(error) => {
                    semantic_errors.push(error);
                    return;
                }
            }
        }
        // If it has no left node, check if it is a member variable.
        None => {
            // First, if any, get local variable entry index.
            let (function_node_index, _) = ast
                .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
                .unwrap();
            let function_table_index = ast.get_element(function_node_index).symbol_table.unwrap();
            let local_variable_entry_index = if let Some(parameter_table_entry_index) =
                get_parameter_entry_in_function_table(ast, function_table_index, &variable_name)
            {
                Some(parameter_table_entry_index)
            } else if let Some(variable_entry_index) =
                get_variable_entry_in_function_table(ast, function_table_index, &variable_name)
            {
                Some(variable_entry_index)
            } else if let Some(variable_entry_index) =
                get_for_entry_in_function_table(ast, function_table_index, &variable_name)
            {
                Some(variable_entry_index)
            } else {
                None
            };

            // Second, if any, get member variable entry index.
            let member_variable_entry_index = match ast.get_node_type(function_node_index) {
                // It is the main function.
                NodeType::MainFuncBody => None,
                // It is not the main function.
                NodeType::FuncDef => {
                    let scope_node_index = ast.get_child(function_node_index, 1);
                    if ast.has_token(scope_node_index) {
                        // It is a member function, check if it is a member variable.
                        let class_table_index = ast
                            .find_class_symbol_table(ast.get_lexeme(scope_node_index))
                            .unwrap();
                        // If it is a member variable, return table entry.
                        ast.is_member_variable(class_table_index, &variable_name)
                            .map(|(_, entry_index)| entry_index)
                    } else {
                        // It is a free function.
                        None
                    }
                }
                _ => unreachable!(),
            };

            // So we have 4 possible cases:
            match (local_variable_entry_index, member_variable_entry_index) {
                // It is both a local and a member variable. Take the local unless it is not valid.
                (Some(local_variable_entry_index), Some(member_variable_entry_index)) => {
                    match check_if_valid_local_variable(
                        ast,
                        node_index,
                        function_table_index,
                        local_variable_entry_index,
                    ) {
                        Ok(()) => local_variable_entry_index,
                        Err(_) => member_variable_entry_index,
                    }
                }
                // It is a local variable. Take it if valid.
                (Some(local_variable_entry_index), None) => {
                    if let Err(error) = check_if_valid_local_variable(
                        ast,
                        node_index,
                        function_table_index,
                        local_variable_entry_index,
                    ) {
                        semantic_errors.push(error);
                    }
                    local_variable_entry_index
                }
                // It is a member variable. Take it.
                (None, Some(member_variable_entry_index)) => member_variable_entry_index,
                // It is neither. Return error.
                (None, None) => {
                    semantic_errors.push(SemanticError::UndefinedLocalVariable(
                        ast.get_leftmost_token(node_index).clone(),
                        ast.symbol_table_arena
                            .get_table(function_table_index)
                            .get_name_clone(),
                    ));
                    return;
                }
            }
        }
    };

    // At this point, the variable entry index has been successfully found.
    let variable_symbol_type = ast
        .symbol_table_arena
        .get_table_entry(variable_entry_index)
        .get_symbol_type()
        .unwrap();
    //     .kind
    // {
    //     SymbolKind::Variable(symbol_type)
    //     | SymbolKind::Parameter(symbol_type)
    //     | SymbolKind::For(symbol_type) => symbol_type.clone(),
    //     _ => unreachable!(),
    // };

    // Check if it is the last element in VarElementList and set the DataMember symbol type accordingly.
    match ast.get_right_sibling(node_index) {
        Some(_) => {
            if let Err(error) = check_number_of_dimensions(ast, variable_entry_index, node_index) {
                semantic_errors.push(error);
            }
            ast.set_data_type(
                node_index,
                Some(variable_symbol_type.clone().remove_dimensions()),
            );
        }
        None => match is_array_type(ast, variable_entry_index, node_index) {
            Ok(is_array_type) => {
                if is_array_type {
                    ast.set_data_type(node_index, Some(variable_symbol_type.clone()));
                } else {
                    if let Err(error) =
                        check_number_of_dimensions(ast, variable_entry_index, node_index)
                    {
                        semantic_errors.push(error);
                    }
                    ast.set_data_type(
                        node_index,
                        Some(variable_symbol_type.clone().remove_dimensions()),
                    );
                }
            }
            Err(error) => semantic_errors.push(error),
        },
    }
    ast.get_mut_element(node_index).symbol_table_entry = Some(variable_entry_index);
}
fn function_call(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use NodeType::*;

    let function_name = ast.get_child_lexeme(node_index, 0);

    // Check if first element in VarElementList
    let left_element_node_index = ast.get_left_sibling(node_index);

    // Get the function table entry index.
    let function_entry_index = match left_element_node_index {
        // If it has a left, node, get the class table index of the previous type.
        Some(left_element_node_index) => {
            match get_symbol_type_and_class_table_from_node(ast, left_element_node_index) {
                Ok(result) => match result {
                    Some((previous_type, class_table_index)) => {
                        match ast.is_member_function(class_table_index, &function_name) {
                            Some((_, function_entry_index)) => function_entry_index,
                            None => {
                                semantic_errors.push(SemanticError::UndefinedMemberFunction(
                                    ast.get_leftmost_token(node_index).clone(),
                                    previous_type.clone(),
                                ));
                                return;
                            }
                        }
                    }
                    None => {
                        return;
                    }
                },
                Err(error) => {
                    semantic_errors.push(error);
                    return;
                }
            }
        }
        // If it has no left node, check if it is a member function.
        None => {
            // Get the parent function node in which this is function is called.
            let (function_node_index, _) = ast
                .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
                .unwrap();

            // Check if it is a parent member function.
            let function_entry_index = match ast.get_node_type(function_node_index) {
                // It is the main function.
                NodeType::MainFuncBody => None,
                // It is not the main function.
                NodeType::FuncDef => {
                    let scope_node_index = ast.get_child(function_node_index, 1);
                    if ast.has_token(scope_node_index) {
                        // It is a parent member function, check if the called function is also a member.
                        let class_table_index = ast
                            .find_class_symbol_table(ast.get_lexeme(scope_node_index))
                            .unwrap();
                        // If it is a member variable, return table entry.
                        ast.is_member_function(class_table_index, &function_name)
                            .map(|(_, entry_index)| entry_index)
                    } else {
                        // It is a free function or the main function.
                        None
                    }
                }
                _ => unreachable!(),
            };

            match function_entry_index {
                // It is a member function, return function table entry index.
                Some(function_entry_index) => function_entry_index,
                // It is not a member function, check if it a free function.
                None => match find_free_function(ast, &function_name) {
                    Some(function_entry_index) => function_entry_index,
                    None => {
                        semantic_errors.push(SemanticError::UndefinedFunction(
                            ast.get_leftmost_token(node_index).clone(),
                        ));
                        return;
                    }
                },
            }
        }
    };

    // At this point, the function table entry index has been successfully found.
    let function_return_symbol_type = ast
        .symbol_table_arena
        .get_table_entry(function_entry_index)
        .get_symbol_type()
        .unwrap();

    // Set symbol type to the function return type.
    ast.set_data_type(node_index, Some(function_return_symbol_type.clone()));

    // FIXME: NOT SURE I WANT TO DO THAT
    ast.get_mut_element(node_index).symbol_table_entry = Some(function_entry_index);
}
fn var_element_list(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Get last element symbol type..
    let last_child_index = *ast.get_children(node_index).iter().last().unwrap();

    // Assign the last symbol type to the list.
    if ast.has_data_type(last_child_index) {
        ast.set_data_type(
            node_index,
            Some(ast.get_data_type(last_child_index).clone()),
        );
        if let Some(symbol_table_entry) = ast.get_mut_element(last_child_index).symbol_table_entry {
            ast.get_mut_element(node_index).symbol_table_entry = Some(symbol_table_entry);
        }
    }
}

fn is_for_variable_in_scope(ast: &AST, node_index: usize) -> bool {
    use NodeType::*;

    let variable_name = ast.get_child_lexeme(node_index, 0);

    let mut for_node_index = node_index;
    loop {
        let for_node = ast.get_parent_node_of_type(for_node_index, &[ForStat], &[]);
        match for_node {
            Some(for_node) => {
                let new_for_node_index = for_node.0;
                let for_variable_name = ast.get_child_lexeme(new_for_node_index, 1);
                if for_variable_name == variable_name {
                    return true;
                } else {
                    for_node_index = ast.get_parent(new_for_node_index).unwrap();
                }
            }
            None => {
                return false;
            }
        }
    }
}

fn is_variable_declared(ast: &AST, node_index: usize) -> bool {
    use NodeType::*;
    //FIXME: Add case of for loop

    let variable_name = ast.get_child_lexeme(node_index, 0);

    // Find top statement node index.
    let mut statement_node_index = node_index;
    loop {
        let (new_statement_node_index, _) = ast
            .get_parent_node_of_type(
                ast.get_parent(statement_node_index).unwrap(),
                &[AssignStat, IfStat, ForStat, ReadStat, WriteStat, ReturnStat],
                &[],
            )
            .unwrap();
        statement_node_index = new_statement_node_index;
        if [MainFuncBody, FuncBody]
            .contains(ast.get_node_type(ast.get_parent(statement_node_index).unwrap()))
        {
            break;
        }
    }

    while let Some(left_statement_node_index) = ast.get_left_sibling(statement_node_index) {
        statement_node_index = left_statement_node_index;
        if *ast.get_node_type(statement_node_index) == VarDecl
            && ast.get_child_lexeme(statement_node_index, 1) == variable_name
        {
            return true;
        }
    }

    false
}

fn get_symbol_type_and_class_table_from_node(
    ast: &AST,
    node_index: usize, //symbol_type: &SymbolType
) -> Result<Option<(SymbolType, usize)>, SemanticError> {
    if ast.has_data_type(node_index) {
        // If the symbol type is found, verify if it is a class.
        let symbol_type = ast.get_data_type(node_index);
        match symbol_type {
            SymbolType::Class(type_name, _) => Ok(Some((
                symbol_type.clone(),
                get_class_table_from_name(ast, &type_name),
            ))),
            _ => Err(SemanticError::DotOperatorWithInvalidClass(
                ast.get_leftmost_token(node_index).clone(),
                symbol_type.clone(),
            )),
        }
    } else {
        // If it is not found, it means that there was an earlier error.
        // Return None so that it can be ignored.
        Ok(None)
    }
}

// We assume that the class name have all been validated.
fn get_class_table_from_name(ast: &AST, class_name: &str) -> usize {
    ast.get_class_tables_in_table(ast.symbol_table_arena.root.unwrap())
        .into_iter()
        .find(|&class_table_index| {
            ast.symbol_table_arena
                .get_table(class_table_index)
                .has_name(class_name)
        })
        .unwrap()
}

fn is_array_type(
    ast: &AST,
    symbol_entry_index: usize,
    data_member_node: usize,
) -> Result<bool, SemanticError> {
    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    let symbol_type = symbol_entry.get_symbol_type().unwrap();
    let dimension_list_len = symbol_type.get_dimension_list().len();

    let index_list_len = ast.get_children_of_child(data_member_node, 1).len();
    match (dimension_list_len == 0, index_list_len == 0) {
        (true, true) => Ok(false),
        (false, true) => Ok(true),
        (true, false) => Err(SemanticError::MismatchedNumberOfDimension(
            ast.get_leftmost_token(data_member_node).clone(),
            dimension_list_len,
            index_list_len,
        )),
        (false, false) => Ok(false),
    }
}

fn check_number_of_dimensions(
    ast: &AST,
    symbol_entry_index: usize,
    data_member_node: usize,
) -> Result<(), SemanticError> {
    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    let symbol_type = symbol_entry.get_symbol_type().unwrap();
    let dimension_list_len = symbol_type.get_dimension_list().len();

    let index_list_len = ast.get_children_of_child(data_member_node, 1).len();
    if dimension_list_len != index_list_len {
        return Err(SemanticError::MismatchedNumberOfDimension(
            ast.get_leftmost_token(data_member_node).clone(),
            dimension_list_len,
            index_list_len,
        ));
    }

    Ok(())
}

pub fn get_variable_entry_in_function_table(
    ast: &AST,
    table_index: usize,
    name: &str,
) -> Option<usize> {
    for entry_index in ast.symbol_table_arena.get_table_entries(table_index) {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
        if symbol_entry.is_variable() && symbol_entry.has_name(name) {
            return Some(*entry_index);
        }
    }

    None
}

pub fn get_parameter_entry_in_function_table(
    ast: &AST,
    table_index: usize,
    name: &str,
) -> Option<usize> {
    for entry_index in ast.symbol_table_arena.get_table_entries(table_index) {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
        if symbol_entry.is_for() && symbol_entry.has_name(name) {
            return Some(*entry_index);
        }
    }

    None
}

pub fn get_for_entry_in_function_table(ast: &AST, table_index: usize, name: &str) -> Option<usize> {
    for entry_index in ast.symbol_table_arena.get_table_entries(table_index) {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
        if symbol_entry.is_parameter() && symbol_entry.has_name(name) {
            return Some(*entry_index);
        }
    }

    None
}

fn find_free_function(ast: &AST, name: &str) -> Option<usize> {
    for entry_index in ast
        .symbol_table_arena
        .get_table_entries(ast.symbol_table_arena.root.unwrap())
    {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(*entry_index);
        if symbol_entry.is_function() && symbol_entry.has_name(name) {
            return Some(*entry_index);
        }
    }

    None
}

fn check_if_valid_local_variable(
    ast: &AST,
    node_index: usize,
    function_table_index: usize,
    variable_entry_index: usize,
) -> Result<(), SemanticError> {
    use SymbolKind::*;

    let variable_symbol_entry = ast.symbol_table_arena.get_table_entry(variable_entry_index);

    match variable_symbol_entry.get_kind() {
        // If it is a variable, check if it is declared before.
        Variable(_) => {
            if !is_variable_declared(ast, node_index) {
                return Err(SemanticError::VariableUsedBeforeBeingDeclared(
                    ast.get_leftmost_token(node_index).clone(),
                    ast.symbol_table_arena
                        .get_table(function_table_index)
                        .get_name_clone(),
                ));
            }
        }
        // If it is a parameter, we know it is find.
        Parameter(_) => {}
        // If it is a for variable, check if it is in scope.
        For(_) => {
            if !is_for_variable_in_scope(ast, node_index) {
                return Err(SemanticError::ForVariableUsedOutOfScope(
                    ast.get_leftmost_token(node_index).clone(),
                    ast.symbol_table_arena
                        .get_table(function_table_index)
                        .get_name_clone(),
                ));
            }
        }
        _ => unreachable!(),
    }

    // At this point, it is valid.
    Ok(())
}
