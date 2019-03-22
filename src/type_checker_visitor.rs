use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::language::*;
use crate::semantic_error::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn type_checker_visitor(ast: &mut AST) -> Vec<SemanticError> {
    use NodeType::*;
    let mut semantic_errors: Vec<SemanticError> = Vec::new();
    let mut semantic_actions: SemanticActionMap<SemanticError> = HashMap::new();
    semantic_actions.insert(AssignStat, assign_stat);
    semantic_actions.insert(AssignStati, assign_stat);
    semantic_actions.insert(RelExpr, rel_expr);
    semantic_actions.insert(AddOp, add_op);
    semantic_actions.insert(MultOp, mult_op);
    semantic_actions.insert(Not, not_sign);
    semantic_actions.insert(Sign, not_sign);
    semantic_actions.insert(Num, num);
    semantic_actions.insert(DataMember, data_member);
    semantic_actions.insert(FunctionCall, function_call);
    semantic_actions.insert(ReturnStat, return_stat);

    ast_traversal(ast, &mut semantic_errors, &semantic_actions);
    semantic_errors
}

fn assign_stat(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    let lhs = ast.get_child_data_type(node_index, 0);
    let rhs = ast.get_child_data_type(node_index, 1);
    let lhs_dimension_list = lhs.get_dimension_list();
    let rhs_dimension_list = rhs.get_dimension_list();

    if lhs_dimension_list != rhs_dimension_list {
        semantic_errors.push(SemanticError::MismatchedTypeDimensions(
            ast.get_child_token(node_index, 0),
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
        (Class(lhs_class_name, _), Class(rhs_class_name, _)) => lhs_class_name == rhs_class_name,
        _ => false,
    } {
        semantic_errors.push(SemanticError::MismatchedTypes(
            ast.get_leftmost_token(node_index),
            lhs,
            rhs,
        ));
    }
}
fn rel_expr(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    //dbg!(ast.g(node_index));
    let left_type = ast.get_child_data_type(node_index, 0);
    let right_type = ast.get_child_data_type(node_index, 2);
    let symbol_type = match (left_type.clone(), right_type.clone()) {
        (Integer(_), Integer(_)) => SymbolType::Integer(Vec::new()),
        (Float(_), Float(_)) => SymbolType::Integer(Vec::new()),
        (Float(_), Integer(_)) => SymbolType::Integer(Vec::new()),
        (Integer(_), Float(_)) => SymbolType::Integer(Vec::new()),
        _ => {
            semantic_errors.push(SemanticError::InvalidRelOperation(
                ast.get_leftmost_token(node_index),
                left_type.clone(),
                right_type,
            ));
            ast.get_mut_element(node_index).data_type = Some(left_type);
            return;
        }
    };
    ast.get_mut_element(node_index).data_type = Some(symbol_type);
}
fn add_op(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    let left_type = ast.get_child_data_type(node_index, 0);
    let right_type = ast.get_child_data_type(node_index, 1);
    let symbol_type = match (left_type.clone(), right_type.clone()) {
        (Integer(_), Integer(_)) => left_type,
        (Float(_), Float(_)) => left_type,
        (Float(_), Integer(_)) => left_type,
        (Integer(_), Float(_)) => right_type,
        _ => {
            semantic_errors.push(SemanticError::InvalidAddOperation(
                ast.get_token(node_index),
                left_type.clone(),
                right_type,
            ));
            ast.get_mut_element(node_index).data_type = Some(left_type);
            return;
        }
    };
    ast.get_mut_element(node_index).data_type = Some(symbol_type);
}
fn mult_op(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    let left_type = ast.get_child_data_type(node_index, 0);
    let right_type = ast.get_child_data_type(node_index, 1);
    let symbol_type = match (left_type.clone(), right_type.clone()) {
        (Integer(_), Integer(_)) => left_type,
        (Float(_), Float(_)) => left_type,
        (Float(_), Integer(_)) => left_type,
        (Integer(_), Float(_)) => right_type,
        _ => {
            semantic_errors.push(SemanticError::InvalidMultOperation(
                ast.get_token(node_index),
                left_type.clone(),
                right_type,
            ));
            ast.get_mut_element(node_index).data_type = Some(left_type);
            return;
        }
    };
    ast.get_mut_element(node_index).data_type = Some(symbol_type);
}

fn not_sign(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let child_type = ast.get_child_data_type(node_index, 0);
    ast.get_mut_element(node_index).data_type = Some(child_type);
}
fn num(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let symbol_type = match get_token_type(ast, node_index) {
        TokenType::IntNum => SymbolType::Integer(Vec::new()),
        TokenType::FloatNum => SymbolType::Float(Vec::new()),
        _ => unreachable!(),
    };
    ast.get_mut_element(node_index).data_type = Some(symbol_type);
}
fn data_member(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let invalid_index_position: Vec<(usize, SymbolType)> = ast
        .get_children_of_child(node_index, 1)
        .iter()
        .enumerate()
        .map(|(dimension_index, child_index)| {
            (
                dimension_index,
                ast.get_element(*child_index).clone().data_type.unwrap(),
            )
        })
        .filter(|(_, data_type)| match data_type {
            SymbolType::Integer(_) => false,
            _ => true,
        })
        .collect();
    for (index, (dimension_index, _)) in invalid_index_position.iter().enumerate() {
        // FIXME: NOT SURE
        // Fix the type issue to continue and catch more errors.
        ast.get_mut_element(ast.get_children_of_child(node_index, 1)[*dimension_index])
            .data_type = Some(SymbolType::Integer(Vec::new()));

        semantic_errors.push(SemanticError::ArrayIndiceMustBeInteger(
            ast.get_leftmost_token(node_index),
            invalid_index_position[index].clone(),
        ));
    }
}
fn function_call(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    let function_parameter_list = match ast
        .symbol_table_arena
        .get_symbol_table_entry(entry_index)
        .kind
        .clone()
    {
        SymbolKind::Function(_, function_parameter_list) => function_parameter_list,
        _ => unreachable!(),
    };
    let argument_node_list = ast.get_children_of_child(node_index, 1);
    let argument_parameter_list: Vec<SymbolType> = argument_node_list
        .iter()
        .map(|&child_index| ast.get_element(child_index).data_type.clone().unwrap())
        .collect();

    if function_parameter_list.len() != argument_parameter_list.len() {
        semantic_errors.push(SemanticError::WrongNumberOfArguments(
            ast.get_child_token(node_index, 0),
            function_parameter_list.len(),
            argument_parameter_list.len(),
        ));
        return;
    }

    let invalid_arguments: Vec<(usize, SymbolType, SymbolType)> = function_parameter_list
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
            ast.get_child_token(node_index, 0),
            invalid_arguments[index].clone(),
        ));
    }
}
fn return_stat(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Get the function node index in which the statement is.
    let function_node = ast.get_parent_node_of_type(node_index, &[NodeType::FuncDef]);

    match function_node {
        // If there is one, verify its return type.
        Some(function_node) => {
            let function_node_index = function_node.0;
            let function_def_entry_index = ast
                .get_element(function_node_index)
                .symbol_table_entry
                .unwrap();
            let function_return_type = match ast
                .symbol_table_arena
                .get_symbol_table_entry(function_def_entry_index)
                .kind
                .clone()
            {
                SymbolKind::Function(return_type, _) => return_type,
                _ => unreachable!(),
            };

            let return_type = ast.get_child_data_type(node_index, 0);

            match function_return_type {
                Some(function_return_type) => {
                    if function_return_type != return_type {
                        semantic_errors.push(
                            SemanticError::ReturnTypeDoesNotMatchFuctionDeclaration(
                                ast.get_leftmost_token(node_index),
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
        // If there is none, we are in the main function.
        None => semantic_errors.push(SemanticError::ShouldNotReturnFromMain(
            ast.get_leftmost_token(node_index),
        )),
    }
}

fn get_token_type(ast: &AST, node_index: usize) -> TokenType {
    ast.get_element(node_index)
        .clone()
        .token
        .unwrap()
        .token_type
}
