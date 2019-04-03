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
    semantic_actions.insert(AssignForStat, assign_stat);
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

    if let (Some(lhs), Some(rhs)) = (
        ast.get_child_data_type(node_index, 0),
        ast.get_child_data_type(node_index, 1),
    ) {
        let lhs_dimension_list = lhs.get_dimension_list();
        let rhs_dimension_list = rhs.get_dimension_list();

        if lhs_dimension_list != rhs_dimension_list {
            semantic_errors.push(SemanticError::MismatchedTypeDimensions(
                ast.get_leftmost_token(node_index).clone(),
                lhs.clone(),
                rhs.clone(),
            ));
            return;
        }

        // Check if types match.
        // The only mismatch allowed is integer to float.
        if !match (lhs, rhs) {
            (Integer(_), Integer(_)) => true,
            (Float(_), Float(_)) => true,
            (Float(_), Integer(_)) => true,
            (Class(lhs_class_name, _), Class(rhs_class_name, _)) => {
                lhs_class_name == rhs_class_name
            }
            _ => false,
        } {
            semantic_errors.push(SemanticError::MismatchedTypes(
                ast.get_leftmost_token(node_index).clone(),
                lhs.clone(),
                rhs.clone(),
            ));
        }
    }
}
fn rel_expr(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    if let (Some(left_type), Some(right_type)) = (
        ast.get_child_data_type(node_index, 0),
        ast.get_child_data_type(node_index, 2),
    ) {
        let symbol_type = match (left_type, right_type) {
            (Integer(_), Integer(_)) => SymbolType::Integer(Vec::new()),
            (Float(_), Float(_)) => SymbolType::Integer(Vec::new()),
            (Float(_), Integer(_)) => SymbolType::Integer(Vec::new()),
            (Integer(_), Float(_)) => SymbolType::Integer(Vec::new()),
            _ => {
                semantic_errors.push(SemanticError::InvalidRelOperation(
                    ast.get_leftmost_token(node_index).clone(),
                    left_type.clone(),
                    right_type.clone(),
                ));
                ast.set_data_type(node_index, Some(left_type.clone()));
                return;
            }
        };
        ast.set_data_type(node_index, Some(symbol_type));
    }
}
fn add_op(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    if let (Some(left_type), Some(right_type)) = (
        ast.get_child_data_type(node_index, 0),
        ast.get_child_data_type(node_index, 1),
    ) {
        let symbol_type = match (left_type, right_type) {
            (Integer(_), Integer(_)) => left_type,
            (Float(_), Float(_)) => left_type,
            (Float(_), Integer(_)) => left_type,
            (Integer(_), Float(_)) => right_type,
            _ => {
                semantic_errors.push(SemanticError::InvalidAddOperation(
                    ast.get_token(node_index).clone(),
                    left_type.clone(),
                    right_type.clone(),
                ));
                ast.set_data_type(node_index, Some(left_type.clone()));
                return;
            }
        };
        ast.set_data_type(node_index, Some(symbol_type.clone()));
    }
}
fn mult_op(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    use SymbolType::*;

    if let (Some(left_type), Some(right_type)) = (
        ast.get_child_data_type(node_index, 0),
        ast.get_child_data_type(node_index, 1),
    ) {
        let symbol_type = match (left_type, right_type) {
            (Integer(_), Integer(_)) => left_type,
            (Float(_), Float(_)) => left_type,
            (Float(_), Integer(_)) => left_type,
            (Integer(_), Float(_)) => right_type,
            _ => {
                semantic_errors.push(SemanticError::InvalidMultOperation(
                    ast.get_token(node_index).clone(),
                    left_type.clone(),
                    right_type.clone(),
                ));
                ast.set_data_type(node_index, Some(left_type.clone()));
                return;
            }
        };
        ast.set_data_type(node_index, Some(symbol_type.clone()));
    }
}

fn not_sign(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    if let Some(child_type) = ast.get_child_data_type(node_index, 0) {
        ast.set_data_type(node_index, Some(child_type.clone()));
    }
}
fn num(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let symbol_type = match ast.get_token_type(node_index) {
        TokenType::IntNum => SymbolType::Integer(Vec::new()),
        TokenType::FloatNum => SymbolType::Float(Vec::new()),
        _ => unreachable!(),
    };
    ast.set_data_type(node_index, Some(symbol_type));
}
fn data_member(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let invalid_index_position: Vec<(usize, SymbolType)> = ast
        .get_children_of_child(node_index, 1)
        .iter()
        .enumerate()
        .filter(|(_, child_index)| ast.has_data_type(**child_index))
        .map(|(dimension_index, child_index)| {
            (dimension_index, ast.get_data_type(*child_index).clone())
        })
        .filter(|(_, data_type)| match data_type {
            SymbolType::Integer(_) => false,
            _ => true,
        })
        .collect();
    for (index, (dimension_index, _)) in invalid_index_position.iter().enumerate() {
        // FIXME: NOT SURE
        // Fix the type issue to continue and catch more errors.
        ast.set_data_type(
            ast.get_children_of_child(node_index, 1)[*dimension_index],
            Some(SymbolType::Integer(Vec::new())),
        );

        semantic_errors.push(SemanticError::ArrayIndiceMustBeInteger(
            ast.get_leftmost_token(node_index).clone(),
            invalid_index_position[index].clone(),
        ));
    }
}
fn function_call(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    if let Some(entry_index) = ast.get_element(node_index).symbol_table_entry {
        let function_parameter_list = ast
            .symbol_table_arena
            .get_table_entry(entry_index)
            .get_parameter_symbol_types();

        let argument_node_list = ast.get_children_of_child(node_index, 1);
        let argument_parameter_list: Vec<&SymbolType> = argument_node_list
            .iter()
            .map(|&child_index| ast.get_data_type(child_index))
            .collect();

        if function_parameter_list.len() != argument_parameter_list.len() {
            semantic_errors.push(SemanticError::WrongNumberOfArguments(
                ast.get_child_token(node_index, 0).clone(),
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
                ast.get_child_token(node_index, 0).clone(),
                invalid_arguments[index].clone(),
            ));
        }
    }
}
fn return_stat(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Get the function node index in which the statement is.
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[NodeType::FuncDef], &[])
    {
        let function_def_entry_index = ast
            .get_element(function_node_index)
            .symbol_table_entry
            .unwrap();
        let function_return_type = ast
            .symbol_table_arena
            .get_table_entry(function_def_entry_index)
            .get_symbol_type();

        if let (Some(return_type), Some(function_return_type)) =
            (ast.get_child_data_type(node_index, 0), function_return_type)
        {
            if function_return_type != return_type {
                semantic_errors.push(SemanticError::ReturnTypeDoesNotMatchFuctionDeclaration(
                    ast.get_leftmost_token(node_index).clone(),
                    function_return_type.clone(),
                    return_type.clone(),
                ));
            }
        }
    }
}
