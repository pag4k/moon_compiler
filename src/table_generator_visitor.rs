use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::semantic_analysis_common::*;
use crate::semantic_error::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn table_generator_visitor(ast: &mut AST) -> Vec<SemanticError> {
    use NodeType::*;
    let mut semantic_errors: Vec<SemanticError> = Vec::new();
    let mut semantic_actions: SemanticActionMap<SemanticError> = HashMap::new();
    semantic_actions.insert(Prog, prog);
    semantic_actions.insert(ClassDecl, class_decl);
    semantic_actions.insert(FuncDef, func_def);
    semantic_actions.insert(FuncDecl, func_decl);
    semantic_actions.insert(VarDecl, var_decl);
    semantic_actions.insert(FParam, f_param);
    semantic_actions.insert(ForStat, for_stat);

    ast_traversal(ast, &mut semantic_errors, &semantic_actions);
    semantic_errors
}

fn prog(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let table_index = ast
        .symbol_table_arena
        .new_symbol_table("Global".to_string());
    ast.get_mut_element(node_index).symbol_table = Some(table_index);

    // Set root.
    ast.symbol_table_arena.root = Some(table_index);

    // Add classes.
    for node_index in ast.get_children_of_child(node_index, 0) {
        if let Err(error) = add_entry_to_table(ast, table_index, node_index) {
            semantic_errors.push(error);
        }
    }

    // Add functions.
    // Here we assume that the list of classes is complete to validate types.
    for function_node_index in ast.get_children_of_child(node_index, 1) {
        if let Err(error) = add_function(ast, function_node_index, table_index) {
            semantic_errors.push(error);
        }
    }

    // Create a table for main.
    let stat_block_node_index = ast.get_children(node_index)[2];
    let program_table_index = ast.symbol_table_arena.new_symbol_table("main".to_string());
    ast.get_mut_element(stat_block_node_index).symbol_table = Some(program_table_index);

    // Add StatBlock variables.
    add_variables_to_table(
        ast,
        semantic_errors,
        stat_block_node_index,
        program_table_index,
    );

    // Create main function entry and add it to the global scope.
    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        "main".to_string(),
        SymbolKind::Function(None, Vec::new()),
        Some(program_table_index),
    );
    ast.get_mut_element(stat_block_node_index)
        .symbol_table_entry = Some(entry_index);
    if let Err(error) = add_entry_to_table(ast, table_index, stat_block_node_index) {
        semantic_errors.push(error);
    }
}

fn class_decl(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let name = ast.get_child_lexeme(node_index, 0).clone();

    let table_index = ast.symbol_table_arena.new_symbol_table(name.clone());
    ast.get_mut_element(node_index).symbol_table = Some(table_index);

    let entry_index =
        ast.symbol_table_arena
            .new_symbol_table_entry(name, SymbolKind::Class, Some(table_index));
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);

    // Get members
    for member_index in ast.get_children_of_child(node_index, 2) {
        if ast.get_element(member_index).symbol_table_entry.is_some() {
            if let Err(error) = add_entry_to_table(ast, table_index, member_index) {
                semantic_errors.push(error);
            }
        }
    }
}
fn func_def(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Function Definition has an entry and a table.
    let name = ast.get_child_lexeme(node_index, 2).clone();

    // Create a table.
    let table_index = ast.symbol_table_arena.new_symbol_table(name.clone());
    ast.get_mut_element(node_index).symbol_table = Some(table_index);

    let return_type = make_type_from_child(ast, node_index, 0, Vec::new());

    // Get parameters
    let mut parameters = Vec::new();
    for parameter_index in ast.get_children_of_child(node_index, 3) {
        let entry_index = ast
            .get_mut_element(parameter_index)
            .symbol_table_entry
            .unwrap();
        let parameter_type = ast
            .symbol_table_arena
            .get_table_entry(entry_index)
            .get_symbol_type()
            .unwrap();

        parameters.push(parameter_type.clone());
        if let Err(error) = add_entry_to_table(ast, table_index, parameter_index) {
            semantic_errors.push(error);
        }
    }

    // Add StatBlock variables.
    add_variables_to_table(
        ast,
        semantic_errors,
        ast.get_child(node_index, 4),
        table_index,
    );

    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name.clone(),
        SymbolKind::Function(Some(return_type), parameters),
        Some(table_index),
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn func_decl(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Function Declaration just has an entry.
    let name = ast.get_child_lexeme(node_index, 1).clone();
    let return_type = make_type_from_child(ast, node_index, 0, Vec::new());

    // Get parameters
    let mut parameters = Vec::new();
    for parameter_index in ast.get_children_of_child(node_index, 2) {
        let entry_index = ast
            .get_mut_element(parameter_index)
            .symbol_table_entry
            .unwrap();
        let parameter_type = ast
            .symbol_table_arena
            .get_table_entry(entry_index)
            .get_symbol_type()
            .unwrap();
        //     .kind
        //     .clone()
        // {
        //     SymbolKind::Parameter(parameter_type) => parameter_type,
        //     _ => unreachable!(),
        // };

        parameters.push(parameter_type.clone());
    }

    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name,
        SymbolKind::Function(Some(return_type), parameters),
        None,
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn var_decl(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let name = ast.get_child_lexeme(node_index, 1);
    for (index, dimension) in ast
        .get_children_of_child(node_index, 2)
        .iter()
        .map(|&child_index| get_dimension(ast, child_index))
        .enumerate()
    {
        if dimension < 1 {
            semantic_errors.push(SemanticError::DimensionMustBeGreaterThanZero(
                ast.get_leftmost_token(node_index).clone(),
                index,
                dimension,
            ));
        }
    }
    let indices = ast
        .get_children_of_child(node_index, 2)
        .iter()
        .map(|&child_index| get_dimension(ast, child_index))
        .collect();
    let parameter_type = make_type_from_child(ast, node_index, 0, indices);
    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name.clone(),
        SymbolKind::Variable(parameter_type),
        None,
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn f_param(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let name = ast.get_child_lexeme(node_index, 1);
    for (index, dimension) in ast
        .get_children_of_child(node_index, 2)
        .iter()
        .map(|&child_index| get_dimension(ast, child_index))
        .enumerate()
    {
        if dimension < 1 {
            semantic_errors.push(SemanticError::DimensionMustBeGreaterThanZero(
                ast.get_leftmost_token(node_index).clone(),
                index,
                dimension,
            ));
        }
    }
    let indices = ast
        .get_children_of_child(node_index, 2)
        .iter()
        .map(|&child_index| get_dimension(ast, child_index))
        .collect();
    let parameter_type = make_type_from_child(ast, node_index, 0, indices);
    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name.clone(),
        SymbolKind::Parameter(parameter_type),
        None,
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn for_stat(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    let symbol_type = make_type_from_child(ast, node_index, 0, Vec::new());
    let name = ast.get_child_lexeme(node_index, 1);
    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name.clone(),
        SymbolKind::For(symbol_type),
        None,
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}

fn make_type_from_child(
    ast: &AST,
    node_index: usize,
    child_index: usize,
    indices: Vec<usize>,
) -> SymbolType {
    SymbolType::new(ast.get_child_lexeme(node_index, child_index), indices)
}

fn get_dimension(ast: &AST, node_index: usize) -> usize {
    // We can directly parse since a non integer dimensions is a syntax error.
    ast.get_lexeme(node_index).parse::<usize>().unwrap()
}

fn add_function(
    ast: &mut AST,
    function_node_index: usize,
    table_index: usize,
) -> Result<(), SemanticError> {
    let function_definition_table_index =
        ast.get_element(function_node_index).symbol_table.unwrap();
    let function_definition_table_entry_index = ast
        .get_element(function_node_index)
        .symbol_table_entry
        .unwrap();
    let function_name = ast
        .symbol_table_arena
        .get_table(function_definition_table_index)
        .get_name_clone();

    let scope_node_index = ast.get_children(function_node_index)[1];
    if ast.has_token(scope_node_index) {
        // If member function, link it in its class.
        let class_name = ast.get_lexeme(scope_node_index);
        let class_table_index = match get_class_table_index_from_name(ast, class_name) {
            Some(class_table_index) => class_table_index,
            None => {
                // There is an UndefinedClass error, but it won't be reported,
                // since it will be caught later.
                return Ok(());
                // return Err(SemanticError::UndefinedClass(
                //     ast.get_child_token(function_node_index, 1).clone(),
                // ));
            }
        };
        match ast.find_function_in_table(class_table_index, &function_name) {
            Some(function_declaration_entry_index) => {
                // Get a clone of the definition entry and remore link.
                let mut function_definition_entry = (*ast
                    .symbol_table_arena
                    .get_table_entry(function_definition_table_entry_index))
                .clone();
                function_definition_entry.set_link(None);
                // Get a clone of the definition entry.
                let function_declaration_entry = (*ast
                    .symbol_table_arena
                    .get_table_entry(function_declaration_entry_index))
                .clone();
                // Compare declaration and definition, assumin neither has a link.
                if function_declaration_entry == function_definition_entry {
                    ast.symbol_table_arena
                        .get_mut_table_entry(function_declaration_entry_index)
                        .set_link(Some(function_definition_table_index));
                    return Ok(());
                } else {
                    return Err(SemanticError::MismatchMemberFunctionDeclAndDef(
                        ast.get_leftmost_token(function_node_index).clone(),
                        function_name.clone(),
                        function_declaration_entry.get_kind_clone(),
                        function_definition_entry.get_kind_clone(),
                    ));
                }
            }
            None => {
                return Err(SemanticError::MemberFunctionDefDoesNotHaveDecl(
                    ast.get_leftmost_token(function_node_index).clone(),
                    function_name.clone(),
                ));
            }
        }
    } else {
        // If free function, add to global.
        add_entry_to_table(ast, table_index, function_node_index)?;
    }

    Ok(())
}

fn add_entry_to_table(
    ast: &mut AST,
    table_index: usize,
    node_index: usize,
) -> Result<(), SemanticError> {
    check_duplicate(ast, table_index, node_index)?;
    let entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    ast.symbol_table_arena.add_entry(table_index, entry_index);

    Ok(())
}

fn check_duplicate(ast: &AST, table_index: usize, node_index: usize) -> Result<(), SemanticError> {
    let entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    let name = ast
        .symbol_table_arena
        .get_table_entry(entry_index)
        .get_name();
    for entry in ast.symbol_table_arena.get_table_entries(table_index) {
        if ast
            .symbol_table_arena
            .get_table_entry(*entry)
            .has_name(name)
        {
            return Err(SemanticError::DuplicateIdentifier(
                ast.get_leftmost_token(node_index).clone(),
                ast.symbol_table_arena
                    .get_table(table_index)
                    .get_name_clone(),
                name.clone(),
            ));
        }
    }

    Ok(())
}

fn get_for_node_indices(ast: &mut AST, node_index: usize, for_node_indices: &mut Vec<usize>) {
    for child_index in ast.get_children(node_index) {
        get_for_node_indices(ast, child_index, for_node_indices);
    }

    if let NodeType::ForStat = ast.get_node_type(node_index) {
        if ast.get_element(node_index).symbol_table_entry.is_some() {
            for_node_indices.push(node_index);
        }
    }
}

fn add_variables_to_table(
    ast: &mut AST,
    semantic_errors: &mut Vec<SemanticError>,
    node_index: usize,
    table_index: usize,
) {
    // Get variables
    for variable_index in ast.get_children(node_index).to_vec() {
        if ast
            .get_mut_element(variable_index)
            .symbol_table_entry
            .is_some()
        {
            if let NodeType::ForStat = ast.get_node_type(variable_index) {
                continue;
            }
            if let Err(error) = add_entry_to_table(ast, table_index, variable_index) {
                semantic_errors.push(error);
            }
        }
    }

    let mut for_node_indices: Vec<usize> = Vec::new();
    get_for_node_indices(ast, node_index, &mut for_node_indices);
    for for_node_index in for_node_indices {
        if let Err(error) = add_entry_to_table(ast, table_index, for_node_index) {
            semantic_errors.push(error);
        }
    }
}
