use crate::ast_node::*;
use crate::ast_visitor::*;
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
    semantic_actions.insert(MainFuncBody, stat_block);
    semantic_actions.insert(FuncBody, stat_block);

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

    // Rename table from StatBlock
    let name = "main".to_string();
    let stat_block_node_index = ast.get_children(node_index)[2];
    let program_table_index = ast.get_element(stat_block_node_index).symbol_table.unwrap();
    ast.symbol_table_arena
        .get_mut_table(program_table_index)
        .name = name.clone();

    // Create main function entry and add it to the global scope.
    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name,
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
    let name = ast.get_child_lexeme(node_index, 0);

    let table_index = ast.symbol_table_arena.new_symbol_table(name.clone());
    ast.get_mut_element(node_index).symbol_table = Some(table_index);

    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name.clone(),
        SymbolKind::Class,
        Some(table_index),
    );
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
    let name = ast.get_child_lexeme(node_index, 2);

    // Take table from StatBlock
    let table_index = transfer_symbol_table(ast, ast.get_child(node_index, 4), node_index);
    ast.symbol_table_arena.get_mut_table(table_index).name = name.clone();

    let return_type = make_type_from_child(ast, node_index, 0, Vec::new());

    // Get parameters
    let mut parameters = Vec::new();
    for parameter_index in ast.get_children_of_child(node_index, 3) {
        let entry_index = ast
            .get_mut_element(parameter_index)
            .symbol_table_entry
            .unwrap();
        let parameter_type = match ast
            .symbol_table_arena
            .get_table_entry(entry_index)
            .kind
            .clone()
        {
            SymbolKind::Parameter(parameter_type) => parameter_type,
            _ => unreachable!(),
        };

        parameters.push(parameter_type.clone());
        if let Err(error) = add_entry_to_table(ast, table_index, parameter_index) {
            semantic_errors.push(error);
        }
    }

    let entry_index = ast.symbol_table_arena.new_symbol_table_entry(
        name,
        SymbolKind::Function(Some(return_type), parameters),
        Some(table_index),
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn func_decl(ast: &mut AST, _semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Function Declaration just has an entry.
    let name = ast.get_child_lexeme(node_index, 1);
    let return_type = make_type_from_child(ast, node_index, 0, Vec::new());

    // Get parameters
    let mut parameters = Vec::new();
    for parameter_index in ast.get_children_of_child(node_index, 2) {
        let entry_index = ast
            .get_mut_element(parameter_index)
            .symbol_table_entry
            .unwrap();
        let parameter_type = match ast
            .symbol_table_arena
            .get_table_entry(entry_index)
            .kind
            .clone()
        {
            SymbolKind::Parameter(parameter_type) => parameter_type,
            _ => unreachable!(),
        };

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
                ast.get_leftmost_token(node_index),
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
        name,
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
                ast.get_leftmost_token(node_index),
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
        name,
        SymbolKind::Parameter(parameter_type),
        None,
    );
    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
}
fn for_stat(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // FIXME: Check if for variable is integer.
    let symbol_type = make_type_from_child(ast, node_index, 0, Vec::new());

    let name = ast.get_child_lexeme(node_index, 1);
    //let table_index = ast.symbol_table_arena.new_symbol_table(name.clone());
    let entry_index =
        ast.symbol_table_arena
            .new_symbol_table_entry(name, SymbolKind::For(symbol_type), None);
    //let first_child_index = ast.get_children(node_index)[0];

    ast.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
    // if let Err(error) = add_entry_to_table(ast, table_index, first_child_index) {
    //     semantic_errors.push(error);
    // }
    //ast.get_mut_element(node_index).symbol_table = Some(table_index);
}
fn stat_block(ast: &mut AST, semantic_errors: &mut Vec<SemanticError>, node_index: usize) {
    // Create a table without a name.
    let table_index = ast.symbol_table_arena.new_symbol_table(String::new());
    ast.get_mut_element(node_index).symbol_table = Some(table_index);

    // Get variables
    for variable_index in ast.get_children(node_index).to_vec() {
        if ast
            .get_mut_element(variable_index)
            .symbol_table_entry
            .is_some()
        {
            if ast.get_element(variable_index).node_type == NodeType::ForStat {
                continue;
            }
            if let Err(error) = add_entry_to_table(ast, table_index, variable_index) {
                semantic_errors.push(error);
            }
        }
    }

    let mut for_node_indices: Vec<usize> = Vec::new();
    get_for_node_indices(ast, node_index, &mut for_node_indices);
    println!("{}", for_node_indices.len());
    for for_node_index in for_node_indices {
        if let Err(error) = add_entry_to_table(ast, table_index, for_node_index) {
            semantic_errors.push(error);
        }
    }
}

fn make_type_from_child(
    ast: &AST,
    node_index: usize,
    child_index: usize,
    indices: Vec<usize>,
) -> SymbolType {
    SymbolType::new(
        &ast.get_element(ast.get_children(node_index)[child_index])
            .token
            .as_ref()
            .unwrap()
            .lexeme
            .clone()
            .unwrap(),
        indices,
    )
}

fn get_dimension(ast: &AST, node_index: usize) -> usize {
    // We can directly parse since a non integer dimensions is a syntax error.
    ast.get_element(node_index)
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .as_ref()
        .unwrap()
        .parse::<usize>()
        .unwrap()
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
        .name
        .clone();

    match ast
        .get_element(ast.get_children(function_node_index)[1])
        .token
        .clone()
    {
        // If member function, link it in its class.
        Some(token) => {
            let class_name = token.lexeme.clone().unwrap();
            let class_table_index = match ast.find_class_symbol_table(&class_name) {
                Some(class_table_index) => class_table_index,
                None => {
                    return Err(SemanticError::UndefinedClass(
                        ast.get_token(ast.get_children(function_node_index)[1]),
                    ));
                }
            };
            match ast.find_function_in_table(class_table_index, &function_name) {
                Some(function_declaration_entry_index) => {
                    // Get a clone of the definition entry and remore link.
                    let mut function_definition_entry = (*ast
                        .symbol_table_arena
                        .get_table_entry(function_definition_table_entry_index))
                    .clone();
                    function_definition_entry.link = None;
                    // Get a clone of the definition entry.
                    let function_declaration_entry = (*ast
                        .symbol_table_arena
                        .get_table_entry(function_declaration_entry_index))
                    .clone();
                    // Compare declaration and definition, assumin neither has a link.
                    if function_declaration_entry == function_definition_entry {
                        ast.symbol_table_arena
                            .get_mut_table_entry(function_declaration_entry_index)
                            .link = Some(function_definition_table_index);
                        return Ok(());
                    } else {
                        return Err(SemanticError::MismatchMemberFunctionDeclAndDef(
                            ast.get_leftmost_token(function_node_index),
                            function_name.clone(),
                            function_declaration_entry.kind,
                            function_definition_entry.kind,
                        ));
                    }
                }
                None => {
                    return Err(SemanticError::MemberFunctionDefDoesNotHaveDecl(
                        ast.get_leftmost_token(function_node_index),
                        function_name.clone(),
                    ));
                }
            }
        }
        // If free function, add to global.
        None => {
            add_entry_to_table(ast, table_index, function_node_index)?;
        }
    }
    Ok(())
}

fn transfer_symbol_table(
    ast: &mut AST,
    origin_node_index: usize,
    destination_node_index: usize,
) -> usize {
    let table_index = ast.get_element(origin_node_index).symbol_table.unwrap();
    ast.get_mut_element(origin_node_index).symbol_table = None;
    ast.get_mut_element(destination_node_index).symbol_table = Some(table_index);
    table_index
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
    let name = &ast.symbol_table_arena.get_table_entry(entry_index).name;
    for entry in ast.symbol_table_arena.get_table_entries(table_index) {
        if ast.symbol_table_arena.get_table_entry(*entry).name == *name {
            return Err(SemanticError::DuplicateIdentifier(
                ast.get_leftmost_token(node_index),
                ast.symbol_table_arena.get_table(table_index).name.clone(),
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

    if let NodeType::ForStat = ast.get_element(node_index).node_type {
        if ast.get_element(node_index).symbol_table_entry.is_some() {
            for_node_indices.push(node_index);
        }
    }
}
