use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::code_generation_error::*;
use crate::memory_table::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn memory_table_generator_visitor(ast: &mut AST) -> Vec<CodeGenError> {
    use NodeType::*;
    let mut code_gen_errors: Vec<CodeGenError> = Vec::new();
    let mut semantic_actions: SemanticActionMap<CodeGenError> = HashMap::new();
    semantic_actions.insert(ClassDecl, class_decl);
    semantic_actions.insert(MainFuncBody, main_func_body);
    semantic_actions.insert(FuncDef, func_def);
    semantic_actions.insert(VarDecl, var_decl);
    //semantic_actions.insert(DataMember, var_element);
    semantic_actions.insert(FunctionCall, var_element);
    semantic_actions.insert(VarElementList, var_element_list);
    // ForVar
    semantic_actions.insert(ForStat, for_var);
    // TempVar
    semantic_actions.insert(RelExpr, temp_var);
    semantic_actions.insert(AddOp, temp_var);
    semantic_actions.insert(MultOp, temp_var);
    semantic_actions.insert(Not, temp_var);
    semantic_actions.insert(Sign, temp_var);
    // LitVar
    semantic_actions.insert(Num, lit_var);

    ast_traversal(ast, &mut code_gen_errors, &semantic_actions);
    code_gen_errors
}

fn class_decl(ast: &mut AST, code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    // Check if inherited classes have been sized.
    if ast
        .get_children_of_child(node_index, 1)
        .iter()
        .any(|&child_index| {
            get_class_size(
                ast,
                &ast.symbol_table_arena
                    .get_table_entry(ast.get_element(child_index).symbol_table_entry.unwrap())
                    .name,
            )
            .is_none()
        })
    {
        code_gen_errors.push(CodeGenError::ClassNotSizedYet);
        return;
    }

    // Check if member variables have been sized.
    if ast
        .get_children_of_child(node_index, 2)
        .iter()
        .filter(|&&child_index| {
            ast.symbol_table_arena
                .get_table_entry(ast.get_element(child_index).symbol_table_entry.unwrap())
                .kind
                .is_variable()
        })
        .any(|&child_index| ast.get_element(child_index).memory_table_entry.is_none())
    {
        code_gen_errors.push(CodeGenError::ClassNotSizedYet);
        return;
    }

    if ast.get_element(node_index).memory_table.is_none() {
        let symbol_table_index = ast.get_element(node_index).symbol_table.unwrap();
        let class_name = &ast.symbol_table_arena.get_table(symbol_table_index).name;
        let memory_table_index = ast
            .memory_table_arena
            .new_memory_table(symbol_table_index, class_name.clone());
        ast.get_mut_element(node_index).memory_table = Some(memory_table_index);
        // Add inherited classes.
        for child_index in ast.get_children_of_child(node_index, 1).into_iter() {
            let inherited_class_name = &ast
                .symbol_table_arena
                .get_table_entry(ast.get_element(child_index).symbol_table_entry.unwrap())
                .name;
            let variable_type = VariableType::Class(inherited_class_name.clone());
            let size = get_class_size(ast, inherited_class_name).unwrap();
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::Inherited,
                variable_type,
                size,
            );
            ast.memory_table_arena
                .add_entry(memory_table_index, entry_index);
        }
        // Add member variables.
        for child_index in ast.get_children_of_child(node_index, 2).into_iter() {
            if ast.get_element(child_index).memory_table_entry.is_some() {
                let entry_index = ast.get_element(child_index).memory_table_entry.unwrap();
                ast.memory_table_arena
                    .add_entry(memory_table_index, entry_index);
            }
        }
    }
}

fn main_func_body(ast: &mut AST, code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if !code_gen_errors.is_empty() {
        return;
    }
    let symbol_table_index = ast.get_element(node_index).symbol_table.unwrap();
    let memory_table_index = ast
        .memory_table_arena
        .new_memory_table(symbol_table_index, "program".to_string());
    ast.get_mut_element(node_index).memory_table = Some(memory_table_index);

    // Add var, temp_var and lit_var.
    for child_index in ast.get_children(node_index).into_iter() {
        let mut var_entries = Vec::new();
        get_var(ast, child_index, &mut var_entries);
        for entry_index in var_entries {
            ast.memory_table_arena
                .add_entry(memory_table_index, entry_index);
        }
    }
}

fn func_def(ast: &mut AST, code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if !code_gen_errors.is_empty() {
        return;
    }
    let symbol_table_index = ast.get_element(node_index).symbol_table.unwrap();
    let class_name = &ast.symbol_table_arena.get_table(symbol_table_index).name;
    let memory_table_index = ast
        .memory_table_arena
        .new_memory_table(symbol_table_index, class_name.clone());
    ast.get_mut_element(node_index).memory_table = Some(memory_table_index);

    let entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();

    // Add return variable.
    let return_symbol_type = match &ast.symbol_table_arena.get_table_entry(entry_index).kind {
        SymbolKind::Function(return_symbol_type, _) => return_symbol_type.as_ref().unwrap(),
        _ => unreachable!(),
    };
    let variable_type = symbol_to_variabl_type(&return_symbol_type);
    let entry_index = ast.memory_table_arena.new_memory_table_entry(
        VariableKind::ReturnVar,
        variable_type,
        get_size(ast, return_symbol_type).unwrap(),
    );
    ast.memory_table_arena
        .add_entry(memory_table_index, entry_index);

    // Add return address
    let entry_index = ast.memory_table_arena.new_memory_table_entry(
        VariableKind::ReturnAddr,
        VariableType::Integer,
        4,
    );
    ast.memory_table_arena
        .add_entry(memory_table_index, entry_index);

    // If member variable, add parameter for instance address.
    let class_node_index = ast.get_child(node_index, 1);
    if let NodeType::Type = ast.get_element(class_node_index).node_type {
        let class_name = ast
            .get_element(class_node_index)
            .token
            .clone()
            .unwrap()
            .lexeme
            .unwrap();
        let entry_index = ast.memory_table_arena.new_memory_table_entry(
            VariableKind::InstAddr,
            VariableType::Class(class_name),
            get_size(ast, return_symbol_type).unwrap(),
        );
        ast.memory_table_arena
            .add_entry(memory_table_index, entry_index);
    }

    // Add parameters in reverse order (need to check the nodes since the symbol table does not have their names).
    for child_index in ast.get_children_of_child(node_index, 3).into_iter().rev() {
        let symbol_entry = ast
            .symbol_table_arena
            .get_table_entry(ast.get_element(child_index).symbol_table_entry.unwrap());
        let variable_name = &symbol_entry.name;
        let symbol_type = match &symbol_entry.kind {
            SymbolKind::Parameter(symbol_type) => symbol_type,
            _ => unreachable!(),
        };
        let variable_type = symbol_to_variabl_type(&symbol_type);
        let entry_index = ast.memory_table_arena.new_memory_table_entry(
            VariableKind::Param(variable_name.clone()),
            variable_type,
            get_size(ast, &symbol_type).unwrap(),
        );
        ast.memory_table_arena
            .add_entry(memory_table_index, entry_index);
    }

    // Add var, temp_var and lit_var.
    for child_index in ast.get_children_of_child(node_index, 4).into_iter() {
        let mut var_entries = Vec::new();
        get_var(ast, child_index, &mut var_entries);
        for entry_index in var_entries {
            ast.memory_table_arena
                .add_entry(memory_table_index, entry_index);
        }
    }
}

fn var_decl(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if ast.get_mut_element(node_index).memory_table_entry.is_none() {
        // FIXME: GET DATA TYPE
        let symbol_entry = ast
            .symbol_table_arena
            .get_table_entry(ast.get_element(node_index).symbol_table_entry.unwrap());
        let variable_name = &symbol_entry.name;
        let symbol_type = match &symbol_entry.kind {
            SymbolKind::Variable(symbol_type) => symbol_type,
            _ => unreachable!(),
        };
        let variable_type = symbol_to_variabl_type(&symbol_type);

        if let Some(size) = get_size(ast, &symbol_type) {
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::Var(variable_name.clone()),
                variable_type,
                size,
            );
            ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
        }
    }
}

fn var_element(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if ast.get_mut_element(node_index).memory_table_entry.is_none() {
        let data_type = ast.get_element(node_index).data_type.as_ref().unwrap();
        let variable_type = symbol_to_variabl_type(&data_type);
        let symbol_enty_index = ast.get_element(node_index).symbol_table_entry.unwrap();
        let symbol_enty = ast.symbol_table_arena.get_table_entry(symbol_enty_index);

        if let Some(size) = get_size(ast, &data_type) {
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::TempVar(0),
                variable_type,
                size,
            );
            ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
        }
    }
}

fn var_element_list(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    // if ast.get_mut_element(node_index).memory_table_entry.is_none() {
    //     let data_type = ast.get_element(node_index).data_type.as_ref().unwrap();
    //     let variable_type = symbol_to_variabl_type(&data_type);
    //     if let Some(size) = get_size(ast, &data_type) {
    //         let entry_index = ast.memory_table_arena.new_memory_table_entry(
    //             VariableKind::TempVar(0),
    //             variable_type,
    //             size,
    //         );
    //         ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
    //     }
    // }
}

fn for_var(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if ast.get_mut_element(node_index).memory_table_entry.is_none() {
        let symbol_entry = ast
            .symbol_table_arena
            .get_table_entry(ast.get_element(node_index).symbol_table_entry.unwrap());
        let variable_name = &symbol_entry.name;
        let symbol_type = match &symbol_entry.kind {
            SymbolKind::For(symbol_type) => symbol_type,
            _ => unreachable!(),
        };
        let variable_type = symbol_to_variabl_type(&symbol_type);

        if let Some(size) = get_size(ast, &symbol_type) {
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::ForVar(variable_name.clone()),
                variable_type,
                size,
            );
            ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
        }
    }
}

fn temp_var(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if ast.get_mut_element(node_index).memory_table_entry.is_none() {
        let data_type = ast.get_element(node_index).data_type.as_ref().unwrap();
        let variable_type = symbol_to_variabl_type(&data_type);
        if let Some(size) = get_size(ast, &data_type) {
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::TempVar(0),
                variable_type,
                size,
            );
            ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
        }
    }
}

fn lit_var(ast: &mut AST, _code_gen_errors: &mut Vec<CodeGenError>, node_index: usize) {
    if ast.get_mut_element(node_index).memory_table_entry.is_none() {
        let data_type = ast.get_element(node_index).data_type.as_ref().unwrap();
        let variable_type = symbol_to_variabl_type(&data_type);
        if let Some(size) = get_size(ast, &data_type) {
            let entry_index = ast.memory_table_arena.new_memory_table_entry(
                VariableKind::LitVar(0),
                variable_type,
                size,
            );
            ast.get_mut_element(node_index).memory_table_entry = Some(entry_index);
        }
    }
}

fn get_var(ast: &mut AST, node_index: usize, var_entries: &mut Vec<usize>) {
    for child_index in ast.get_children(node_index) {
        get_var(ast, child_index, var_entries);
    }

    if let Some(entry_index) = ast.get_element(node_index).memory_table_entry {
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        //if memory_entry.is_temp_var() || memory_entry.is_lit_var() {
        var_entries.push(entry_index);
        //} else {
        //    unreachable!();
        //}
    }
}
