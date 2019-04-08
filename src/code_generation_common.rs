use crate::ast_node::*;
use crate::memory_table::*;
use crate::symbol_table::*;

pub fn get_class_node_index(ast: &AST, class_name: &str) -> Option<usize> {
    ast.get_children_of_child(ast.root.unwrap(), 0)
        .into_iter()
        .find(|&child_index| {
            ast.symbol_table_arena
                .get_table(ast.get_element(child_index).symbol_table.unwrap())
                .has_name(class_name)
        })
}

pub fn get_size(ast: &AST, symbol_type: &SymbolType) -> Option<usize> {
    use SymbolType::*;
    match symbol_type {
        Integer(dimension_list) => Some(dimension_list.iter().product::<usize>() * 4),
        Float(dimension_list) => Some(dimension_list.iter().product::<usize>() * 8),
        Class(name, dimension_list) => get_class_size(ast, name)
            .map(|class_size| dimension_list.iter().product::<usize>() * class_size),
    }
}

pub fn get_class_size(ast: &AST, class_name: &str) -> Option<usize> {
    match get_class_node_index(ast, class_name) {
        // Check if the class can be found.
        Some(node_index) => {
            // Check if the class has a memory table.
            if ast.has_memory_table_index(node_index) {
                let memory_table_index = ast.get_memory_table_index(node_index);
                Some(
                    ast.memory_table_arena
                        .get_table(memory_table_index)
                        .offset
                        .abs() as usize,
                )
            } else {
                None
            }
        }
        None => None,
    }
}

pub fn symbol_to_variabl_type(symbol_type: &SymbolType) -> VariableType {
    use SymbolType::*;
    match symbol_type {
        Integer(_) => VariableType::Integer,
        Float(_) => VariableType::Float,
        Class(name, _) => VariableType::Class(name.clone()),
    }
}

pub fn get_symbol_entry(ast: &AST, node_index: usize) -> &SymbolTableEntry {
    ast.symbol_table_arena
        .get_table_entry(ast.get_symbol_entry_index(node_index))
}

pub fn get_memory_entry(ast: &AST, node_index: usize) -> &MemoryTableEntry {
    ast.memory_table_arena
        .get_table_entry(ast.get_memory_entry_index(node_index))
}

pub fn get_memory_from_symbol(ast: &AST, node_index: usize) -> Option<usize> {
    use NodeType::*;
    let symbol_entry_index = ast.get_symbol_entry_index(node_index);
    let symbol_entry = get_symbol_entry(ast, node_index);
    let symbol_name = &symbol_entry.get_name();

    // Check if it is a member variable.
    if let Some(class_node_index) = get_class_node_of_symbol_entry(ast, symbol_entry_index) {
        return find_variable_memory_entry(ast, class_node_index, symbol_name);
    }

    // If not, it must be a local variable.
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
    {
        return find_variable_memory_entry(ast, function_node_index, symbol_name);
    }

    unreachable!();
}

pub fn get_inst_addr_offset(ast: &AST, memory_table_index_index: usize) -> Option<isize> {
    for &entry_index in ast
        .memory_table_arena
        .get_table_entries(memory_table_index_index)
    {
        let memory_entry = &ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_inst_addr() {
            return Some(memory_entry.get_offset());
        }
    }
    None
}

pub fn get_function_node_index_and_memory_table_index(
    ast: &AST,
    node_index: usize,
) -> Option<(usize, usize)> {
    use NodeType::*;
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
    {
        Some((
            function_node_index,
            ast.get_memory_table_index(function_node_index),
        ))
    } else {
        None
    }
}

pub fn find_variable_memory_entry(
    ast: &AST,
    node_index: usize,
    variable_name: &str,
) -> Option<usize> {
    let memory_table_index = ast.get_memory_table_index(node_index);
    for &entry_index in ast.memory_table_arena.get_table_entries(memory_table_index) {
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_named_var() && *variable_name == memory_entry.get_name() {
            return Some(entry_index);
        }
    }
    None
}

pub fn get_class_node_of_symbol_entry(ast: &AST, symbol_entry_index: usize) -> Option<usize> {
    for class_node_index in ast.get_children_of_child(ast.root.unwrap(), 0) {
        let class_table_index = ast.get_symbol_table_index(class_node_index);
        if ast
            .symbol_table_arena
            .get_table_entries(class_table_index)
            .iter()
            .any(|&entry_index| entry_index == symbol_entry_index)
        {
            return Some(class_node_index);
        }
    }
    None
}
