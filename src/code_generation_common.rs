use crate::ast_node::*;
use crate::memory_table::*;
use crate::symbol_table::*;

pub fn get_class_node_index(ast: &AST, class_name: &str) -> Option<usize> {
    // println!("{}", class_name);
    // println!("{:?}", ast.symbol_table_arena.root.unwrap());
    ast.get_children_of_child(ast.root.unwrap(), 0)
        .into_iter()
        .find(|&child_index| {
            ast.symbol_table_arena
                .get_table(ast.get_element(child_index).symbol_table.unwrap())
                .name
                == class_name
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
    get_class_node_index(ast, class_name)
        .map(|node_index| {
            ast.get_element(node_index)
                .memory_table
                .map(|table_index| ast.memory_table_arena.get_table(table_index).size)
        })
        .unwrap()
}

pub fn symbol_to_variabl_type(symbol_type: &SymbolType) -> VariableType {
    use SymbolType::*;
    match symbol_type {
        Integer(_) => VariableType::Integer,
        Float(_) => VariableType::Float,
        Class(name, _) => VariableType::Class(name.clone()),
    }
}
