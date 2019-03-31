use crate::ast_node::*;

use std::collections::HashMap;

pub type SemanticActionMap<ErrorType> = HashMap<NodeType, fn(&mut AST, &mut Vec<ErrorType>, usize)>;

pub fn ast_traversal<ErrorType>(
    ast: &mut AST,
    errors: &mut Vec<ErrorType>,
    semantic_actions: &SemanticActionMap<ErrorType>,
) {
    visit(ast, errors, semantic_actions, ast.root.unwrap());
}

fn visit<ErrorType>(
    ast: &mut AST,
    errors: &mut Vec<ErrorType>,
    semantic_actions: &SemanticActionMap<ErrorType>,
    node_index: usize,
) {
    for child_index in ast.get_children(node_index).to_vec() {
        visit(ast, errors, semantic_actions, child_index);
    }

    let node_type = ast.get_node_type(node_index);
    if let Some(semantic_action) = semantic_actions.get(&node_type) {
        semantic_action(ast, errors, node_index);
    }
}
