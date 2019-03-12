use crate::ast_node::*;
use crate::symbol_table::*;
use crate::tree::*;

use std::collections::HashMap;

#[derive(Debug)]
pub enum SemanticError {}

impl Tree<NodeElement, SymbolTableArena> {
    pub fn semantic_checking(&mut self) -> Result<(), SemanticError> {
        //FIXME: Check if there is a root
        let mut table_stack: Vec<usize> = Vec::new();
        self.semantic_checking2(self.root.unwrap(), &mut table_stack)
    }

    pub fn semantic_checking2(
        &mut self,
        node_index: usize,
        table_stack: &mut Vec<usize>,
    ) -> Result<(), SemanticError> {
        use NodeType::*;

        if let Some(symbol_table_index) = self.get_mut_element(node_index).symbol_table {
            table_stack.push(symbol_table_index);
        }

        for child_index in self.get_children(node_index).to_vec() {
            self.semantic_checking2(child_index, table_stack)?;
        }

        match self.get_element(node_index).node_type {
            Prog => {}
            DataMember => {
                //let name = self.get_name(node_index, 0);
                // dbg!(&id);
                // dbg!(&table_stack);
            }
            _ => {}
        }

        if self.get_mut_element(node_index).symbol_table.is_some() {
            table_stack.pop();
        }
        Ok(())
    }
}
