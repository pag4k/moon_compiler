// use std::collections::HashMap;

// pub struct Element<E> {
//     element: HashMap<usize, E>,
// }

// impl<E> Element<E> {
//     fn get(&self, node_index: usize) -> Option<&E> {
//         self.element.get(&node_index)
//     }
//     fn set(&mut self, node_index: usize, element: E) {
//         self.element.insert(node_index, element);
//     }
// }

use crate::register_pool::*;

pub struct Tree<E: Sized, S: Sized, M> {
    pub root: Option<usize>,
    pub nodes: Vec<Node<E>>,
    pub symbol_table_arena: S,
    pub memory_table_arena: M,
    pub register_pool: RegisterPool,
}

pub struct Node<E> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub element: E,
}

impl<E, T, M> Default for Tree<E, T, M>
where
    T: Default,
    M: Default,
{
    fn default() -> Self {
        Tree {
            root: None,
            nodes: Vec::new(),
            symbol_table_arena: T::default(),
            memory_table_arena: M::default(),
            register_pool: RegisterPool::new(1, 12),
        }
    }
}

impl<E, T, M> Tree<E, T, M>
where
    E: Clone,
{
    pub fn new_node(&mut self, element: E) -> usize {
        let index = self.nodes.len();
        let node = Node {
            index,
            parent: None,
            children: Vec::new(),
            element,
        };
        self.nodes.push(node);
        index
    }

    pub fn add_left_child(&mut self, parent_id: usize, child_id: usize) {
        {
            let parent_node = &mut self.nodes[parent_id];
            parent_node.children.insert(0, child_id);
        }
        {
            let mut child_node = &mut self.nodes[child_id];
            child_node.parent = Some(parent_id);
        }
    }

    pub fn get_element(&self, node_id: usize) -> &E {
        &self.nodes[node_id].element
    }

    pub fn get_mut_element(&mut self, node_id: usize) -> &mut E {
        &mut self.nodes[node_id].element
    }
    pub fn get_parent(&self, node_id: usize) -> Option<usize> {
        self.nodes[node_id].parent
    }
    pub fn get_child(&self, node_id: usize, child_index: usize) -> usize {
        self.nodes[node_id].children[child_index]
    }
    pub fn get_left_sibling(&self, node_id: usize) -> Option<usize> {
        let parent_index = self.nodes[node_id].parent?;
        let children = self.get_children(parent_index);
        let node_position = children
            .iter()
            .position(|&sibling_index| sibling_index == node_id)
            .unwrap();
        if node_position > 0 {
            Some(children[node_position - 1])
        } else {
            None
        }
    }
    pub fn get_right_sibling(&self, node_id: usize) -> Option<usize> {
        let parent_index = self.nodes[node_id].parent?;
        let children = self.get_children(parent_index);
        let node_position = children
            .iter()
            .position(|&sibling_index| sibling_index == node_id)
            .unwrap();
        if node_position < children.len() - 1 {
            Some(children[node_position + 1])
        } else {
            None
        }
    }
    pub fn get_children(&self, node_id: usize) -> Vec<usize> {
        self.nodes[node_id].children.to_vec()
    }

    pub fn get_children_of_child(&self, node_index: usize, child_index: usize) -> Vec<usize> {
        self.get_children(self.get_children(node_index)[child_index])
            .to_vec()
    }
}
