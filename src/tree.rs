pub struct Tree<E, T> {
    pub root: Option<usize>,
    pub nodes: Vec<Node<E>>,
    pub symbol_table_arena: T,
}

pub struct Node<E> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub element: E,
}

impl<E, T> Default for Tree<E, T>
where
    T: Default,
{
    fn default() -> Self {
        Tree {
            root: None,
            nodes: Vec::new(),
            symbol_table_arena: T::default(),
        }
    }
}

impl<E, T> Tree<E, T>
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

    pub fn get_children(&self, node_id: usize) -> &[usize] {
        &self.nodes[node_id].children
    }

    pub fn get_children_of_child(&self, node_index: usize, child_index: usize) -> Vec<usize> {
        self.get_children(self.get_children(node_index)[child_index])
            .to_vec()
    }
}
