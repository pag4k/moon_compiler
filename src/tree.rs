pub struct Tree<E> {
    pub root: Option<usize>,
    pub nodes: Vec<Node<E>>,
}

pub struct Node<E> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub element: E,
}

impl<E> Default for Tree<E> {
    fn default() -> Self {
        Tree {
            root: None,
            nodes: Vec::new(),
        }
    }
}

impl<E> Tree<E>
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

    pub fn get_children(&self, node_id: usize) -> &[usize] {
        &self.nodes[node_id].children
    }
}
