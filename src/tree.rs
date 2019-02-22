#[derive(Debug)]
pub struct Tree<E> {
    pub root: Option<usize>,
    pub nodes: Vec<Node<E>>,
}

#[derive(Debug)]
pub struct Node<E> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub element: E,
}

impl<E> Tree<E>
where
    E: Copy,
{
    fn get_right_sibling(&self, node: usize) -> Option<usize> {
        match self.nodes[node].parent {
            None => None,
            Some(parent) => {
                if node == *self.nodes[parent].children.last().unwrap() {
                    None
                } else {
                    let position = self.nodes[parent]
                        .children
                        .iter()
                        .position(|&n| n == node)
                        .unwrap();
                    Some(self.nodes[parent].children[position + 1])
                }
            }
        }
    }
    fn get_leftmost_sibling(&self, node: usize) -> usize {
        match self.nodes[node].parent {
            None => node,
            Some(parent) => self.nodes[parent].children[0],
        }
    }
    fn sibling_iter(&self, node: usize) -> SiblingIterator<E> {
        SiblingIterator {
            tree: self,
            index: node,
        }
    }

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
            let mut parent_node = self.nodes.get_mut(parent_id).unwrap();
            parent_node.children.insert(0, child_id);
        }
        {
            let mut child_node = self.nodes.get_mut(child_id).unwrap();
            child_node.parent = Some(parent_id);
        }
    }

    pub fn get_element(&self, node_id: usize) -> E {
        self.nodes.get(node_id).unwrap().element
    }
}

// FIXME: I don't need all of this. I can just iterate over the children vec.
pub struct SiblingIterator<'a, E> {
    tree: &'a Tree<E>,
    index: usize,
}
impl<'a, E> Iterator for SiblingIterator<'a, E>
where
    E: Copy,
{
    type Item = usize;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.tree.get_right_sibling(self.index) {
            Some(node) => {
                self.index = node;
                Some(node)
            }
            None => None,
        }
    }
}
