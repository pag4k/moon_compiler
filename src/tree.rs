pub struct Tree<E> {
    root: usize,
    nodes: Vec<Node<E>>,
}

pub struct Node<E> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub element: E
}

impl<E> Tree<E> {
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
}

// FIXME: I don't need all of this. I can just iterate over the children vec.
pub struct SiblingIterator<'a, E> {
    tree: &'a Tree<E>,
    index: usize,
}
impl<'a, E> Iterator for SiblingIterator<'a, E> {
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
