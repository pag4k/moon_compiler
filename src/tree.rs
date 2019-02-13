struct Tree {
    root: usize,
    nodes: Vec<Node>,
}

struct Node {
    index: usize,
    parent: Option<usize>,
    children: Vec<usize>,
}

impl Tree {
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
    fn sibling_iter(&self, node: usize) -> SiblingIterator {
        SiblingIterator {
            tree: self,
            index: node,
        }
    }
}

// FIXME: I don't need all of this. I can just iterate over the children vec.
pub struct SiblingIterator<'a> {
    tree: &'a Tree,
    index: usize,
}
impl<'a> Iterator for SiblingIterator<'a> {
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
