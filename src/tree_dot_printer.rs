use crate::tree::*;

use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

impl<E, T, M> Tree<E, T, M>
where
    E: Display + Clone,
{
    pub fn print_tree_to(&self, filename: &str) -> Result<(), String> {
        let mut string = String::new();

        string.push_str("digraph AST {\n");
        string.push_str("\tnode [shape=record];\n");
        string.push_str(
            "\tnode [fontname=Sans];charset=\"UTF-8\" splines=true splines=spline rankdir =LR\n",
        );

        match self.root {
            Some(root) => {
                self.print_node_to(&mut string, root);
                self.print_edge_to(&mut string, root);
                string.push_str("}\n");
                // Add espace character for '||', '<' and '>' since they cause problem in DOT.
                string = string.replace(" ||", " \\|\\|");
                string = string.replace(" <", " \\<");
                string = string.replace(" >", " \\>");
            }
            None => return Err("ERROR: AST has not root.".to_string()),
        }

        let path = Path::new(filename);
        match File::create(&path).as_mut() {
            Ok(file) => match file.write_all(string.as_bytes()) {
                Ok(_) => {}
                Err(_) => {
                    return Err(format!(
                        "ERROR: Something went wrong writing DOT file: {}.",
                        filename
                    ));
                }
            },
            Err(_) => {
                return Err(format!(
                    "Something went wrong creating DOT file: {}.",
                    filename
                ));
            }
        }

        Ok(())
    }

    fn print_node_to(&self, string: &mut String, node_index: usize) {
        string.push_str(&format!(
            "\tnode [label=\"{}, {}\"] {};\n",
            node_index,
            self.get_element(node_index),
            node_index
        ));
        for child_index in self.get_children(node_index) {
            self.print_node_to(string, child_index);
        }
    }

    fn print_edge_to(&self, string: &mut String, node_index: usize) {
        for child_index in self.get_children(node_index) {
            string.push_str(&format!("\t{} -> {};\n", node_index, child_index));
            self.print_edge_to(string, child_index);
        }
    }
}
