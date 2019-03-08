use crate::ast_node::*;
use crate::symbol_table::*;
use crate::tree::*;

impl Tree<NodeElement, SymbolTableArena> {
    pub fn generate_symbol_table(&mut self) {
        //FIXME: Verify if there is a root node.
        self.create_symbol_table(self.root.unwrap());
    }

    fn create_symbol_table(&mut self, node_index: usize) {
        use NodeType::*;

        for child_index in self.get_children(node_index).to_vec() {
            self.create_symbol_table(child_index);
        }

        match self.get_element(node_index).node_type {
            Prog => {
                let table_index = self
                    .symbol_table_arena
                    .new_symbol_table("Global".to_string());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                // Set root.
                self.symbol_table_arena.root = Some(table_index);

                // Add classes.
                for node_index in self.get_children(self.get_children(node_index)[0]).to_vec() {
                    self.symbol_table_arena.add_entry(
                        table_index,
                        self.get_element(node_index).symbol_table_entry.unwrap(),
                    );
                }
                // Add functions.
                for node_index in self.get_children(self.get_children(node_index)[1]).to_vec() {
                    self.symbol_table_arena.add_entry(
                        table_index,
                        self.get_element(node_index).symbol_table_entry.unwrap(),
                    );
                }

                // TODO: ADD PROGRAM/MAIN
            }
            ClassDecl => {
                let name = self.get_name(node_index, 0);

                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Class,
                    Some(table_index),
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            FuncDef => {
                let name = self.get_name(node_index, 2);

                let table_index = self.symbol_table_arena.new_symbol_table(name.clone());
                self.get_mut_element(node_index).symbol_table = Some(table_index);

                let return_type = self.get_type(node_index, 0, Vec::new());

                let mut parameters = Vec::new();
                for parameter in self.get_children(self.get_children(node_index)[3]).to_vec() {
                    let name = self.get_name(parameter, 1);
                    let mut indices = Vec::new();
                    for index in self.get_children(self.get_children(parameter)[2]) {
                        indices.push(self.get_index(*index));
                    }
                    let parameter_type = self.get_type(parameter, 0, indices);

                    parameters.push(parameter_type.clone());
                    self.symbol_table_arena.new_symbol_table_entry(
                        name,
                        SymbolKind::Parameter(parameter_type),
                        None,
                    );
                }

                let entry_index = self.symbol_table_arena.new_symbol_table_entry(
                    name,
                    SymbolKind::Function(Some(return_type), parameters),
                    Some(table_index),
                );
                self.get_mut_element(node_index).symbol_table_entry = Some(entry_index);
            }
            VarDecl => {}
            _ => {}
        };
    }

    fn get_name(&self, node_index: usize, child_index: usize) -> String {
        self.get_element(self.get_children(node_index)[child_index])
            .clone()
            .data
            .unwrap()
    }

    fn get_type(&self, node_index: usize, child_index: usize, indices: Vec<usize>) -> SymbolType {
        SymbolType::new(
            &self
                .get_element(self.get_children(node_index)[child_index])
                .clone()
                .data
                .unwrap(),
            indices,
        )
    }

    fn get_index(&self, node_index: usize) -> usize {
        self.get_element(node_index)
            .clone()
            .data
            .unwrap()
            .parse::<usize>()
            .unwrap()
    }
}
