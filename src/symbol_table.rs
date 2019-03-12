use std::fmt::{Display, Formatter};
use std::str::FromStr;

const integer: &str = "integer";
const float: &str = "float";
const none: &str = "None";

#[derive(Clone, PartialEq, Eq)]
pub enum SymbolType {
    Integer(Vec<usize>),
    Float(Vec<usize>),
    Class(String, Vec<usize>),
}

// impl FromStr for SymbolType {
//     type Err = String;
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         use SymbolType::*;
//         match s {
//             "Integer" => Ok(Integer(Vec::new())),
//             "Float" => Ok(Float(Vec::new())),
//             _ => Ok(Class(s.to_string(), Vec::new())),
//         }
//     }
// }

impl SymbolType {
    pub fn new(symbol_type: &str, indices: Vec<usize>) -> Self {
        use SymbolType::*;
        match symbol_type {
            "Integer" => Integer(indices),
            "Float" => Float(indices),
            _ => Class(symbol_type.to_string(), indices),
        }
    }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolType::*;
        let (symbol_type, indices) = match self {
            Integer(indices) => (integer.to_string(), indices),
            Float(indices) => (float.to_string(), indices),
            Class(name, indices) => (name.clone(), indices),
        };
        let mut indices_str = String::new();
        for index in indices.iter() {
            indices_str.push_str(&format!("[{}]", index));
        }

        write!(f, "{}", format!("{}{}", symbol_type, indices_str))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Class,
    Function(Option<SymbolType>, Vec<SymbolType>),
    Parameter(SymbolType),
    Variable(SymbolType),
}

impl Display for SymbolKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolKind::*;
        let output = match self {
            Class => format!("Class"),
            Function(return_type, types) => {
                let mut parameters = String::new();
                for symbol_type in types.iter() {
                    parameters.push_str(&format!("{}, ", symbol_type));
                }
                parameters.pop();
                parameters.pop();
                format!(
                    "Function: Return Type: {}, Parameters: {}",
                    match return_type {
                        Some(return_type) => format!("{}", return_type),
                        None => none.to_string(),
                    },
                    parameters
                )
            }
            Parameter(symbol_type) => format!("Parameter: {}", symbol_type),
            Variable(symbol_type) => format!("Variable: {}", symbol_type),
        };

        write!(f, "{}", output)
    }
}

pub struct SymbolTableArena {
    pub root: Option<usize>,
    symbol_tables: Vec<SymbolTable>,
    symbol_table_entries: Vec<SymbolTableEntry>,
}

impl Default for SymbolTableArena {
    fn default() -> Self {
        SymbolTableArena {
            root: None,
            symbol_tables: Vec::new(),
            symbol_table_entries: Vec::new(),
        }
    }
}
impl SymbolTableArena {
    pub fn new_symbol_table(&mut self, name: String) -> usize {
        let index = self.symbol_tables.len();
        let symbol_table = SymbolTable {
            name,
            index,
            entries: Vec::new(),
        };
        self.symbol_tables.push(symbol_table);
        index
    }
    pub fn new_symbol_table_entry(
        &mut self,
        name: String,
        kind: SymbolKind,
        link: Option<usize>,
    ) -> usize {
        let index = self.symbol_table_entries.len();
        let symbol_table_entry = SymbolTableEntry { name, kind, link };
        self.symbol_table_entries.push(symbol_table_entry);
        index
    }
    pub fn add_entry(&mut self, table_index: usize, entry_index: usize) {
        self.symbol_tables[table_index].entries.push(entry_index);
    }
    pub fn get_symbol_table(&self, index: usize) -> &SymbolTable {
        &self.symbol_tables[index]
    }
    pub fn get_symbol_table_entries(&self, index: usize) -> &[usize] {
        &self.symbol_tables[index].entries
    }
    pub fn get_symbol_table_entry(&self, index: usize) -> &SymbolTableEntry {
        &self.symbol_table_entries[index]
    }
    pub fn get_mut_symbol_table_entry(&mut self, index: usize) -> &mut SymbolTableEntry {
        &mut self.symbol_table_entries[index]
    }
    pub fn search(&self, name: &str) -> Option<(usize, usize)> {
        let root = self.root?;
        self.search_in_symbol_table(root, name)
    }
    pub fn search_in_symbol_table(&self, table_index: usize, name: &str) -> Option<(usize, usize)> {
        let symbol_table_entries = self.get_symbol_table_entries(table_index);
        for &entry_index in symbol_table_entries {
            let entry = self.get_symbol_table_entry(entry_index);
            if entry.link.is_some() {
                let result = self.search_in_symbol_table(entry.link.unwrap(), name);
                if result.is_some() {
                    return result;
                }
            }
        }
        symbol_table_entries
            .iter()
            .find(|&&entry_index| self.get_symbol_table_entry(entry_index).name == name)
            .map(|&entry_index| (table_index, entry_index))
    }

    pub fn print(&self) {
        self.print_symbol_table(self.root.unwrap());
    }

    fn print_symbol_table(&self, index: usize) {
        let mut output = String::new();
        let symbol_table = self.get_symbol_table(index);
        output.push_str(&format!(
            "SYMBOL TABLE: NAME: {} INDEX: {}\n",
            symbol_table.name, symbol_table.index
        ));
        for (index, entry) in symbol_table.entries.iter().enumerate() {
            output.push_str(&format!(
                "{}. {}\n",
                index,
                self.get_symbol_table_entry(*entry)
            ));
        }
        print!("{}", output);
        for entry in symbol_table.entries.iter() {
            if let Some(link_index) = self.get_symbol_table_entry(*entry).link {
                if let SymbolKind::Class = self.get_symbol_table_entry(*entry).kind {
                    if index != self.root.unwrap() {
                        continue;
                    }
                }
                self.print_symbol_table(link_index);
            }
        }
    }
}

pub struct SymbolTable {
    index: usize,
    pub name: String,
    entries: Vec<usize>,
}

impl Display for SymbolTable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut output = String::new();
        output.push_str(&format!(
            "SYMBOL TABLE: NAME: {} INDEX: {}\n",
            self.name, self.index
        ));
        for (index, entry) in self.entries.iter().enumerate() {
            output.push_str(&format!("Index: {}, {}\n", index, entry));
        }

        write!(f, "{}", output)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SymbolTableEntry {
    pub name: String,
    pub kind: SymbolKind,
    pub link: Option<usize>,
}

impl Display for SymbolTableEntry {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!("Name: {}, {}, Link: {:?}", self.name, self.kind, self.link)
        )
    }
}
