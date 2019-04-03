use crate::table::*;
use std::fmt::{Display, Formatter};

const INTEGER: &str = "integer";
const FLOAT: &str = "float";
const NONE: &str = "None";

pub type SymbolTableArena = TableArena<SymbolTable, SymbolTableEntry>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Integer(Vec<usize>),
    Float(Vec<usize>),
    Class(String, Vec<usize>),
}

impl SymbolType {
    pub fn new(symbol_type: &str, indices: Vec<usize>) -> Self {
        use SymbolType::*;
        match symbol_type {
            "integer" => Integer(indices),
            "float" => Float(indices),
            _ => Class(symbol_type.to_string(), indices),
        }
    }
    pub fn get_dimension_list(&self) -> Vec<usize> {
        use SymbolType::*;
        match self {
            Integer(dimension_list) => dimension_list.clone(),
            Float(dimension_list) => dimension_list.clone(),
            Class(_, dimension_list) => dimension_list.clone(),
        }
    }
    pub fn remove_dimensions(&self) -> Self {
        use SymbolType::*;
        match self {
            Integer(_) => Integer(Vec::new()),
            Float(_) => Float(Vec::new()),
            Class(class_name, _) => Class(class_name.clone(), Vec::new()),
        }
    }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolType::*;
        let (symbol_type, indices) = match self {
            Integer(indices) => (INTEGER.to_string(), indices),
            Float(indices) => (FLOAT.to_string(), indices),
            Class(name, indices) => (name.clone(), indices),
        };
        let mut indices_str = String::new();
        for index in indices.iter() {
            indices_str.push_str(&format!("[{}]", index));
        }

        write!(f, "{}", format!("{}{}", symbol_type, indices_str))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Class,
    Function(Option<SymbolType>, Vec<SymbolType>),
    Parameter(SymbolType),
    Variable(SymbolType),
    For(SymbolType),
}

impl Display for SymbolKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolKind::*;
        let output = match self {
            Class => "Class".to_string(),
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
                        None => NONE.to_string(),
                    },
                    parameters
                )
            }
            Parameter(symbol_type) => format!("Parameter: {}", symbol_type),
            Variable(symbol_type) => format!("Variable: {}", symbol_type),
            For(symbol_type) => format!("For: {}", symbol_type),
        };

        write!(f, "{}", output)
    }
}

// impl SymbolKind {
//     pub fn is_variable(&self) -> bool {
//         use SymbolKind::*;
//         match self {
//             Variable(_) => true,
//             _ => false,
//         }
//     }
//     pub fn is_parameter(&self) -> bool {
//         use SymbolKind::*;
//         match self {
//             Parameter(_) => true,
//             _ => false,
//         }
//     }
//     pub fn is_for(&self) -> bool {
//         use SymbolKind::*;
//         match self {
//             For(_) => true,
//             _ => false,
//         }
//     }
//     pub fn is_function(&self) -> bool {
//         use SymbolKind::*;
//         match self {
//             Function(_, _) => true,
//             _ => false,
//         }
//     }
// }

#[derive(Debug)]
pub struct SymbolTable {
    index: usize,
    name: String,
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

impl Table<SymbolTable, SymbolTableEntry> for SymbolTable {
    fn get_entries(&self) -> &[usize] {
        &self.entries
    }
    fn get_mut_entries(&mut self) -> &mut Vec<usize> {
        &mut self.entries
    }
}

impl SymbolTable {
    pub fn get_index(&self) -> usize {
        self.index
    }
    pub fn get_name(&self) -> &String {
        &self.name
    }
    pub fn get_name_clone(&self) -> String {
        self.name.clone()
    }
    pub fn has_name(&self, other_name: &str) -> bool {
        self.name == other_name
    }
    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTableEntry {
    name: String,
    kind: SymbolKind,
    link: Option<usize>,
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

impl SymbolTableEntry {
    pub fn get_name(&self) -> &String {
        &self.name
    }
    pub fn get_name_clone(&self) -> String {
        self.name.clone()
    }
    pub fn has_name(&self, other_name: &str) -> bool {
        self.name == other_name
    }
    pub fn get_kind(&self) -> &SymbolKind {
        &self.kind
    }
    pub fn get_kind_clone(&self) -> SymbolKind {
        self.kind.clone()
    }
    pub fn get_link(&self) -> Option<usize> {
        self.link
    }
    pub fn set_link(&mut self, link: Option<usize>) {
        self.link = link;
    }
    pub fn get_symbol_type(&self) -> Option<&SymbolType> {
        use SymbolKind::*;
        match &self.kind {
            Parameter(symbol_type) | Variable(symbol_type) | For(symbol_type) => Some(symbol_type),
            Function(symbol_type, _) => symbol_type.as_ref(),
            _ => None,
        }
    }
    pub fn get_parameter_symbol_types(&self) -> &[SymbolType] {
        use SymbolKind::*;
        match &self.kind {
            Function(_, parameter_symbol_types) => parameter_symbol_types,
            _ => unreachable!(),
        }
    }
    pub fn is_class(&self) -> bool {
        use SymbolKind::*;
        match self.kind {
            Class => true,
            _ => false,
        }
    }
    pub fn has_class_name(&self, class_name: &str) -> bool {
        use SymbolKind::*;
        match &self.kind {
            Variable(symbol_type) => match symbol_type {
                SymbolType::Class(name, _) => name == class_name,
                _ => false,
            },
            _ => unreachable!(),
        }
    }
    pub fn is_variable(&self) -> bool {
        use SymbolKind::*;
        match self.kind {
            Variable(_) => true,
            _ => false,
        }
    }
    pub fn is_parameter(&self) -> bool {
        use SymbolKind::*;
        match self.kind {
            Parameter(_) => true,
            _ => false,
        }
    }
    pub fn is_for(&self) -> bool {
        use SymbolKind::*;
        match self.kind {
            For(_) => true,
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        use SymbolKind::*;
        match self.kind {
            Function(_, _) => true,
            _ => false,
        }
    }
}

impl SymbolTableArena {
    pub fn new_symbol_table(&mut self, name: String) -> usize {
        let index = self.tables.len();
        let symbol_table = SymbolTable {
            name,
            index,
            entries: Vec::new(),
        };
        self.tables.push(symbol_table);
        index
    }
    pub fn new_symbol_table_entry(
        &mut self,
        name: String,
        kind: SymbolKind,
        link: Option<usize>,
    ) -> usize {
        let index = self.table_entries.len();
        let symbol_table_entry = SymbolTableEntry { name, kind, link };
        self.table_entries.push(symbol_table_entry);
        index
    }
    pub fn add_entry(&mut self, table_index: usize, entry_index: usize) {
        self.tables[table_index].entries.push(entry_index);
    }

    pub fn print(&self) -> String {
        self.print_symbol_table(self.root.unwrap())
    }
    fn print_symbol_table(&self, index: usize) -> String {
        let mut output = String::new();
        let symbol_table = self.get_table(index);
        output.push_str(&format!(
            "SYMBOL TABLE: NAME: {} INDEX: {}\n",
            symbol_table.name, symbol_table.index
        ));
        for (index, entry) in symbol_table.entries.iter().enumerate() {
            output.push_str(&format!("{}. {}\n", index, self.get_table_entry(*entry)));
        }
        for entry in symbol_table.entries.iter() {
            if let Some(link_index) = self.get_table_entry(*entry).link {
                if let SymbolKind::Class = self.get_table_entry(*entry).kind {
                    if index != self.root.unwrap() {
                        continue;
                    }
                }
                output.push_str(&self.print_symbol_table(link_index));
            }
        }
        output
    }
}
