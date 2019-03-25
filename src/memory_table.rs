use crate::table::*;
use std::fmt::{Display, Formatter};

const INTEGER: &str = "integer";
const FLOAT: &str = "float";

pub type MemoryTableArena = TableArena<MemoryTable, MemoryTableEntry>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Integer,
    Float,
    Class(String),
}

impl SymbolType {
    pub fn new(symbol_type: &str) -> Self {
        use SymbolType::*;
        match symbol_type {
            "integer" => Integer,
            "float" => Float,
            _ => Class(symbol_type.to_string()),
        }
    }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolType::*;
        let symbol_type = match self {
            Integer => INTEGER.to_string(),
            Float => FLOAT.to_string(),
            Class(name) => name.clone(),
        };
        write!(f, "{}", symbol_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Param,
    Var,
    TempVar,
    LitVar,
}

impl Display for SymbolKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SymbolKind::*;
        let output = match self {
            Param => "Param",
            Var => "Var",
            TempVar => "TempVar",
            LitVar => "LitVar",
        };
        write!(f, "{}", output)
    }
}

#[derive(Debug)]
pub struct MemoryTable {
    index: usize,
    pub name: String,
    size: usize,
    entries: Vec<usize>,
}

impl Display for MemoryTable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut output = String::new();
        output.push_str(&format!(
            "MEMORY TABLE: NAME: {} SIZE: {} INDEX: {}\n",
            self.name, self.size, self.index
        ));
        for (index, entry) in self.entries.iter().enumerate() {
            output.push_str(&format!("Index: {}, {}\n", index, entry));
        }
        write!(f, "{}", output)
    }
}

impl Table<MemoryTable, MemoryTableEntry> for MemoryTable {
    fn get_entries(&self) -> &[usize] {
        &self.entries
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryTableEntry {
    kind: SymbolKind,
    name: String,
    symbol_type: SymbolType,
    size: usize,
    offset: usize,
}

impl Display for MemoryTableEntry {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "{}, {}, {}, {}, {}",
                self.kind, self.name, self.symbol_type, self.size, self.offset
            )
        )
    }
}

impl MemoryTableArena {
    pub fn new_symbol_table(&mut self, name: String) -> usize {
        let index = self.tables.len();
        let symbol_table = MemoryTable {
            name,
            index,
            size: 0,
            entries: Vec::new(),
        };
        self.tables.push(symbol_table);
        index
    }
    pub fn new_symbol_table_entry(
        &mut self,
        kind: SymbolKind,
        name: String,
        symbol_type: SymbolType,
        size: usize,
    ) -> usize {
        let index = self.table_entries.len();
        let symbol_table_entry = MemoryTableEntry {
            kind,
            name,
            symbol_type,
            size,
            offset: 0,
        };
        self.table_entries.push(symbol_table_entry);
        index
    }
    pub fn add_entry(&mut self, table_index: usize, entry_index: usize) {
        let offset = match self.tables[table_index].entries.last() {
            Some(entry_index) => {
                self.table_entries[*entry_index].size + self.table_entries[*entry_index].offset
            }
            None => 0,
        };
        self.table_entries[entry_index].offset = offset;
        self.tables[table_index].size = offset + self.table_entries[entry_index].size;
        // FIXME: Maybe remove the entry from where it came from.
        self.tables[table_index].entries.push(entry_index);
    }
    pub fn print(&self) -> String {
        let mut output = String::new();
        for table in self.tables.iter() {
            output.push_str(&format!("{}\n", table));
            for &entry_index in table.get_entries() {
                output.push_str(&format!("{}\n", self.table_entries[entry_index]));
            }
        }
        output
    }
}
