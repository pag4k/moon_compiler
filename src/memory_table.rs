use crate::table::*;
use std::fmt::{Display, Formatter};

const INTEGER: &str = "integer";
const FLOAT: &str = "float";

pub type MemoryTableArena = TableArena<MemoryTable, MemoryTableEntry>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableType {
    Integer,
    Float,
    Class(String),
}

impl VariableType {
    pub fn new(memory_type: &str) -> Self {
        use VariableType::*;
        match memory_type {
            "integer" => Integer,
            "float" => Float,
            _ => Class(memory_type.to_string()),
        }
    }
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use VariableType::*;
        let memory_type = match self {
            Integer => INTEGER.to_string(),
            Float => FLOAT.to_string(),
            Class(name) => name.clone(),
        };
        write!(f, "{}", memory_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableKind {
    Inherited,
    ReturnVar,
    ReturnAddr,
    InstAddr,
    Param(String),
    Var(String),
    ForVar(String),
    TempVar(usize),
    LitVar(usize),
}

impl Display for VariableKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use VariableKind::*;
        let output = match self {
            Inherited => format!("Inherited"),
            ReturnVar => format!("ReturnVar"),
            ReturnAddr => format!("ReturnAddr"),
            InstAddr => format!("InstAddr"),
            Param(name) => format!("Param: {}", name),
            Var(name) => format!("Var: {}", name),
            ForVar(name) => format!("ForVar: {}", name),
            TempVar(temp_index) => format!("TempVar: t{}", temp_index),
            LitVar(temp_index) => format!("LitVar: t{}", temp_index),
        };
        write!(f, "{}", output)
    }
}

#[derive(Debug)]
pub struct MemoryTable {
    index: usize,
    symbol_index: usize,
    pub name: String,
    pub offset: isize,
    temp_count: usize,
    entries: Vec<usize>,
}

impl Display for MemoryTable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut output = String::new();
        output.push_str(&format!(
            "MEMORY TABLE: NAME: {} OFFSET: {} SYMBOL INDEX: {} INDEX: {}\n",
            self.name, self.offset, self.symbol_index, self.index
        ));
        write!(f, "{}", output)
    }
}

impl Table<MemoryTable, MemoryTableEntry> for MemoryTable {
    fn get_entries(&self) -> &[usize] {
        &self.entries
    }
}

impl MemoryTable {
    pub fn get_index(&self) -> usize {
        self.index
    }
    pub fn get_symbol_index(&self) -> usize {
        self.symbol_index
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryTableEntry {
    kind: VariableKind,
    memory_type: VariableType,
    size: usize,
    offset: isize,
}

impl Display for MemoryTableEntry {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "{}, {}, {}, {}",
                self.kind, self.memory_type, self.size, self.offset
            )
        )
    }
}

impl MemoryTableEntry {
    pub fn is_temp_var(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            TempVar(_) => true,
            _ => false,
        }
    }
    pub fn is_lit_var(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            LitVar(_) => true,
            _ => false,
        }
    }
    pub fn is_return_addr(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            ReturnAddr => true,
            _ => false,
        }
    }
    pub fn is_return_var(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            ReturnVar => true,
            _ => false,
        }
    }
    pub fn is_inst_addr(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            InstAddr => true,
            _ => false,
        }
    }
    pub fn is_param(&self) -> bool {
        use VariableKind::*;
        match self.kind {
            Param(_) => true,
            _ => false,
        }
    }
    // FIXME: I should probably prevent variable names in the form t#.
    pub fn get_name(&self) -> String {
        use VariableKind::*;
        match &self.kind {
            Param(name) => name.clone(),
            Var(name) => name.clone(),
            ForVar(name) => name.clone(),
            TempVar(temp_index) => format!("t{}", temp_index),
            LitVar(temp_index) => format!("t{}", temp_index),
            _ => "%".to_string(),
        }
    }
    pub fn get_offset(&self) -> isize {
        self.offset
    }
    pub fn get_size(&self) -> isize {
        self.size as isize
    }
}

impl MemoryTableArena {
    pub fn new_memory_table(&mut self, symbol_index: usize, name: String) -> usize {
        let index = self.tables.len();
        let memory_table = MemoryTable {
            index,
            symbol_index,
            name,
            offset: 0,
            temp_count: 0,
            entries: Vec::new(),
        };
        self.tables.push(memory_table);
        index
    }
    pub fn new_memory_table_entry(
        &mut self,
        kind: VariableKind,
        memory_type: VariableType,
        size: usize,
    ) -> usize {
        let index = self.table_entries.len();
        let memory_table_entry = MemoryTableEntry {
            kind,
            memory_type,
            size,
            offset: 0,
        };
        self.table_entries.push(memory_table_entry);
        index
    }
    pub fn add_entry(&mut self, table_index: usize, entry_index: usize) {
        use VariableKind::*;
        let offset = match self.tables[table_index].entries.last() {
            Some(last_entry_index) => self.table_entries[*last_entry_index].offset,
            None => 0,
        } - self.table_entries[entry_index].size as isize;
        self.table_entries[entry_index].offset = offset;
        match self.table_entries[entry_index].kind {
            TempVar(_) => {
                self.table_entries[entry_index].kind = TempVar(self.tables[table_index].temp_count);
                self.tables[table_index].temp_count += 1;
            }
            LitVar(_) => {
                self.table_entries[entry_index].kind = LitVar(self.tables[table_index].temp_count);
                self.tables[table_index].temp_count += 1;
            }
            _ => {}
        }

        self.tables[table_index].offset = offset;
        // FIXME: Maybe remove the entry from where it came from.
        self.tables[table_index].entries.push(entry_index);
    }
    pub fn print(&self) -> String {
        let mut output = String::new();
        for table in self.tables.iter() {
            output.push_str(&format!("{}", table));
            for &entry_index in table.get_entries() {
                output.push_str(&format!("{}\n", self.table_entries[entry_index]));
            }
            output.push_str("\n");
        }
        output
    }
}
