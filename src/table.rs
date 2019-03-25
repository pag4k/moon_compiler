pub trait Table<T, E> {
    fn get_entries(&self) -> &[usize];
}

pub struct TableArena<T, E> {
    pub root: Option<usize>,
    pub tables: Vec<T>,
    pub table_entries: Vec<E>,
}

impl<T, E> Default for TableArena<T, E> {
    fn default() -> Self {
        TableArena {
            root: None,
            tables: Vec::new(),
            table_entries: Vec::new(),
        }
    }
}
impl<T, E> TableArena<T, E>
where
    T: Table<T, E>,
{
    pub fn get_table(&self, index: usize) -> &T {
        &self.tables[index]
    }
    pub fn get_mut_table(&mut self, index: usize) -> &mut T {
        &mut self.tables[index]
    }
    pub fn get_table_entries(&self, index: usize) -> &[usize] {
        self.tables[index].get_entries()
    }
    pub fn get_table_entry(&self, index: usize) -> &E {
        &self.table_entries[index]
    }
    pub fn get_mut_table_entry(&mut self, index: usize) -> &mut E {
        &mut self.table_entries[index]
    }
}
