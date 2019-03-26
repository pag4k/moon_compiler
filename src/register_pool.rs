use std::collections::HashMap;

pub struct RegisterPool {
    registers: HashMap<usize, bool>,
    min: usize,
    max: usize,
}

impl RegisterPool {
    pub fn new(min: usize, max: usize) -> Self {
        let mut registers = HashMap::new();
        for n in min..=max {
            registers.insert(n, true);
        }
        RegisterPool {
            registers,
            min,
            max,
        }
    }
    pub fn pop(&mut self) -> usize {
        if let Some(index) = (self.min..=self.max).find(|index| self.registers[index]) {
            self.registers.insert(index, false);
            index
        } else {
            unreachable!("Out of registers!")
        }
    }
    pub fn push(&mut self, index: usize) {
        if self.min <= index && index <= self.max {
            self.registers.insert(index, true);
        } else {
            unreachable!("Invalid register!")
        }
    }
}
