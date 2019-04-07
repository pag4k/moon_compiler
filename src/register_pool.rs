use std::collections::HashMap;

pub struct RegisterPool {
    registers: HashMap<usize, bool>,
    min: usize,
    max: usize,
    and_count: usize,
    or_count: usize,
    not_count: usize,
    if_count: usize,
    for_count: usize,
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
            and_count: 0,
            or_count: 0,
            not_count: 0,
            if_count: 0,
            for_count: 0,
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
            // unreachable!("Invalid register!")
        }
    }
    pub fn get_and(&mut self) -> usize {
        let temp = self.and_count;
        self.and_count += 1;
        temp
    }
    pub fn get_or(&mut self) -> usize {
        let temp = self.or_count;
        self.or_count += 1;
        temp
    }
    pub fn get_not(&mut self) -> usize {
        let temp = self.not_count;
        self.not_count += 1;
        temp
    }
    pub fn get_if(&mut self) -> usize {
        let temp = self.if_count;
        self.if_count += 1;
        temp
    }
    pub fn get_for(&mut self) -> usize {
        let temp = self.for_count;
        self.for_count += 1;
        temp
    }
}
