use crate::types::RegisterIndex;

pub struct RegisterAllocator {
    // The total array of registers, marking whether they are allocated
    registers: [bool; 256],
    // The first free register
    first_free: u16,
    // The free register after the last used register
    stack_top: u16,
    // The index of the largest used register + 1 (e.g. the stack size required for the function)
    stack_size: u16,
}

impl Default for RegisterAllocator {
    fn default() -> RegisterAllocator {
        RegisterAllocator {
            registers: [false; 256],
            first_free: 0,
            stack_top: 0,
            stack_size: 0,
        }
    }
}

impl RegisterAllocator {
    /// Returns the free register index after the currently largest used register index
    pub fn stack_top(&self) -> u16 {
        self.stack_top
    }

    /// Returns the index of the largest ever used register + 1 (e.g. the stack size required for
    /// the function)
    pub fn stack_size(&self) -> u16 {
        self.stack_size
    }

    /// Allocates any single available register, returns it if one is available.
    pub fn allocate(&mut self) -> Option<RegisterIndex> {
        if self.first_free < 256 {
            let register = self.first_free as u8;
            self.registers[register as usize] = true;

            if self.first_free == self.stack_top {
                self.stack_top += 1;
            }
            self.stack_size = self.stack_size.max(self.stack_top);

            let mut i = self.first_free;
            self.first_free = loop {
                if i == 256 || !self.registers[i as usize] {
                    break i;
                }
                i += 1;
            };

            Some(RegisterIndex(register))
        } else {
            None
        }
    }

    /// Free a single register.
    pub fn free(&mut self, register: RegisterIndex) {
        assert!(
            self.registers[register.0 as usize],
            "cannot free unallocated register",
        );
        self.registers[register.0 as usize] = false;
        self.first_free = self.first_free.min(register.0 as u16);
        if register.0 as u16 + 1 == self.stack_top {
            self.stack_top -= 1;
            self.shrink_top();
        }
    }

    /// Allocates a block of registers of the given size (which must be > 0) always at the end of the
    /// allocated area.  If successful, returns the starting register of the block.
    pub fn push(&mut self, size: u8) -> Option<RegisterIndex> {
        if size == 0 {
            None
        } else if size as u16 <= 256 - self.stack_top {
            let rbegin = self.stack_top as u8;
            for i in rbegin..rbegin + size {
                self.registers[i as usize] = true;
            }
            if self.first_free == self.stack_top {
                self.first_free += size as u16;
            }
            self.stack_top += size as u16;
            self.stack_size = self.stack_size.max(self.stack_top);
            Some(RegisterIndex(rbegin))
        } else {
            None
        }
    }

    /// Free all registers past the given register, making the given register the new top of the
    /// stack.  If the given register is >= to the current top, this will have no effect.
    pub fn pop_to(&mut self, new_top: u16) {
        if self.stack_top > new_top {
            for i in new_top..self.stack_top {
                self.registers[i as usize] = false;
            }
            self.stack_top = new_top;
            self.first_free = self.first_free.min(self.stack_top);
            self.shrink_top();
        }
    }

    // Move the stack_top backwards over newly freed registers until it is past the last used
    // register
    fn shrink_top(&mut self) {
        for i in (self.first_free..self.stack_top).rev() {
            if self.registers[i as usize] {
                break;
            }
            self.stack_top = i;
        }
    }
}
