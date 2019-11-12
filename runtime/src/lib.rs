


pub type MemoryOffset = u32;
#[derive(Debug)]
#[repr(C)]
pub struct BlockHeader {
    alloc_size: u16,
    variant_tag: u8,
    _empty: u8,
}

#[derive(Debug)]
#[repr(C)]
pub struct Block {
    header: BlockHeader,
    data: MemoryOffset,
}

#[derive(Debug)]
#[repr(u32)]
pub enum Instruction {
    CONST_I32,
    ADD_I32,
}


#[derive(Debug)]
struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn with_load(data: Vec<u8>) -> Self {
        Memory {
            data
        }
    }

    pub fn read_u32(&self, offset: usize) -> u32 {
        unsafe {
            *(self.data.as_ptr().offset(offset as isize) as *const u32)
        }
    }
}

#[derive(Debug)]
struct Stack {
    data: Vec<u8>,
    stack_base: usize,
    stack_top: usize,
}

use std::mem;

impl Stack {
    pub fn empty() -> Self {
        Stack {
            data: Vec::new(),
            stack_base: 0,
            stack_top: 0,
        }
    }

    pub fn push<T>(&mut self, data: T) {
        if self.data.len() < (self.stack_top + mem::size_of::<T>()) {
            self.data.resize(self.data.len().max(8) * 2, 0);
        }
        unsafe {
            *(self.data.as_ptr().offset(self.stack_top as isize) as *mut T) = data;
        }
        self.stack_top += mem::size_of::<T>();
    }

    pub fn pop<T>(&mut self) -> T 
        where T: Copy {
        let rt = unsafe {
            *(self.data.as_ptr().offset((self.stack_top - mem::size_of::<T>()) as isize) as *const T)
        };
        self.stack_top -= mem::size_of::<T>();
        rt
    }
}

#[derive(Debug)]
pub struct Runtime {
    memory: Memory,
    stack: Stack,
    isp: usize,
}

impl Runtime {
    pub fn load(data: Vec<u8>) -> Self {
        Runtime {
            memory: Memory::with_load(data),
            stack: Stack::empty(),
            isp: 0,
        }
    }

    pub fn step(&mut self) {

        let inst: Instruction = unsafe {
            mem::transmute(self.memory.read_u32(self.isp))
        };

        use Instruction::*;
        match inst {
            ADD_I32 => {
                let l = self.stack.pop::<i32>();
                let r = self.stack.pop::<i32>();
            
                self.stack.push(l + r);
            },
            CONST_I32 => {
                let c = self.memory.read_u32(self.isp + 4);
                self.isp += 4;
                self.stack.push::<i32>(unsafe {
                    mem::transmute(c)
                })
            }
        }

        self.isp += 4;
    }


}



mod tests {
    use super::*;

    #[test]
    fn simple() {
        
        let mut assembly = Stack::empty();
        assembly.push::<u32>(unsafe { mem::transmute(Instruction::CONST_I32) });
        assembly.push::<u32>(unsafe { mem::transmute(5i32) });
        assembly.push::<u32>(unsafe { mem::transmute(Instruction::CONST_I32) });
        assembly.push::<u32>(unsafe { mem::transmute(7i32) });
        assembly.push::<u32>(unsafe { mem::transmute(Instruction::ADD_I32) });

        println!("{:?}", assembly);

        let mut runtime = Runtime::load(assembly.data);

        runtime.step();
        runtime.step();
        runtime.step();

        println!("{:?}", runtime);
    }
}