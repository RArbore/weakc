/*
 * This file is part of weakc.
 * weakc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * weakc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with weakc. If not, see <https://www.gnu.org/licenses/>.
 */

use std::alloc;
use std::cmp::max;
use std::slice;

const STARTING_SIZE: usize = 4096;

pub struct BumpAllocator {
    blocks: Vec<(*mut u8, usize, usize)>,
}

impl BumpAllocator {
    pub fn new() -> BumpAllocator {
        let layout = alloc::Layout::from_size_align(STARTING_SIZE, STARTING_SIZE)
            .expect("ERROR: Couldn't create layout for initial bump allocator block.");
        BumpAllocator {
            blocks: vec![(unsafe { alloc::alloc(layout) }, 0, STARTING_SIZE)],
        }
    }

    fn alloc_impl(&self, layout_size: usize, layout_align: usize) -> *mut u8 {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        for (ptr, size, alloc) in mut_self.blocks.iter_mut().rev() {
            let mut align_offset = 0;
            if (*ptr as usize + *size) % layout_align != 0 {
                align_offset = layout_align - ((*ptr as usize + *size) % layout_align);
            }
            if *size + align_offset + layout_size <= *alloc {
                let base = *size + align_offset;
                *size += align_offset + layout_size;
                let alloc = unsafe { (*ptr).offset(base as isize) } as *mut u8;
                return alloc;
            }
        }

        let new_size = max(
            mut_self
                .blocks
                .last()
                .expect("ERROR: Bump allocator block list is erroneously empty.")
                .2,
            layout_size,
        );
        let new_block_layout = alloc::Layout::from_size_align(new_size, layout_align)
            .expect("ERROR: Couldn't create layout for new bump allocator block.");
        let new_block = (
            unsafe { alloc::alloc(new_block_layout) },
            layout_size,
            new_size,
        );
        mut_self.blocks.push(new_block);

        let alloc = mut_self
            .blocks
            .last()
            .expect("ERROR: Bump allocator block list is erroneously empty.")
            .0 as *mut u8;
        return alloc;
    }

    pub fn alloc<T: Sized>(&self, to_alloc: T) -> &mut T {
        let layout = alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size(), layout.align()) as *mut T;
        let alloc = unsafe { &mut *alloc };
        *alloc = to_alloc;
        return alloc;
    }

    pub fn alloc_slice<'a, 'b, T: Sized + Clone>(&'a self, to_alloc: &'b [T]) -> &'a mut [T] {
        assert!(
            to_alloc.len() > 0,
            "ERROR: Cannot allocate a slice of size 0."
        );
        let layout = alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size() * to_alloc.len(), layout.align()) as *mut T;
        let alloc = unsafe { slice::from_raw_parts_mut(alloc, to_alloc.len()) };
        alloc.clone_from_slice(to_alloc);
        return alloc;
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        self.blocks.clear();
        let layout = alloc::Layout::from_size_align(STARTING_SIZE, STARTING_SIZE)
            .expect("ERROR: Couldn't create layout for initial bump allocator block.");
        self.blocks
            .push((unsafe { alloc::alloc(layout) }, 0, STARTING_SIZE));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_uniform() {
        let bp = BumpAllocator::new();
        let num = 10000;
        let mut ptrs = vec![];
        for i in 0..num {
            let a = bp.alloc::<usize>(i);
            ptrs.push(a);
        }
        for i in 0..num {
            println!("{} {}", *ptrs[i], i);
            assert!(*ptrs[i] == i);
        }
    }

    #[test]
    fn alloc_non_uniform() {
        let bp = BumpAllocator::new();
        let num = 10000;
        let mut ptrs1 = vec![];
        let mut ptrs2 = vec![];
        for i in 0..num {
            let a = bp.alloc::<usize>(i);
            ptrs1.push(a);
        }
        for i in 0..num {
            let a = bp.alloc::<f32>(i as f32);
            ptrs2.push(a);
        }
        for i in 0..num {
            assert!(*ptrs1[i] == i);
        }
        for i in 0..num {
            assert!(*ptrs2[i] == i as f32);
        }
    }
}
