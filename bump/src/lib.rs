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
const MINIMUM_LIST_ALLOC: usize = 4;

#[derive(Debug, PartialEq, Eq)]
pub struct BumpAllocator {
    blocks: Vec<(*mut u8, usize, alloc::Layout)>,
    snapshots: Vec<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Checkpoint<'a> {
    snapshot: &'a [usize],
    position: usize,
    bump: &'a BumpAllocator,
    commit: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct List<'a, T: Sized + Clone> {
    chunk: &'a mut [T],
    size: usize,
    next: Option<&'a mut List<'a, T>>,
    bump: &'a BumpAllocator,
}

impl BumpAllocator {
    pub fn new() -> BumpAllocator {
        let layout = alloc::Layout::from_size_align(STARTING_SIZE, STARTING_SIZE)
            .expect("ERROR: Couldn't create layout for initial bump allocator block.");
        BumpAllocator {
            blocks: vec![(unsafe { alloc::alloc(layout) }, 0, layout)],
            snapshots: vec![],
        }
    }

    fn alloc_impl(&self, layout_size: usize, layout_align: usize) -> *mut u8 {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        for (ptr, size, layout) in mut_self.blocks.iter_mut().rev() {
            let mut align_offset = 0;
            if (*ptr as usize + *size) % layout_align != 0 {
                align_offset = layout_align - ((*ptr as usize + *size) % layout_align);
            }
            if *size + align_offset + layout_size <= layout.size() {
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
                .2
                .size()
                * 2,
            layout_size,
        );
        let new_block_layout = alloc::Layout::from_size_align(new_size, layout_align)
            .expect("ERROR: Couldn't create layout for new bump allocator block.");
        let new_block = (
            unsafe { alloc::alloc(new_block_layout) },
            layout_size,
            new_block_layout,
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

    pub fn create_checkpoint<'a>(&'a self) -> Checkpoint<'a> {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        let before_len = mut_self.snapshots.len();
        for (_, size, _) in mut_self.blocks.iter() {
            mut_self.snapshots.push(*size);
        }

        return Checkpoint {
            snapshot: &mut_self.snapshots[before_len..],
            position: mut_self.snapshots.len(),
            bump: self,
            commit: false,
        };
    }

    fn drop_snapshots(&self, drop_num: usize, commit: bool) {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        if !commit {
            for block_num in mut_self.snapshots.len() - drop_num..mut_self.snapshots.len() {
                let block_size = mut_self.snapshots[block_num];
                mut_self.blocks[block_num].1 = block_size;
            }
        }

        mut_self
            .snapshots
            .truncate(mut_self.snapshots.len() - drop_num);
    }

    fn create_list_impl<T: Sized + Clone>(&self, size: usize) -> &mut List<T> {
        assert!(size > 0, "ERROR: Cannot allocate a slice of size 0.");
        let layout = alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size() * size, layout.align()) as *mut T;
        let alloc = unsafe { slice::from_raw_parts_mut(alloc, size) };
        self.alloc(List {
            chunk: alloc,
            size: 0,
            next: None,
            bump: self,
        })
    }

    pub fn create_list<T: Sized + Clone>(&self) -> &mut List<T> {
        self.create_list_impl(MINIMUM_LIST_ALLOC)
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        for (mem, _, layout) in self.blocks.iter() {
            unsafe { alloc::dealloc(*mem, *layout) };
        }
    }
}

impl Checkpoint<'_> {
    pub fn commit(&mut self) {
        self.commit = true;
    }
}

impl Drop for Checkpoint<'_> {
    fn drop(&mut self) {
        assert_eq!(self.position, self.bump.snapshots.len());
        self.bump.drop_snapshots(self.snapshot.len(), self.commit);
    }
}

impl<T: Sized + Clone> List<'_, T> {
    pub fn push(&mut self, item: T) {
        match &mut self.next {
            None => {
                if self.size < self.chunk.len() {
                    self.chunk[self.size] = item;
                    self.size += 1;
                } else {
                    let next = self.bump.create_list_impl(self.size * 2);
                    next.push(item);
                    self.next = Some(next);
                }
            }
            Some(next) => {
                next.push(item);
            }
        }
    }

    pub fn at_mut(&mut self, idx: usize) -> &mut T {
        if idx < self.chunk.len() {
            &mut self.chunk[idx]
        } else {
            match &mut self.next {
                Some(next) => next.at_mut(idx - self.chunk.len()),
                None => panic!("ERROR: Index {} out of bounds.", idx),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

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

    #[test]
    fn take_snapshot() {
        let bp = BumpAllocator::new();

        let num = 12387;
        for i in 0..num {
            bp.alloc::<usize>(i);
        }

        let mut cp = bp.create_checkpoint();
        let correct = Checkpoint {
            snapshot: &[4096, 8192, 16384, 32768, 37656],
            position: 5,
            bump: &bp,
            commit: false,
        };
        assert_eq!(bp.snapshots.len(), 5);
        assert_eq!(cp, correct);
        mem::forget(correct);

        cp.commit();
        mem::drop(cp);
        let correct_block_sizes = &[4096, 8192, 16384, 32768, 37656];
        for i in 0..bp.blocks.len() {
            assert_eq!(bp.blocks[i].1, correct_block_sizes[i]);
        }

        let cp = bp.create_checkpoint();
        let num = 8742;
        for i in 0..num {
            bp.alloc::<f32>(i as f32);
        }
        mem::drop(cp);
        let correct_block_sizes = &[4096, 8192, 16384, 32768, 37656];
        for i in 0..correct_block_sizes.len() {
            assert_eq!(bp.blocks[i].1, correct_block_sizes[i]);
        }
    }

    #[test]
    fn allocate_list() {
        let bp = BumpAllocator::new();
        let list1 = bp.create_list();
        let num1 = 47824;
        for i in 0..num1 {
            list1.push(i);
        }
        let list2 = bp.create_list();
        let num2 = 789017;
        for i in 0..num2 {
            list2.push(i as f32);
        }
        for i in 0..num1 {
            assert_eq!(*list1.at_mut(i), i);
        }
        for i in 0..num2 {
            assert_eq!(*list2.at_mut(i), i as f32);
        }
    }
}
