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

extern crate alloc;

use core::cmp::max;
use core::fmt;
use core::slice;

const STARTING_SIZE: usize = 512;
const MINIMUM_LIST_ALLOC: usize = 4;
const MAX_NUM_BLOCKS: usize = 20;

#[derive(Debug, PartialEq, Eq)]
pub struct BumpAllocator {
    blocks: [(*mut u8, usize, alloc::alloc::Layout); MAX_NUM_BLOCKS],
    num_blocks: usize,
    snapshots: Vec<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Checkpoint<'a> {
    snapshot: &'a [usize],
    position: usize,
    bump: &'a BumpAllocator,
    commit: bool,
}

pub struct List<'a, T: Sized + PartialEq + fmt::Debug> {
    chunk: &'a mut [T],
    size: usize,
    next: Option<&'a mut List<'a, T>>,
    bump: &'a BumpAllocator,
}

impl BumpAllocator {
    pub fn new() -> BumpAllocator {
        Self::new_with_start_size(STARTING_SIZE)
    }

    fn new_with_start_size(start_size: usize) -> BumpAllocator {
        let layout = alloc::alloc::Layout::from_size_align(start_size, start_size)
            .expect("ERROR: Couldn't create layout for initial bump allocator block.");
        let mut blocks = [(0 as *mut u8, 0, unsafe {
            alloc::alloc::Layout::from_size_align_unchecked(0, 0)
        }); MAX_NUM_BLOCKS];
        blocks[0] = (unsafe { alloc::alloc::alloc(layout) }, 0, layout);
        BumpAllocator {
            blocks,
            num_blocks: 1,
            snapshots: vec![],
        }
    }

    fn alloc_impl(&self, layout_size: usize, layout_align: usize) -> *mut u8 {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        for (ptr, size, layout) in mut_self.blocks[0..mut_self.num_blocks].iter_mut().rev() {
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
            mut_self.blocks[mut_self.num_blocks - 1].2.size() * 2,
            layout_size,
        );
        let new_block_layout = alloc::alloc::Layout::from_size_align(new_size, layout_align)
            .expect("ERROR: Couldn't create layout for new bump allocator block.");
        let new_block = (
            unsafe { alloc::alloc::alloc(new_block_layout) },
            layout_size,
            new_block_layout,
        );
        mut_self.blocks[mut_self.num_blocks] = new_block;
        mut_self.num_blocks += 1;

        let alloc = mut_self.blocks[mut_self.num_blocks - 1].0 as *mut u8;
        return alloc;
    }

    pub fn alloc<T: Sized>(&self, to_alloc: T) -> &mut T {
        let layout = alloc::alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size(), layout.align()) as *mut T;
        let alloc = unsafe { &mut *alloc };
        *alloc = to_alloc;
        return alloc;
    }

    pub fn alloc_slice<'a, 'b, T: Sized + Clone>(&'a self, to_alloc: &'b [T]) -> &'a mut [T] {
        if to_alloc.len() == 0 {
            return &mut [];
        }
        let layout = alloc::alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size() * to_alloc.len(), layout.align()) as *mut T;
        let alloc = unsafe { slice::from_raw_parts_mut(alloc, to_alloc.len()) };
        alloc.clone_from_slice(to_alloc);
        return alloc;
    }

    pub unsafe fn alloc_slice_raw<'a, 'b, T: Sized>(&'a self, len: usize) -> &'a mut [T] {
        let layout = alloc::alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size() * len, layout.align()) as *mut T;
        slice::from_raw_parts_mut(alloc, len)
    }

    pub fn create_checkpoint<'a>(&'a self) -> Checkpoint<'a> {
        let mut_self = unsafe { &mut *(self as *const BumpAllocator as *mut BumpAllocator) };

        let before_len = mut_self.snapshots.len();
        for (_, size, _) in mut_self.blocks[0..mut_self.num_blocks].iter() {
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
            for block_num in 0..drop_num {
                let block_size =
                    mut_self.snapshots[mut_self.snapshots.len() - drop_num + block_num];
                mut_self.blocks[block_num].1 = block_size;
            }
        }

        mut_self
            .snapshots
            .truncate(mut_self.snapshots.len() - drop_num);
    }

    fn create_list_impl<T: Sized + PartialEq + fmt::Debug>(&self, size: usize) -> &mut List<T> {
        assert!(size > 0, "ERROR: Cannot allocate a slice of size 0.");
        let layout = alloc::alloc::Layout::new::<T>();
        let alloc = self.alloc_impl(layout.size() * size, layout.align()) as *mut T;
        let alloc = unsafe { slice::from_raw_parts_mut(alloc, size) };
        self.alloc(List {
            chunk: alloc,
            size: 0,
            next: None,
            bump: self,
        })
    }

    pub fn create_list<T: Sized + PartialEq + fmt::Debug>(&self) -> &mut List<T> {
        self.create_list_impl(MINIMUM_LIST_ALLOC)
    }

    pub fn create_list_with<T: Sized + Clone + PartialEq + fmt::Debug>(
        &self,
        items: &[T],
    ) -> &mut List<T> {
        let list = self.create_list();
        for item in items {
            list.push(item.clone());
        }
        list
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        for (mem, _, layout) in self.blocks[0..self.num_blocks].iter() {
            unsafe { alloc::alloc::dealloc(*mem, *layout) };
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

impl<'a, T: Sized + PartialEq + fmt::Debug> List<'a, T> {
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

    pub fn at(&self, idx: usize) -> &T {
        if idx < self.chunk.len() {
            &self.chunk[idx]
        } else {
            match &self.next {
                Some(next) => next.at(idx - self.chunk.len()),
                None => panic!("PANIC: Index {} out of bounds.", idx),
            }
        }
    }

    pub fn at_mut(&mut self, idx: usize) -> &mut T {
        if idx < self.chunk.len() {
            &mut self.chunk[idx]
        } else {
            match &mut self.next {
                Some(next) => next.at_mut(idx - self.chunk.len()),
                None => panic!("PANIC: Index {} out of bounds.", idx),
            }
        }
    }

    pub fn len(&self) -> usize {
        match &self.next {
            None => self.size,
            Some(next) => self.size + next.len(),
        }
    }

    fn eq_impl(&self, other: &Self, idx_self: usize, idx_other: usize) -> bool {
        let num_check_self = self.size - idx_self;
        let num_check_other = other.size - idx_other;
        if num_check_self < num_check_other {
            for idx in 0..num_check_self {
                if self.chunk[idx + idx_self] != other.chunk[idx + idx_other] {
                    return false;
                }
            }
            match &self.next {
                None => false,
                Some(next) => next.eq_impl(other, 0, idx_other + num_check_self),
            }
        } else if num_check_self > num_check_other {
            for idx in 0..num_check_other {
                if self.chunk[idx + idx_self] != other.chunk[idx + idx_other] {
                    return false;
                }
            }
            match &other.next {
                None => false,
                Some(next) => self.eq_impl(next, idx_self + num_check_other, 0),
            }
        } else {
            for idx in 0..num_check_self {
                if self.chunk[idx + idx_self] != other.chunk[idx + idx_other] {
                    return false;
                }
            }
            match (&self.next, &other.next) {
                (None, None) => true,
                (Some(self_next), Some(other_next)) => self_next.eq_impl(other_next, 0, 0),
                _ => false,
            }
        }
    }
}

impl<T: Sized + PartialEq + fmt::Debug> PartialEq for List<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.eq_impl(other, 0, 0)
    }
}

impl<T: Sized + PartialEq + fmt::Debug> fmt::Debug for List<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt_list = f.debug_list();
        for i in 0..self.len() {
            fmt_list.entry(self.at(i));
        }
        fmt_list.finish()
    }
}

#[macro_export]
macro_rules! bump_list {
    ($b:expr, $($x:expr),*) => {
        {
            let list = $b.create_list();
            $(
                list.push($x);
            )*
                list
        }
    };
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
        let bp = BumpAllocator::new_with_start_size(4096);

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
        for i in 0..bp.num_blocks {
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
        let list3 = bp.create_list();
        let num3 = 47824;
        for i in 0..num3 {
            list3.push(i);
        }
        assert_eq!(list1, list3);
        list3.push(0);
        assert_ne!(list1, list3);
    }
}
