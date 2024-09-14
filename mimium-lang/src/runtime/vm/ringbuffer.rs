use std::ptr::slice_from_raw_parts_mut;

use super::RawVal;

#[repr(C)]
pub(super) struct Ringbuffer<'a> {
    read_idx: &'a mut [u64],
    write_idx: &'a mut [u64],
    data: &'a mut [u64],
}

impl<'a> Ringbuffer<'a> {
    pub fn new(head: *mut u64, size_in_samples: u64) -> Self {
        let (read_idx, write_idx, data) = unsafe {
            let read_idx = slice_from_raw_parts_mut(head, 1 as usize);
            let write_ptr = head.offset(1);
            let write_idx = slice_from_raw_parts_mut(write_ptr, 1 as usize);
            let data_head = head.offset(2);
            let data = slice_from_raw_parts_mut(data_head, size_in_samples as usize);
            (
                read_idx.as_mut().unwrap(),
                write_idx.as_mut().unwrap(),
                data.as_mut().unwrap(),
            )
        };

        Self {
            read_idx,
            write_idx,
            data,
        }
    }
    pub fn process(&mut self, input: RawVal, time_raw: u64) -> RawVal {
        let len = self.data.len() as u64;
        let res = unsafe {
            let time = std::mem::transmute::<u64, f64>(time_raw) as u64;
            let read_idx = *self.read_idx.get_unchecked(0);
            *self.write_idx.get_unchecked_mut(0) = (read_idx + time) % len;
            let write_idx = *self.write_idx.get_unchecked(0);
            let res = *self.data.get_unchecked(read_idx as usize);
            *self.data.get_unchecked_mut(write_idx as usize) = input;
            *self.read_idx.get_unchecked_mut(0) = (read_idx + 1) % len;
            res
        };
        res
    }
}

impl<'a> From<&mut [RawVal]> for Ringbuffer<'a> {
    fn from(value: &mut [RawVal]) -> Self {
        Self::new(value.as_mut_ptr(), value.len() as u64)
    }
}
