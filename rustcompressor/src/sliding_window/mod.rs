use std::collections::VecDeque;
use std::iter::Iterator;

/** Sliding windows always occur on raw inputs, but it might be over larger
integer types in later experiments.
*/
struct SlidingWindow<'a, S> {
    buf: &'a mut dyn Iterator<Item = S>,
    hashes: VecDeque<usize>,
    winsz: usize,
}

impl<'a, S> SlidingWindow<'a, S>
where
    S: Copy,
{
    const WIN_SIZE: usize = 1024;
    const NUM_HASHES: usize = 50;
    fn new(iter: &'a mut dyn Iterator<Item = S>) -> Self {
        Self::new_with_params(iter, Self::WIN_SIZE, Self::NUM_HASHES)
    }

    fn new_with_params(iter: &'a mut dyn Iterator<Item = S>, sz: usize, nh: usize) -> Self {
        Self {
            buf: iter,
            hashes: VecDeque::with_capacity(nh),
            winsz: sz,
        }
    }

    /* Hash function:  hash(i) [(i × 256 ) % 101], taken from wiki */
    fn next_sym(&mut self) {
        if let Some(x) = self.buf.next() {
            if self.hashes.len() == self.winsz {
                for x in self.hashes.iter_mut() {}
            }
        }
        // If the stream is empty, do nothing.
    }
}
