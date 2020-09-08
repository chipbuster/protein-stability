use std::collections::HashMap;
use std::vec::Vec;

use std::cmp::Eq;
use std::cmp::Ord;
use std::fmt::Debug;
use std::hash::Hash;

use bitvec::vec::BitVec;

type HuffTree<S> = Tree<(Option<S>, usize)>;

pub struct HuffEncoder<S> {
  frequency: HashMap<S, usize>,
  mapping: HashMap<S, BitVec>,
  hufftree: HuffTree<S>,
}

impl<S> HuffEncoder<S>
where
  S: Eq + Ord + Hash + Clone + Debug,
{
  pub fn new(src: &Vec<S>) -> HuffEncoder<S> {
    let frequency = HuffEncoder::get_frequencies(&src);
    let hufftree = HuffEncoder::build_huffman_tree(&frequency);
    let mapping = HuffEncoder::gen_mapping(hufftree.clone(), BitVec::new());

    HuffEncoder {
      frequency,
      mapping,
      hufftree,
    }
  }

  fn push_bitvec(v1: &mut BitVec, v2: &BitVec) -> () {
    for bit in v2.iter() {
      v1.push(*bit);
    }
  }

  pub fn encode(&self, inp: &Vec<S>) -> BitVec {
    let mut encoded = BitVec::new();

    for inchar in inp.iter() {
      Self::push_bitvec(&mut encoded, &self.mapping[inchar]);
    }

    encoded
  }

  /// Decodes a bitvector by walking the Huffman Tree
  pub fn decode(&self, inp: &BitVec) -> Vec<S> {
    let mut tree_base: &Tree<(Option<S>, usize)> = &self.hufftree;
    let mut decoded = Vec::new();

    // Decode loop: if we are at a leaf node, reset the tree reference and
    // restart from the root, adding the decoded symbol into the tree.
    // Otherwise, follow the assigned bit, panicking if we are unable to
    // do so.
    for bit in inp.iter() {
      if *bit {
        if let Some(ref t) = tree_base.right {
          tree_base = t;
        }
      } else {
        if let Some(ref t) = &tree_base.left {
          tree_base = t;
        }
      }

      if tree_base.is_leaf() {
        decoded.push(tree_base.val.0.clone().unwrap());
        tree_base = &self.hufftree;
      }
    }
    decoded
  }

  /// Get the frequencies of characters in a given input
  fn get_frequencies(src: &Vec<S>) -> HashMap<S, usize> {
    let mut counter: HashMap<S, usize> = HashMap::new();
    for ch in src.iter() {
      let chcount = counter.entry(ch.clone()).or_insert(0);
      *chcount += 1;
    }
    if counter.len() <= 1 {
      panic! {"Input is blank or has only one character."}
    }
    counter
  }

  /// Generate the forward mapping of symbols to bitstrings
  fn gen_mapping(tree: HuffTree<S>, repr: BitVec) -> HashMap<S, BitVec> {
    if tree.left.is_none() && tree.right.is_none() {
      return vec![(tree.val.0.unwrap(), repr)].into_iter().collect();
    } else {
      let leftmap = match tree.left {
        Some(t) => {
          let mut newrepr = repr.clone();
          newrepr.push(false);
          HuffEncoder::gen_mapping(*t, newrepr)
        }
        None => HashMap::new(),
      };
      let rightmap = match tree.right {
        Some(t) => {
          let mut newrepr = repr.clone();
          newrepr.push(true);
          HuffEncoder::gen_mapping(*t, newrepr)
        }
        None => HashMap::new(),
      };
      leftmap.into_iter().chain(rightmap).collect()
    }
  }

  fn build_huffman_tree(freqs: &HashMap<S, usize>) -> HuffTree<S> {
    let mut trees = Vec::new();
    for (ch, ct) in freqs {
      trees.push(Tree::new((Some(ch.clone()), ct.clone())));
    }

    /* Huffman's algorithm: take the two trees with the least frequency
    and merge them, creating a tree with the sum of the two frequencies.
    Replace both trees with the new trees, then repeat

    Since Rust only allows push/pop on the back of the vector, we work
    reverse of how we'd do it in Haskell: we sort in descending order by
    weights, then pop trees off the back.

    Leaf nodes have concrete symbols and counts, while internal nodes have
    counts, but use `None` as their symbol.
    */
    trees.sort_by(|t1, t2| t2.val.1.cmp(&t1.val.1)); // Important: reverse sort!
    while trees.len() > 1 {
      // Because length >= 2, we should be able to pop twice.
      let t1 = trees.pop().unwrap();
      let t2 = trees.pop().unwrap();

      let sum_of_counts = t1.val.1 + t2.val.1;
      let joined = Tree::new((None, sum_of_counts)).left(t1).right(t2);

      trees.push(joined);

      /* Maintain sorted invariant by bubbling largest tree backwards in
        vector until it reaches the correct location

         vec = [ .... ... ... t2, t1, ... ...]
                                  ^---index
      */

      let mut index = trees.len() - 1;
      while index > 1 {
        let t1 = &trees[index];
        let t2 = &trees[index - 1];
        if t1.val.1 > t2.val.1 {
          trees.swap(index, index - 1);
          index -= 1;
        } else {
          break; // List is now sorted. Let's GTFO!
        }
      }
    } // End tree generation loop.
    assert!(trees.len() == 1, "Bad number of trees at hufftree exit");
    trees[0].clone()
  }
}

/* Tree structure nicked from https://matthias-endler.de/2017/boxes-and-trees/
 * and augmented with additional operations */
#[derive(Default, Debug, PartialEq, Clone)]
struct Tree<T> {
  val: T,
  left: Option<Box<Tree<T>>>,
  right: Option<Box<Tree<T>>>,
}

impl<T: Eq + Ord> Tree<T> {
  fn new(root: T) -> Tree<T> {
    Tree {
      val: root,
      left: None,
      right: None,
    }
  }

  fn left(mut self, leaf: Tree<T>) -> Self {
    self.left = Some(Box::new(leaf));
    self
  }

  fn right(mut self, leaf: Tree<T>) -> Self {
    self.right = Some(Box::new(leaf));
    self
  }

  fn is_leaf(&self) -> bool {
    self.left.is_none() && self.right.is_none()
  }

  fn insert(mut self, val: T) -> Self {
    if val == self.val {
      return self;
    }

    if val < self.val {
      match self.left {
        None => {
          self.left = Some(Box::new(Tree::new(val)));
          self
        }
        Some(lchild) => {
          self.left = Some(Box::new(lchild.insert(val)));
          self
        }
      }
    } else {
      match self.right {
        None => {
          self.right = Some(Box::new(Tree::new(val)));
          self
        }
        Some(rchild) => {
          self.right = Some(Box::new(rchild.insert(val)));
          self
        }
      }
    }
  }
}

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
mod tests {
  use crate::Tree;

  #[test]
  fn insert_into_singleton() {
    let singleton = Tree::new(3);
    let insert = Tree::new(3).insert(5);
    let post_insert = Tree::new(3).right(Tree::new(5));

    assert_eq!(post_insert, insert);
  }
  #[test]
  fn insert_tree_1() {
    let tree_base: Tree<i32> = Tree::new(15)
      .left(Tree::new(12).right(Tree::new(13)))
      .right(Tree::new(22).left(Tree::new(18)).right(Tree::new(100)));
    let tree_post_insert: Tree<i32> = Tree::new(15)
      .left(Tree::new(12).left(Tree::new(10)).right(Tree::new(13)))
      .right(Tree::new(22).left(Tree::new(18)).right(Tree::new(100)));
    let tree_insert = tree_base.insert(10);
    assert_eq!(tree_insert, tree_post_insert);
  }

  use crate::HuffEncoder;
  use std::collections::HashMap;

  #[test]
  fn freqcount_1() {
    let teststring = "aaaabbcc".chars().collect();
    let h1 = HuffEncoder::get_frequencies(&teststring);
    let mut h2 = HashMap::new();
    h2.insert('a', 4);
    h2.insert('b', 2);
    h2.insert('c', 2);
    assert_eq!(h1, h2);
  }

  #[test]
  fn hufftree_1() {
    let teststring = "aaaaabbc".chars().collect();
    let hufffreq = HuffEncoder::get_frequencies(&teststring);
    let hufftree = HuffEncoder::build_huffman_tree(&hufffreq);

    let answer = Tree::new((None, 8))
      .left(
        Tree::new((None, 3))
          .left(Tree::new((Some('c'), 1)))
          .right(Tree::new((Some('b'), 2))),
      )
      .right(Tree::new((Some('a'), 5)));

    assert_eq!(answer, hufftree);
  }

  use bitvec::*;

  #[test]
  fn simple_map() {
    let teststring = "aaaaabbc".chars().collect();
    let hufffreq = HuffEncoder::get_frequencies(&teststring);
    let hufftree = HuffEncoder::build_huffman_tree(&hufffreq);
    let hufftabl = HuffEncoder::gen_mapping(hufftree, BitVec::new());

    let answer: HashMap<char, BitVec> = vec![
      ('c', bitvec![0, 0]),
      ('b', bitvec![0, 1]),
      ('a', bitvec![1]),
    ]
    .into_iter()
    .collect();
    assert_eq!(answer, hufftabl);
  }

  #[test]
  fn encode_1() {
    let teststring = "aaaaabbc".chars().collect();
    let tocode = "abc".chars().collect();
    let coder = HuffEncoder::new(&teststring);
    let encoded1 = coder.encode(&tocode);
    let encoded2 = coder.encode(&teststring);

    assert_eq!(encoded1, bitvec![1, 0, 1, 0, 0]);
    assert_eq!(encoded2, bitvec![1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0]);
  }

  #[test]
  fn encode_decode_1() {
    let teststring = "aaaaabbc".chars().collect();

    let coder = HuffEncoder::new(&teststring);
    let encoded = coder.encode(&teststring);
    let decoded = coder.decode(&encoded);

    assert_eq!(encoded, bitvec![1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0]);
    assert_eq!(decoded, teststring);
  }

  #[test]
  #[should_panic]
  fn encode_decode_2() {
    let teststring = "444444".chars().collect();

    let coder = HuffEncoder::new(&teststring);
    let encoded = coder.encode(&teststring);
    let decoded = coder.decode(&encoded);

    assert_eq!(encoded, bitvec![1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0]);
    assert_eq!(decoded, teststring);
  }

  quickcheck! {
      fn prop(xs: String) -> bool {
          if xs.len() < 2 {return true;}
          if !xs.is_ascii() {return true;}
          let ys = xs.chars().collect();
          let coder = HuffEncoder::new(&ys);
          ys == coder.decode(&coder.encode(&ys))
      }
  }
}
