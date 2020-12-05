extern crate rayon;
use rayon::prelude::*;

// I was thinking having problem with reference and mutation in rust

fn main() {
    println!("Hello, world!");
}

struct Node {
    value: u64,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn insertVal(n: u64, tree: &mut Option<Box<Node>>) -> Option<Node> {
    match tree.is_some() {
        true => {}
        // Return a tree that rec call the value by comparing the value
        false => {
            return Some(Node {
                value: n,
                left: None,
                right: None,
            })
        }
    }
}

fn walkTree(tree: Option<Box<Node>>) -> Vec<u64> {
    fn walk(tree: Option<Box<Node>>, vec: &mut Vec<u64>) -> Vec<u64> {
        if tree.is_none() {
            return vec.clone();
        }
        walk(tree.unwrap().left, vec);
        vec.push(tree.unwrap().value);
        walk(tree.unwrap().right, vec);
    }

    let mut vec: Vec<u64> = Vec::new();

    return walk(tree, &mut vec);
}

fn heightCheck(tree: Option<Box<Node>>) -> u64 {
    if tree.is_none() {
        return 0u64;
    }

    let left = heightCheck(tree.unwrap().left);
    let right = heightCheck(tree.unwrap().right);

    if left > right {
        return left + 1;
    } else {
        return right + 1;
    }
}

fn parallelLookup(tree: Option<Box<Node>>, vec: Vec<u64>) -> Vec<u64> {
    return vec
        .into_par_iter()
        .map(|val| {
            if contain(val) {
                return val;
            }
        })
        .collect();
}
