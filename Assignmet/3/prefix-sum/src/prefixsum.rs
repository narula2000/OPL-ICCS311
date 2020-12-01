extern crate rayon;
use rayon::join;

// This was done by the help from Parma and MIT lecture
// http://courses.csail.mit.edu/18.337/2004/book/Lecture_03-Parallel_Prefix.pdf

// Node structure for the BinaryTree
struct Node {
    left: Option<Box<Node>>,
    val: u64,
    right: Option<Box<Node>>,
}

// Concatinate two vectors
//
// ```
// assert_eq!(!vec[1,2], concat(&vec![1], &vec![2]))
// ```
fn concat(left: &[u64], right: &[u64]) -> Vec<u64> {
    return [left, right].concat();
}

// Create a result vector from a given vector by using prefix_sum method to
// output it
//
// ```
// assert_eq!(vec![0, 1, 4, 9, 11], prefix_sum(&vec![1, 3, 5, 2]));
// ```
fn prefix_sum(xs: &Vec<u64>) -> Vec<u64> {
    fn create_tree(vector: &Vec<u64>) -> Node {
        let length: usize = vector.len();
        match length {
            0 => {
                return Node {
                    left: None,
                    val: 0,
                    right: None,
                }
            }
            1 => {
                return Node {
                    left: None,
                    val: vector[0],
                    right: None,
                }
            }
            _ => {
                let (left, right) = vector.split_at(length / 2);
                let (left_node, right_node) = join(
                    || create_tree(&left.clone().to_vec()),
                    || create_tree(&right.clone().to_vec()),
                );
                let left_val = left_node.val.clone();
                let right_val = right_node.val.clone();
                return Node {
                    left: Some(Box::new(left_node)),
                    val: left_val + right_val,
                    right: Some(Box::new(right_node)),
                };
            }
        }
    }

    fn create_vector_from_tree(tree: &Option<Box<Node>>, accum: u64) -> Vec<u64> {
        if tree.is_none() {
            return vec![0];
        } else {
            let tree_node = tree.as_ref().unwrap();
            if tree_node.left.is_some() && tree_node.right.is_some() {
                let (left_vector, right_vector) = join(
                    || {
                        create_vector_from_tree(
                            &tree_node.left,
                            accum - tree_node.right.as_ref().unwrap().val,
                        )
                    },
                    || create_vector_from_tree(&tree_node.right, accum),
                );
                return concat(&left_vector, &right_vector);
            } else {
                return vec![accum];
            }
        }
    }

    let tree = create_tree(&xs.clone());
    let max_val = tree.val.clone();
    let vector = create_vector_from_tree(&Some(Box::new(tree)), max_val);
    match xs.len() {
        length if length < 1 => return vector,
        _ => return concat(&*vec![0], &*vector),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty() {
        assert_eq!(vec![0], prefix_sum(&vec![]));
    }

    #[test]
    fn test_repeats_val() {
        assert_eq!(vec![0, 0, 3, 6], prefix_sum(&vec![0, 3, 3]));
    }

    #[test]
    fn test_example() {
        assert_eq!(vec![0, 1, 4, 9, 11], prefix_sum(&vec![1, 3, 5, 2]));
    }

    #[test]
    fn test_random_vales() {
        assert_eq!(vec![0, 5, 83, 89, 121], prefix_sum(&vec![5, 78, 6, 32]));
    }
}

fn main() {
    let vec1: Vec<u64> = vec![1, 3, 5, 2];
    let vec2: Vec<u64> = vec![0, 3, 3];
    let vec3: Vec<u64> = vec![];

    println!("{:?} => {:?}", &vec1, prefix_sum(&vec1));
    println!("{:?} => {:?}", &vec2, prefix_sum(&vec2));
    println!("{:?} => {:?}", &vec3, prefix_sum(&vec3));
}
