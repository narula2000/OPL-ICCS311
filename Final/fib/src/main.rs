fn main() {
    println!("fib(15) == {}", fib(15));
}

fn fib(n: i32) -> u64 {
    if n < 0 {
        panic!("It's negative number: {}", n);
    }

    match n {
        n if n < 0 => panic!("It's negative!!: {}", n),
        1 | 2 => 1,
        _ => {
            let mut mem: Vec<u64> = Vec::new();
            mem.push(1);
            mem.push(1);

            (2..n + 1).into_iter().for_each(|i| {
                mem.push(
                    mem.get((i - 1) as usize).unwrap().clone()
                        + mem.get((i - 2) as usize).unwrap().clone(),
                )
            });
            return mem.get(n as usize).unwrap().clone();
        }
    }
}
