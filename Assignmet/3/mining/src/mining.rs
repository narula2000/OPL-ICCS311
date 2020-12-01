use std::path::PathBuf;

fn main() {
    let mut buffer = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    buffer.push("resources/test/2008.csv");
    let path = buffer.clone();

    println!("{:?}", path);
}

