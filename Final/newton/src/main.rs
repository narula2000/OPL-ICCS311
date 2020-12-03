fn main() {
    println!("Euler value: {}", compute_e());
}

fn good_enough(guess: f64) -> bool {
    return (guess.ln() - 1.0).abs() < 1e-10;
}

fn improve(guess: f64) -> f64 {
    return guess - ((guess.ln() - 1.0) * guess);
}

fn compute_e() -> f64 {
    fn repeat(guess: f64) -> f64 {
        if good_enough(guess) {
            return guess;
        } else {
            return repeat(improve(guess));
        }
    }

    return repeat(1.0);
}
