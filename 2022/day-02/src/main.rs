// See
// https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
// https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::collections::HashMap;

fn main() {
    let mut running_score = 0;

    // File input must exist in current path before this produces output
    if let Ok(lines) = read_lines("input.txt") {
        for line in lines {
            if let Ok(line_text) = line {
                running_score += score_line(line_text)
            }
        }
    }

    println!("Final Score: {:?}", running_score);

}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

// Score a line
fn score_line(line: String) -> i64 {
    // The general case isn't crazy, but it's actually just more
    // concise to enumerate options for scoring.
    // Looking at the numbers, there's probably a mathematical way.
    let outcomes = HashMap::from([
        ("A X", 1 + 3),
        ("A Y", 2 + 6),
        ("A Z", 3 + 0),
        ("B X", 1 + 0),
        ("B Y", 2 + 3),
        ("B Z", 3 + 6),
        ("C X", 1 + 6),
        ("C Y", 2 + 0),
        ("C Z", 3 + 3),
    ]);

    match outcomes.get(&line as &str) {
        Some(&score) => return score,
        None => return -100000
    }
}
