// See
// https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
// https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    println!("Hello, world!");

    let mut highest_totals = [0, 0, 0];

    // File input must exist in current path before this produces output
    if let Ok(lines) = read_lines("input.txt") {
        let mut calorie_sum = 0;
        for line in lines {
            if let Ok(line_text) = line {
                if line_text == "" {
                    if calorie_sum > highest_totals[2] {
                        highest_totals.rotate_left(1);
                        highest_totals[2] = calorie_sum;
                    } else if calorie_sum > highest_totals[1] {
                        highest_totals[0] = highest_totals[1];
                        highest_totals[1] = calorie_sum;
                    } else if calorie_sum > highest_totals[0] {
                        highest_totals[0] = calorie_sum;
                    }
                    calorie_sum = 0;
                } else {
                    let parsed: i32 = line_text.parse().unwrap();
                    calorie_sum += parsed;
                }
            }
        }
    }

    println!("Final highest sums: {:?}", highest_totals);
    let sum: i32 = highest_totals.iter().sum();
    println!("Final highest sum totals: {}", sum);

}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
