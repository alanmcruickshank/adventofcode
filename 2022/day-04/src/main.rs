// See
// https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
// https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
// https://rust-lang-nursery.github.io/rust-cookbook/text/string_parsing.html
use std::str::FromStr;


struct ElfAssignment {
    a1: u8,
    a2: u8,
    b1: u8,
    b2: u8,
}


impl FromStr for ElfAssignment {
    type Err = std::num::ParseIntError;

    // Parses a string assignment into ElfAssignment. e.g. "19-52,18-39"
    fn from_str(hex_code: &str) -> Result<Self, Self::Err> {
        // Split up the input string
        let v: Vec<&str> = hex_code.split(&['-', ','][..]).collect();

        // Parse the numbers
        // u8::from_str_radix(src: &str, radix: u32) converts a string
        // slice in a given base to u8
        let a1: u8 = u8::from_str_radix(&v[0], 10)?;
        let a2: u8 = u8::from_str_radix(&v[1], 10)?;
        let b1: u8 = u8::from_str_radix(&v[2], 10)?;
        let b2: u8 = u8::from_str_radix(&v[3], 10)?;

        Ok(ElfAssignment { a1, a2, b1, b2 })
    }
}


fn is_fully_contained(a: &ElfAssignment) -> bool {
    // need to check both directions.
    // a contains b
    return (a.a1 <= a.b1 && a.a2 >= a.b2)
    // or b contains a
        || (a.b1 <= a.a1 && a.b2 >= a.a2)
}


fn is_overlap(a: &ElfAssignment) -> bool {
    // overlap is just no gap.
    // Either: a starts lower than b ends, but middle crosses
    return (a.a1 <= a.b2 && a.a2 >= a.b1)
    // or b starts lower than a ends, but middle crosses
        || (a.b1 < a.a2 && a.b2 >= a.a1)
}


fn main() {
    let mut contained_score = 0;
    let mut overlap_score = 0;

    // File input must exist in current path before this produces output
    if let Ok(lines) = read_lines("input.txt") {
        for line in lines {
            if let Ok(line_text) = line {
                // Convert to assignment. (Proceeding if successful)
                if let Ok(assignment) = ElfAssignment::from_str(&line_text) {
                    // Check containment
                    if is_overlap(&assignment) {
                        overlap_score += 1;
                        // Check containment
                        if is_fully_contained(&assignment) {
                            contained_score += 1;
                        }
                    }
                }
            }
        }
    }

    println!("Contained Count: {:?}", contained_score);
    println!("Overlap Count: {:?}", overlap_score);

}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
