// See
// https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
// https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let mut idx = 0;
    let mut running_score = 0;
    let mut group_score = 0;
    let mut group = Vec::with_capacity(3);

    // File input must exist in current path before this produces output
    if let Ok(lines) = read_lines("input.txt") {
        for line in lines {
            if let Ok(line_text) = line {
                // Convert to bytes first.
                let b = line_text.into_bytes();

                running_score += score_line(&b);

                group.push(b);

                if idx % 3 == 2 {
                    // Process vector.
                    group_score += priority(find_common(&group[0], &group[1], &group[2]));
                    // clear the vector going forward
                    group.clear();
                }

                idx += 1;
            }
        }
    }

    println!("Final Priority Sum: {:?}", running_score);
    println!("Final Group Sum: {:?}", group_score);

}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

// Score a line
fn score_line(line: &[u8]) -> i64 {
    // Line is actually two strings, first divide.
    let (first, last) = line.split_at(line.len() / 2);
    //println!("First: {:?}, Last: {:?}", first, last);

    let mut priority_sum = 0;
    // Compare the two. We're going to loop rather than
    // convert to a hash set for efficiency at this scale.
    for i in 0..first.len() {
        // Has this already occurred?
        let mut repeat = false;
        for j in 0..i {
            if first[i] == first[j] {
                repeat = true;
                break;
            }
        }

        // Only do the next search if it's not a repeat.
        if !repeat {
            for j in 0..last.len() {
                // Have we found a match?
                if first[i] == last[j] {
                    // Add priority
                    priority_sum += priority(first[i]);
                    //println!("Matched: {:?}, Priority: {:?}", first[i], priority(first[i]));
                    // Move onto the next step in the outer loop, so we don't double count.
                    break;
                }
            }
        }
    }
    return priority_sum
}

// Priority conversion
fn priority(i: u8) -> i64 {
    // In ASCII capitals come first
    // A = 65 = 101 Z = 5A = 90
    // a = 97 = 141 z = 7A = 122
    // Assume that it's a valid a-z or A-Z
    if i < 97 {
        return (i - 64 + 26) as i64
    } else {
        return (i - 96) as i64
    }
}

fn find_common(a: &Vec<u8>, b: &Vec<u8>, c: &Vec<u8>) -> u8 {
    // For each value in a
    for p in a.iter() {

        let mut in_b = false;
        for q in b.iter() {
            if p == q {
                in_b = true;
                break
            }
        }

        if in_b {
            for r in c.iter() {
                if r == p {
                    return *r
                }
            }
        }
    }

    return 0
}