use std::{
    io::{self, BufRead},
    process,
};

type Point = [usize; 2];

fn collect_numbers(lines: &Vec<String>) -> Vec<(u32, Vec<Point>)> {
    let mut numbers: Vec<(u32, Vec<Point>)> = Vec::new();

    for (i_line, line) in lines.iter().enumerate() {
        let mut digits = String::new();
        let mut coords: Vec<Point> = Vec::new();

        for (i_char, c) in line.chars().enumerate() {
            if c.is_digit(10) {
                digits.push(c);
                coords.push([i_char, i_line]);
            } else {
                if digits.len() > 0 {
                    match digits.parse::<u32>() {
                        Ok(x) => numbers.push((u32::from(x), coords.clone())),
                        Err(e) => {
                            eprintln!("ERROR: {e}");
                            process::exit(1);
                        }
                    }
                    digits.clear();
                    coords.clear();
                }
            }
        }
        // todo refactor
        if digits.len() > 0 {
            match digits.parse::<u32>() {
                Ok(x) => numbers.push((u32::from(x), coords.clone())),
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    process::exit(1);
                }
            }
        }
    }
    numbers
}

fn check(points: &Vec<Point>, lines: &Vec<String>) -> bool {
    let line_len = lines[0].len();
    let n_lines = lines.len();

    for point in points {
        let x = point[0];
        let y = point[1];
        // check above
        if y > 0 && check_char(lines[y - 1].chars().nth(x).unwrap()) {
            return true;
        }
        // check below
        if y < n_lines - 1 && check_char(lines[y + 1].chars().nth(x).unwrap()) {
            return true;
        }
    }

    // check ends
    // check left
    let [x, y] = points[0];
    if x > 0 {
        if check_char(lines[y].chars().nth(x - 1).unwrap()) {
            return true;
        }
        // check up diag
        if y > 0 && check_char(lines[y - 1].chars().nth(x - 1).unwrap()) {
            return true;
        }
        // check down diag
        if y < n_lines - 1 && check_char(lines[y + 1].chars().nth(x - 1).unwrap()) {
            return true;
        }
    }
    // check right
    let [x, y] = points[points.len() - 1];
    if x < line_len - 1 {
        if check_char(lines[y].chars().nth(x + 1).unwrap()) {
            return true;
        }
        // check up diag
        if y > 0 && check_char(lines[y - 1].chars().nth(x + 1).unwrap()) {
            return true;
        }
        // check down diag
        if y < n_lines - 1 && check_char(lines[y + 1].chars().nth(x + 1).unwrap()) {
            return true;
        }
    }

    false
}

fn check_char(c: char) -> bool {
    c != '.' && !c.is_numeric()
}

fn main() -> () {
    let stdin = io::stdin();

    let lines = stdin
        .lock()
        .lines()
        .filter_map(|x| x.ok())
        .collect::<Vec<String>>();

    let numbers = collect_numbers(&lines);

    let adjacents = numbers
        .iter()
        .filter_map(|(x, ps)| if check(ps, &lines) { Some(x) } else { None });

    let sum: u32 = adjacents.sum();
    println!("{sum}");
}
