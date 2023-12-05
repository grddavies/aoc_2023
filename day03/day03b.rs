use std::{
    io::{self, BufRead},
    process,
};

type Point = [usize; 2];

fn collect_points(lines: &Vec<String>) -> (Vec<(u32, Vec<Point>)>, Vec<Point>) {
    let mut numbers: Vec<(u32, Vec<Point>)> = Vec::new();
    let mut specials: Vec<Point> = Vec::new();

    for (i_line, line) in lines.iter().enumerate() {
        let mut digits = String::new();
        let mut coords: Vec<Point> = Vec::new();

        for (i_char, c) in line.chars().enumerate() {
            if c.is_digit(10) {
                digits.push(c);
                coords.push([i_char, i_line])
            } else {
                if check_char(c) {
                    specials.push([i_char, i_line])
                }
                if !digits.is_empty() {
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
        if !digits.is_empty() {
            match digits.parse::<u32>() {
                Ok(x) => numbers.push((x, coords.clone())),
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    process::exit(1);
                }
            }
        }
    }
    (numbers, specials)
}

fn check_char(c: char) -> bool {
    c == '*'
}

fn main() -> () {
    let stdin = io::stdin();

    let lines = stdin
        .lock()
        .lines()
        .filter_map(|x| x.ok())
        .collect::<Vec<String>>();

    let (numbers, asterisks) = collect_points(&lines);

    let gears = asterisks.iter().filter_map(|[x1, y1]| {
        let adjacents: Vec<u32> = numbers
            .iter()
            .filter_map(|(num, points)| {
                if points.iter().any(|[x2, y2]| {
                    let a = *x2 as i32 - *x1 as i32;
                    let b = *y2 as i32 - *y1 as i32;
                    (a * a + b * b) <= 2
                }) {
                    Some(*num)
                } else {
                    None
                }
            })
            .collect();
        match adjacents.len() == 2 {
            true => Some(adjacents),
            false => None,
        }
    });

    let sum: u32 = gears.map(|v| v.iter().product::<u32>()).sum();

    println!("{sum}");
}
