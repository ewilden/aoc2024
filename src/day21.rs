use auto_enums::auto_enum;
use derivative::Derivative;
use itertools::Itertools;
use priority_queue::PriorityQueue;
use std::collections::{HashMap, HashSet};

use aoc_runner_derive::{aoc, aoc_generator};

const SAMPLE: &str = "029A
980A
179A
456A
379A";

#[derive(Debug)]
enum KeypadKind {
    Numeric,
    Directional,
}

struct Keypad {
    kind: KeypadKind,
    keypad: HashMap<(i32, i32), u8>,
}

impl Keypad {
    fn numeric() -> Keypad {
        Keypad {
            kind: KeypadKind::Numeric,
            keypad: [
                [b'7', b'8', b'9'],
                [b'4', b'5', b'6'],
                [b'1', b'2', b'3'],
                [b'X', b'0', b'A'],
            ]
            .into_iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.into_iter()
                    .enumerate()
                    .map(move |(c, v)| ((r as i32, c as i32), v))
            })
            .collect(),
        }
    }

    fn directional() -> Keypad {
        Keypad {
            kind: KeypadKind::Directional,
            keypad: [[b'X', b'^', b'A'], [b'<', b'v', b'>']]
                .into_iter()
                .enumerate()
                .flat_map(|(r, row)| {
                    row.into_iter()
                        .enumerate()
                        .map(move |(c, v)| ((r as i32, c as i32), v))
                })
                .collect(),
        }
    }

    fn direction_preference(&self) -> [u8; 4] {
        [b'>', b'^', b'v', b'<']
    }

    fn location_of(&self, c: u8) -> (i32, i32) {
        self.keypad
            .iter()
            .find_map(|((r, c_), v)| if *v == c { Some((*r, *c_)) } else { None })
            .unwrap()
    }

    fn path_to(&self, a: u8, b: u8) -> Vec<u8> {
        let (ar, ac) = self.location_of(a);
        let (br, bc) = self.location_of(b);
        let dr = br - ar;
        let dc = bc - ac;

        let mut steps = Vec::with_capacity(5);
        match dr.signum() {
            -1 => {
                // row is decreasing, so we go "up" on the keypad
                steps.extend(std::iter::repeat_n(b'^', dr.abs() as usize));
            }
            0 => {}
            1 => {
                // row is increasing, so we go "down" on the keypad
                steps.extend(std::iter::repeat_n(b'v', dr.abs() as usize));
            }
            _ => unreachable!(),
        };
        match dc.signum() {
            -1 => {
                // col is decreasing, so we go "left" on the keypad
                steps.extend(std::iter::repeat_n(b'<', dc.abs() as usize));
            }
            0 => {}
            1 => {
                // col is increasing, so we go "right" on the keypad
                steps.extend(std::iter::repeat_n(b'>', dc.abs() as usize));
            }
            _ => unreachable!(),
        };
        let direction_preference = self.direction_preference();
        steps.sort_by_key(|key| {
            direction_preference
                .iter()
                .position(|&v| v == *key)
                .unwrap()
        });
        steps
    }

    fn expand_path(&self, path: &[u8]) -> Vec<u8> {
        let mut current_key = b'A';
        let mut expanded = Vec::with_capacity(path.len() * 5);
        for step in path {
            let path_to_step = self.path_to(current_key, *step);
            expanded.extend(path_to_step);
            expanded.push(b'A');
            current_key = *step;
        }
        expanded
    }
}

const USE_SAMPLE: bool = true;

#[aoc_generator(day21)]
fn parse_input(input: &str) -> Vec<[u8; 4]> {
    let input = if USE_SAMPLE { SAMPLE } else { input };

    input
        .lines()
        .map(|line| line.as_bytes().try_into().unwrap())
        .collect()
}

#[aoc(day21, part1)]
fn part1(input: &[[u8; 4]]) -> usize {
    let mut total_complexity = 0usize;
    let numeric = Keypad::numeric();
    let directional = Keypad::directional();

    for code in input {
        println!("original code: {}", std::str::from_utf8(code).unwrap());
        let numeric_part = std::str::from_utf8(
            code.strip_prefix(&[b'0'])
                .unwrap_or(code)
                .strip_suffix(&[b'A'])
                .unwrap(),
        )
        .unwrap()
        .parse::<usize>()
        .unwrap();
        let mut code = code.to_vec();
        code = numeric.expand_path(&code);
        // println!("expanded: {}", std::str::from_utf8(&code).unwrap());
        code = directional.expand_path(&code);
        // println!("expanded: {}", std::str::from_utf8(&code).unwrap());
        code = directional.expand_path(&code);
        // println!("expanded: {}", std::str::from_utf8(&code).unwrap());
        println!("code length: {}", code.len());
        println!("numeric_part: {}", numeric_part);
        total_complexity += code.len() * numeric_part;
        println!("");
    }

    total_complexity
}
