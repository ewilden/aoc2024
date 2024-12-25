use derivative::Derivative;
use itertools::Itertools;
use priority_queue::PriorityQueue;
use std::{collections::HashMap, fmt::Write};

use aoc_runner_derive::{aoc, aoc_generator};

const SAMPLE: &str = "029A
980A
179A
456A
379A";

fn format_as_char(c: &u8, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    f.write_char(*c as char)
}

fn format_as_char_vec(v: &[u8], f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    for c in v {
        f.write_char(*c as char).unwrap();
    }
    Ok(())
}

struct Keypad {
    keypad: HashMap<(i32, i32), u8>,
}

impl Keypad {
    fn numeric() -> Keypad {
        Keypad {
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
                    .filter(|(_, v)| *v != b'X')
            })
            .collect(),
        }
    }

    fn directional() -> Keypad {
        Keypad {
            keypad: [[b'X', b'^', b'A'], [b'<', b'v', b'>']]
                .into_iter()
                .enumerate()
                .flat_map(|(r, row)| {
                    row.into_iter()
                        .enumerate()
                        .map(move |(c, v)| ((r as i32, c as i32), v))
                        .filter(|(_, v)| *v != b'X')
                })
                .collect(),
        }
    }

    fn location_of(&self, c: u8) -> (i32, i32) {
        self.keypad
            .iter()
            .find_map(|((r, c_), v)| if *v == c { Some((*r, *c_)) } else { None })
            .unwrap()
    }

    fn edges_on_path_to(&self, a: u8, b: u8) -> impl Iterator<Item = u8> + use<'_> {
        if a == b {
            return itertools::Either::Left(std::iter::once(b'A'));
        }

        let (ar, ac) = self.location_of(a);
        let (br, bc) = self.location_of(b);
        let dr = br - ar;
        let dc = bc - ac;

        let dr_step = match dr.signum() {
            -1 => {
                // row is decreasing, so we go "up" on the keypad
                Some(b'^')
            }
            0 => None,
            1 => {
                // row is increasing, so we go "down" on the keypad
                Some(b'v')
            }
            _ => unreachable!(),
        };
        let dc_step = match dc.signum() {
            -1 => {
                // col is decreasing, so we go "left" on the keypad
                Some(b'<')
            }
            0 => None,
            1 => {
                // col is increasing, so we go "right" on the keypad
                Some(b'>')
            }
            _ => unreachable!(),
        };

        itertools::Either::Right(dr_step.into_iter().chain(dc_step).filter(move |v| {
            let (dr, dc) = match v {
                b'^' => (-1, 0),
                b'v' => (1, 0),
                b'<' => (0, -1),
                b'>' => (0, 1),
                _ => unreachable!(),
            };
            let loc = (ar + dr, ac + dc);
            self.keypad.contains_key(&loc)
        }))
    }
}

#[derive(Derivative, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
#[derivative(Debug)]
struct SearchNode<const NUM_DIRECTIONALS: usize> {
    #[derivative(Debug(format_with = "format_as_char_vec"))]
    code_so_far: Vec<u8>,
    #[derivative(Debug(format_with = "format_as_char"))]
    numeric: u8,
    #[derivative(Debug(format_with = "format_as_char_vec"))]
    directionals: [u8; NUM_DIRECTIONALS],
}

#[derive(Derivative)]
#[derivative(Debug)]
struct SearchState<const NUM_DIRECTIONALS: usize> {
    #[derivative(Debug = "ignore")]
    numeric_keypad: Keypad,
    #[derivative(Debug = "ignore")]
    directional_keypad: Keypad,
    open_set: PriorityQueue<SearchNode<NUM_DIRECTIONALS>, i64>,
    came_from: HashMap<SearchNode<NUM_DIRECTIONALS>, SearchNode<NUM_DIRECTIONALS>>,
    g_score: HashMap<SearchNode<NUM_DIRECTIONALS>, i64>,
    #[derivative(Debug(format_with = "format_as_char_vec"))]
    target_code: [u8; 4],
}

#[derive(Debug)]
enum Done {
    QueueEmpty,
    ReachedEnd(i64),
}

#[derive(Debug)]
enum Level {
    Numeric,
    Directional(usize),
}

impl Level {
    fn next(&self) -> Self {
        match self {
            Level::Directional(0) => Level::Numeric,
            Level::Directional(i) => Level::Directional(i - 1),
            Level::Numeric => panic!("no more levels"),
        }
    }
}

impl<const NUM_DIRECTIONALS: usize> SearchState<NUM_DIRECTIONALS> {
    fn heuristic(&self, _node: SearchNode<NUM_DIRECTIONALS>) -> i64 {
        // Let's implement a heuristic later if it seems necessary.
        return 0;
    }

    fn new(target_code: [u8; 4]) -> Self {
        let start_node = SearchNode {
            code_so_far: Vec::new(),
            numeric: b'A',
            directionals: [b'A'; NUM_DIRECTIONALS],
        };
        let mut open_set = PriorityQueue::new();
        open_set.push(start_node.clone(), 0);
        Self {
            numeric_keypad: Keypad::numeric(),
            directional_keypad: Keypad::directional(),
            open_set,
            came_from: HashMap::new(),
            g_score: HashMap::from_iter([(start_node, 0)]),
            target_code,
        }
    }

    fn apply_input(&self, level: Level, node: &mut SearchNode<NUM_DIRECTIONALS>, input: u8) {
        eprintln!("apply_input {level:?} {node:?} {}", input as char);
        let offset = match input {
            b'A' => None,
            b'<' => Some((0, -1)),
            b'>' => Some((0, 1)),
            b'^' => Some((-1, 0)),
            b'v' => Some((1, 0)),
            _ => unreachable!(),
        };
        match offset {
            None => match level {
                Level::Numeric => {
                    node.code_so_far.push(node.numeric);
                }
                Level::Directional(i) => self.apply_input(level.next(), node, node.directionals[i]),
            },
            Some((dr, dc)) => {
                let (keypad, curr) = match level {
                    Level::Numeric => (&self.numeric_keypad, node.numeric),
                    Level::Directional(i) => (&self.directional_keypad, node.directionals[i]),
                };
                let (r, c) = keypad.location_of(curr);
                let loc = (r + dr, c + dc);
                match level {
                    Level::Numeric => node.numeric = keypad.keypad[&loc],
                    Level::Directional(i) => node.directionals[i] = keypad.keypad[&loc],
                }
            }
        }
    }

    fn node_edges(
        &self,
        node: &SearchNode<NUM_DIRECTIONALS>,
    ) -> Vec<(SearchNode<NUM_DIRECTIONALS>, i64)> {
        let SearchNode {
            code_so_far,
            numeric,
            directionals,
        } = node;
        if code_so_far.len() == self.target_code.len() {
            panic!("shouldn't be in this method if we're done: {node:?}")
        }
        let next_digit = self.target_code[code_so_far.len()];
        let mut potential_inputs = if *numeric == next_digit {
            // We just want to push 'A'.
            vec![b'A']
        } else {
            // We know we want to navigate `numeric` to the correct next digit,
            // but we don't know exactly what direction to go in.
            self.numeric_keypad
                .edges_on_path_to(*numeric, next_digit)
                .collect_vec()
        };
        for &directional in directionals {
            potential_inputs = potential_inputs
                .into_iter()
                .flat_map(|input| self.directional_keypad.edges_on_path_to(directional, input))
                .collect_vec();
        }
        potential_inputs
            .into_iter()
            .map(move |input| {
                let mut node = node.clone();
                self.apply_input(Level::Directional(NUM_DIRECTIONALS - 1), &mut node, input);
                (node, 1)
            })
            .collect_vec()
    }

    fn step(&mut self) -> Option<Done> {
        let Some((current, _)) = self.open_set.pop() else {
            return Some(Done::QueueEmpty);
        };
        eprintln!("processing {current:?}");

        if self
            .target_code
            .strip_prefix(current.code_so_far.as_slice())
            .is_none()
        {
            panic!("node {current:?} has incorrect code_so_far, state={self:?}");
        }

        let cost = *self.g_score.get(&current).unwrap();
        if current
            == (SearchNode {
                code_so_far: self.target_code.to_vec(),
                numeric: b'A',
                directionals: [b'A'; NUM_DIRECTIONALS],
            })
        {
            return Some(Done::ReachedEnd(cost));
        }

        for (neighbor, edge_weight) in self.node_edges(&current) {
            let tentative_g_score = cost + edge_weight;
            if self
                .g_score
                .get(&neighbor)
                .map(|&s| s == tentative_g_score)
                .unwrap_or(false)
            {
                // If we need to track multiple ancestors, we can make `came_from` map to sets
                // rather than single elements and then add them here.
            }
            if self
                .g_score
                .get(&neighbor)
                .map(|&s| s > tentative_g_score)
                .unwrap_or(true)
            {
                self.came_from.insert(neighbor.clone(), current.clone());
                self.g_score.insert(neighbor.clone(), tentative_g_score);
                let f_score = tentative_g_score + self.heuristic(neighbor.clone());
                self.open_set.push(neighbor, -f_score);
            }
        }
        None
    }

    fn run_to_completion(&mut self) -> Done {
        loop {
            match self.step() {
                Some(done) => return done,
                None => (),
            }
        }
    }
}

const USE_SAMPLE: bool = false;

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
        println!("numeric_part: {numeric_part}");

        let mut search_state = SearchState::<2>::new(*code);
        let done = search_state.run_to_completion();
        let cost = match done {
            Done::QueueEmpty => unreachable!(),
            Done::ReachedEnd(cost) => cost,
        };

        println!("cost: {cost}");
        total_complexity += cost as usize * numeric_part;
    }
    total_complexity
}
