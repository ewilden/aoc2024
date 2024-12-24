use auto_enums::auto_enum;
use derivative::Derivative;
use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};

use aoc_runner_derive::{aoc, aoc_generator};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Space {
    Empty,
    Wall,
}

#[derive(Clone, Debug)]
struct Input {
    grid: HashMap<(i64, i64), Space>,
    start: (i64, i64),
    end: (i64, i64),
}

const SAMPLE: &str = "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############";

const USE_SAMPLE: bool = true;

#[aoc_generator(day20)]
fn parse_input(input: &str) -> Input {
    let input = if USE_SAMPLE { SAMPLE } else { input };
    let mut grid = HashMap::new();
    let mut start = None;
    let mut end = None;
    for (r, row) in input.lines().enumerate() {
        for (c, chr) in row.chars().enumerate() {
            let pos = (r as i64, c as i64);
            grid.insert(
                pos,
                match chr {
                    '#' => Space::Wall,
                    'S' => {
                        start = Some(pos);
                        Space::Empty
                    }
                    'E' => {
                        end = Some(pos);
                        Space::Empty
                    }
                    '.' => Space::Empty,
                    _ => panic!(),
                },
            );
        }
    }
    Input {
        grid,
        start: start.unwrap(),
        end: end.unwrap(),
    }
}

fn manhattan_dist((r1, c1): (i64, i64), (r2, c2): (i64, i64)) -> i64 {
    (r1 - r2).abs() + (c1 - c2).abs()
}

impl Input {
    fn neighbors(&self, pos: (i64, i64)) -> impl Iterator<Item = (i64, i64)> + '_ {
        [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .into_iter()
            .filter_map(move |(dr, dc)| {
                let neighbor = (pos.0 + dr, pos.1 + dc);
                (self.grid.get(&neighbor) == Some(&Space::Empty)).then_some(neighbor)
            })
    }

    fn neighbors_within_cheat_radius<const CHEAT_RADIUS: i64>(
        &self,
        (r, c): (i64, i64),
    ) -> impl Iterator<Item = (i64, i64)> + '_ {
        ((-CHEAT_RADIUS)..=CHEAT_RADIUS)
            .cartesian_product((-CHEAT_RADIUS)..=CHEAT_RADIUS)
            .filter_map(move |(dr, dc)| {
                let manhattan = dr.abs() + dc.abs();
                if manhattan < 2 || manhattan > CHEAT_RADIUS {
                    return None;
                }
                let neighbor = (r + dr, c + dc);
                (self.grid.get(&neighbor) == Some(&Space::Empty)).then_some(neighbor)
            })
    }

    #[auto_enum(Iterator)]
    fn node_edges<const CHEAT_RADIUS: i64>(
        &self,
        queue_entry: QueueEntry,
    ) -> impl Iterator<Item = QueueEntry> + '_ {
        let QueueEntry { node, steps } = queue_entry;
        match node.cheated {
            Some(cheat) => self.neighbors(node.pos).map(move |pos| QueueEntry {
                steps: steps + 1,
                node: Node {
                    pos,
                    cheated: Some(cheat),
                },
            }),
            None => self
                .neighbors_within_cheat_radius::<CHEAT_RADIUS>(node.pos)
                .map(move |pos| QueueEntry {
                    steps: manhattan_dist(node.pos, pos),
                    node: Node {
                        pos,
                        cheated: Some(Cheat {
                            start_pos: node.pos,
                            end_pos: pos,
                        }),
                    },
                })
                .chain(self.neighbors(node.pos).map(move |pos| QueueEntry {
                    steps: steps + 1,
                    node: Node { pos, cheated: None },
                })),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Cheat {
    start_pos: (i64, i64),
    end_pos: (i64, i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Node {
    pos: (i64, i64),
    cheated: Option<Cheat>,
}

impl Node {
    fn at_step(self, step: i64) -> QueueEntry {
        QueueEntry {
            node: self,
            steps: step,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct QueueEntry {
    node: Node,
    steps: i64,
}

#[derive(Derivative)]
#[derivative(Debug)]
struct State<'a, const CHEAT_RADIUS: i64> {
    #[derivative(Debug = "ignore")]
    input: &'a Input,
    queue: VecDeque<QueueEntry>,
    winning_cheats: HashSet<Cheat>,
    ending_condition: EndingCondition,
    visited: HashSet<Node>,
}

#[derive(Debug)]
enum Done {
    QueueEmpty,
    ReachedEnd(i64),
    StepLimitReached,
}

impl<'a, const CHEAT_RADIUS: i64> State<'a, CHEAT_RADIUS> {
    fn new(input: &'a Input, ending_condition: EndingCondition) -> State<'a, CHEAT_RADIUS> {
        let mut queue = VecDeque::new();
        queue.push_back(
            Node {
                pos: input.start,
                cheated: None,
            }
            .at_step(0),
        );
        State {
            input,
            queue,
            winning_cheats: HashSet::new(),
            ending_condition,
            visited: HashSet::new(),
        }
    }

    fn step(&mut self) -> Option<Done> {
        let Some(entry) = self.queue.pop_front() else {
            return Some(Done::QueueEmpty);
        };
        if !self.visited.insert(entry.node) {
            return None;
        }
        if let EndingCondition::StepLimit(step_limit) = self.ending_condition {
            if entry.steps > step_limit {
                return Some(Done::StepLimitReached);
            }
        }
        if entry.node.pos == self.input.end {
            if let Some(cheat) = entry.node.cheated {
                self.winning_cheats.insert(cheat);
                eprintln!("Found winning cheat: {cheat:?}");
            }
            if matches!(self.ending_condition, EndingCondition::FoundEnd) {
                return Some(Done::ReachedEnd(entry.steps));
            }
        }
        if let Some(cheat) = entry.node.cheated {
            if self.winning_cheats.contains(&cheat) {
                // No need to explore this node further since we already know its cheat is successful.
                return None;
            }
        }
        self.queue
            .extend(self.input.node_edges::<CHEAT_RADIUS>(entry));
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

#[derive(Debug)]
enum EndingCondition {
    FoundEnd,
    StepLimit(i64),
}

const CHEAT_ADVANTAGE_PART_1: i64 = if USE_SAMPLE { 40 } else { 100 };

#[aoc(day20, part1)]
fn part1(input: &Input) -> i64 {
    let original_cost = {
        let mut state = State::<0>::new(input, EndingCondition::FoundEnd);
        match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            done => panic!("unexpected done {done:?} state {state:?}"),
        }
    };
    eprintln!("cost without cheating: {original_cost}");
    let threshold = original_cost - CHEAT_ADVANTAGE_PART_1;
    let mut state = State::<2>::new(input, EndingCondition::StepLimit(threshold));
    match state.run_to_completion() {
        Done::StepLimitReached => state.winning_cheats.len() as i64,
        _ => panic!("expected to reach step limit"),
    }
}

const CHEAT_ADVANTAGE_PART_2: i64 = if USE_SAMPLE { 74 } else { 100 };

#[aoc(day20, part2)]
fn part2(input: &Input) -> i64 {
    let original_cost = {
        let mut state = State::<0>::new(input, EndingCondition::FoundEnd);
        match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            done => panic!("unexpected done {done:?} state {state:?}"),
        }
    };
    eprintln!("cost without cheating: {original_cost}");
    let threshold = original_cost - CHEAT_ADVANTAGE_PART_2;
    let mut state = State::<20>::new(input, EndingCondition::StepLimit(threshold));
    match state.run_to_completion() {
        Done::StepLimitReached => state.winning_cheats.len() as i64,
        _ => panic!("expected to reach step limit"),
    }
}
