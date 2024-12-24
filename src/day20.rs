use auto_enums::auto_enum;
use derivative::Derivative;
use itertools::Itertools;
use priority_queue::PriorityQueue;
use std::collections::{HashMap, HashSet};

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

const USE_SAMPLE: bool = false;

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
    fn node_edges<'a, const CHEAT_RADIUS: i64>(
        &'a self,
        node: Node,
        banned_cheats: &'a HashSet<Cheat>,
    ) -> impl Iterator<Item = (Node, i64)> + 'a {
        match node.cheated {
            true => self
                .neighbors(node.pos)
                .map(move |pos| (Node { pos, cheated: true }, 1)),
            false => self
                .neighbors_within_cheat_radius::<CHEAT_RADIUS>(node.pos)
                .filter(move |pos| {
                    !banned_cheats.contains(&Cheat {
                        start_pos: node.pos,
                        end_pos: *pos,
                    })
                })
                .map(move |pos| (Node { pos, cheated: true }, manhattan_dist(node.pos, pos)))
                .chain(self.neighbors(node.pos).map(move |pos| {
                    (
                        Node {
                            pos,
                            cheated: false,
                        },
                        1,
                    )
                })),
        }
    }

    fn heuristic(&self, node: Node) -> i64 {
        manhattan_dist(node.pos, self.end)
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
    cheated: bool,
}

#[derive(Derivative)]
#[derivative(Debug)]
struct State<'a, const CHEAT_RADIUS: i64> {
    #[derivative(Debug = "ignore")]
    input: &'a Input,
    open_set: PriorityQueue<Node, i64>,
    came_from: HashMap<Node, Node>,
    g_score: HashMap<Node, i64>,
    banned_cheats: &'a HashSet<Cheat>,
}

#[derive(Debug)]
enum Done {
    QueueEmpty,
    ReachedEnd(i64),
}

impl<'a, const CHEAT_RADIUS: i64> State<'a, CHEAT_RADIUS> {
    fn new(input: &'a Input, banned_cheats: &'a HashSet<Cheat>) -> State<'a, CHEAT_RADIUS> {
        let mut open_set = PriorityQueue::new();
        let start_node = Node {
            pos: input.start,
            cheated: false,
        };
        open_set.push(start_node, -input.heuristic(start_node));
        State {
            input,
            open_set,
            came_from: HashMap::new(),
            g_score: HashMap::from_iter([(start_node, 0)]),
            banned_cheats,
        }
    }

    fn step(&mut self) -> Option<Done> {
        let Some((current, _)) = self.open_set.pop() else {
            return Some(Done::QueueEmpty);
        };
        let cost = *self.g_score.get(&current).unwrap();
        if current.pos == self.input.end {
            return Some(Done::ReachedEnd(cost));
        }
        for (neighbor, edge_weight) in self
            .input
            .node_edges::<CHEAT_RADIUS>(current, self.banned_cheats)
        {
            let tentative_g_score = cost + edge_weight;
            if self
                .g_score
                .get(&neighbor)
                .map(|&s| s > tentative_g_score)
                .unwrap_or(true)
            {
                self.came_from.insert(neighbor, current);
                self.g_score.insert(neighbor, tentative_g_score);
                let f_score = tentative_g_score + self.input.heuristic(neighbor);
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

    fn find_cheat(&self) -> Cheat {
        let mut current = Node {
            pos: self.input.end,
            cheated: true,
        };
        loop {
            let next = self.came_from[&current];
            if !next.cheated {
                return Cheat {
                    start_pos: next.pos,
                    end_pos: current.pos,
                };
            } else {
                current = next;
            }
        }
    }
}

const CHEAT_ADVANTAGE_PART_1: i64 = if USE_SAMPLE { 40 } else { 100 };

#[aoc(day20, part1)]
fn part1(input: &Input) -> i64 {
    let original_cost = {
        let banned_cheats = HashSet::new();
        let mut state = State::<0>::new(input, &banned_cheats);
        match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            done => panic!("unexpected done {done:?} state {state:?}"),
        }
    };
    eprintln!("cost without cheating: {original_cost}");
    let threshold = original_cost - CHEAT_ADVANTAGE_PART_1;

    let mut banned_cheats = HashSet::new();
    loop {
        let mut state = State::<2>::new(input, &banned_cheats);
        let cost = match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            _ => panic!("expected to reach step limit"),
        };
        if cost > threshold {
            return banned_cheats.len() as i64;
        } else {
            let cheat = state.find_cheat();
            // eprintln!("banning cheat {cheat:?}");
            banned_cheats.insert(cheat);
        }
    }
}

const CHEAT_ADVANTAGE_PART_2: i64 = if USE_SAMPLE { 74 } else { 100 };

#[aoc(day20, part2)]
fn part2(input: &Input) -> i64 {
    let original_cost = {
        let banned_cheats = HashSet::new();
        let mut state = State::<0>::new(input, &banned_cheats);
        match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            done => panic!("unexpected done {done:?} state {state:?}"),
        }
    };
    eprintln!("cost without cheating: {original_cost}");
    let threshold = original_cost - CHEAT_ADVANTAGE_PART_2;

    let mut banned_cheats = HashSet::new();
    loop {
        let mut state = State::<20>::new(input, &banned_cheats);
        let cost = match state.run_to_completion() {
            Done::ReachedEnd(cost) => cost,
            _ => panic!("expected to reach step limit"),
        };
        if cost > threshold {
            return banned_cheats.len() as i64;
        } else {
            let cheat = state.find_cheat();
            // eprintln!("banning cheat {cheat:?}");
            banned_cheats.insert(cheat);
        }
    }
}
