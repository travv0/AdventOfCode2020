fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let instructions = parse_instructions(&input);
    println!("Part 1: {}", run_instructions(&instructions, Ship::update1));
    println!("Part 2: {}", run_instructions(&instructions, Ship::update2));
}

#[derive(Debug, Copy, Clone)]
enum Action {
    N,
    S,
    E,
    W,
    L,
    R,
    F,
}

#[derive(Debug, Copy, Clone)]
struct Instruction {
    action: Action,
    value: isize,
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    East,
    South,
    West,
    North,
}

impl From<isize> for Direction {
    fn from(i: isize) -> Self {
        match i {
            0 => Direction::East,
            1 => Direction::South,
            2 => Direction::West,
            3 => Direction::North,
            _ => panic!("no Direction for {}", i),
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Waypoint {
    x: isize,
    y: isize,
}

#[derive(Debug, Copy, Clone)]
struct Ship {
    facing: Direction,
    waypoint: Waypoint,
    x: isize,
    y: isize,
}

impl Ship {
    fn new() -> Self {
        Self {
            facing: Direction::East,
            waypoint: Waypoint { x: 10, y: -1 },
            x: 0,
            y: 0,
        }
    }

    fn update1(&mut self, instruction: Instruction) {
        match instruction {
            Instruction {
                action: Action::N,
                value,
            } => self.y -= value,
            Instruction {
                action: Action::S,
                value,
            } => self.y += value,
            Instruction {
                action: Action::E,
                value,
            } => self.x += value,
            Instruction {
                action: Action::W,
                value,
            } => self.x -= value,
            Instruction {
                action: Action::L,
                value,
            } => self.facing = Direction::from((self.facing as isize - value / 90).rem_euclid(4)),
            Instruction {
                action: Action::R,
                value,
            } => self.facing = Direction::from((self.facing as isize + value / 90) % 4),
            Instruction {
                action: Action::F,
                value,
            } => match self.facing {
                Direction::East => self.x += value,
                Direction::South => self.y += value,
                Direction::West => self.x -= value,
                Direction::North => self.y -= value,
            },
        }
    }

    fn update2(&mut self, instruction: Instruction) {
        match instruction {
            Instruction {
                action: Action::N,
                value,
            } => self.waypoint.y -= value,
            Instruction {
                action: Action::S,
                value,
            } => self.waypoint.y += value,
            Instruction {
                action: Action::E,
                value,
            } => self.waypoint.x += value,
            Instruction {
                action: Action::W,
                value,
            } => self.waypoint.x -= value,
            Instruction {
                action: Action::L,
                value,
            } => {
                let s = (-value as f64).to_radians().sin();
                let c = (-value as f64).to_radians().cos();
                let new_x =
                    (self.waypoint.x as f64 * c - self.waypoint.y as f64 * s).round() as isize;
                let new_y =
                    (self.waypoint.x as f64 * s + self.waypoint.y as f64 * c).round() as isize;
                self.waypoint.x = new_x;
                self.waypoint.y = new_y;
            }
            Instruction {
                action: Action::R,
                value,
            } => {
                let s = (value as f64).to_radians().sin();
                let c = (value as f64).to_radians().cos();
                let new_x =
                    (self.waypoint.x as f64 * c - self.waypoint.y as f64 * s).round() as isize;
                let new_y =
                    (self.waypoint.x as f64 * s + self.waypoint.y as f64 * c).round() as isize;
                self.waypoint.x = new_x;
                self.waypoint.y = new_y;
            }
            Instruction {
                action: Action::F,
                value,
            } => {
                self.x += self.waypoint.x * value;
                self.y += self.waypoint.y * value
            }
        }
    }
}

fn parse_instructions(input: &str) -> Vec<Instruction> {
    input.lines().map(parse_line).collect()
}

fn parse_line(line: &str) -> Instruction {
    let (c, i) = line.split_at(1);
    let action = match c {
        "N" => Action::N,
        "S" => Action::S,
        "E" => Action::E,
        "W" => Action::W,
        "L" => Action::L,
        "R" => Action::R,
        "F" => Action::F,
        _ => panic!("bad Action parse: {}", c),
    };
    Instruction {
        action,
        value: i.parse().expect("unable to parse value"),
    }
}

fn manhattan_distance(x1: isize, y1: isize, x2: isize, y2: isize) -> isize {
    (x2 - x1).abs() + (y2 - y1).abs()
}

fn run_instructions(instructions: &[Instruction], update: fn(&mut Ship, Instruction)) -> isize {
    let mut ship = Ship::new();
    for &instruction in instructions {
        update(&mut ship, instruction);
    }
    manhattan_distance(0, 0, ship.x, ship.y)
}

#[test]
fn test_example1() {
    let input = "F10
N3
F7
R90
F11";
    let instructions = parse_instructions(&input);
    assert_eq!(25, run_instructions(&instructions, Ship::update1));
}

#[test]
fn test_example2() {
    let input = "F10
N3
F7
R90
F11";
    let instructions = parse_instructions(&input);
    assert_eq!(286, run_instructions(&instructions, Ship::update2));
}
