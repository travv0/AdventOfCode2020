use std::{cmp::max, collections::HashMap};

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let cubes = parse_input(&input);
    println!(
        "Part 1: {}",
        simulate_cycles(6, &cubes)
            .iter()
            .map(|(_, &cube)| if cube { 1 } else { 0 })
            .sum::<usize>()
    );
    let hypercubes = parse_input_part2(&input);
    println!(
        "Part 2: {}",
        simulate_cycles_part2(6, &hypercubes)
            .iter()
            .map(|(_, &cube)| if cube { 1 } else { 0 })
            .sum::<usize>()
    );
}

type Cubes = HashMap<(isize, isize, isize), bool>;

fn parse_input(input: &str) -> Cubes {
    let mut cubes = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                cubes.insert(
                    (
                        x as isize - line.len() as isize / 2,
                        y as isize - input.lines().count() as isize / 2,
                        0,
                    ),
                    true,
                );
            }
        }
    }
    cubes
}

fn simulate_cycles(n: usize, cubes: &Cubes) -> Cubes {
    let (mut max_x, mut max_y, mut max_z) = cubes
        .keys()
        .fold((0, 0, 0), |(max_x, max_y, max_z), &(x, y, z)| {
            (max(x, max_x), max(y, max_y), max(z, max_z))
        });
    let mut cubes = cubes.clone();
    let mut new_cubes = HashMap::new();
    for _ in 0..n {
        for z in -max_z - 2..=max_z + 2 {
            for y in -max_y - 2..=max_y + 2 {
                for x in -max_x - 2..=max_x + 2 {
                    let pos = (x, y, z);
                    let cube = cubes.get(&pos).copied().unwrap_or_default();
                    new_cubes.insert(pos, is_active(pos, cube, &cubes));
                }
            }
        }
        let temp = cubes;
        cubes = new_cubes;
        new_cubes = temp;
        max_x += 1;
        max_y += 1;
        max_z += 1;
    }
    cubes
}

fn is_active(pos: (isize, isize, isize), cube: bool, cubes: &Cubes) -> bool {
    let (x, y, z) = pos;
    let mut neighbors = 0;
    for l in z - 1..=z + 1 {
        for k in y - 1..=y + 1 {
            for j in x - 1..=x + 1 {
                if (j, k, l) != pos {
                    if cubes.get(&(j, k, l)).copied().unwrap_or_default() {
                        neighbors += 1;
                    }
                }
            }
        }
    }
    if cube {
        neighbors == 2 || neighbors == 3
    } else {
        neighbors == 3
    }
}

#[test]
fn test_simulate_cycles() {
    let input = ".#.
..#
###";
    let cubes = parse_input(&input);
    assert_eq!(
        112,
        simulate_cycles(6, &cubes)
            .iter()
            .map(|(_, &cube)| if cube { 1 } else { 0 })
            .sum::<usize>()
    );
}

type Hypercubes = HashMap<(isize, isize, isize, isize), bool>;

fn parse_input_part2(input: &str) -> Hypercubes {
    let mut cubes = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                cubes.insert(
                    (
                        x as isize - line.len() as isize / 2,
                        y as isize - input.lines().count() as isize / 2,
                        0,
                        0,
                    ),
                    true,
                );
            }
        }
    }
    cubes
}

fn simulate_cycles_part2(n: usize, cubes: &Hypercubes) -> Hypercubes {
    let (mut max_x, mut max_y, mut max_z, mut max_w) = cubes.keys().fold(
        (0, 0, 0, 0),
        |(max_x, max_y, max_z, max_w), &(x, y, z, w)| {
            (max(x, max_x), max(y, max_y), max(z, max_z), max(w, max_w))
        },
    );
    let mut cubes = cubes.clone();
    let mut new_cubes = HashMap::new();
    for _ in 0..n {
        for w in -max_w - 2..=max_w + 2 {
            for z in -max_z - 2..=max_z + 2 {
                for y in -max_y - 2..=max_y + 2 {
                    for x in -max_x - 2..=max_x + 2 {
                        let pos = (x, y, z, w);
                        let cube = cubes.get(&pos).copied().unwrap_or_default();
                        new_cubes.insert(pos, is_active_part2(pos, cube, &cubes));
                    }
                }
            }
        }
        let temp = cubes;
        cubes = new_cubes;
        new_cubes = temp;
        max_x += 1;
        max_y += 1;
        max_z += 1;
        max_w += 1;
    }
    cubes
}

fn is_active_part2(pos: (isize, isize, isize, isize), cube: bool, cubes: &Hypercubes) -> bool {
    let (x, y, z, w) = pos;
    let mut neighbors = 0;
    for m in w - 1..=w + 1 {
        for l in z - 1..=z + 1 {
            for k in y - 1..=y + 1 {
                for j in x - 1..=x + 1 {
                    if (j, k, l, m) != pos {
                        if cubes.get(&(j, k, l, m)).copied().unwrap_or_default() {
                            neighbors += 1;
                        }
                    }
                }
            }
        }
    }
    if cube {
        neighbors == 2 || neighbors == 3
    } else {
        neighbors == 3
    }
}

#[test]
fn test_simulate_cycles_part2() {
    let input = ".#.
..#
###";
    let cubes = parse_input_part2(&input);
    assert_eq!(
        848,
        simulate_cycles_part2(6, &cubes)
            .iter()
            .map(|(_, &cube)| if cube { 1 } else { 0 })
            .sum::<usize>()
    );
}
