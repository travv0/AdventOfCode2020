use std::cmp::{max, min};

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let cells = parse_input(&input);
    let final_cells_part_1 = run_to_completion(&cells, get_new_state_part_1);
    println!("Part 1: {}", count_occupied_seats(&final_cells_part_1));
    let final_cells_part_2 = run_to_completion(&cells, get_new_state_part_2);
    println!("Part 2: {}", count_occupied_seats(&final_cells_part_2));
}

type Cells = Vec<Vec<Cell>>;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Cell {
    Floor,
    Empty,
    Occupied,
}

fn parse_input(input: &str) -> Cells {
    input.lines().map(parse_line).collect()
}

fn parse_line(line: &str) -> Vec<Cell> {
    line.chars()
        .map(|c| match c {
            '.' => Cell::Floor,
            'L' => Cell::Empty,
            '#' => Cell::Occupied,
            _ => panic!("invalid character in input: '{}'", c),
        })
        .collect()
}

fn count_occupied_adjacent_seats(x: usize, y: usize, cells: &Cells) -> usize {
    let mut count = 0;
    for j in max(0, y as isize - 1) as usize..min(cells.len(), y + 2) {
        for i in max(0, x as isize - 1) as usize..min(cells[j].len(), x + 2) {
            if (x != i || y != j) && cells[j][i] == Cell::Occupied {
                count += 1;
            }
        }
    }
    count
}

fn run_round(cells: &Cells, get_new_state: fn(x: usize, y: usize, cells: &Cells) -> Cell) -> Cells {
    cells
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(|(x, _)| get_new_state(x, y, &cells))
                .collect()
        })
        .collect()
}

fn get_new_state_part_1(x: usize, y: usize, cells: &Cells) -> Cell {
    match cells[y][x] {
        Cell::Empty if count_occupied_adjacent_seats(x, y, cells) == 0 => Cell::Occupied,
        Cell::Occupied if count_occupied_adjacent_seats(x, y, cells) >= 4 => Cell::Empty,
        cell => cell,
    }
}

fn count_occupied_visible_seats(x: usize, y: usize, cells: &Cells) -> usize {
    let dirs = [
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
    ];
    let mut count = 0;
    for &dir in &dirs {
        let (mut i, mut j) = (x as isize + dir.0, y as isize + dir.1);
        while 0 <= j
            && (j as usize) < cells.len()
            && 0 <= i
            && (i as usize) < cells[j as usize].len()
        {
            if cells[j as usize][i as usize] != Cell::Floor {
                if cells[j as usize][i as usize] == Cell::Occupied {
                    count += 1;
                }
                break;
            }
            i = i + dir.0;
            j = j + dir.1;
        }
    }
    count
}

fn get_new_state_part_2(x: usize, y: usize, cells: &Cells) -> Cell {
    match cells[y][x] {
        Cell::Empty if count_occupied_visible_seats(x, y, cells) == 0 => Cell::Occupied,
        Cell::Occupied if count_occupied_visible_seats(x, y, cells) >= 5 => Cell::Empty,
        cell => cell,
    }
}

fn run_to_completion(
    cells: &Cells,
    get_new_state: fn(x: usize, y: usize, cells: &Cells) -> Cell,
) -> Cells {
    let mut cells = cells.clone();
    loop {
        let new_cells = run_round(&cells, get_new_state);
        if new_cells == cells {
            break;
        } else {
            cells = new_cells;
        }
    }
    cells
}

fn count_occupied_seats(cells: &Cells) -> usize {
    cells
        .iter()
        .map(|row| {
            row.iter()
                .copied()
                .filter(|&cell| cell == Cell::Occupied)
                .count()
        })
        .sum()
}

#[test]
fn test_example_part_1() {
    let input = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";
    let cells = parse_input(&input);
    let final_cells = run_to_completion(&cells, get_new_state_part_1);
    assert_eq!(37, count_occupied_seats(&final_cells));
}

#[test]
fn test_example_part_2() {
    let input = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";
    let cells = parse_input(&input);
    let final_cells = run_to_completion(&cells, get_new_state_part_2);
    assert_eq!(26, count_occupied_seats(&final_cells));
}
