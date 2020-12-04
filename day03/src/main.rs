fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let grid = parse_input(&input).unwrap_or_else(|| panic!("unable to parse input: {}", input));
    println!("Part 1: {}", grid.count_trees_on_path(3, 1));

    let slopes = &[(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    println!(
        "Part 2: {}",
        slopes
            .iter()
            .map(|&(dx, dy)| grid.count_trees_on_path(dx, dy))
            .product::<usize>()
    );
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Square {
    Open,
    Tree,
}

struct Grid {
    squares: Vec<Vec<Square>>,
    width: usize,
}

fn parse_input(input: &str) -> Option<Grid> {
    let squares = input.lines().map(parse_line).collect::<Vec<Vec<_>>>();
    squares.get(0).map(|row| row.len()).map(|len| Grid {
        width: len,
        squares,
    })
}

fn parse_line(line: &str) -> Vec<Square> {
    line.chars()
        .map(|c| if c == '#' { Square::Tree } else { Square::Open })
        .collect()
}

impl Grid {
    fn get_square(&self, x: usize, y: usize) -> Option<Square> {
        let Grid { squares, width } = self;
        squares.get(y).map(|v| v[x % width])
    }

    fn count_trees_on_path(&self, dx: usize, dy: usize) -> usize {
        let (mut x, mut y) = (0, 0);
        let mut count = 0;
        while let Some(square) = self.get_square(x, y) {
            if square == Square::Tree {
                count += 1;
            }
            x += dx;
            y += dy;
        }
        count
    }
}

#[test]
fn test_get_square() {
    let input = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";
    let grid = parse_input(input).unwrap();
    assert_eq!(Some(Square::Open), grid.get_square(3, 1));
    assert_eq!(Some(Square::Tree), grid.get_square(6, 2));
    assert_eq!(Some(Square::Open), grid.get_square(9, 3));
    assert_eq!(Some(Square::Tree), grid.get_square(30, 10));
    assert_eq!(None, grid.get_square(33, 11));
}

#[test]
fn test_count_trees_on_path() {
    let input = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";
    let grid = parse_input(input).unwrap();
    assert_eq!(7, grid.count_trees_on_path(3, 1));
}
