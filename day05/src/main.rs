fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut seat_ids = input
        .lines()
        .map(|line| input_to_seat_id(line).unwrap())
        .collect::<Vec<_>>();
    println!("Part 1: {}", seat_ids.iter().max().unwrap());
    println!("Part 2: {}", find_missing_seat_id(&mut seat_ids).unwrap());
}

fn input_to_seat_id(input: &str) -> Result<usize, String> {
    let (row_chars, col_chars) = input.split_at(7);
    let rows = partition(row_chars, 'F', 'B')?;
    let cols = partition(col_chars, 'L', 'R')?;
    Ok(rows * 8 + cols)
}

fn partition(input: &str, left_char: char, right_char: char) -> Result<usize, String> {
    let mut nums = (0..(2 as usize).pow(input.len() as u32)).collect::<Vec<_>>();
    for c in input.chars() {
        if c == left_char {
            nums = nums.iter().copied().take(nums.len() / 2).collect();
        } else if c == right_char {
            nums = nums.iter().copied().skip(nums.len() / 2).collect();
        } else {
            return Err(format!("invalid char in input: '{}'", c));
        }
    }
    Ok(nums[0])
}

fn find_missing_seat_id(seat_ids: &mut [usize]) -> Option<usize> {
    seat_ids.sort_unstable();
    let mut prev_num = seat_ids[0];
    let mut result = None;
    for &seat_id in &seat_ids[1..] {
        if seat_id != prev_num + 1 {
            result = Some(prev_num + 1);
        }
        prev_num = seat_id;
    }
    result
}

#[test]
fn test_input_to_seat_id() {
    assert_eq!(Ok(357), input_to_seat_id("FBFBBFFRLR"));
    assert_eq!(Ok(567), input_to_seat_id("BFFFBBFRRR"));
    assert_eq!(Ok(119), input_to_seat_id("FFFBBBFRRR"));
    assert_eq!(Ok(820), input_to_seat_id("BBFFBBFRLL"));
}
