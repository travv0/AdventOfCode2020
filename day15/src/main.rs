use std::collections::HashMap;

fn main() {
    let starting_numbers = "1,20,8,12,0,14"
        .split(',')
        .filter_map(|i| i.parse().ok())
        .collect::<Vec<_>>();
    println!("Part 1: {}", nth_number_spoken(&starting_numbers, 2020));
    println!("Part 2: {}", nth_number_spoken(&starting_numbers, 30000000));
}

fn nth_number_spoken(starting_numbers: &[usize], n: usize) -> usize {
    let mut last_spokens = HashMap::new();
    let mut last_spoken = starting_numbers[0];
    let mut last_i = 1;
    for (i, &num) in starting_numbers.iter().enumerate().skip(1) {
        last_spokens.insert(last_spoken, last_i);
        last_spoken = num;
        last_i = i + 1;
    }
    for i in starting_numbers.len() + 1..=n {
        let last_pos = last_spokens.entry(last_spoken).or_default();
        last_spoken = if *last_pos == 0 {
            *last_pos
        } else {
            i - 1 - *last_pos
        };
        *last_pos = i - 1;
    }
    last_spoken
}

#[test]
fn test_nth_number_spoken() {
    assert_eq!(436, nth_number_spoken(&[0, 3, 6], 2020));
    assert_eq!(1, nth_number_spoken(&[1, 3, 2], 2020));
    assert_eq!(10, nth_number_spoken(&[2, 1, 3], 2020));
    assert_eq!(27, nth_number_spoken(&[1, 2, 3], 2020));
    assert_eq!(78, nth_number_spoken(&[2, 3, 1], 2020));
    assert_eq!(438, nth_number_spoken(&[3, 2, 1], 2020));
    assert_eq!(1836, nth_number_spoken(&[3, 1, 2], 2020));
}
