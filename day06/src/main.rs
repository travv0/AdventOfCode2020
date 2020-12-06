use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let groups = input.split("\n\n");
    println!(
        "Part 1: {}",
        groups.clone().map(count_group_yesses).sum::<usize>()
    );
    println!(
        "Part 2: {}",
        groups.map(count_unanimous_yesses).sum::<usize>()
    );
}

fn count_group_yesses(group: &str) -> usize {
    let mut yesses = HashSet::new();
    for line in group.lines() {
        for c in line.chars() {
            yesses.insert(c);
        }
    }
    yesses.len()
}

#[test]
fn test_count_group_yesses() {
    assert_eq!(6, count_group_yesses("abcx\nabcy\nabcz\n"));
}

fn count_unanimous_yesses(group: &str) -> usize {
    let mut yesses: HashMap<char, usize> = HashMap::new();
    let lines = group.lines();
    for line in lines.clone() {
        for c in line.chars() {
            let yes = yesses.entry(c).or_default();
            *yes += 1;
        }
    }
    let line_count = lines.count();
    yesses
        .iter()
        .filter(|(_, yes_count)| **yes_count == line_count)
        .count()
}

#[test]
fn test_count_unanimous_yesses() {
    assert_eq!(3, count_unanimous_yesses("abc"));
    assert_eq!(0, count_unanimous_yesses("a\nb\nc\n"));
    assert_eq!(1, count_unanimous_yesses("ab\nac\n"));
    assert_eq!(1, count_unanimous_yesses("a\na\na\na\n"));
    assert_eq!(1, count_unanimous_yesses("b"));
}
