fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let lines = input.trim().split('\n').map(parse_line);
    println!(
        "Part 1: {}",
        lines
            .clone()
            .filter(|&(policy, password)| { password_valid_part1(policy, password) })
            .count()
    );
    println!(
        "Part 2: {}",
        lines
            .clone()
            .filter(|&(policy, password)| { password_valid_part2(policy, password) })
            .count()
    );
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct Policy {
    min: usize,
    max: usize,
    character: char,
}

fn password_valid_part1(policy: Policy, password: &str) -> bool {
    let count = password.chars().filter(|&c| c == policy.character).count();
    policy.min <= count && count <= policy.max
}

#[test]
fn test_password_valid_part1() {
    assert!(password_valid_part1(
        Policy {
            min: 1,
            max: 3,
            character: 'a'
        },
        "abcde"
    ));
    assert!(!password_valid_part1(
        Policy {
            min: 1,
            max: 3,
            character: 'b'
        },
        "cdefg"
    ));
    assert!(password_valid_part1(
        Policy {
            min: 2,
            max: 9,
            character: 'c'
        },
        "ccccccccc"
    ));
}

fn password_valid_part2(policy: Policy, password: &str) -> bool {
    (password.chars().nth(policy.min - 1) == Some(policy.character))
        ^ (password.chars().nth(policy.max - 1) == Some(policy.character))
}

#[test]
fn test_password_valid_part2() {
    assert!(password_valid_part2(
        Policy {
            min: 1,
            max: 3,
            character: 'a'
        },
        "abcde"
    ));
    assert!(!password_valid_part2(
        Policy {
            min: 1,
            max: 3,
            character: 'b'
        },
        "cdefg"
    ));
    assert!(!password_valid_part2(
        Policy {
            min: 2,
            max: 9,
            character: 'c'
        },
        "ccccccccc"
    ));
}

fn parse_line(line: &str) -> (Policy, &str) {
    let words = line.split(' ').collect::<Vec<_>>();
    let min_max = words[0]
        .split('-')
        .map(|i| {
            i.parse()
                .unwrap_or_else(|e| panic!("couldn't parse '{}' as integer: {}", i, e))
        })
        .collect::<Vec<_>>();
    let min = min_max[0];
    let max = min_max[1];
    let character = words[1]
        .chars()
        .next()
        .unwrap_or_else(|| panic!("couldn't parse character from line '{}'", line));
    let password = words[2];
    (
        Policy {
            min,
            max,
            character,
        },
        password,
    )
}

#[test]
fn test_parse_line() {
    assert_eq!(
        parse_line("1-3 a: abcde"),
        (
            Policy {
                min: 1,
                max: 3,
                character: 'a'
            },
            "abcde"
        )
    );
    assert_eq!(
        parse_line("1-3 b: cdefg"),
        (
            Policy {
                min: 1,
                max: 3,
                character: 'b'
            },
            "cdefg"
        )
    );
    assert_eq!(
        parse_line("2-9 c: ccccccccc"),
        (
            Policy {
                min: 2,
                max: 9,
                character: 'c'
            },
            "ccccccccc"
        )
    );
}
