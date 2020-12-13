fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let (earliest_timestamp, busses) = parse_input(&input);
    println!(
        "Part 1: {}",
        find_earliest_bus_answer(earliest_timestamp, &busses)
    );
    println!("Part 2: {}", find_first_with_offsets(&busses));
}

fn parse_input(input: &str) -> (isize, Vec<(isize, isize)>) {
    if let &[timestamp, bus_nums] = &input.lines().collect::<Vec<_>>()[..] {
        (
            timestamp
                .parse()
                .expect("couldn't parse first line as integer"),
            bus_nums
                .split(',')
                .enumerate()
                .filter_map(|(i, num)| {
                    if let Ok(num) = num.parse() {
                        Some((i as isize, num))
                    } else {
                        None
                    }
                })
                .collect(),
        )
    } else {
        panic!("bad input");
    }
}

#[test]
fn test_parse_input() {
    let input = "939\n7,13,x,x,59,x,31,19";
    assert_eq!(
        (939, vec![(0, 7), (1, 13), (4, 59), (6, 31), (7, 19)]),
        parse_input(&input)
    );
}

fn find_earliest_bus_answer(earliest_timestamp: isize, busses: &[(isize, isize)]) -> isize {
    let (earliest_bus, earliest_time) = busses.iter().fold(
        (0, isize::MAX),
        |(earliest_bus, earliest_time), &(_, bus)| {
            let time = earliest_timestamp + (bus - earliest_timestamp % bus);
            if time < earliest_time {
                (bus, time)
            } else {
                (earliest_bus, earliest_time)
            }
        },
    );
    earliest_bus * (earliest_time - earliest_timestamp)
}

#[test]
fn test_find_earliest_bus_answer() {
    let input = "939\n7,13,x,x,59,x,31,19";
    let (earliest_timestamp, busses) = parse_input(&input);
    assert_eq!(295, find_earliest_bus_answer(earliest_timestamp, &busses));
}

fn find_coefficients(a: isize, b: isize) -> (isize, isize) {
    let (mut old_r, mut r) = (a, b);
    let (mut old_s, mut s) = (1, 0);
    let (mut old_t, mut t) = (0, 1);

    while r != 0 {
        let quotient = old_r / r;
        {
            let prov = r;
            r = old_r - quotient * prov;
            old_r = prov;
        }
        {
            let prov = s;
            s = old_s - quotient * prov;
            old_s = prov;
        }
        {
            let prov = t;
            t = old_t - quotient * prov;
            old_t = prov;
        }
    }

    (old_s, old_t)
}

#[test]
fn test_find_coefficients() {
    assert_eq!((1, -1), find_coefficients(4, 3));
    assert_eq!((5, -2), find_coefficients(5, 12));
}

fn find_first_with_offsets(busses: &[(isize, isize)]) -> isize {
    let mut result = 0;
    let prod = busses.iter().map(|&(_, bus)| bus).product::<isize>();

    for &(a, n) in busses {
        let (_, y) = find_coefficients(n, prod / n);
        result += (n - a) * y * (prod / n);
    }

    result.rem_euclid(prod)
}

#[test]
fn test_find_first_with_offsets() {
    let input = "939\n7,13,x,x,59,x,31,19";
    let (_, busses) = parse_input(&input);
    assert_eq!(1068781, find_first_with_offsets(&busses));
    assert_eq!(3417, find_first_with_offsets(&[(0, 17), (2, 13), (3, 19)]));
    assert_eq!(
        754018,
        find_first_with_offsets(&[(0, 67), (1, 7), (2, 59), (3, 61)])
    );
    assert_eq!(
        1202161486,
        find_first_with_offsets(&[(0, 1789), (1, 37), (2, 47), (3, 1889)])
    );
}
