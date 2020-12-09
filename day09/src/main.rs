use itertools::Itertools;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let nums = parse_input(&input);
    let invalid_num = first_invalid_num(&nums, 25).unwrap();
    println!("Part 1: {}", invalid_num);
    println!(
        "Part 2: {}",
        find_encryption_weakness(&nums, invalid_num).unwrap()
    );
}

fn parse_input(input: &str) -> Vec<usize> {
    input.lines().filter_map(|line| line.parse().ok()).collect()
}

fn first_invalid_num(nums: &[usize], preamble_size: usize) -> Option<usize> {
    for i in preamble_size..nums.len() {
        if let None = find_nums_that_sum_to(2, &nums[i - preamble_size..i], nums[i]) {
            return Some(nums[i]);
        }
    }
    None
}

// from day 1
fn find_nums_that_sum_to(n: usize, nums: &[usize], sum: usize) -> Option<Vec<usize>> {
    nums.iter()
        .copied()
        .combinations(n)
        .find(|comb| comb.iter().sum::<usize>() == sum)
}

fn find_encryption_weakness(nums: &[usize], num: usize) -> Option<usize> {
    let num_pos = nums.iter().position(|&x| x == num).unwrap_or(nums.len());
    'outer: for i in 0..num_pos {
        let mut sum = nums[i];
        for j in i + 1..num_pos {
            sum += nums[j];
            if sum == num {
                return Some(nums[i..=j].iter().min().unwrap() + nums[i..=j].iter().max().unwrap());
            } else if sum > num {
                continue 'outer;
            }
        }
    }
    None
}

#[test]
fn test_first_invalid_num() {
    let nums = parse_input(
        "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576",
    );
    assert_eq!(Some(127), first_invalid_num(&nums, 5));
}

#[test]
fn test_find_encryption_weakness() {
    let nums = parse_input(
        "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576",
    );
    assert_eq!(Some(62), find_encryption_weakness(&nums, 127));
}
