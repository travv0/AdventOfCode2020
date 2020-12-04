use itertools::Itertools;

fn main() {
    let nums = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .filter_map(|s| s.parse().ok())
        .collect::<Vec<_>>();
    for part in &[1, 2] {
        println!(
            "Part {}: {}",
            part,
            find_nums_that_sum_to(part + 1, &nums, 2020)
                .unwrap_or_else(|| panic!("No result found for part {}", part))
                .iter()
                .product::<usize>()
        );
    }
}

fn find_nums_that_sum_to(n: usize, nums: &[usize], sum: usize) -> Option<Vec<usize>> {
    nums.iter()
        .copied()
        .combinations(n)
        .find(|comb| comb.iter().sum::<usize>() == sum)
}
