use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let joltage_ratings = input
        .lines()
        .filter_map(|i| i.parse::<usize>().ok())
        .collect::<Vec<_>>();
    let diffs = count_joltage_ratings_diffs(&joltage_ratings);
    println!("Part 1: {}", diffs[0] * diffs[2]);
    println!("Part 2: {}", count_arrangements(&joltage_ratings));
}

fn count_joltage_ratings_diffs(joltage_ratings: &[usize]) -> [usize; 3] {
    let mut joltage_ratings = joltage_ratings.to_owned();
    joltage_ratings.sort();
    joltage_ratings.insert(0, 0);
    joltage_ratings.push(joltage_ratings.last().unwrap() + 3);
    let mut counts = [0; 3];
    for (i, &rating) in joltage_ratings.iter().enumerate().skip(1) {
        counts[rating - joltage_ratings[i - 1] - 1] += 1;
    }
    counts
}

#[test]
fn test_count_joltage_ratings_diffs() {
    let input = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4";
    let joltage_ratings = input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let diffs = count_joltage_ratings_diffs(&joltage_ratings);
    assert_eq!(7, diffs[0]);
    assert_eq!(5, diffs[2]);

    let input = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3";
    let joltage_ratings = input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let diffs = count_joltage_ratings_diffs(&joltage_ratings);
    assert_eq!(22, diffs[0]);
    assert_eq!(10, diffs[2]);
}

fn count_arrangements(joltage_ratings: &[usize]) -> usize {
    fn count_arrangements(
        joltage_ratings: &HashSet<usize>,
        i: usize,
        end: usize,
        mut cache: &mut HashMap<usize, usize>,
    ) -> usize {
        if i == end {
            1
        } else if let Some(&result) = cache.get(&i) {
            result
        } else if let None = joltage_ratings.get(&i) {
            0
        } else {
            let result = count_arrangements(&joltage_ratings, i + 1, end, &mut cache)
                + count_arrangements(&joltage_ratings, i + 2, end, &mut cache)
                + count_arrangements(&joltage_ratings, i + 3, end, &mut cache);
            cache.insert(i, result);
            result
        }
    }

    let mut joltage_ratings = joltage_ratings.to_owned();
    joltage_ratings.sort();
    joltage_ratings.insert(0, 0);
    joltage_ratings.push(joltage_ratings.last().unwrap() + 3);
    count_arrangements(
        &HashSet::from_iter(joltage_ratings.iter().copied()),
        joltage_ratings[0],
        joltage_ratings[joltage_ratings.len() - 1],
        &mut HashMap::new(),
    )
}

#[test]
fn test_count_arrangements() {
    let input = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4";
    let joltage_ratings = input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(8, count_arrangements(&joltage_ratings));

    let input = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3";
    let joltage_ratings = input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(19208, count_arrangements(&joltage_ratings));
}
