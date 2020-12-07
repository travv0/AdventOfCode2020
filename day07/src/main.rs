#![warn(clippy::pedantic)]

use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let reverse_rules = map_from_inner_to_outer(&input);
    println!(
        "Part 1: {}",
        get_containing_bags("shiny gold", &reverse_rules)
            .iter()
            .count()
    );
    let rules = map_from_outer_to_inner(&input);
    println!("Part 2: {}", count_contained_bags("shiny gold", &rules));
}

fn parse_rule(rule: &str) -> (&str, Vec<(usize, &str)>) {
    if let [outer, inner] = rule.split(" bags contain ").collect::<Vec<_>>()[..] {
        let mut vec = Vec::new();
        let inner_bags = inner.split(", ");
        for bag in inner_bags {
            let space_index = bag
                .chars()
                .position(|c| c == ' ')
                .expect("no space in line");
            if let Ok(count) = bag[..space_index].parse::<usize>() {
                let color = bag[space_index..]
                    .trim_end_matches('.')
                    .trim_end_matches('s')
                    .trim_end_matches("bag")
                    .trim();
                vec.push((count, color));
            }
        }
        (outer, vec)
    } else {
        panic!("invalid line: didn't contain expected text");
    }
}

#[test]
fn test_parse_rule() {
    assert_eq!(
        ("light red", vec![(1, "bright white"), (2, "muted yellow")]),
        parse_rule("light red bags contain 1 bright white bag, 2 muted yellow bags.")
    );
    assert_eq!(
        ("faded blue", vec![]),
        parse_rule("faded blue bags contain no other bags.")
    );
}

fn map_from_inner_to_outer(input: &str) -> HashMap<&str, HashSet<&str>> {
    let mut result: HashMap<&str, HashSet<&str>> = HashMap::new();
    for line in input.lines() {
        let (outer, inner) = parse_rule(line);
        for (_, bag) in inner {
            let bags = result.entry(bag).or_default();
            bags.insert(outer);
        }
    }
    result
}

fn get_containing_bags<'a>(
    your_bag: &'a str,
    rules: &'a HashMap<&'a str, HashSet<&'a str>>,
) -> HashSet<&'a str> {
    let mut result = HashSet::new();
    if let Some(bags) = rules.get(your_bag) {
        for &bag in bags {
            result.insert(bag);
            result = result
                .union(&get_containing_bags(&bag, &rules))
                .cloned()
                .collect();
        }
    }
    result
}

#[test]
fn test_get_containing_bags() {
    use std::iter::FromIterator;

    let rules = map_from_inner_to_outer(
        "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
",
    );
    assert_eq!(
        HashSet::from_iter(
            ["bright white", "muted yellow", "dark orange", "light red"]
                .iter()
                .cloned()
        ),
        get_containing_bags("shiny gold", &rules)
    )
}

fn map_from_outer_to_inner(input: &str) -> HashMap<&str, HashSet<(usize, &str)>> {
    let mut result: HashMap<&str, HashSet<(usize, &str)>> = HashMap::new();
    for line in input.lines() {
        let (outer, inner) = parse_rule(line);
        for bag in inner {
            let bags = result.entry(outer).or_default();
            bags.insert(bag);
        }
    }
    result
}

fn count_contained_bags(your_bag: &str, rules: &HashMap<&str, HashSet<(usize, &str)>>) -> usize {
    fn count_contained_bags(
        your_bag: &str,
        rules: &HashMap<&str, HashSet<(usize, &str)>>,
    ) -> usize {
        let mut count = 1;
        if let Some(bags) = rules.get(your_bag) {
            for (n, bag) in bags {
                count += n * count_contained_bags(&bag, &rules);
            }
        }
        count
    }
    count_contained_bags(your_bag, rules) - 1
}

#[test]
fn test_count_contained_bags() {
    let rules = map_from_outer_to_inner(
        "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
",
    );
    assert_eq!(126, count_contained_bags("shiny gold", &rules));
}
