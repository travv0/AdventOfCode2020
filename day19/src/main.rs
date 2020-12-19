use std::collections::HashMap;

fn main() {
    // let input = std::fs::read_to_string("input.txt").unwrap();
    // let (rules, rule, messages) = parse_input(&input);
    // println!("Part 1: {}", count_valid_messages(&rules, &rule, &messages));
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
enum Rule {
    Char(char),
    SubRules(Vec<usize>),
}

fn parse_input(input: &str) -> (HashMap<usize, Vec<Rule>>, Vec<&str>) {
    if let &[rules, messages] = &input.split("\n\n").collect::<Vec<_>>()[..] {
        let rules = rules.lines().map(parse_rule).collect();
        let messages = messages.lines().collect();
        (rules, messages)
    } else {
        panic!("bad parse")
    }
}

fn parse_rule(rule: &str) -> (usize, Vec<Rule>) {
    let mut rules = Vec::new();
    let mut parts = rule.split_ascii_whitespace();
    let rule_num = parts
        .next()
        .and_then(|i| i[..i.len() - 1].parse().ok())
        .expect("bad parse");
    if parts.clone().count() == 1 {
        let s = parts.next().unwrap();
        rules.push(Rule::Char(s.chars().nth(1).expect("bad parse")));
    } else {
        let mut sub_rules = Vec::new();
        for s in parts {
            if s == "|" {
                rules.push(Rule::SubRules(sub_rules));
                sub_rules = Vec::new();
            } else {
                sub_rules.push(s.parse::<usize>().expect("bad parse"));
            }
        }
        rules.push(Rule::SubRules(sub_rules));
    };
    (rule_num, rules)
}

fn first_pass(rules: &HashMap<Rule, usize>, message: &str) -> Vec<usize> {
    message
        .chars()
        .map(|c| rules.get(&Rule::Char(c)).unwrap())
        .copied()
        .collect()
}

fn build_matches(rules: &HashMap<usize, Rule>) {}

fn matches(rules: &HashMap<Rule, usize>, rule: &[usize], message: &str) -> bool {
    let mut message = first_pass(rules, message);
    let mut i = 0;
    while i < message.len() {
        let mut changed = false;
        if rule.get(i) == message.get(i) {
            i += 1;
        } else {
            let mut j = 0;
            loop {
                if i + j < message.len() {
                    let first = dbg!(&message)[dbg!(i) + dbg!(j)];
                    if let Some(&replacement) = if message.len() > i + j + 1 {
                        rules.get(&Rule::SubRules(vec![first, message[i + j + 1]]))
                    } else {
                        None
                    } {
                        message[i + j] = replacement;
                        message.remove(i + j + 1);
                        changed = true;
                    } else if let Some(&replacement) = rules.get(&Rule::SubRules(vec![first])) {
                        message[i + j] = replacement;
                        changed = true;
                    }
                }
                j += 1;
                if j >= message.len() {
                    break;
                }
            }
            if !changed {
                break;
            }
        }
    }
    message == rule
}

#[test]
fn test_matches() {
    let input = r#"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"#;
    let (rules, _messages) = parse_input(&input);
    // assert!(matches(&rules, "ababbb"));
    // assert!(!matches(&rules, "bababa"));
}

fn count_valid_messages(rules: &HashMap<Rule, usize>, rule: &[usize], messages: &[&str]) -> usize {
    messages
        .iter()
        .filter(|message| matches(rules, rule, message))
        .count()
}

#[test]
fn test_count_valid_messages() {
    let input = r#"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"#;
    let (rules, messages) = parse_input(&input);
    // assert_eq!(2, count_valid_messages(&rules, &messages));
}
