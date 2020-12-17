use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let (rules, my_ticket, nearby_tickets) = parse_input(&input);
    let (invalid_tickets, invalid_vals) = find_invalid_vals(&rules, &nearby_tickets);
    println!("Part 1: {}", invalid_vals.iter().sum::<usize>());
    println!(
        "Part 2: {}",
        find_departure_fields(&rules, invalid_tickets, &my_ticket, &nearby_tickets)
            .iter()
            .filter(|(_, v)| v.starts_with("departure"))
            .map(|(&k, _)| my_ticket[k])
            .product::<usize>()
    );
}

#[derive(Debug, Clone, Copy)]
struct Range {
    min: usize,
    max: usize,
}

type Rules<'a> = HashMap<&'a str, (Range, Range)>;

type Ticket = Vec<usize>;

fn parse_input(input: &str) -> (Rules, Ticket, Vec<Ticket>) {
    if let &[rules, my_ticket, nearby_tickets] = &input.split("\n\n").collect::<Vec<_>>()[..] {
        (
            parse_rules(rules),
            parse_ticket(my_ticket.lines().nth(1).expect("bad parse: my_ticket")),
            nearby_tickets.lines().skip(1).map(parse_ticket).collect(),
        )
    } else {
        panic!("bad parse");
    }
}

fn parse_rules(rules: &str) -> Rules {
    let mut result = HashMap::new();
    for rule in rules.lines() {
        let (field, rules) =
            rule.split_at(rule.chars().position(|c| c == ':').expect("no : in rule"));
        let range_strs = rules[2..].split(" or ").collect::<Vec<_>>();
        let ranges = {
            if let &[range1, range2] = &range_strs[..] {
                (parse_range(range1), parse_range(range2))
            } else {
                panic!("bad ranges parse");
            }
        };
        result.insert(field, ranges);
    }
    result
}

fn parse_range(range: &str) -> Range {
    if let &[min, max] = &range.split('-').collect::<Vec<_>>()[..] {
        Range {
            min: min.parse::<usize>().expect("bad parse: min"),
            max: max.parse::<usize>().expect("bad parse: max"),
        }
    } else {
        panic!("bad range parse");
    }
}

fn parse_ticket(ticket: &str) -> Ticket {
    ticket.split(',').filter_map(|i| i.parse().ok()).collect()
}

fn find_invalid_vals<'a>(
    rules: &Rules,
    tickets: &'a [Ticket],
) -> (HashSet<&'a Ticket>, Vec<usize>) {
    let mut invalid_tickets = HashSet::new();
    let mut result = Vec::new();
    for ticket in tickets {
        'field: for &field in ticket {
            for (range1, range2) in rules.values() {
                if (range1.min <= field && field <= range1.max)
                    || (range2.min <= field && field <= range2.max)
                {
                    continue 'field;
                }
            }
            invalid_tickets.insert(ticket);
            result.push(field);
        }
    }
    (invalid_tickets, result)
}

#[test]
fn test_find_invalid_vals() {
    let input = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";
    let (rules, _my_ticket, nearby_tickets) = parse_input(&input);
    assert_eq!(
        71,
        find_invalid_vals(&rules, &nearby_tickets)
            .1
            .iter()
            .sum::<usize>()
    );
}

fn find_possible_indices<'a, 'b>(
    rules: &'a Rules,
    invalid_tickets: HashSet<&'b Ticket>,
    my_ticket: &Ticket,
    tickets: &[Ticket],
) -> HashMap<&'a str, HashSet<usize>> {
    let mut possible_indices: HashMap<_, HashSet<usize>> = HashMap::new();

    let mut tickets = tickets.to_vec();
    tickets = tickets
        .iter()
        .cloned()
        .filter(|ticket| !invalid_tickets.contains(ticket))
        .collect();
    tickets.push(my_ticket.clone());

    for (&rule, &(range1, range2)) in rules {
        'field: for i in 0..my_ticket.len() {
            for ticket in &tickets {
                let field = ticket[i];
                if !((range1.min <= field && field <= range1.max)
                    || (range2.min <= field && field <= range2.max))
                {
                    continue 'field;
                }
            }
            let indices = possible_indices.entry(rule).or_default();
            indices.insert(i);
        }
    }
    possible_indices
}

fn find_departure_fields<'a, 'b>(
    rules: &'a Rules,
    invalid_tickets: HashSet<&'b Ticket>,
    my_ticket: &Ticket,
    tickets: &[Ticket],
) -> HashMap<usize, &'a str> {
    let mut result = HashMap::new();

    let possibles = find_possible_indices(rules, invalid_tickets, my_ticket, tickets);
    let mut possibles = possibles.iter().collect::<Vec<_>>();
    possibles.sort_by(|(_, v1), (_, v2)| v1.len().cmp(&v2.len()));

    for (&label, possible_indices) in possibles {
        let i = *possible_indices
            .difference(&result.keys().copied().collect())
            .next()
            .expect("no valid index found");
        result.insert(i, label);
    }
    result
}

#[test]
fn test_find_departure_fields() {
    let input = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";
    let (rules, my_ticket, nearby_tickets) = parse_input(&input);
    let (invalid_tickets, _) = find_invalid_vals(&rules, &nearby_tickets);
    let fields = find_departure_fields(&rules, invalid_tickets, &my_ticket, &nearby_tickets);
    assert_eq!("row", *fields.get(&0).unwrap());
    assert_eq!("class", *fields.get(&1).unwrap());
    assert_eq!("seat", *fields.get(&2).unwrap());
}
