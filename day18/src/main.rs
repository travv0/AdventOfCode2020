fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    println!("Part 1: {}", input.lines().map(eval).sum::<usize>());
    println!("Part 2: {}", input.lines().map(eval2).sum::<usize>());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Plus,
    Times,
}

fn find_matching_paren(s: &str, i: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, c) in s.chars().enumerate().skip(i + 1) {
        match c {
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return Some(i);
                } else {
                    depth -= 1;
                }
            }
            _ => {}
        }
    }
    None
}

fn calc(new_int: usize, last_int: Option<usize>, last_op: Option<Op>) -> Option<usize> {
    if let Some(li) = last_int {
        match last_op {
            Some(Op::Times) => Some(li * new_int),
            Some(Op::Plus) => Some(li + new_int),
            None => panic!("bad parse"),
        }
    } else {
        Some(new_int)
    }
}

fn eval(s: &str) -> usize {
    let mut i = 0;
    let mut last_int = None;
    let mut last_op = None;
    while i < s.len() {
        match &s[i..=i] {
            " " => {}
            "(" => {
                let close_paren = find_matching_paren(s, i).expect("bad parse");
                let new_int = eval(&s[i + 1..close_paren]);
                last_int = calc(new_int, last_int, last_op);
                i = close_paren;
            }
            ")" => break,
            "*" => last_op = Some(Op::Times),
            "+" => last_op = Some(Op::Plus),
            int => {
                let new_int = int.parse::<usize>().expect("bad parse");
                last_int = calc(new_int, last_int, last_op);
            }
        }
        i += 1;
    }
    last_int.expect("bad parse")
}

#[test]
fn test_eval() {
    assert_eq!(71, eval("1 + 2 * 3 + 4 * 5 + 6"));
    assert_eq!(51, eval("1 + (2 * 3) + (4 * (5 + 6))"));
    assert_eq!(26, eval("2 * 3 + (4 * 5)"));
    assert_eq!(437, eval("5 + (8 * 3 + 9 + 3 * 4 * 3)"));
    assert_eq!(12240, eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"));
    assert_eq!(
        13632,
        eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    );
}

fn eval2(s: &str) -> usize {
    let mut ns = Vec::new();
    let mut ops = Vec::new();
    let mut i = 0;
    while i < s.len() {
        match &s[i..=i] {
            " " => {}
            "(" => {
                let close_paren = find_matching_paren(s, i).expect("bad parse");
                ns.push(eval2(&s[i + 1..close_paren]));
                i = close_paren;
            }
            ")" => break,
            "*" => {
                while let Some(Op::Plus) = ops.last() {
                    ops.pop();
                    let (n, m) = (ns.pop().expect("bad parse"), ns.pop().expect("bad parse"));
                    ns.push(n + m);
                }
                ops.push(Op::Times)
            }
            "+" => ops.push(Op::Plus),
            int => {
                let new_int = int.parse::<usize>().expect("bad parse");
                ns.push(new_int);
            }
        }
        i += 1;
    }
    while let Some(op) = ops.pop() {
        let (n, m) = (ns.pop().expect("bad parse"), ns.pop().expect("bad parse"));
        match op {
            Op::Plus => ns.push(n + m),
            Op::Times => ns.push(n * m),
        }
    }
    *ns.first().expect("bad parse")
}

#[test]
fn test_eval2() {
    assert_eq!(231, eval2("1 + 2 * 3 + 4 * 5 + 6"));
    assert_eq!(51, eval2("1 + (2 * 3) + (4 * (5 + 6))"));
    assert_eq!(46, eval2("2 * 3 + (4 * 5)"));
    assert_eq!(1445, eval2("5 + (8 * 3 + 9 + 3 * 4 * 3)"));
    assert_eq!(669060, eval2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"));
    assert_eq!(
        23340,
        eval2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    );
}
