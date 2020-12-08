use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let instructions = parse_instructions(&input);
    println!("Part 1: {}", find_accumulator_before_loop(&instructions).1);
    println!(
        "Part 2: {}",
        find_accumulator_after_fix(&instructions).unwrap()
    );
}

#[derive(Debug, Copy, Clone)]
enum Operation {
    Acc,
    Jmp,
    Nop,
}

#[derive(Debug, Copy, Clone)]
struct Instruction(Operation, isize);

fn parse_instructions(input: &str) -> Vec<Instruction> {
    input.lines().map(parse_line).collect()
}

fn parse_line(line: &str) -> Instruction {
    let (operation, argument) = line.split_at(
        line.chars()
            .position(|c| c == ' ')
            .expect("space not found in line"),
    );
    let argument = argument
        .trim()
        .parse::<isize>()
        .expect("error parsing argument as integer");
    match operation {
        "acc" => Instruction(Operation::Acc, argument),
        "jmp" => Instruction(Operation::Jmp, argument),
        "nop" => Instruction(Operation::Nop, argument),
        _ => panic!("error matching operation"),
    }
}

fn run_instruction(
    instructions: &[Instruction],
    index: usize,
    accumulator: isize,
) -> (usize, isize) {
    let instruction = instructions[index];
    match instruction {
        Instruction(Operation::Acc, arg) => (index + 1, accumulator + arg),
        Instruction(Operation::Jmp, arg) => ((index as isize + arg) as usize, accumulator),
        Instruction(Operation::Nop, _) => (index + 1, accumulator),
    }
}

enum State {
    Loop,
    Complete,
}

fn find_accumulator_before_loop(instructions: &[Instruction]) -> (State, isize) {
    let mut seen_indices = HashSet::new();
    let mut index = 0;
    let mut accumulator = 0;
    loop {
        seen_indices.insert(index);
        let (new_index, new_accumulator) = run_instruction(instructions, index, accumulator);
        if seen_indices.contains(&new_index) {
            return (State::Loop, accumulator);
        } else if new_index >= instructions.len() {
            return (State::Complete, new_accumulator);
        } else {
            index = new_index;
            accumulator = new_accumulator;
        }
    }
}

#[test]
fn test_find_accumulator_before_loop() {
    let input = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";
    let instructions = parse_instructions(input);
    assert_eq!(5, find_accumulator_before_loop(&instructions).1);
}

fn toggle_instruction(instruction: Instruction) -> Instruction {
    match instruction {
        Instruction(Operation::Jmp, arg) => Instruction(Operation::Nop, arg),
        Instruction(Operation::Nop, arg) => Instruction(Operation::Jmp, arg),
        ins => ins,
    }
}

fn find_accumulator_after_fix(instructions: &[Instruction]) -> Option<isize> {
    let mut instructions = instructions.iter().copied().collect::<Vec<_>>();
    let mut toggle_index = 0;
    while toggle_index < instructions.len() {
        while let Instruction(Operation::Acc, _) = instructions[toggle_index] {
            toggle_index += 1;
        }
        instructions[toggle_index] = toggle_instruction(instructions[toggle_index]);
        if let (State::Complete, accumulator) = find_accumulator_before_loop(&instructions) {
            return Some(accumulator);
        }
        instructions[toggle_index] = toggle_instruction(instructions[toggle_index]);
        toggle_index += 1;
    }
    None
}

#[test]
fn test_find_accumulator_after_fix() {
    let input = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";
    let instructions = parse_instructions(input);
    assert_eq!(8, find_accumulator_after_fix(&instructions).unwrap());
}
