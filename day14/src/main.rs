use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let instructions = parse_input(&input);
    println!(
        "Part 1: {}",
        sum_values_in_memory(&instructions, Memory::update_part1)
    );
    println!(
        "Part 2: {}",
        sum_values_in_memory(&instructions, Memory::update_part2)
    );
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Bit {
    One,
    Zero,
    Floating,
}

#[derive(Debug, Clone, Copy)]
struct Mask {
    offset: usize,
    bit: Bit,
}

#[derive(Debug, Clone)]
enum Instruction {
    Mask(Vec<Mask>),
    Mem { addr: usize, val: usize },
}

const CAPACITY: usize = 36;

#[derive(Debug, Clone)]
struct Memory {
    mask: Vec<Mask>,
    addrs: HashMap<usize, usize>,
}

impl Memory {
    fn new() -> Self {
        Self {
            mask: Vec::with_capacity(CAPACITY),
            addrs: HashMap::new(),
        }
    }

    fn update_part1(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Mask(mask) => self.mask = mask.clone(),
            &Instruction::Mem { addr, val } => {
                self.addrs.insert(addr, self.apply_mask(val));
            }
        }
    }

    fn apply_mask(&self, val: usize) -> usize {
        let mut result = val;
        for &Mask { bit, offset } in &self.mask {
            match bit {
                Bit::One => result |= 1 << offset,
                Bit::Zero => result &= !(1 << offset),
                _ => {}
            }
        }
        result
    }

    fn update_part2(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Mask(mask) => self.mask = mask.clone(),
            &Instruction::Mem { addr, val } => {
                let addrs = self.apply_mask_with_floating_bits(addr);
                for addr in addrs {
                    self.addrs.insert(addr, val);
                }
            }
        }
    }

    fn apply_mask_with_floating_bits(&self, val: usize) -> Vec<usize> {
        let mut results = Vec::new();
        let floating_bits_count = self
            .mask
            .iter()
            .filter(|&&m| m.bit == Bit::Floating)
            .count();
        let mut bits = vec![0; floating_bits_count];
        bits.append(&mut vec![1; floating_bits_count]);
        let combos = ones_and_zeros(floating_bits_count);
        for combo in combos {
            let mut result = val;
            let mut floating_index = 0;
            for &Mask { bit, offset } in &self.mask {
                match bit {
                    Bit::One => result |= 1 << offset,
                    Bit::Zero => {}
                    Bit::Floating => {
                        if combo[floating_index] == 1 {
                            result |= 1 << offset;
                        } else {
                            result &= !(1 << offset);
                        }
                        floating_index += 1;
                    }
                }
            }
            results.push(result);
        }
        results
    }
}

fn ones_and_zeros(n: usize) -> Vec<Vec<u8>> {
    fn go(results: &mut Vec<Vec<u8>>, result: &[u8], n: usize) {
        if n <= 0 {
            results.push(result.to_vec());
        } else {
            let mut one = Vec::from(result);
            one.push(1);
            go(results, &one, n - 1);
            let mut zero = Vec::from(result);
            zero.push(0);
            go(results, &zero, n - 1);
        }
    }
    let mut results = Vec::new();
    go(&mut results, &[], n);
    results
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            if line.starts_with("mask") {
                let mask = &line[line.len() - CAPACITY..];
                let mut result = Vec::with_capacity(CAPACITY);
                for (i, c) in mask.chars().rev().enumerate() {
                    result.push(Mask {
                        bit: match c {
                            '1' => Bit::One,
                            '0' => Bit::Zero,
                            'X' => Bit::Floating,
                            _ => panic!("bad parse"),
                        },
                        offset: i,
                    });
                }
                Instruction::Mask(result)
            } else {
                let addr = line[4..line.chars().position(|c| c == ']').unwrap()]
                    .parse::<usize>()
                    .unwrap();
                let val = line[line.chars().position(|c| c == '=').unwrap() + 2..]
                    .parse::<usize>()
                    .unwrap();
                Instruction::Mem { addr, val }
            }
        })
        .collect()
}

fn sum_values_in_memory(
    instructions: &[Instruction],
    update: fn(&mut Memory, &Instruction),
) -> usize {
    let mut memory = Memory::new();
    for instruction in instructions {
        update(&mut memory, instruction);
    }
    memory.addrs.values().sum()
}

#[test]
fn test_sum_values_in_memory_part1() {
    let input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";
    let instructions = parse_input(&input);
    assert_eq!(
        165,
        sum_values_in_memory(&instructions, Memory::update_part1)
    );
}

#[test]
fn test_sum_values_in_memory_part2() {
    let input = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1";
    let instructions = parse_input(&input);
    assert_eq!(
        208,
        sum_values_in_memory(&instructions, Memory::update_part2)
    );
}
