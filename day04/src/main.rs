#![warn(clippy::pedantic)]

use std::convert::TryFrom;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let passports = parse_passports(&input);
    println!("Part 1: {}", passports.len());
    println!(
        "Part 2: {}",
        passports.into_iter().filter(Passport::validate).count()
    );
}

fn parse_passports(input: &str) -> Vec<Passport> {
    input
        .split("\n\n")
        .filter_map(|passport_str| Passport::try_from(passport_str).ok())
        .collect()
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
struct Passport<'a> {
    birth_year: &'a str,
    issue_year: &'a str,
    expiration_year: &'a str,
    height: &'a str,
    hair_color: &'a str,
    eye_color: &'a str,
    passport_id: &'a str,
    country_id: Option<&'a str>,
}

impl<'a> TryFrom<&'a str> for Passport<'a> {
    type Error = String;

    fn try_from(passport_str: &'a str) -> Result<Passport<'a>, Self::Error> {
        let fields = passport_str.split_ascii_whitespace();
        let mut misc_errs = vec![];
        let mut birth_year = Err("byr not found");
        let mut issue_year = Err("iyr not found");
        let mut expiration_year = Err("eyr not found");
        let mut height = Err("hgt not found");
        let mut hair_color = Err("hcl not found");
        let mut eye_color = Err("ecl not found");
        let mut passport_id = Err("pid not found");
        let mut country_id = None;
        for pair in fields.map(|field| {
            field
                .find(':')
                .map(|i| field.split_at(i + 1))
                .ok_or(format!("invalid format: {}", field))
        }) {
            match pair {
                Ok((key, val)) => match &key[..key.len() - 1] {
                    "byr" => birth_year = Ok(val),
                    "iyr" => issue_year = Ok(val),
                    "eyr" => expiration_year = Ok(val),
                    "hgt" => height = Ok(val),
                    "hcl" => hair_color = Ok(val),
                    "ecl" => eye_color = Ok(val),
                    "pid" => passport_id = Ok(val),
                    "cid" => country_id = Some(val),
                    field => misc_errs.push(format!("unknown field: {}", field)),
                },
                Err(err) => misc_errs.push(err),
            }
        }
        match [
            birth_year,
            issue_year,
            expiration_year,
            height,
            hair_color,
            eye_color,
            passport_id,
        ] {
            [Ok(birth_year), Ok(issue_year), Ok(expiration_year), Ok(height), Ok(hair_color), Ok(eye_color), Ok(passport_id)]
                if misc_errs.is_empty() =>
            {
                Ok(Passport {
                    birth_year,
                    issue_year,
                    expiration_year,
                    height,
                    hair_color,
                    eye_color,
                    passport_id,
                    country_id,
                })
            }
            fields => {
                let mut errs = fields
                    .iter()
                    .filter_map(|field| field.err().map(ToString::to_string))
                    .collect::<Vec<_>>();
                errs.append(&mut misc_errs);
                Err(errs.join(", "))
            }
        }
    }
}

impl<'a> Passport<'a> {
    fn validate(&self) -> bool {
        validate_range(self.birth_year, 1920, 2002)
            && validate_range(self.issue_year, 2010, 2020)
            && validate_range(self.expiration_year, 2020, 2030)
            && validate_height(self.height)
            && validate_hair_color(self.hair_color)
            && validate_eye_color(self.eye_color)
            && validate_passport_id(self.passport_id)
    }
}

fn validate_range(s: &str, min: usize, max: usize) -> bool {
    if let Ok(i) = s.parse() {
        min <= i && i <= max
    } else {
        false
    }
}

fn validate_height(height: &str) -> bool {
    let (height, unit) = height.split_at(height.len().max(2) - 2);
    match unit {
        "cm" => validate_range(height, 150, 193),
        "in" => validate_range(height, 59, 76),
        _ => false,
    }
}

fn validate_hair_color(hair_color: &str) -> bool {
    let (pound, hex) = hair_color.split_at(1);
    pound == "#" && u32::from_str_radix(hex, 16).is_ok()
}

fn validate_eye_color(eye_color: &str) -> bool {
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&eye_color)
}

fn validate_passport_id(passport_id: &str) -> bool {
    passport_id.len() == 9 && passport_id.parse::<u32>().is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_passport() {
        assert_eq!(
            Passport::try_from(
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"
            ),
            Ok(Passport {
                birth_year: "1937",
                issue_year: "2017",
                expiration_year: "2020",
                height: "183cm",
                hair_color: "#fffffd",
                eye_color: "gry",
                passport_id: "860033327",
                country_id: Some("147"),
            })
        );
        assert!(Passport::try_from(
            "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929"
        )
        .is_err());
        assert_eq!(
            Passport::try_from(
                "hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm"
            ),
            Ok(Passport {
                birth_year: "1931",
                issue_year: "2013",
                expiration_year: "2024",
                height: "179cm",
                hair_color: "#ae17e1",
                eye_color: "brn",
                passport_id: "760753108",
                country_id: None,
            })
        );
        assert!(Passport::try_from(
            "hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
        )
        .is_err());
    }

    #[test]
    fn test_validate_passport() {
        assert!(!parse_passports(
            "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"
        )
        .iter()
        .any(Passport::validate));

        assert!(parse_passports(
            "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
        )
        .iter()
        .all(Passport::validate));
    }
}
