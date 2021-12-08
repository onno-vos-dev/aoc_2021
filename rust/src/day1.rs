use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::time::{Instant};

fn main() {
  let start = Instant::now();
  let file = File::open("../../aoc_2021/apps/aoc/priv/inputs/day1.txt").expect("file wasn't found.");
  let reader = BufReader::new(file);
  let numbers: Vec<i64> = reader
      .lines()
      .map(|line| line.unwrap().parse::<i64>().unwrap())
      .collect();
  let mut part1 = 0;
  for (a, b) in numbers.iter().zip(numbers.iter().skip(1)) {
    if b > a {
      part1 += 1;
    }
  }
  let mut part2 = 0;
  for (((a, b), c), d) in numbers.iter().zip(numbers.iter().skip(1)).zip(numbers.iter().skip(2)).zip(numbers.iter().skip(3)) {
    if a + b + c < b + c + d {
      part2 += 1;
    }
  }
  let end = start.elapsed();
  println!("Took: {:?}", end);
  println!("Part1: {}", part1);
  println!("Part2: {}", part2);
}
