use std::io::{Read};
use std::fs::File;
use std::time::{Instant};

fn median(mut xs: Vec<i32>) -> i32 {
  xs.sort_by(|x,y| x.partial_cmp(y).unwrap() );
  let n = xs.len();
  if n % 2 == 0 {
      (xs[n/2] + xs[n/2 - 1]) / 2
  } else {
      xs[n/2]
  }
}

fn mean(arr: Vec<i32>) -> i32 {
  arr.iter().sum::<i32>() as i32 / arr.len() as i32
}

fn calc_fuel_part1(median: i32, input: Vec<i32>) -> i32 {
  input.iter()
    .fold(0, |acc: i32, i| {
      if median - i >= 0 {
        acc + (median - i)
      } else {
        acc + ((median - i) * -1)
      }
    })
}

fn calc_fuel_part2(mean: i32, input: Vec<i32>) -> i32 {
  input.iter()
    .fold(0, |acc: i32, i| {
      if mean - i >= 0 {
        acc + termial(mean - i)
      } else {
        acc + termial((mean - i) * -1)
      }
    })
}

fn termial(x: i32) -> i32 {
  x * (x + 1) / 2 as i32
}

fn main() {
  let start = Instant::now();
  let mut file = File::open("../../aoc_2021/apps/aoc/priv/inputs/day7.txt").expect("file wasn't found.");
  let mut s = String::new();
  file.read_to_string(&mut s);
  let numbers_str: Vec<&str> = s.split(",").collect();
  let numbers: Vec<i32> = numbers_str.iter().map(|s| s.parse::<i32>().unwrap()).collect();
  let input1: Vec<i32> = numbers.clone();
  let input2: Vec<i32> = numbers.clone();
  let l1: Vec<i32> = numbers.clone();
  let l2: Vec<i32> = numbers.clone();
  let median = median(l1);
  let mean = mean(l2);
  let part1 = calc_fuel_part1(median, input1);
  let part2 = calc_fuel_part2(mean, input2);
  let end = start.elapsed();
  println!("Took: {:?}", end);
  println!("Part1: {}", part1);
  println!("Part2: {}", part2)
}
