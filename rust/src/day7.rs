use std::io::{Read};
use std::fs::File;
use std::time::{Instant};

fn median(mut xs: Vec<i32>, len: usize) -> i32 {
  xs.sort();
  if len % 2 == 0 {
      (xs[len/2] + xs[len/2 - 1]) / 2
  } else {
      xs[len/2]
  }
}

fn mean(arr: Vec<i32>, len: usize) -> i32 {
  arr.iter().sum::<i32>() as i32 / len as i32
}

fn calc_fuel(median: i32, mean: i32, input: Vec<i32>) -> (i32, i32) {
  let mut median_acc: i32 = 0;
  let mut mean_acc: i32 = 0;
  for i in input.iter() {
    if median - i >= 0 {
      median_acc += median - i
    } else {
      median_acc += (median - i) * -1
    }
    if mean - i >= 0 {
      mean_acc += termial(mean - i)
    } else {
      mean_acc += termial((mean - i) * -1)
    }
  }
  (median_acc, mean_acc)
}

fn termial(x: i32) -> i32 {
  x * (x + 1) / 2 as i32
}

fn day7() -> (u128, i32, i32) {
  let start = Instant::now();
  let mut file = File::open("../../aoc_2021/apps/aoc/priv/inputs/day7.txt").expect("file wasn't found.");
  let mut s = String::new();
  file.read_to_string(&mut s);
  let numbers_str: Vec<&str> = s.split(",").collect();
  let numbers: Vec<i32> = numbers_str.iter().map(|s| s.parse::<i32>().unwrap()).collect();
  let len: usize = numbers.len();
  let input: Vec<i32> = numbers.clone();
  let l1: Vec<i32> = numbers.clone();
  let l2: Vec<i32> = numbers.clone();
  let median = median(l1, len);
  let mean = mean(l2, len);
  let (part1, part2) = calc_fuel(median, mean, input);
  let end = start.elapsed().as_nanos();
  (end, part1, part2)
}

fn main() {
  let mut average: u128 = 0;
  for _ in 1..1000 {
    let (time, _part1, _part2) = day7();
    average += time
  }
  let (_time, part1, part2) = day7();
  println!("Average time after 1000 iterations: {} nanoseconds ({} us) ", average / 1000, average / 1000000);
  println!("Part1: {}", part1);
  println!("Part2: {}", part2)
}
