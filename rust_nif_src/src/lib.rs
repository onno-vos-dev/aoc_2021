use rustler::{Env, Encoder, Term, NifResult, Error};

rustler::init!("util", [sort_ints, mean_nif, median_nif, calculate_fuel]);

#[rustler::nif]
fn sort_ints<'a>(env: Env<'a>, term: Term) -> NifResult<Term<'a>> {
    if let Ok(mut i) = term.decode::<Vec<i32>>() {
      i.sort();
      Ok((i).encode(env))
    } else {
        Err(Error::BadArg)
    }
}

#[rustler::nif]
fn mean_nif<'a>(env: Env<'a>, term: Term) -> NifResult<Term<'a>> {
    if let Ok(numbers) = term.decode::<Vec<i32>>() {
      let mean = numbers.iter().sum::<i32>() as f32 / numbers.len() as f32;
      Ok((mean).encode(env))
    } else {
        Err(Error::BadArg)
    }
}

#[rustler::nif]
fn median_nif<'a>(env: Env<'a>, term: Term) -> NifResult<Term<'a>> {
    if let Ok(mut numbers) = term.decode::<Vec<i32>>() {
      numbers.sort();
      if (numbers.len() % 2)==0 {
        let ind_left = numbers.len()/2-1;
        let ind_right = numbers.len()/2 ;
        let median = (numbers[ind_left]+numbers[ind_right]) as f64 / 2.0;
        Ok((median).encode(env))
      } else {
        let median = numbers[(numbers.len()/2)] as f64;
        Ok((median).encode(env))
      }
    } else {
        Err(Error::BadArg)
    }
}

// Day7 as a NIF --------------------------------------------------------------
#[rustler::nif]
fn calculate_fuel<'a>(env: Env<'a>, term: Term) -> NifResult<Term<'a>> {
  if let Ok(list) = term.decode::<Vec<i32>>() {
    let input: Vec<i32> = list.clone();
    let l1: Vec<i32> = list.clone();
    let l2: Vec<i32> = list.clone();
    let median = median(l1);
    let mean = mean(l2);
    let (part1, part2) = calc_fuel(median, mean, input);
    Ok(((part1, part2)).encode(env))
  } else {
    Err(Error::BadArg)
  }
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

fn termial(x: i32) -> i32 {
  x * (x + 1) / 2 as i32
}
