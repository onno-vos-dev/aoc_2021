use rustler::{Env, Encoder, Term, NifResult, Error};

rustler::init!("util", [sort_ints, mean_nif, median_nif]);

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
