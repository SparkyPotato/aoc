use std::env::args;
use std::fs::read_to_string;
use std::hint::black_box;
use std::time::{Duration, Instant};

pub mod y2023;
mod driver;

const YEAR: u32 = 2023;
const DAY: u32 = 10;
const PART: u32 = 2;

macro_rules! f {
    ($x:expr) => { (|x| unsafe { y2023::d10::p2(x) })($x) };
}

fn sample(input: &str) -> Duration {
	let start = Instant::now();
	for _ in 0..100 {
		black_box(f!(black_box(input)));
	}
	start.elapsed()
}

fn bench(input: &str) {
	println!("Warming up for 2 secs");
	let start = Instant::now();
	loop {
		black_box(sample(input));

		if start.elapsed().as_secs_f64() >= 2.0 {
			break;
		}
	}

	println!("Benching for 3 secs");
	let mut samples = Vec::with_capacity(1000);
	let start = Instant::now();
	loop {
		samples.push(sample(input));

		if start.elapsed().as_secs_f64() >= 3.0 {
			break;
		}
	}

	println!();

	samples.sort_unstable();
	let mean = samples.iter().sum::<Duration>().as_secs_f64() / samples.len() as f64;
	println!("Mean: {:.2}us", mean * 1000.0);
	let median = samples[samples.len() / 2].as_secs_f64() * 0.5 + samples[samples.len() / 2 - 1].as_secs_f64() * 0.5;
	println!("Median: {:.2}us", median * 1000.0);
	let std_dev = (samples.iter().map(|x| (x.as_secs_f64() - mean).powi(2)).sum::<f64>() / samples.len() as f64).sqrt();
	println!("Std Dev: {:.2}us", std_dev * 1000.0);
}

fn main() {
	println!("{}-{}-{}", YEAR, DAY, PART);
	println!();

	if args().nth(1).as_deref() == Some("-b") {
		let session = read_to_string(".session.txt").unwrap();
		bench(&driver::get_input(&session, YEAR, DAY).unwrap());
	} else {
		driver::execute(YEAR, DAY, PART, |x| f!(x));
	}
}
