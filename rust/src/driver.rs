use std::collections::HashSet;
use std::fmt::Display;
use std::fs::read_to_string;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};
use ureq::{Error, get, post};

pub fn get_input(session: &str, year: u32, day: u32) -> Result<String, Error> {
    let mut input_path = PathBuf::from_iter(["inputs", &format!("{}", year)]);
    input_path.push(format!("{}.txt", day));

    if let Ok(x) = read_to_string(&input_path) { return Ok(x) }

    let url = format!("https://adventofcode.com/{}/day/{}/input", year, day);
    let cookies = format!("session={}", session);
    let resp = get(&url)
        .set("User-Agent", "rust/aoc_driver")
        .set("Cookie", &cookies)
        .call()?;

    let mut body = resp.into_string()?;
    if !body.ends_with('\n') {
        body.push('\n');
    }

    std::fs::write(&input_path, &body).unwrap();

    Ok(body)
}

fn post_answer(session: &str, answer: String, year: u32, day: u32, part: u32) -> Result<(), Error> {
    let mut cache_path = PathBuf::from_iter(["cache", &format!("{}", year)]);
    std::fs::create_dir_all(&cache_path).unwrap();
    cache_path.push(format!("{}-{}.json", day, part));

    let mut cache = match read_to_string(&cache_path) {
        Ok(x) => serde_json::from_str::<Cache>(&x).unwrap(),
        Err(_) => Cache::default(),
    };
    match cache.answer {
        Some(ref x) if *x == answer => return Ok(println!("{}: Correct", answer)),
        Some(_) => return Ok(println!("{}: Incorrect", answer)),
        None if cache.wrong.contains(&answer) => return Ok(println!("{}: Incorrect", answer)),
        None => {}
    }

    let url = format!("https://adventofcode.com/{}/day/{}/answer", year, day);
    let cookies = format!("session={}", session);
    let form_level = format!("{}", part);
    let form = [("level", form_level.as_str()), ("answer", &answer)];

    let resp = post(&url)
        .set("User-Agent", "rust/aoc_driver")
        .set("Cookie", &cookies)
        .send_form(&form)?;

    let body = resp.into_string().expect("response was not a string");
    let correct = body.contains("That's the right answer!")
        | body.contains("Did you already complete it?");

    if correct {
        println!("{}: Correct", answer);
        cache.answer = Some(answer);
        cache.wrong.clear();
    } else {
        println!("{}: Incorrect", answer);
        cache.wrong.insert(answer);
    }

    std::fs::write(&cache_path, serde_json::to_string(&cache).unwrap()).unwrap();

    Ok(())
}

#[derive(Serialize, Deserialize, Default)]
struct Cache {
    answer: Option<String>,
    wrong: HashSet<String>,
}

pub fn execute<R: Display>(year: u32, day: u32, part: u32, f: impl FnOnce(&str) -> R) {
    let session = read_to_string(".session.txt").unwrap();
    let input = get_input(&session, year, day).unwrap();
    let out = f(&input).to_string();
    post_answer(&session, out, year, day, part).unwrap()
}
