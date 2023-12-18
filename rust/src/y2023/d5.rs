use std::fmt::Display;

pub fn p1(input: &str) -> impl Display {
    let mut lines = input.lines();
    let mut seeds = lines.next().unwrap().split(':').nth(1).unwrap().trim().split(' ').map(|x| x.parse::<isize>().unwrap()).collect::<Vec<_>>();
    let mut mapped = (0..seeds.len()).map(|_| false).collect::<Vec<_>>();

    for line in lines {
        if line.is_empty() || line.chars().next().unwrap().is_ascii_alphabetic() {
            for x in mapped.iter_mut() { *x = false; }
            continue;
        }

        let mut nums = line.split(' ').map(|x| x.parse::<isize>().unwrap());
        let dst_start = nums.next().unwrap();
        let src_start = nums.next().unwrap();
        let len = nums.next().unwrap();

        let src_range = src_start..(src_start + len);
        for (i, seed) in seeds.iter_mut().enumerate() {
            if src_range.contains(seed) && !mapped[i] {
                *seed += dst_start - src_start;
                mapped[i] = true;
            }
        }
    }

    seeds.into_iter().min().unwrap()
}

pub fn p2(input: &str) -> impl Display {
    let mut lines = input.lines();
    let mut line = lines.next().unwrap().split(':').nth(1).unwrap().trim().split(' ').map(|x| x.parse::<isize>().unwrap());
    let mut seeds = Vec::new();
    while let Some(start) = line.next() {
        let len = line.next().unwrap();
        seeds.extend(start..(start + len));
    }
    let mut mapped = (0..seeds.len()).map(|_| false).collect::<Vec<_>>();

    for line in lines {
        if line.is_empty() || line.chars().next().unwrap().is_ascii_alphabetic() {
            for x in mapped.iter_mut() { *x = false; }
            continue;
        }

        let mut nums = line.split(' ').map(|x| x.parse::<isize>().unwrap());
        let dst_start = nums.next().unwrap();
        let src_start = nums.next().unwrap();
        let len = nums.next().unwrap();

        let src_range = src_start..(src_start + len);
        for (i, seed) in seeds.iter_mut().enumerate() {
            if src_range.contains(seed) && !mapped[i] {
                *seed += dst_start - src_start;
                mapped[i] = true;
            }
        }
    }

    seeds.into_iter().min().unwrap()
}
