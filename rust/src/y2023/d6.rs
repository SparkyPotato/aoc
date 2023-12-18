use std::fmt::Display;

pub fn p1(input: &str) -> impl Display {
    let mut lines = input.lines();
    let times = lines.next().unwrap().split(':').nth(1).unwrap().trim().split(' ').filter_map(|x| x.parse::<usize>().ok()).collect::<Vec<_>>();
    let distances = lines.next().unwrap().split(':').nth(1).unwrap().trim().split(' ').filter_map(|x| x.parse::<usize>().ok()).collect::<Vec<_>>();

    let mut prod = 1usize;
    for (time, distance) in times.into_iter().zip(distances) {
        let mut sum = 0;
        for i in 0..=time {
            if i * (time - i) > distance {
                sum += 1;
            }
        }
        prod *= sum;
    }

    prod
}

pub fn p2(input: &str) -> impl Display {
    let mut lines = input.lines();
    let l = lines.next().unwrap().split(':').nth(1).unwrap().bytes();
    let mut time = 0usize;
    for b in l {
        if b >= b'0' && b <= b'9' {
            time = time * 10 + (b - b'0') as usize;
        }
    }
    let l = lines.next().unwrap().split(':').nth(1).unwrap().bytes();
    let mut distance = 0usize;
    for b in l {
        if b >= b'0' && b <= b'9' {
            distance = distance * 10 + (b - b'0') as usize;
        }
    }

    let mut sum = 0usize;
    for i in 0..=time {
        if i * (time - i) > distance {
            sum += 1;
        }
    }

    sum
}
