use std::fmt::Display;

pub fn p1(input: &str) -> impl Display {
    input
        .lines()
        .map(|x| x.chars().filter_map(|c| (c >= '0' && c <= '9').then_some(c as u64 - 48)))
        .map(|mut d| {
            let s = d.next().unwrap();
            s * 10 + d.last().unwrap_or(s)
        })
        .sum::<u64>()
}

pub unsafe fn p1_opt(input: &str) -> impl Display {
    let mut sum: u64 = 0;
    let mut bys = input.bytes();
    while bys.len() > 0 {
        let mut b = bys.next().unwrap_unchecked();
        while b < b'0' || b > b'9' { b = bys.next().unwrap_unchecked(); }
        sum += (b - b'0') as u64 * 10;
        let mut last = b;
        b = bys.next().unwrap_unchecked();
        while b != b'\n' && bys.len() > 0 {
            if b >= b'0' && b <= b'9' { last = b }
            b = bys.next().unwrap_unchecked();
        }
        sum += (last - b'0') as u64;
    }
    sum
}

fn num(input: &str) -> Option<u64> {
    let c = input.bytes().next().unwrap();
    (c >= b'0' && c <= b'9').then_some((c - b'0') as u64).or_else(|| {
        match input.as_bytes() {
            &[b'o', b'n', b'e', ..] => Some(1),
            &[b't', b'w', b'o', ..] => Some(2),
            &[b't', b'h', b'r', b'e', b'e', ..] => Some(3),
            &[b'f', b'o', b'u', b'r', ..] => Some(4),
            &[b'f', b'i', b'v', b'e', ..] => Some(5),
            &[b's', b'i', b'x', ..] => Some(6),
            &[b's', b'e', b'v', b'e', b'n', ..] => Some(7),
            &[b'e', b'i', b'g', b'h', b't', ..] => Some(8),
            &[b'n', b'i', b'n', b'e', ..] => Some(9),
            _ => None
        }
    })
}

pub fn p2(input: &str) -> impl Display {
    input
        .lines()
        .map(|x| (0..x.len()).filter_map(|i| num(&x[i..])))
        .map(|mut d| {
            let s = d.next().unwrap();
            s * 10 + d.last().unwrap_or(s)
        })
        .sum::<u64>()
}

