use std::fmt::Display;
use std::hint::unreachable_unchecked;

pub fn p1(input: &str) -> impl Display {
    input.lines().enumerate().map(|(i, line)| {
        (i, line.split_once(':').unwrap().1.split(';').map(|p| {
            let mut red = 0;
            let mut green = 0;
            let mut blue = 0;
            for s in p.split(',') {
                let mut b = s.trim().bytes();
                let p = b.by_ref().take_while(|&b| b >= b'0' && b <= b'9').fold(0, |a, b| a * 10 + (b - b'0') as u64);
                match b.next().unwrap() {
                    b'r' => red = p,
                    b'g' => green = p,
                    b'b' => blue = p,
                    x => panic!("Found: {}", x),
                }
            }
            (red, green, blue)
        }).fold((0, 0, 0), |(r, g, b), (r1, g1, b1)| (r.max(r1), g.max(g1), b.max(b1))))
    }).filter(|&(_, (r, g, b))| r <= 12 && g <= 13 && b <= 14).map(|(i, _)| i as u64 + 1).sum::<u64>()
}

pub unsafe fn p1_opt(input: &str) -> impl Display {
    let mut sum = 0;
    let mut game = 1;
    let mut bytes = input.bytes();
    while bytes.len() > 0 {
        let mut red = 0;
        let mut green = 0;
        let mut blue = 0;
        while bytes.next().unwrap_unchecked() != b':' {}
        bytes.next().unwrap_unchecked();
        while bytes.len() > 0 {
            let p;
            let b = bytes.next().unwrap_unchecked();
            let b2 = bytes.next().unwrap_unchecked();
            if b2 != b' ' {
                bytes.next().unwrap_unchecked();
                p = (b - b'0') * 10 + (b2 - b'0');
            } else {
                p = b - b'0';
            }
            match bytes.next().unwrap_unchecked() {
                b'r' => red = red.max(p),
                b'g' => green = green.max(p),
                b'b' => blue = blue.max(p),
                _ => unreachable_unchecked(),
            }

            let mut b = bytes.next().unwrap_unchecked();
            while (b != b';') & (b != b',') & (b != b'\n') {
                b = bytes.next().unwrap_unchecked();
            }
            if b == b'\n' {
                break;
            }
            bytes.next().unwrap_unchecked();
        }
        if red <= 12 && green <= 13 && blue <= 14 {
            sum += game;
        }
        game += 1;
    }
    sum
}

pub fn p2(input: &str) -> impl Display {
    input.lines().map(|line| {
        line.split_once(':').unwrap().1.split(';').map(|p| {
            let mut red = 0;
            let mut green = 0;
            let mut blue = 0;
            for s in p.split(',') {
                let mut b = s.trim().bytes();
                let p = b.by_ref().take_while(|&b| b >= b'0' && b <= b'9').fold(0, |a, b| a * 10 + (b - b'0') as u64);
                match b.next().unwrap() {
                    b'r' => red = p,
                    b'g' => green = p,
                    b'b' => blue = p,
                    x => panic!("Found: {}", x),
                }
            }
            (red, green, blue)
        }).fold((0, 0, 0), |(r, g, b), (r1, g1, b1)| (r.max(r1), g.max(g1), b.max(b1)))
    }).map(|(r, g, b)| r * g * b).sum::<u64>()
}

pub unsafe fn p2_opt(input: &str) -> impl Display {
    let mut sum = 0;
    let mut bytes = input.bytes();
    while bytes.len() > 0 {
        let mut red = 0;
        let mut green = 0;
        let mut blue = 0;
        while bytes.next().unwrap_unchecked() != b':' {}
        bytes.next().unwrap_unchecked();
        while bytes.len() > 0 {
            let mut p = 0;
            let mut b = bytes.next().unwrap_unchecked();
            while b >= b'0' && b <= b'9' {
                p = p * 10 + (b - b'0') as u64;
                b = bytes.next().unwrap_unchecked();
            }
            match bytes.next().unwrap_unchecked() {
                b'r' => red = red.max(p),
                b'g' => green = green.max(p),
                b'b' => blue = blue.max(p),
                _ => unreachable_unchecked(),
            }

            let mut b = bytes.next().unwrap_unchecked();
            while b != b';' && b != b',' && b != b'\n' {
                b = bytes.next().unwrap_unchecked();
            }
            if b == b'\n' {
                break;
            }
            bytes.next().unwrap_unchecked();
        }
        sum += red * green * blue;
    }
    sum
}

