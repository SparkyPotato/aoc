use std::fmt::Display;

pub fn parse(input: &str) -> impl Iterator<Item = usize> + '_ {
    input.lines().map(|x| x.split(':').nth(1).unwrap()).map(|x| {
        let mut x = x.trim().split('|');
        let mut a = x.next().unwrap().trim().split(' ').filter_map(|x| x.trim().parse::<usize>().ok());
        let mut b = x.next().unwrap().trim().split(' ').filter_map(|x| x.trim().parse::<usize>().ok());
        let mut w = [0; 10];
        for i in 0..10 {
            w[i] = a.next().unwrap();
        }

        let mut c = [0; 25];
        for i in 0..25 {
            c[i] = b.next().unwrap();
        }

        let mut count = 0;
        for w in w {
            for c in c {
                if w == c {
                    count += 1;
                }
            }
        }

        count
    })
}

unsafe fn parse_opt(input: &str) -> [u8; 223] {
    let mut out = [0; 223];
    let mut bytes = input.bytes();

    let mut i = 0;
    while bytes.len() > 0 {
        let mut b = bytes.next().unwrap_unchecked();
        while b != b':' { b = bytes.next().unwrap_unchecked(); }

        let mut w = [0; 10];
        for i in 0..10 {
            while b < b'0' || b > b'9' { b = bytes.next().unwrap_unchecked(); }
            while b >= b'0' && b <= b'9' {
                w[i] = w[i] * 10 + b - b'0';
                b = bytes.next().unwrap_unchecked();
            }
        }

        let mut c = [0; 25];
        for i in 0..25 {
            while b < b'0' || b > b'9' { b = bytes.next().unwrap_unchecked(); }
            while b >= b'0' && b <= b'9' {
                c[i] = c[i] * 10 + b - b'0';
                b = bytes.next().unwrap_unchecked();
            }
        }

        while b != b'\n' && bytes.len() > 0 { b = bytes.next().unwrap_unchecked(); }

        let mut count = 0;
        for w in w {
            for c in c {
                if w == c {
                    count += 1;
                }
            }
        }
        out[i] = count;
        i += 1;
    }

    out
}

pub unsafe fn p1(input: &str) -> impl Display {
    let mut score = 0;

    for count in parse_opt(input) {
        if count > 0 {
            score += 2usize.pow(count as u32 - 1);
        }
    }

    score
}

pub unsafe fn p2(input: &str) -> impl Display {
    let mut copies = [1; 223];

    for (i, count) in parse_opt(input).into_iter().enumerate() {
        let curr = copies[i];
        for j in 0..count {
            copies[i + j as usize + 1] += curr;
        }
    }

    copies.iter().sum::<usize>()
}
