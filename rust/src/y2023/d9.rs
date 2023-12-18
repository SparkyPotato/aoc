use std::fmt::Display;

pub fn p1(input: &str) -> impl Display {
    input.lines().map(|x| {
        let mut vals: Vec<_> = x.split(' ').map(|x| x.parse::<isize>().unwrap()).collect();
        let mut last = *vals.last().unwrap();
        loop {
            vals = vals.windows(2).map(|a| a[1] - a[0]).collect();
            let &l = vals.last().unwrap();
            last += l;
            if vals.iter().all(|&x| x == 0) {
                break last;
            }
        }
    }).sum::<isize>()
}

#[test]
fn aaaa() {
    assert_eq!(p2("10 13 16 21 30 45").to_string(), "5");
}

pub fn p2(input: &str) -> impl Display {
    input.lines().map(|x| {
        let mut vals: Vec<_> = x.split(' ').map(|x| x.parse::<isize>().unwrap()).collect();
        let mut first = *vals.first().unwrap();
        loop {
            vals = vals.windows(2).map(|a| a[0] - a[1]).collect();
            let &f = vals.first().unwrap();
            first += f;
            if vals.iter().all(|&x| x == 0) {
                break first;
            }
        }
    }).sum::<isize>()
}
