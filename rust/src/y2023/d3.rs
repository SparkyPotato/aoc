use std::fmt::Display;

#[derive(Debug, Eq, PartialEq)]
enum Cell {
    Empty,
    Sym(u8),
    Num(usize, u8),
    PrevNum(u8),
}

fn parse(input: &str) -> (Vec<Cell>, usize) {
    let width = input.lines().next().unwrap().len();
    let mut cells = Vec::with_capacity(input.len());

    for l in input.lines() {
        let mut b = l.bytes();
        while b.len() > 0 {
            let mut num = 0;
            let mut len = 0;
            let mut after = None;
            while let Some(c) = b.next() {
                match c {
                    b'.' => {
                        after = Some(Cell::Empty);
                        break;
                    },
                    x if x >= b'0' && x <= b'9' => {
                        num = num * 10 + (c - b'0') as usize;
                        len += 1;
                    },
                    b'\n' => break,
                    x => {
                        after = Some(Cell::Sym(x));
                        break;
                    },
                }

            }

            if len > 0 {
                cells.push(Cell::Num(num, len));
                for i in 1..len {
                    cells.push(Cell::PrevNum(i));
                }
            }
            if let Some(c) = after {
                cells.push(c);
            }
        }
    }

    (cells, width)
}

pub fn p1(input: &str) -> impl Display {
    let (cells, width) = parse(input);
    let mut sum = 0;

    for y in 0..(cells.len() / width) {
        let mut x = 0;
        while x < width {
            if let Cell::Num(num, len) = cells[y * width + x] {
                let mut counted = false;
                for _ in 0..len {
                    for (dx, dy) in &[(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)] {
                        counted |= cells.get((y as isize + dy) as usize * width + (x as isize + dx) as usize).map(|x| matches!(x, Cell::Sym(_))).unwrap_or(false);
                    }
                    x += 1;
                }

                if counted {
                    sum += num;
                }
            } else {
                x += 1;
            }
        }
    }

    sum
}

pub fn p2(input: &str) -> impl Display {
    let (cells, width) = parse(input);
    let mut sum = 0;

    for y in 0..(cells.len() / width) {
        for x in 0..width {
            if let Cell::Sym(b'*') = cells[y * width + x] {
                let mut count = 0;
                let mut num1 = 0;
                let mut num2 = 0;

                for dy in -1..=1 {
                    let mut saw_empty = true;
                    for dx in -1..=1 {
                        let x = x as isize + dx;
                        let y = y as isize + dy;
                        match cells.get(y as usize * width + x as usize) {
                            Some(Cell::Num(num, _)) if saw_empty => {
                                if saw_empty {
                                    if num1 == 0 { num1 = *num }
                                    else { num2 = *num };
                                    count += 1;
                                    saw_empty = false;
                                }
                            },
                            Some(Cell::PrevNum(n)) => {
                                if saw_empty {
                                    let Cell::Num(num, _) = cells.get(y as usize * width + (x - *n as isize) as usize).unwrap() else { unreachable!() };

                                    if num1 == 0 { num1 = *num }
                                    else { num2 = *num }
                                    count += 1;
                                    saw_empty = false;
                                }
                            }
                            _ => saw_empty = true,
                        }
                    }
                }

                if count == 2 {
                    sum += num1 * num2;
                }
            }
        }
    }

    sum
}
