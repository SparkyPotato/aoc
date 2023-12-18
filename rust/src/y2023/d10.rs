use std::collections::VecDeque;
use std::fmt::Display;
use std::hint::unreachable_unchecked;

#[derive(Copy, Clone, Eq, PartialEq)]
enum Dir {
    N,
    S,
    E,
    W,
}

struct Pipe {
    pos: [Dir; 2],
}

enum Cell {
    Pipe(Pipe),
    Empty,
}

struct Grid {
    cells: Vec<Cell>,
    start: usize,
    width: usize,
    height: usize,
}

unsafe fn parse(input: &str) -> Grid {
    let mut cells = Vec::new();
    let mut start = 0;
    let mut width = 0;
    let mut height = 0;

    for line in input.lines() {
        height += 1;
        width = 0;
        for c in line.bytes() {
            width += 1;
            let cell = match c {
                b'|' => Cell::Pipe(Pipe { pos: [Dir::N, Dir::S] }),
                b'-' => Cell::Pipe(Pipe { pos: [Dir::E, Dir::W] }),
                b'L' => Cell::Pipe(Pipe { pos: [Dir::N, Dir::E] }),
                b'J' => Cell::Pipe(Pipe { pos: [Dir::N, Dir::W] }),
                b'7' => Cell::Pipe(Pipe { pos: [Dir::S, Dir::W] }),
                b'F' => Cell::Pipe(Pipe { pos: [Dir::S, Dir::E] }),
                b'.' => Cell::Empty,
                b'S' => {
                    start = cells.len();
                    Cell::Empty
                }
                x => unreachable!("{}", x),
            };
            cells.push(cell);
        }
    }

    let mut f = None;
    let mut s = None;
    for (x, y) in [(0, 1), (1, 0), (0, -1), (-1, 0)] {
        if matches!(cells[(start as isize + y * width as isize + x) as usize], Cell::Pipe(_)) {
            if f.is_none() {
                f = Some((x, y));
            } else {
                s = Some((x, y));
            }
        }
    }

    let (fx, fy) = f.unwrap_unchecked();
    let (sx, sy) = s.unwrap_unchecked();
    let fd = match (fx, fy) {
        (0, 1) => Dir::N,
        (1, 0) => Dir::E,
        (0, -1) => Dir::S,
        (-1, 0) => Dir::W,
        _ => unreachable_unchecked(),
    };
    let sd = match (sx, sy) {
        (0, 1) => Dir::N,
        (1, 0) => Dir::E,
        (0, -1) => Dir::S,
        (-1, 0) => Dir::W,
        _ => unreachable_unchecked(),
    };
    cells[start] = Cell::Pipe(Pipe { pos: [fd, sd] });

    Grid {
        cells,
        start,
        width,
        height,
    }
}

pub unsafe fn p1(input: &str) -> impl Display {
    let grid = parse(input);
    let mut curr = grid.start;
    let mut dir = Dir::S;
    let mut steps = 0usize;
    loop {
        let cell = &grid.cells[curr];
        steps += 1;
        match cell {
            Cell::Pipe(pipe) => {
                let d = pipe.pos.iter().filter(|&x| *x != dir).next().unwrap_unchecked();
                match d {
                    Dir::N => {
                        curr -= grid.width;
                        dir = Dir::S;
                    },
                    Dir::S => {
                        curr += grid.width;
                        dir = Dir::N;
                    },
                    Dir::E => {
                        curr += 1;
                        dir = Dir::W;
                    },
                    Dir::W => {
                        curr -= 1;
                        dir = Dir::E;
                    },
                }
            }
            Cell::Empty => unreachable_unchecked(),
        }

        if curr == grid.start {
            break (steps + 1) / 2;
        }
    }
}

pub unsafe fn p2(input: &str) -> impl Display {
    let grid = parse(input);
    let mut curr = grid.start;
    let mut dir = Dir::S;
    let mut fill = vec![0usize; grid.cells.len()];
    let mut min = grid.cells.len();
    let mut max = 0usize;

    let mut f = 1;
    loop {
        let cell = &grid.cells[curr];
        match cell {
            Cell::Pipe(pipe) => {
                let d = pipe.pos.iter().filter(|&x| *x != dir).next().unwrap();
                match d {
                    Dir::N => {
                        curr -= grid.width;
                        dir = Dir::S;
                    },
                    Dir::S => {
                        curr += grid.width;
                        dir = Dir::N;
                    },
                    Dir::E => {
                        curr += 1;
                        dir = Dir::W;
                    },
                    Dir::W => {
                        curr -= 1;
                        dir = Dir::E;
                    },
                }
            }
            Cell::Empty => unreachable!(),
        }

        fill[curr] = f;
        f += 1;
        min = min.min(curr);
        max = max.max(curr);

        if curr == grid.start {
            break;
        }
    }

    let mut sum = 0usize;
    for i in min..=max {
        let val = fill[i];
        if val == 0 {
            let x = i % grid.width;
            let mut count = 0;
            for p in (i + 1)..(i + grid.width - x) {
                if fill[p] != 0 {
                    count += 1;
                }
            }
            if count % 2 == 1 {
                sum += 1;
            }
        }
    }
    sum
}

