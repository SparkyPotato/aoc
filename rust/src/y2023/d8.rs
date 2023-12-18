use std::fmt::Display;

#[derive(Debug)]
struct Node<'a> {
    name: &'a str,
    left: &'a str,
    right: &'a str,
}

fn parse(input: &str) -> (Vec<bool>, Vec<Node>) {
    let mut lines = input.lines();
    let path = lines.next().unwrap().bytes().map(|x| x == b'R').collect::<Vec<_>>();
    lines.next().unwrap();

    (path, lines.map(|x| {
        let mut s = x.split(' ');
        let name = s.next().unwrap();
        let left = s.nth(1).unwrap();
        let left = &left[1..left.len() - 1];
        let right = s.next().unwrap();
        let right = &right[0..right.len() - 1];
        Node { name, left, right }
    }).collect::<Vec<_>>())
}

pub fn p1(input: &str) -> impl Display {
    let (path, nodes) = parse(input);

    let mut steps = 0usize;
    let mut node = nodes.iter().enumerate().find(|(_, x)| x.name == "AAA").unwrap().0;
    for &right in path.iter().cycle() {
        let next = if right { nodes[node].right } else { nodes[node].left };
        steps += 1;
        if next == "ZZZ" {
            break;
        }
        node = nodes.iter().enumerate().find(|(_, x)| x.name == next).unwrap().0;
    }

    steps
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

pub fn p2(input: &str) -> impl Display {
    let (path, nodes) = parse(input);

    let curr = nodes.iter().filter_map(|x| (x.name.bytes().nth(2).unwrap() == b'A').then_some(x)).collect::<Vec<_>>();
    let steps: Vec<_> = curr.into_iter().map(|mut node| {
        let mut steps = 0usize;
        for &right in path.iter().cycle() {
            steps += 1;
            let next = if right { node.right } else { node.left };
            if next.bytes().nth(2).unwrap() == b'Z' {
                break;
            }
            node = nodes.iter().find(|x| x.name == next).unwrap();
        }
        steps
    }).collect();

    steps.into_iter().fold(1, lcm)
}
