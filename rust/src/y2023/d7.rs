use std::cmp::Ordering;
use std::fmt::Display;

pub mod p1 {
    use super::*;

    #[derive(Copy, Clone, PartialEq, Eq)]
    struct Hand {
        cards: [u8; 5],
        bid: usize,
    }

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
    enum Kind {
        High,
        One,
        Two,
        Three,
        Full,
        Four,
        Five
    }

    impl Hand {
        fn kind(&self) -> Kind {
            let mut counts = [0; 15];
            for &c in &self.cards {
                counts[c as usize] += 1;
            }
            counts.sort_unstable();
            counts.reverse();

            match counts {
                [1, 1, 1, 1, 1, ..] => Kind::High,
                [2, 1, 1, 1, ..] => Kind::One,
                [2, 2, 1, ..] => Kind::Two,
                [3, 1, 1, ..] => Kind::Three,
                [3, 2, ..] => Kind::Full,
                [4, 1, ..] => Kind::Four,
                [5, ..] => Kind::Five,
                x => unreachable!("{:?}", x),
            }
        }
    }

    impl PartialOrd for Hand {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(<Self as Ord>::cmp(self, other))
        }
    }

    impl Ord for Hand {
        fn cmp(&self, other: &Self) -> Ordering {
            let kind = self.kind();
            let other_kind = other.kind();

            if kind != other_kind {
                return kind.cmp(&other_kind);
            } else {
                for (c1, c2) in self.cards.into_iter().zip(other.cards) {
                    if c1 != c2 {
                        return c1.cmp(&c2);
                    }
                }
            }

            Ordering::Equal
        }
    }

    fn parse(input: &str) -> Vec<Hand> {
        input.lines().map(|x| {
            let mut s = x.split(' ');
            let mut cards = [0; 5];
            let mut l = s.next().unwrap().bytes();
            for x in &mut cards {
                *x = match l.next().unwrap() {
                    b'A' => 14,
                    b'K' => 13,
                    b'Q' => 12,
                    b'J' => 11,
                    b'T' => 10,
                    x => x - b'0',
                }
            }

            let bid = s.next().unwrap().parse().unwrap();
            Hand { cards, bid }
        }).collect()
    }

    pub fn p1(input: &str) -> impl Display {
        let mut hands = parse(input);
        hands.sort_unstable();

        let mut sum = 0usize;
        for (i, h) in hands.iter().enumerate() {
            sum += h.bid * (i + 1);
        }
        sum
    }
}

pub mod p2 {
    use super::*;

    #[derive(Copy, Clone, PartialEq, Eq)]
    struct Hand {
        cards: [u8; 5],
        bid: usize,
    }

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
    enum Kind {
        High,
        One,
        Two,
        Three,
        Full,
        Four,
        Five
    }

    impl Hand {
        fn kind(&self) -> Kind {
            let mut counts = [0; 15];
            for &c in &self.cards {
                if c == 1 {
                    for c in &mut counts {
                        *c += 1;
                    }
                } else {
                    counts[c as usize] += 1;
                }
            }
            counts.sort_unstable();
            counts.reverse();

            match counts {
                [1, 1, 1, 1, 1, .., 0] => Kind::High,
                [2, .., 1] => Kind::One,
                [2, 1, .., 0] => Kind::One,
                [2, 2, .., 0] => Kind::Two,
                [3, 1, ..] => Kind::Three,
                [3, 2, .., 1] => Kind::Three,
                [3, 3, .., 2] => Kind::Three,
                [3, 2, .., 0] => Kind::Full,
                [3, 3, .., 1] => Kind::Full,
                [4, ..] => Kind::Four,
                [5, ..] => Kind::Five,
                x => unreachable!("{:?}", x),
            }
        }
    }

    impl PartialOrd for Hand {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(<Self as Ord>::cmp(self, other))
        }
    }

    impl Ord for Hand {
        fn cmp(&self, other: &Self) -> Ordering {
            let kind = self.kind();
            let other_kind = other.kind();

            if kind != other_kind {
                return kind.cmp(&other_kind);
            } else {
                for (c1, c2) in self.cards.into_iter().zip(other.cards) {
                    if c1 != c2 {
                        return c1.cmp(&c2);
                    }
                }
            }

            Ordering::Equal
        }
    }

    fn parse(input: &str) -> Vec<Hand> {
        input.lines().map(|x| {
            let mut s = x.split(' ');
            let mut cards = [0; 5];
            let mut l = s.next().unwrap().bytes();
            for x in &mut cards {
                *x = match l.next().unwrap() {
                    b'A' => 14,
                    b'K' => 13,
                    b'Q' => 12,
                    b'T' => 11,
                    b'J' => 1,
                    x => (x - b'0') + 1,
                }
            }

            let bid = s.next().unwrap().parse().unwrap();
            Hand { cards, bid }
        }).collect()
    }

    pub fn p2(input: &str) -> impl Display {
        let mut hands = parse(input);
        hands.sort_unstable();

        let mut sum = 0usize;
        for (i, h) in hands.iter().enumerate() {
            sum += h.bid * (i + 1);
        }
        sum
    }
}

