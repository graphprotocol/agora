pub struct Contest<T> {
    winners: Vec<(usize, T)>,
}

impl<T> Contest<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            winners: Vec::with_capacity(capacity),
        }
    }

    pub fn take(self) -> Vec<T> {
        self.winners.into_iter().map(|(_, item)| item).collect()
    }

    #[inline]
    pub fn insert_unique(&mut self, score: usize, value: T, cmp: impl Fn(&T, &T) -> bool) {
        self.insert_with_unique(score, move || value, cmp)
    }

    pub fn insert_with_unique(
        &mut self,
        score: usize,
        value: impl FnOnce() -> T,
        cmp: impl Fn(&T, &T) -> bool,
    ) {
        if self.winners.len() == self.winners.capacity()
            && self.winners[self.winners.len() - 1].0 > score
        {
            return;
        }

        let value = value();

        // Well, none of the below turned out to be very elegant.
        for (i, (other, winner)) in self.winners.iter().enumerate() {
            if cmp(winner, &value) {
                if *other < score {
                    self.winners.remove(i);
                    break;
                }
                return;
            }
        }
        for (i, (other, _)) in self.winners.iter().enumerate() {
            if score > *other {
                if self.winners.len() == self.winners.capacity() {
                    self.winners.pop();
                }
                self.winners.insert(i, (score, value));
                return;
            }
        }
        if self.winners.len() < self.winners.capacity() {
            self.winners.push((score, value));
        }
    }
}

impl<T: Clone> Contest<T> {
    pub fn cloned(&self) -> Vec<T> {
        self.winners.iter().map(|(_, item)| item.clone()).collect()
    }
}
