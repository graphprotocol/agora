use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use std::sync::Mutex;

// TODO: The fast-out parallel stuff doesn't appear to be necessary.
// The lock and atomics can be removed if that stays true.
pub struct Contest<T> {
    winners: Mutex<Vec<(usize, T)>>,
    worst_score: AtomicUsize,
    count: AtomicUsize,
    capacity: usize,
}

impl<T> Contest<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            winners: Mutex::new(Vec::with_capacity(capacity)),
            worst_score: AtomicUsize::new(0),
            capacity,
            count: AtomicUsize::new(0),
        }
    }

    pub fn take(self) -> Vec<T> {
        self.winners
            .into_inner()
            .unwrap()
            .into_iter()
            .map(|(_, item)| item)
            .collect()
    }

    pub fn insert_unique(&self, score: usize, value: T, cmp: impl Fn(&T, &T) -> bool) {
        self.insert_with_unique(score, move || value, cmp)
    }

    pub fn insert_with_unique(
        &self,
        score: usize,
        value: impl FnOnce() -> T,
        cmp: impl Fn(&T, &T) -> bool,
    ) {
        let worst_score = self.worst_score.load(SeqCst);
        let count = self.count.load(SeqCst);

        if worst_score > score && count == self.capacity {
            return;
        }

        // Always call callbacks outside of locks
        let value = value();

        // Well, none of the below turned out to be very elegant.
        let mut winners = self.winners.lock().unwrap();
        for (i, (other, winner)) in winners.iter().enumerate() {
            if cmp(winner, &value) {
                if *other < score {
                    winners.remove(i);
                    break;
                }
                return;
            }
        }
        for (i, (other, _)) in winners.iter().enumerate() {
            if score > *other {
                if winners.len() == self.capacity {
                    winners.pop();
                }
                winners.insert(i, (score, value));
                return;
            }
        }
        if winners.len() < self.capacity {
            winners.push((score, value));
        }

        self.worst_score.store(winners[winners.len() - 1].0, SeqCst);
        self.count.store(winners.len(), SeqCst);
    }
}

impl<T: Clone> Contest<T> {
    pub fn cloned(&self) -> Vec<T> {
        let clone = {
            let lock = self.winners.lock().unwrap();
            // This could panic...
            lock.clone()
        };
        clone.into_iter().map(|(_, item)| item).collect()
    }
}
