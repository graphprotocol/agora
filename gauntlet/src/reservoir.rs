use rand::Rng;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use std::sync::Mutex;

pub struct Reservoir<T> {
    data: Mutex<Vec<T>>,
    count: AtomicUsize,
    capacity: usize,
}

impl<T> Reservoir<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Mutex::new(Vec::with_capacity(capacity)),
            count: AtomicUsize::new(0),
            capacity,
        }
    }

    pub fn take(self) -> Vec<T> {
        self.data.into_inner().unwrap()
    }

    pub fn insert<RNG: Rng>(&self, value: T, rng: &mut RNG) {
        self.insert_with(move || value, rng)
    }
}

impl<T: Clone> Reservoir<T> {
    pub fn cloned(&self) -> Vec<T> {
        let lock = self.data.lock().unwrap();
        // This could panic...
        lock.clone()
    }
}

impl<T> Reservoir<T> {
    pub fn insert_with<RNG: Rng>(&self, value: impl FnOnce() -> T, rng: &mut RNG) {
        let count = self.count.fetch_add(1, SeqCst) + 1;
        if count < self.capacity {
            // Never call closure in lock to prevent poisoning
            let value = value();

            let mut lock = self.data.lock().unwrap();
            // TODO: Race conditions here probably bias the sampling
            // Not a huge problem for this use-case
            if lock.len() < self.capacity {
                lock.push(value);
            } else {
                let index = rng.gen_range(0, self.capacity);
                lock[index] = value;
            }
        } else {
            let index = rng.gen_range(0, count);
            if index < self.capacity {
                let value = value();
                let mut lock = self.data.lock().unwrap();
                if lock.len() < self.capacity {
                    lock.push(value);
                } else {
                    lock[index] = value;
                }
            }
        }
    }
}
