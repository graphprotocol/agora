use crate::reservoir::Reservoir;
use crate::Query;
use cost_model::{CostError, CostModel};
use num_bigint::BigInt;
use rayon::prelude::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Default)]
pub struct CostManyResult {
    successes: usize,
    total_value: BigInt,
    total_err: BigInt,
    total_squared_err: BigInt,
    failures: HashMap<&'static str, FailureBucket>,
}

fn fail_name(err: CostError) -> &'static str {
    match err {
        CostError::FailedToParseQuery => "Failed to parse query",
        CostError::QueryNotCosted => "Query not costed",
        CostError::QueryNotSupported => "Query not supported",
        CostError::CostModelFail => "Cost model failure",
    }
}

impl fmt::Display for CostManyResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Successes: {}", self.successes)?;
        writeln!(f, "Total Value: {}", self.total_value)?;
        writeln!(f, "Total Err: {}", self.total_err)?;
        writeln!(f, "Mean Squared Err: {}", self.mean_squared_error())?;
        writeln!(f, "")?;
        writeln!(
            f,
            "Failures: {}",
            self.failures
                .iter()
                .map(|(_, bucket)| bucket.count)
                .sum::<usize>()
        )?;
        for (name, bucket) in self.failures.iter() {
            writeln!(f, "\t{:?} {}", name, bucket)?;
            for example in bucket.examples.cloned() {
                writeln!(f, "\t\t{}", &example.query)?;
            }
        }

        Ok(())
    }
}

pub struct FailureBucket {
    count: usize,
    total_value: BigInt,
    examples: Reservoir<Query>,
}

impl FailureBucket {
    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            total_value: BigInt::from(0),
            examples: Reservoir::new(capacity),
        }
    }
}

impl fmt::Display for FailureBucket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "count: {} total_value: {}", self.count, self.total_value)
    }
}

struct CostOne {
    expected: BigInt,
    actual: Result<BigInt, CostError>,
    query: Query,
}

impl FailureBucket {
    fn merge(&mut self, other: FailureBucket) {
        self.count += other.count;
        self.total_value += other.total_value;
        // TODO: This is not how to merge a reservoir without bias
        for example in other.examples.take() {
            self.examples.insert(example, &mut rand::thread_rng())
        }
    }
}

impl CostManyResult {
    fn add(&mut self, result: CostOne) {
        match result.actual {
            Ok(cost) => {
                let err = cost.clone() - result.expected;
                self.successes += 1;
                self.total_value += cost;
                self.total_squared_err += err.clone() * err.clone();
                self.total_err += err;
            }
            Err(e) => {
                let bucket = self.failure_bucket(fail_name(e));
                bucket.count += 1;
                bucket.total_value = result.expected;
                bucket
                    .examples
                    .insert(result.query, &mut rand::thread_rng());
            }
        }
    }

    pub fn merge(mut self, mut other: Self) -> Self {
        self.successes += other.successes;
        self.total_value += other.total_value;
        self.total_err += other.total_err;
        self.total_squared_err += other.total_squared_err;
        for (key, bucket) in other.failures.drain() {
            self.failure_bucket(key).merge(bucket);
        }
        self
    }

    pub fn mean_squared_error(&self) -> BigInt {
        if self.successes == 0 {
            BigInt::from(0)
        } else {
            self.total_squared_err.clone() / BigInt::from(self.successes)
        }
    }

    fn failure_bucket(&mut self, name: &'static str) -> &mut FailureBucket {
        self.failures
            .entry(name)
            .or_insert_with(|| FailureBucket::new(3))
    }
}

fn cost_one(model: &CostModel, query: Query) -> CostOne {
    let cost_per_effort: u64 = 1000;
    let cost = model.cost(&query.query);
    let expected = BigInt::from(query.effort as u64 * cost_per_effort);
    CostOne {
        actual: cost,
        expected,
        query,
    }
}

pub fn cost_many(model: &CostModel, entries: Vec<Query>) -> CostManyResult {
    //let count = AtomicUsize::new(0);
    //let total = entries.len();
    entries
        .into_par_iter()
        .map(|entry| {
            /*
            let count = count.fetch_add(1, Ordering::SeqCst);
            if count % 100000 == 0 {
                println!("{} / {}", count, total);
            }
            */
            cost_one(model, entry)
        })
        .fold(CostManyResult::default, |mut acc, value| {
            acc.add(value);
            acc
        })
        .reduce(CostManyResult::default, CostManyResult::merge)
}
