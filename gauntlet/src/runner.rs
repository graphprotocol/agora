use crate::Query;
use cost_model::{CostError, CostModel};
use num_bigint::BigInt;
use rayon::prelude::*;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Eq, PartialEq, Default)]
pub struct CostManyResult {
    successes: usize,
    total_value: BigInt,
    total_err: BigInt,
    total_squared_err: BigInt,
    // TODO: Put these in a HashMap
    fail_cost_model_fail: FailureBucket,
    fail_query_not_costed: FailureBucket,
    fail_failed_to_parse_query: FailureBucket,
    fail_query_not_supported: FailureBucket,
}

impl fmt::Display for CostManyResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Successes: {}", self.successes)?;
        writeln!(f, "Total Value: {}", self.total_value)?;
        writeln!(f, "Total Err: {}", self.total_err)?;
        writeln!(f, "Mean Squared Err: {}", self.mean_squared_error())?;
        writeln!(f, "")?;
        writeln!(f, "Failures:")?;
        writeln!(f, "CostModelFail: {}", self.fail_cost_model_fail)?;
        writeln!(f, "QueryNotSupported: {}", self.fail_query_not_supported)?;
        writeln!(f, "FailedToParseQuery: {}", self.fail_failed_to_parse_query)?;
        writeln!(f, "QueryNotCosted: {}", self.fail_query_not_costed)?;

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct FailureBucket {
    count: usize,
    total_value: BigInt,
}

impl fmt::Display for FailureBucket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "count: {} total_value: {}", self.count, self.total_value)
    }
}

struct CostOne {
    expected: BigInt,
    actual: Result<BigInt, CostError>,
}

impl FailureBucket {
    fn merge(&mut self, other: FailureBucket) {
        self.count += other.count;
        self.total_value += other.total_value;
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
                let bucket = self.failure_bucket(e);
                bucket.count += 1;
                bucket.total_value = result.expected;
            }
        }
    }
    fn merge(mut self, other: Self) -> Self {
        self.successes += other.successes;
        self.total_value += other.total_value;
        self.total_err += other.total_err;
        self.total_squared_err += other.total_squared_err;
        self.fail_cost_model_fail.merge(other.fail_cost_model_fail);
        self.fail_failed_to_parse_query
            .merge(other.fail_failed_to_parse_query);
        self.fail_query_not_costed
            .merge(other.fail_query_not_costed);
        self.fail_query_not_supported
            .merge(other.fail_query_not_supported);
        self
    }

    pub fn mean_squared_error(&self) -> BigInt {
        if self.successes == 0 {
            BigInt::from(0)
        } else {
            self.total_squared_err.clone() / BigInt::from(self.successes)
        }
    }

    fn failure_bucket(&mut self, err: CostError) -> &mut FailureBucket {
        match err {
            CostError::CostModelFail => &mut self.fail_cost_model_fail,
            CostError::QueryNotCosted => &mut self.fail_query_not_costed,
            CostError::FailedToParseQuery => &mut self.fail_failed_to_parse_query,
            CostError::QueryNotSupported => &mut self.fail_query_not_supported,
        }
    }
}

fn cost_one(model: &CostModel, entry: &Query) -> CostOne {
    let cost_per_effort: u64 = 1000;
    let cost = model.cost(&entry.query);
    let expected = BigInt::from(entry.effort as u64 * cost_per_effort);
    CostOne {
        actual: cost,
        expected,
    }
}

pub fn cost_many(model: &CostModel, entries: Vec<Query>) -> CostManyResult {
    let count = AtomicUsize::new(0);
    let total = entries.len();
    entries
        .par_iter()
        .map(|entry| {
            let count = count.fetch_add(1, Ordering::SeqCst);
            if count % 500000 == 0 {
                dbg!(count, total);
            }
            cost_one(model, entry)
        })
        .fold(CostManyResult::default, |mut acc, value| {
            acc.add(value);
            acc
        })
        .reduce(CostManyResult::default, CostManyResult::merge)
}
