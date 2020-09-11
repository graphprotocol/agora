use crate::contest::Contest;
use cost_model::{CostError, CostModel};
use num_bigint::BigInt;
use num_format::{Locale, ToFormattedString as _};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use tree_buf::prelude::*;

#[derive(Encode, Decode, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct Query {
    query: String,
    variables: String,
    effort: u32,
}

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
        writeln!(
            f,
            "Successes: {} queries",
            self.successes.to_formatted_string(&Locale::en)
        )?;
        writeln!(
            f,
            "Total Value: {} GRT",
            self.total_value.to_formatted_string(&Locale::en)
        )?;
        writeln!(
            f,
            "Total Err: {} GRT. ({} GRT per query)",
            self.total_err.to_formatted_string(&Locale::en),
            (&self.total_err / self.successes).to_formatted_string(&Locale::en)
        )?;
        writeln!(
            f,
            "Mean Squared Err: {} GRT²",
            self.mean_squared_error().to_formatted_string(&Locale::en)
        )?;
        writeln!(f, "")?;
        writeln!(
            f,
            "Failures: {}",
            self.failures
                .iter()
                .map(|(_, bucket)| bucket.count)
                .sum::<usize>()
                .to_formatted_string(&Locale::en)
        )?;
        for (name, bucket) in self.failures.iter() {
            writeln!(f, "\t{:?} {}", name, bucket)?;
            for example in bucket.examples.cloned() {
                writeln!(
                    f,
                    "\t\t{} | variables: {}",
                    &example.query, &example.variables
                )?;
            }
        }

        Ok(())
    }
}

pub struct FailureBucket {
    count: usize,
    total_value: BigInt,
    examples: Contest<Query>,
}

fn score_for_query_fail(query: &Query) -> usize {
    // Does not underflow because that would imply going over the memory limit
    usize::MAX - query.query.len() - query.variables.len()
}

fn contest_query_cmp(a: &Query, b: &Query) -> bool {
    // Ignoring the effort because that is partly random,
    // and ignoring the variables because they are meant to be
    // different but may not materially affect the query.
    &a.query == &b.query /* && &a.variables == &b.variables*/
}

impl FailureBucket {
    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            total_value: BigInt::from(0),
            examples: Contest::new(capacity),
        }
    }
}

impl fmt::Display for FailureBucket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "count: {} total_value: {}",
            self.count.to_formatted_string(&Locale::en),
            self.total_value.to_formatted_string(&Locale::en)
        )
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
        for example in other.examples.take() {
            self.examples
                .insert_unique(score_for_query_fail(&example), example, contest_query_cmp)
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
                bucket.examples.insert_unique(
                    score_for_query_fail(&result.query),
                    result.query,
                    contest_query_cmp,
                );
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
            .or_insert_with(|| FailureBucket::new(4))
    }
}

fn cost_one(model: &CostModel, query: Query, grt_per_effort: &BigInt) -> CostOne {
    let cost = model.cost(&query.query);
    let expected = grt_per_effort * query.effort;
    CostOne {
        actual: cost,
        expected,
        query,
    }
}

pub fn cost_many(
    model: &CostModel,
    entries: Vec<Query>,
    grt_per_effort: &BigInt,
) -> CostManyResult {
    entries
        .into_par_iter()
        .map(|entry| cost_one(model, entry, grt_per_effort))
        .fold(CostManyResult::default, |mut acc, value| {
            acc.add(value);
            acc
        })
        .reduce(CostManyResult::default, CostManyResult::merge)
}
