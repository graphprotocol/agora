use crate::contest::Contest;
use cost_model::{wei_to_grt, CostError, CostModel};
use fraction::BigFraction;
use num_bigint::{BigInt, BigUint};
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
    time: u32,
}

#[derive(Default)]
pub struct QueryCostSummary {
    successes: usize,
    total_grt: BigUint,
    /// The difference between the goal stated
    /// in GRT/time over all queries in the summary,
    /// and the total_grt in the summary.
    total_err: Option<BigInt>,
    total_squared_err: Option<BigInt>,
    failures: HashMap<&'static str, FailureBucket>,
}

fn fail_name(err: CostError) -> &'static str {
    match err {
        CostError::FailedToParseQuery => "Failed to parse query",
        CostError::QueryNotCosted => "Query not costed",
        CostError::QueryNotSupported => "Query not supported",
        CostError::QueryInvalid => "Query invalid",
        CostError::CostModelFail => "Cost model failure",
        CostError::FailedToParseVariables => "Failed to parse variables",
    }
}

impl fmt::Display for QueryCostSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Successes: {} queries",
            self.successes.to_formatted_string(&Locale::en)
        )?;

        let total_grt = BigFraction::new(self.total_grt.clone(), wei_to_grt());
        writeln!(f, "Total Value: {:.1$} GRT", total_grt, 2)?;

        if self.successes != 0 {
            if let Some(total_err) = &self.total_err {
                let mut total_err = BigFraction::from(total_err.clone());
                total_err *= BigFraction::new(BigUint::from(1u32), wei_to_grt());
                let per_query = total_err.clone()
                    * BigFraction::new(BigUint::from(1u32), BigUint::from(self.successes));
                writeln!(
                    f,
                    "Total Err: {:.2$} GRT. ({:.3$} GRT per query)",
                    total_err, per_query, 2, 5
                )?;
            }
        }
        if let Some(mse) = self.mean_squared_error() {
            let mut mse = BigFraction::from(mse);
            mse *= BigFraction::new(BigUint::from(1u32), wei_to_grt() * wei_to_grt());
            writeln!(f, "Mean Squared Err: {:.1$} GRT²", mse, 2)?;
        }

        writeln!(f)?;
        writeln!(
            f,
            "Failures: {}",
            self.failures
                .values()
                .map(|bucket| bucket.count)
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
    total_grt: BigUint,
    examples: Contest<Query>,
}

impl FailureBucket {
    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            total_grt: BigUint::from(0u32),
            examples: Contest::new(capacity),
        }
    }

    fn merge(&mut self, other: FailureBucket) {
        self.count += other.count;
        self.total_grt += other.total_grt;
        for example in other.examples.take() {
            self.examples.insert_unique(
                failed_query_complexity(&example),
                example,
                contest_query_cmp,
            )
        }
    }
}

fn failed_query_complexity(query: &Query) -> usize {
    // Does not underflow because that would imply going over the memory limit
    usize::MAX - query.query.len() - query.variables.len()
}

fn contest_query_cmp(a: &Query, b: &Query) -> bool {
    // Ignoring the time because that is partly random,
    // and ignoring the variables because they are meant to be
    // different but may not materially affect the query.
    a.query == b.query
}

impl fmt::Display for FailureBucket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let total_grt = BigFraction::new(self.total_grt.clone(), wei_to_grt());
        write!(
            f,
            "count: {} total_grt: {:.2$} GRT",
            self.count.to_formatted_string(&Locale::en),
            total_grt,
            2
        )
    }
}

struct CostedQuery {
    expected: Option<BigUint>,
    actual: Result<BigUint, CostError>,
    query: Query,
}

impl QueryCostSummary {
    fn add(&mut self, result: CostedQuery) {
        match result.actual {
            Ok(cost) => {
                if let Some(expected) = result.expected {
                    let err = BigInt::from(cost.clone()) - BigInt::from(expected);
                    *self.total_squared_err.get_or_insert_with(Default::default) +=
                        err.clone() * err.clone();
                    *self.total_err.get_or_insert_with(Default::default) += err;
                }
                self.successes += 1;
                self.total_grt += cost;
            }
            Err(e) => {
                let bucket = self.failure_bucket(fail_name(e));
                bucket.count += 1;
                if let Some(expected) = result.expected {
                    bucket.total_grt += expected;
                }
                bucket.examples.insert_unique(
                    failed_query_complexity(&result.query),
                    result.query,
                    contest_query_cmp,
                );
            }
        }
    }

    pub fn merge(mut self, mut other: Self) -> Self {
        self.successes += other.successes;
        self.total_grt += other.total_grt;
        if let Some(total_err) = other.total_err {
            *self.total_err.get_or_insert_with(Default::default) += total_err;
        }
        if let Some(total_squared_err) = other.total_squared_err {
            *self.total_squared_err.get_or_insert_with(Default::default) += total_squared_err;
        }
        for (key, bucket) in other.failures.drain() {
            self.failure_bucket(key).merge(bucket);
        }
        self
    }

    pub fn mean_squared_error(&self) -> Option<BigInt> {
        if let Some(total_squared_err) = &self.total_squared_err {
            if self.successes != 0 {
                return Some(total_squared_err.clone() / BigInt::from(self.successes));
            }
        }
        None
    }

    fn failure_bucket(&mut self, name: &'static str) -> &mut FailureBucket {
        self.failures
            .entry(name)
            .or_insert_with(|| FailureBucket::new(4))
    }
}

fn cost_one(model: &CostModel, query: Query, grt_per_time: Option<&BigUint>) -> CostedQuery {
    let cost = model.cost(&query.query, &query.variables);
    let expected = grt_per_time.map(|g| g * query.time);
    CostedQuery {
        actual: cost,
        expected,
        query,
    }
}

pub fn cost_many(
    model: &CostModel,
    entries: Vec<Query>,
    grt_per_time: Option<&BigUint>,
) -> QueryCostSummary {
    entries
        .into_par_iter()
        .map(|entry| cost_one(model, entry, grt_per_time))
        .fold(QueryCostSummary::default, |mut acc, value| {
            acc.add(value);
            acc
        })
        .reduce(QueryCostSummary::default, QueryCostSummary::merge)
}
