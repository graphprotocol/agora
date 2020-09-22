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
pub struct QueryCostSummary {
    successes: usize,
    total_grt: BigInt,
    /// The difference between the goal stated
    /// in GRT/effort over all queries in the summary,
    /// and the total_grt in the summary.
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
        writeln!(
            f,
            "Total Value: {} GRT",
            self.total_grt.to_formatted_string(&Locale::en)
        )?;
        if self.successes != 0 {
            writeln!(
                f,
                "Total Err: {} GRT. ({} GRT per query)",
                self.total_err.to_formatted_string(&Locale::en),
                (&self.total_err / self.successes).to_formatted_string(&Locale::en)
            )?;
        }
        if let Some(mse) = self.mean_squared_error() {
            writeln!(
                f,
                "Mean Squared Err: {} GRT²",
                mse.to_formatted_string(&Locale::en)
            )?;
        }

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
    total_grt: BigInt,
    examples: Contest<Query>,
}

impl FailureBucket {
    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            total_grt: BigInt::from(0),
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
    // Ignoring the effort because that is partly random,
    // and ignoring the variables because they are meant to be
    // different but may not materially affect the query.
    &a.query == &b.query
}

impl fmt::Display for FailureBucket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "count: {} total_grt: {}",
            self.count.to_formatted_string(&Locale::en),
            self.total_grt.to_formatted_string(&Locale::en)
        )
    }
}

struct CostedQuery {
    expected: BigInt,
    actual: Result<BigInt, CostError>,
    query: Query,
}

impl QueryCostSummary {
    fn add(&mut self, result: CostedQuery) {
        match result.actual {
            Ok(cost) => {
                let err = cost.clone() - result.expected;
                self.successes += 1;
                self.total_grt += cost;
                self.total_squared_err += err.clone() * err.clone();
                self.total_err += err;
            }
            Err(e) => {
                let bucket = self.failure_bucket(fail_name(e));
                bucket.count += 1;
                bucket.total_grt += result.expected;
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
        self.total_err += other.total_err;
        self.total_squared_err += other.total_squared_err;
        for (key, bucket) in other.failures.drain() {
            self.failure_bucket(key).merge(bucket);
        }
        self
    }

    pub fn mean_squared_error(&self) -> Option<BigInt> {
        if self.successes == 0 {
            None
        } else {
            Some(self.total_squared_err.clone() / BigInt::from(self.successes))
        }
    }

    fn failure_bucket(&mut self, name: &'static str) -> &mut FailureBucket {
        self.failures
            .entry(name)
            .or_insert_with(|| FailureBucket::new(4))
    }
}

fn cost_one(model: &CostModel, query: Query, grt_per_effort: &BigInt) -> CostedQuery {
    let cost = model.cost(&query.query, &query.variables);
    let expected = grt_per_effort * query.effort;
    CostedQuery {
        actual: cost,
        expected,
        query,
    }
}

pub fn cost_many(
    model: &CostModel,
    entries: Vec<Query>,
    grt_per_effort: &BigInt,
) -> QueryCostSummary {
    entries
        .into_par_iter()
        .map(|entry| cost_one(model, entry, grt_per_effort))
        .fold(QueryCostSummary::default, |mut acc, value| {
            acc.add(value);
            acc
        })
        .reduce(QueryCostSummary::default, QueryCostSummary::merge)
}
