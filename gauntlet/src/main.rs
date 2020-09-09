mod preprocessor;
mod runner;
use cost_model::CostModel;
use std::time::Instant;
use tree_buf::prelude::*;

fn main() {
    let model = "
        query { tokens } => 1000000;
    ";
    let model = CostModel::compile(model).unwrap();

    let root = shellexpand::tilde("~/Downloads/queries/").to_string();
    let mut path = root.clone();
    path.push_str("QmXKwSEMirgWVn41nRzkT3hpUBw29cp619Gx58XW6mPhZP.treebuf");
    let start = Instant::now();
    println!("Starting");
    let data = std::fs::read(path).unwrap();
    println!("Read file {:?}", Instant::now() - start);
    let queries: Vec<Query> = tree_buf::decode(&data).unwrap();
    println!(
        "Decoded queries, {:?}, {}",
        Instant::now() - start,
        queries.len()
    );
    drop(data);
    println!("Dropped binary {:?}", Instant::now() - start);

    // TODO: FIXME: Support these queries
    // Filter unsupported queries
    let queries: Vec<_> = queries
        .into_iter()
        .filter(|q| &q.variables == "{}" && !q.query.starts_with("fragment"))
        .collect();

    println!(
        "Filtered unsupported: {:?}, {}",
        Instant::now() - start,
        queries.len()
    );

    // Take 1/10 sample
    let queries = queries
        .into_iter()
        .enumerate()
        .filter_map(|(i, query)| if (i % 10) == 0 { Some(query) } else { None })
        .collect();

    // TODO: Ideas:
    // Group data in each category by both aggregate and shape hash
    // Command-line arguments for:
    //    loading queries (tree-buf or qlog)
    //    sample-percent
    //    filter shape hash
    //    saving output (as tree-buf)
    //    running cost model
    // Comparing results across runs

    let result = runner::cost_many(&model, queries);

    println!("Finished {:?}", Instant::now() - start);

    println!("{}", &result);
}

#[derive(Encode, Decode, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Query {
    query: String,
    variables: String,
    effort: u32,
}
