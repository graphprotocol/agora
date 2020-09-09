#![allow(dead_code)]

use crate::Query;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Write};
use tree_buf::prelude::*;

#[derive(Deserialize)]
struct JsonQuery {
    subgraph: String,
    query: String,
    variables: String,
    effort: u32,
}

pub(crate) fn preprocess_queries() {
    let root = shellexpand::tilde("~/Downloads/queries/").to_string();
    let mut path = root.clone();
    path.push_str("2020-08-07.json");

    let file = File::open(&path).unwrap();
    let lines = io::BufReader::new(file).lines();

    let mut per_subgraph = HashMap::<String, Vec<Query>>::new();
    let mut line_count = 0;

    for line in lines {
        line_count += 1;
        if line_count % 500000 == 0 {
            dbg!(line_count);
        }
        let line = line.unwrap();
        let deserialized: JsonQuery = serde_json::from_str(&line).unwrap();
        let JsonQuery {
            subgraph,
            query,
            variables,
            effort,
        } = deserialized;

        let query = Query {
            query,
            variables,
            effort,
        };

        let subgraph = per_subgraph.entry(subgraph).or_default();
        subgraph.push(query);
    }

    dbg!(per_subgraph.len());

    for (subgraph, mut queries) in per_subgraph.drain() {
        dbg!("Starting", &subgraph);
        // This makes the file smaller
        queries.sort_unstable();
        dbg!("Sorted", &subgraph);

        let tree_buf = encode(&queries);
        dbg!("Encoded", &subgraph);

        let mut path = root.clone();
        path.push_str(&subgraph);
        path.push_str(".treebuf");

        let mut file = File::create(path).unwrap();
        file.write_all(&tree_buf).unwrap();
        dbg!("Wrote file", &subgraph);
    }
}
