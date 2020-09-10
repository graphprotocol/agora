mod args;
mod log_loader;
mod model_loader;
mod reservoir;
mod runner;

use tree_buf::prelude::*;

const CHUNK_SIZE: usize = 262144 * 4;

fn main() {
    let args = args::load();

    if let Some(model) = &args.cost {
        cost_many(model, &args.load_log, args.sample);
    }

    if let Some(save_log) = &args.save_log {
        save(save_log, &args.load_log, args.sample);
    }

    // TODO: Ideas:
    // Group data in each category by both aggregate and shape hash
    // Command-line arguments for:
    //    filter shape hash
    //    saving output (as tree-buf)
    // Comparing results across runs
}

fn cost_many(model: &str, logs: &[String], sample: f64) {
    let model = model_loader::load(model);
    let mut result: runner::CostManyResult = Default::default();
    for chunk in log_loader::load_all_chunks(logs, sample) {
        let update = runner::cost_many(&model, chunk);
        result = result.merge(update);
    }

    println!("{}", &result);
}

fn save(path: &str, logs: &[String], sample: f64) {
    use std::{fs::File, io::Write};
    let mut out_chunk = Vec::new();
    let mut out_file = File::create(path).unwrap();

    let mut flush = move |data: &mut Vec<Query>| {
        data.sort_unstable();
        let bin = encode(data);
        let size = (bin.len() as u64).to_le_bytes();
        out_file.write_all(&size).unwrap();
        out_file.write_all(&bin).unwrap();
    };

    for mut chunk in log_loader::load_all_chunks(logs, sample) {
        if chunk.len() >= CHUNK_SIZE {
            flush(&mut chunk);
        } else {
            out_chunk.extend(chunk);
            if out_chunk.len() >= CHUNK_SIZE {
                flush(&mut out_chunk);
                out_chunk.clear();
            }
        }
    }
    if out_chunk.len() > 0 {
        flush(&mut out_chunk);
    }
}

#[derive(Encode, Decode, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct Query {
    query: String,
    variables: String,
    effort: u32,
}
