mod args;
mod contest;
mod errors;
mod log_loader;
mod model_loader;
mod runner;

use anyhow::{anyhow, Result};
use errors::WithPath;
use num_bigint::BigUint;
use tree_buf::prelude::*;

const CHUNK_SIZE_HINT: usize = 262144 * 4;

fn execute_args(args: args::Args) -> Result<()> {
    if let Some(model) = &args.cost {
        // Convert decimal GRT to wei
        let grt_per_time = args
            .grt_per_time
            .as_ref()
            .map(|s| {
                let real = cost_model::parse_real(s)?;
                let cost = cost_model::fract_to_cost(real)
                    .map_err(|()| anyhow!("Failed to convert --grt-per-time to wei"))?;
                Result::<_>::Ok(cost)
            })
            .transpose()?;

        cost_many(
            model,
            args.globals.as_deref(),
            &args.load_log,
            args.sample,
            grt_per_time.as_ref(),
        )?;
    }

    if let Some(save_log) = &args.save_log {
        save(save_log, &args.load_log, args.sample)?;
    }

    Ok(())
}

/// Loads, processes, and saves query logs. This can...
/// * Load multiple log files in treebuf and/or jsonl format.
/// * Load a cost model, cost each log and report on various metrics
/// * Sample logs
/// * Save logs in the treebuf format
/// For usage details, see the command-line help
fn main() {
    let args = args::load();

    match execute_args(args) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("Failed with error: {}", e);
            std::process::exit(1);
        }
    }

    // TODO: Ideas:
    // Group data in each category by both aggregate and shape hash
    // Command-line arguments for:
    //    filter shape hash
    //    bucket size? Might be important if running on very large queries and using too much memory.
    //    filter by subgraph id
    // Comparing results across runs
}

fn cost_many(
    model: &str,
    globals: Option<&str>,
    logs: &[String],
    sample: f64,
    grt_per_time: Option<&BigUint>,
) -> Result<()> {
    let model = model_loader::load(model, globals)?;
    let mut result: runner::QueryCostSummary = Default::default();
    for chunk in log_loader::load_all_chunks::<runner::Query>(logs, sample) {
        let update = runner::cost_many(&model, chunk?, grt_per_time);
        result = result.merge(update);
    }

    println!("{}", &result);
    Ok(())
}

fn save(path: &str, logs: &[String], sample: f64) -> Result<()> {
    use std::{fs::File, io::Write};
    let mut out_chunk = Vec::new();
    let mut out_file = WithPath::context(path, |p| File::create(p))?;

    let mut flush = move |data: &mut Vec<log_loader::Query>| {
        // Makes the file smaller
        data.sort_unstable();
        let bin = encode(data);
        let size = (bin.len() as u64).to_le_bytes();
        out_file.write_all(&size)?;
        out_file.write_all(&bin)?;
        Result::<_>::Ok(())
    };

    for chunk in log_loader::load_all_chunks::<log_loader::Query>(logs, sample) {
        let mut chunk = chunk?;
        if chunk.len() >= CHUNK_SIZE_HINT {
            flush(&mut chunk)?;
        } else {
            out_chunk.extend(chunk);
            if out_chunk.len() >= CHUNK_SIZE_HINT {
                flush(&mut out_chunk)?;
                out_chunk.clear();
            }
        }
    }
    if !out_chunk.is_empty() {
        flush(&mut out_chunk)?;
    }

    Ok(())
}
