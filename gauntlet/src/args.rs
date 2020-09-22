use num_bigint::BigUint;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Args {
    /// Load request log file(s) supports json and tree-buf.
    #[structopt(short, long)]
    pub load_log: Vec<String>,

    /// Save the request log file. Only tree-buf is supported.
    #[structopt(long)]
    pub save_log: Option<String>,

    /// Take a sample of the request log. Unit interval.
    #[structopt(long, default_value = "1.0")]
    pub sample: f64,

    /// A cost model to use for costing
    #[structopt(long, short, requires("grt-per-effort"))]
    pub cost: Option<String>,

    #[structopt(long, short)]
    pub grt_per_effort: Option<BigUint>,
}

pub fn load() -> Args {
    Args::from_args()
}
