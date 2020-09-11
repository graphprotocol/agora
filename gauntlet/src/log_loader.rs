use rand::{thread_rng, Rng};
use serde::{
    de::DeserializeOwned,
    {Deserialize, Serialize},
};
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Lines, Read};
use std::iter::from_fn;
use std::path::Path;
use std::time::Instant;
use tree_buf::prelude::*;

pub fn load_all_chunks<T>(logs: &[String], sample: f64) -> impl Iterator<Item = Vec<T>>
where
    T: tree_buf::Decodable + DeserializeOwned,
    // TODO: Tree-buf should be able to figure out this bound
    Vec<T>: tree_buf::Decodable,
{
    let logs = logs.iter().cloned().collect::<Vec<_>>();
    let mut logs = logs.into_iter();
    let mut current_log: Option<AnyLoader> = logs.next().map(AnyLoader::new);
    let start = Instant::now();
    from_fn(move || {
        while let Some(log) = &mut current_log {
            if let Some(chunk) = log.load_chunk(sample) {
                println!(
                    "Starting {} queries at {:?}",
                    chunk.len(),
                    Instant::now() - start
                );
                return Some(chunk);
            } else {
                current_log = logs.next().map(AnyLoader::new);
            }
        }
        println!("Finished at {:?}", Instant::now() - start);
        None
    })
}

enum AnyLoader {
    JsonLoader(JsonLoader),
    TreeBufLoader(TreeBufLoader),
}

impl AnyLoader {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        match path.as_ref().extension().and_then(|ext| ext.to_str()) {
            Some("json") => Self::JsonLoader(JsonLoader::new(path)),
            Some("treebuf") => Self::TreeBufLoader(TreeBufLoader::new(path)),
            _ => panic!("Expecting json or treebuf file"),
        }
    }
}

impl AnyLoader {
    fn load_chunk<T: tree_buf::Decodable + DeserializeOwned>(
        &mut self,
        sample: f64,
    ) -> Option<Vec<T>>
    where
        Vec<T>: tree_buf::Decodable,
    {
        match self {
            Self::JsonLoader(inner) => inner.load_chunk(sample),
            Self::TreeBufLoader(inner) => inner.load_chunk(sample),
        }
    }
}

struct JsonLoader {
    lines: Lines<BufReader<File>>,
}

impl JsonLoader {
    fn new<P: AsRef<Path>>(path: P) -> Self {
        let f = File::open(path).unwrap();
        let f = BufReader::new(f);
        let lines = f.lines();
        Self { lines }
    }
}

#[derive(Serialize, Deserialize, Encode, Decode, Eq, PartialEq, Ord, PartialOrd, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Query {
    subgraph: String,
    query: String,
    variables: String,
    effort: u32,
}

impl JsonLoader {
    fn load_chunk<T: DeserializeOwned>(&mut self, sample: f64) -> Option<Vec<T>> {
        let mut rand = thread_rng();

        let mut result = Vec::new();

        for line in &mut self.lines {
            let prob: f64 = rand.gen();
            if prob >= sample {
                continue;
            }

            let deserialized = serde_json::from_str(&line.unwrap()).unwrap();

            /*
            // TODO: FIXME: Support these queries
            // Filter unsupported queries
            if &variables != "{}" || query.starts_with("fragment") {
                continue;
            }

            let query = Query {
                query,
                variables,
                effort,
            };
            */

            result.push(deserialized);

            if result.len() == crate::CHUNK_SIZE {
                break;
            }
        }
        if result.len() == 0 {
            None
        } else {
            Some(result)
        }
    }
}

struct TreeBufLoader {
    file: File,
}

impl TreeBufLoader {
    fn new<P: AsRef<Path>>(path: P) -> Self {
        let file = File::open(path).unwrap();
        Self { file }
    }
}

impl TreeBufLoader {
    fn load_chunk<T: tree_buf::Decodable>(&mut self, sample: f64) -> Option<Vec<T>>
    where
        Vec<T>: tree_buf::Decodable,
    {
        let mut chunk_size: [u8; 8] = Default::default();
        match self.file.read_exact(&mut chunk_size) {
            Ok(()) => (),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                return None;
            }
            err => err.unwrap(),
        }

        let chunk_size = u64::from_le_bytes(chunk_size);
        let mut buf = vec![0; chunk_size as usize];

        self.file.read_exact(&mut buf).unwrap();

        let mut result: Vec<T> = decode(&buf).unwrap();

        if sample < 1.0 {
            let mut rand = thread_rng();
            let mut i = result.len();
            while i != 0 {
                i -= 1;

                let prob: f64 = rand.gen();
                if prob >= sample {
                    result.swap_remove(i);
                }
            }
        }

        Some(result)
    }
}
