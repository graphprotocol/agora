use crate::Query;
use rand::{thread_rng, Rng};
use serde::Deserialize;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Lines, Read};
use std::iter::from_fn;
use std::path::Path;
use std::time::Instant;
use tree_buf::decode;

pub fn load_all_chunks(logs: &[String], sample: f64) -> impl Iterator<Item = Vec<Query>> {
    let logs = logs.iter().cloned().collect::<Vec<_>>();
    let mut logs = logs.into_iter();
    let mut current_log: Option<AnyLoader> = logs.next().map(AnyLoader::new);
    let start = Instant::now();
    from_fn(move || {
        while let Some(log) = &mut current_log {
            if let Some(chunk) = log.load_chunk(sample) {
                println!("Starting {} at {:?}", chunk.len(), Instant::now() - start);
                return Some(chunk);
            } else {
                current_log = logs.next().map(AnyLoader::new);
            }
        }
        println!("Finished at {:?}", Instant::now() - start);
        None
    })
}

pub trait Loader {
    fn load_chunk(&mut self, sample: f64) -> Option<Vec<Query>>;
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

impl Loader for AnyLoader {
    fn load_chunk(&mut self, sample: f64) -> Option<Vec<Query>> {
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

#[derive(Deserialize)]
struct JsonQuery {
    query: String,
    variables: String,
    effort: u32,
}

impl Loader for JsonLoader {
    fn load_chunk(&mut self, sample: f64) -> Option<Vec<Query>> {
        let mut rand = thread_rng();

        let mut result = Vec::new();

        for line in &mut self.lines {
            let prob: f64 = rand.gen();
            if prob >= sample {
                continue;
            }

            // This is dropping the subgraph_id.
            // It might be desirable at some point to add it back in to support a
            // "filter by subgraph id" feature.
            // Probably the right thing to do here is just to make the loader generic,
            // so that the save/load pipeline can load the data or not. The subgraph id
            // shouldn't affect the tree-buf file size much because of de-duplication.
            let JsonQuery {
                query,
                variables,
                effort,
                ..
            } = serde_json::from_str(&line.unwrap()).unwrap();

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

            result.push(query);

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

impl Loader for TreeBufLoader {
    fn load_chunk(&mut self, sample: f64) -> Option<Vec<Query>> {
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

        let mut result: Vec<Query> = decode(&buf).unwrap();

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
