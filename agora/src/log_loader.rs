use crate::errors::WithPath;
use anyhow::Result;
use libflate::gzip::Decoder;
use rand::{thread_rng, Rng};
use serde::{
    de::DeserializeOwned,
    {Deserialize, Serialize},
};
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Lines, Read};
use std::marker::PhantomData;
use std::path::Path;
use std::time::Instant;
use tree_buf::prelude::*;

struct ChunkLoader<T> {
    start: Instant,
    logs: <Vec<String> as IntoIterator>::IntoIter,
    sample: f64,
    current: Option<AnyLoader>,
    _marker: PhantomData<*const T>,
}

impl<T> ChunkLoader<T> {
    fn elapsed(&self) -> std::time::Duration {
        Instant::now() - self.start
    }
}

impl<T> Iterator for ChunkLoader<T>
where
    T: tree_buf::Decodable + DeserializeOwned,
    // TODO: Tree-buf should be able to figure out this bound
    Vec<T>: tree_buf::Decodable,
{
    type Item = Result<Vec<T>>;

    fn next(&mut self) -> Option<Result<Vec<T>>> {
        loop {
            // Use the current file, if any
            match &mut self.current {
                // Load chunk from file and continue to next file if need be
                Some(current) => match current.load_chunk(self.sample) {
                    Ok(Some(sample)) => {
                        println!("{:?} Loaded {} queries", self.elapsed(), sample.len());
                        return Some(Ok(sample));
                    }
                    Ok(None) => self.current = None,
                    Err(e) => return Some(Err(e)),
                },
                // Load next file
                None => {
                    match self.logs.next() {
                        Some(log) => {
                            println!("{:?}, Loading file: {}", self.elapsed(), &log);
                            match AnyLoader::new(log) {
                                Ok(loader) => self.current = Some(loader),
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        // No more logs, all done.
                        None => {
                            println!("{:?} Finished", self.elapsed());
                            return None;
                        }
                    }
                }
            }
        }
    }
}

pub fn load_all_chunks<T>(logs: &[String], sample: f64) -> impl Iterator<Item = Result<Vec<T>>>
where
    T: tree_buf::Decodable + DeserializeOwned,
    // TODO: Tree-buf should be able to figure out this bound
    Vec<T>: tree_buf::Decodable,
{
    let start = Instant::now();
    let logs = logs.to_vec();
    let logs = logs.into_iter();

    println!();

    ChunkLoader {
        start,
        logs,
        current: None,
        _marker: PhantomData,
        sample,
    }
}

#[derive(Serialize, Deserialize, Encode, Decode, Eq, PartialEq, Ord, PartialOrd, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Query {
    subgraph: String,
    query: String,
    variables: String,
    time: u32,
}

enum AnyLoader {
    Gz(JsonLinesLoader<Decoder<File>>),
    Json(JsonLinesLoader<File>),
    TreeBuf(TreeBufLoader),
}

impl AnyLoader {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let file = WithPath::context(&path, |p| File::open(p))?;
        let s = match path.as_ref().extension().and_then(|ext| ext.to_str()) {
            Some("gz") => Self::Gz(JsonLinesLoader::new(Decoder::new(file)?)?),
            Some("jsonl") => Self::Json(JsonLinesLoader::new(file)?),
            Some("treebuf") => Self::TreeBuf(TreeBufLoader::new(file)?),
            _ => panic!("Expecting json or treebuf file"),
        };
        Ok(s)
    }

    fn load_chunk<T: tree_buf::Decodable + DeserializeOwned>(
        &mut self,
        sample: f64,
    ) -> Result<Option<Vec<T>>>
    where
        Vec<T>: tree_buf::Decodable,
    {
        match self {
            Self::Json(inner) => inner.load_chunk(sample),
            Self::TreeBuf(inner) => inner.load_chunk(sample),
            Self::Gz(inner) => inner.load_chunk(sample),
        }
    }
}

struct JsonLinesLoader<T> {
    lines: Lines<BufReader<T>>,
}

impl<T> JsonLinesLoader<T>
where
    T: Read,
{
    fn new(file: T) -> Result<Self> {
        let file = BufReader::new(file);
        let lines = file.lines();
        Ok(Self { lines })
    }

    fn load_chunk<V: DeserializeOwned>(&mut self, sample: f64) -> Result<Option<Vec<V>>> {
        let mut rand = thread_rng();

        let mut result = Vec::new();

        for line in &mut self.lines {
            let prob: f64 = rand.gen();
            if prob >= sample {
                continue;
            }

            let deserialized = serde_json::from_str(&line?)?;

            result.push(deserialized);

            if result.len() == crate::CHUNK_SIZE_HINT {
                break;
            }
        }
        Ok(if result.is_empty() {
            None
        } else {
            Some(result)
        })
    }
}

struct TreeBufLoader {
    file: File,
}

impl TreeBufLoader {
    fn new(file: File) -> Result<Self> {
        Ok(Self { file })
    }

    fn load_chunk<T: tree_buf::Decodable>(&mut self, sample: f64) -> Result<Option<Vec<T>>>
    where
        Vec<T>: tree_buf::Decodable,
    {
        let mut chunk_size: [u8; 8] = Default::default();

        match self.file.read_exact(&mut chunk_size) {
            Ok(()) => (),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // TODO: This isn't quite right, because we should
                // err bytes read is not 0.
                return Ok(None);
            }
            err => err?,
        }

        let chunk_size = u64::from_le_bytes(chunk_size);
        let mut buf = vec![0; chunk_size as usize];

        self.file.read_exact(&mut buf)?;

        let mut result: Vec<T> = decode(&buf)?;

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

        Ok(Some(result))
    }
}
