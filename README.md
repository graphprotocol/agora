# Agora


## The evaluation tool

The source for the evaluation tool lives under this repo at `./agora/`.

### Installation
To compile and run from source:

**Install Rustup**
If you don't have `cargo` already, install it by following the instructions here: https://www.rust-lang.org/tools/install

**Compile and Run**

The first step in using the tool is to compile it with `cargo`.

```shell
cd ./agora
cargo build --release
```

The tool will be compiled to `/target/release/`.

```shell
cd target/release/
./agora -h
```

This should print the available command-line arguments, which, at the time of this writing will look like this:

```
agora 0.1.0

USAGE:
    agora [OPTIONS]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -c, --cost <cost>                        A cost model to use for costing
        --globals <globals>                  
        --grt-per-effort <grt-per-effort>    
    -l, --load-log <load-log>...             Load request log file(s) supports json and tree-buf
        --sample <sample>                    Take a sample of the request log. Unit interval [default: 1.0]
        --save-log <save-log>                Save the request log file. Only tree-buf is supported
```

**Usage**

The arguments to the tool are meant to be combined to instruct `agora` to accomplish tasks. What follows are just examples:

```shell
# Load two log files (from json lines format)
# Sample the logs at 10%
# Save the result as a single tree-buf file
./agora \
   --load-log ./log1.jsonl \
   --load-log ./log2.jsonl \
   --sample 0.1 \
   --save-log ./logs.treebuf

# Load the sampled/combined file
# And evaluate the effectiveness of our pricing
./agora \
  --load-log ./logs.treebuf \
  --globals ./globals.json \
  --grt-per-effort 1000 \
  --cost ./cost-model.agora
```
